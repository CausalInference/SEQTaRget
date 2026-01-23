#' Create formula cache
#' @param params parameter from SEQuential
#' @keywords internal
init_formula_cache <- function(params) {
  
  cache <- new.env(hash = TRUE, parent = emptyenv())
  
  # Detect simple formulas (no interactions, no I(), no poly(), no factors)
  is_simple_additive <- function(covs) {
    if (is.null(covs) || is.na(covs) || covs == "") return(FALSE)
    !grepl(":|I\\(|poly\\(|factor\\(|as\\.factor|ns\\(|bs\\(|\\^", covs)
  }
  
  # Helper to parse formula string and extract columns
  parse_covs <- function(covs) {
    if (is.null(covs) || is.na(covs) || covs == "") return(NULL)
    # Clean column names by trimming whitespace
    cols <- unique(trimws(unlist(strsplit(covs, "\\+|\\*|\\:"))))
    cols <- cols[nchar(cols) > 0]
    list(
      formula = as.formula(paste0("~", covs)),
      cols = cols,
      is_simple = is_simple_additive(covs)
    )
  }
  
  # Default case - numerator/denominator
  cache$numerator <- parse_covs(params@numerator)
  cache$denominator <- parse_covs(params@denominator)
  
  # LTFU case
  cache$cense_numerator <- parse_covs(params@cense.numerator)
  cache$cense_denominator <- parse_covs(params@cense.denominator)
  
  # Visit case
  cache$visit_numerator <- parse_covs(params@visit.numerator)
  cache$visit_denominator <- parse_covs(params@visit.denominator)
  
  # Surv/outcome case
  cache$covariates <- parse_covs(params@covariates)
  
  # Store indicator strings for quick access
  cache$time_sq_col <- paste0(params@time, params@indicator.squared)
  cache$tx_bas <- paste0(params@treatment, params@indicator.baseline)
  
  return(cache)
}

#' Internal analysis tool for handling parallelization/bootstrapping on multiple OS types
#'
#'
#' @import data.table future doFuture doRNG future.apply
#' @keywords internal
internal.analysis <- function(params) {
  result <- local({
    on.exit({
      rm(list = setdiff(ls(), "result"))
    }, add = TRUE)

    formula_cache <- init_formula_cache(params)
    
    trial <- trial.first <- NULL
    numerator <- denominator <- NULL
    wt <- weight <- cense1 <- visit <- NULL
    tmp <- NULL
    followup <- NULL
    isExcused <- NULL

    handler <- function(DT, data, params) {
      if (!params@weighted) {
        model <- internal.model(DT, params)
        WDT <- data.table()
      } else if (params@weighted) {
        WT <- internal.weights(DT, data, params, formula_cache)

        if (params@weight.preexpansion) {
          if (params@excused | params@deviation.excused) {
            params@time <- "period"
            WDT <- DT[WT@weights, on = c(eval(params@id), eval(params@time)), nomatch = NULL
                      ][get(params@time) == 0 & trial == 0, denominator := 1
                        ][denominator < 1e-15, denominator := 1
                          ][is.na(get(params@outcome)), denominator := 1
                            ][, wt := numerator / denominator
                              ][is.na(wt), wt := 1
                                ][followup == 0, wt := 1
                                  ][, tmp := cumsum(ifelse(is.na(isExcused), 0, isExcused)), by = c(eval(params@id), "trial")
                                   ][tmp > 0, wt := 1, by = c(eval(params@id), "trial")
                                     ][, weight := cumprod(ifelse(is.na(wt), 1, wt)), by = c(eval(params@id), "trial")
                                       ][, weight := weight[1], list(cumsum(!is.na(weight)))]
          } else {
            # Static - pre-expansion
            params@time <- "period"
            WDT <- DT[WT@weights, on = c(eval(params@id), eval(params@time)), nomatch = NULL
                      ][, trial.first := min(trial), by = c(params@id)
                        ][followup == 0 & trial == trial.first, `:=`(numerator = 1, denominator = 1)
                          ][, wt := numerator / denominator
                            ][is.na(wt), wt := 1
                              ][, weight := cumprod(wt), by = c(eval(params@id), "trial")
                                ][, trial.first := NULL]
          }
        } else {
          if (params@excused | params@deviation.excused) {
            params@time <- "period"
            WDT <- DT[WT@weights, on = c(eval(params@id), eval(params@time), "trial"), nomatch = NULL
                      ][followup == 0, `:=`(numerator = 1, denominator = 1)
                        ][denominator < 1e-15, denominator := 1
                          ][numerator < 1e-15, numerator := 1
                            ][is.na(get(params@outcome)), denominator := 1
                              ][, wt := numerator / denominator
                                ][is.na(wt), wt := 1
                                  ][followup == 0, wt := 1
                                    ][, tmp := cumsum(ifelse(is.na(isExcused), 0, isExcused)), by = c(eval(params@id), "trial")
                                      ][tmp > 0, wt := 1, by = c(eval(params@id), "trial")
                                        ][, weight := cumprod(ifelse(is.na(wt), 1, wt)), by = c(eval(params@id), "trial")
                                          ][, weight := weight[1], list(cumsum(!is.na(weight)))]
          } else {
            # Static - post-expansion
            params@time <- "period"
            WDT <- DT[WT@weights, on = c(eval(params@id), eval(params@time), "trial"), nomatch = NULL
                      ][followup == 0, `:=`(numerator = 1, denominator = 1)
                        ][, wt := numerator / denominator
                          ][is.na(wt), wt := 1
                            ][followup == 0, wt := 1
                              ][, weight := cumprod(wt), by = c(eval(params@id), "trial")]
          }
        }
        if (params@LTFU) WDT[, weight := weight * cense1]
        if (!is.na(params@visit)) WDT[, weight := weight * visit]

        weights_valid <- WDT[!is.na(get(params@outcome)), weight]
        percentile <- quantile(weights_valid, probs = c(.01, .25, .5, .75, .99), na.rm = TRUE)
        stats <- list(
          coef.numerator = WT@coef.numerator,
          coef.denominator = WT@coef.denominator,
          ncense.coef = WT@coef.ncense,
          dcense.coef = WT@coef.dcense,
          min = min(weights_valid, na.rm = TRUE),
          max = max(weights_valid, na.rm = TRUE),
          sd = sd(weights_valid, na.rm = TRUE),
          p01 = percentile[[1]],
          p25 = percentile[[2]],
          p50 = percentile[[3]],
          p75 = percentile[[4]],
          p99 = percentile[[5]]
        )
        if (params@weight.p99) {
          params@weight.lower <- stats$p01
          params@weight.upper <- stats$p99
        }
        model <- internal.model(WDT, params)
      }
      return(list(
        model = model,
        weighted_stats = if (params@weighted) stats else NA,
        WDT = WDT
      ))
    }

    original_nrow <- nrow(params@DT)
    original_names <- names(params@DT)
    full <- handler(params@DT, params@data, params)
    stopifnot(identical(nrow(params@DT), original_nrow))
    stopifnot(identical(names(params@DT), original_names))
    
    if (params@bootstrap & params@verbose) cat("Bootstrapping with", params@bootstrap.sample * 100, "% of data", params@bootstrap.nboot, "times\n")
    UIDs <- unique(params@DT[[params@id]])
    lnID <- length(UIDs)

    if (!identical(key(params@DT), params@id)) setkeyv(params@DT, params@id)
    if (!identical(key(params@data), params@id)) setkeyv(params@data, params@id)
    
    # Helper function for efficient bootstrap resampling
    bootstrap_sample <- function(DT, data, params, UIDs, lnID) {
      n_sample <- round(params@bootstrap.sample * lnID)
    
      # Create lookup table with sampled IDs and unique suffixes
      id_lookup <- data.table(
        orig_id = sample(UIDs, n_sample, replace = TRUE),
        boot_idx = seq_len(n_sample)
      )
    
      # Integer ID: guaranteed unique if boot_idx < max_periods
      # e.g., orig_id * 1e6 + boot_idx, or use a pre-computed multiplier
      id_mult <- max(UIDs) + 1L
      
      # Single keyed join instead of n_sample separate filters
      RMDT <- DT[id_lookup, on = setNames("orig_id", params@id), allow.cartesian = TRUE
                 ][, (params@id) := as.integer(get(params@id) * id_mult + boot_idx)
                    ][, boot_idx := NULL]
      
      
      RMdata <- data[id_lookup, on = setNames("orig_id", params@id), allow.cartesian = TRUE
                     ][, (params@id) := as.integer(get(params@id) * id_mult + boot_idx)
                        ][, boot_idx := NULL]
      
      return(list(RMDT = RMDT, RMdata = RMdata))
    }
    
    bootstrap <- if (params@bootstrap) {
      if (params@parallel) {
        setDTthreads(1)
        future_lapply(seq_len(params@bootstrap.nboot), function(x) {
          bs <- bootstrap_sample(params@DT, params@data, params, UIDs, lnID)
          out <- handler(bs$RMDT, bs$RMdata, params)
          out$WDT <- NULL
          return(out)
        }, future.seed = if (length(params@seed) > 1) params@seed[1] else params@seed)
      } else {
        lapply(seq_len(params@bootstrap.nboot), function(x) {
          bs <- bootstrap_sample(params@DT, params@data, params, UIDs, lnID)
          out <- handler(bs$RMDT, bs$RMdata, params)
          out$WDT <- NULL
          return(out)
        })
      }
    } else {
      list()
    }

    result <- c(list(full), bootstrap)

    return(result)
  })
  return(result)
}
