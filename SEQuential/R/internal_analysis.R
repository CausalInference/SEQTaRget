#' Internal analysis tool for handling parallelization/bootstrapping on multiple OS types
#'
#'
#' @import data.table future doFuture doRNG future.apply
#' @keywords internal
internal.analysis <- function(params) {
  # Variable pre-definition ===================================
  trial <- NULL
  denominator <- NULL
  numerator <- NULL
  wt <- NULL
  tmp <- NULL
  weight <- cense1 <- NULL
  followup <- NULL
  isExcused <- NULL
  #TODO cleanup unused columns before they go to internal.analysis

  handler <- function(DT, data, params) {
    if (!params@weighted) {
      model <- internal.model(DT, params)
    } else if (params@weighted) {
      WT <- internal.weights(DT, data, params)

      if (params@pre.expansion) {
        if (params@excused) {
          params@time <- "period"
          WDT <- DT[WT@weights, on = c(eval(params@id), eval(params@time)), nomatch = NULL
                    ][get(params@time) == 0 & trial == 0, denominator := 1
                      ][denominator < 1e-15, denominator := 1
                        ][is.na(get(params@outcome)), denominator := 1
                          ][, wt := numerator / denominator
                            ][, tmp := cumsum(ifelse(is.na(isExcused), 0, isExcused)), by = c(eval(params@id), "trial")
                              ][tmp > 0, wt := 1, by = c(eval(params@id), "trial")
                                ][, weight := cumprod(ifelse(is.na(wt), 1, wt)), by = c(eval(params@id), "trial")
                                  ][, weight := weight[1], list(cumsum(!is.na(weight)))]
        } else {
          params@time <- "period"
          WDT <- DT[WT@weights, on = c(eval(params@id), eval(params@time)), nomatch = NULL
                    ][get(params@time) == 0 & trial == 0, `:=`(numerator = 1,
                                                               denominator = 1)
                      ][, wt := numerator / denominator
                        ][, weight := cumprod(wt), by = c(eval(params@id), "trial")]
        }
      } else {
        if (params@excused) {
          params@time <- "period"
          WDT <- DT[WT@weights, on = c(eval(params@id), eval(params@time), "trial"), nomatch = NULL
                    ][followup == 0, `:=`(numerator = 1,
                                          denominator = 1)
                      ][denominator < 1e-15, denominator := 1
                        ][numerator < 1e-15, numerator := 1
                          ][is.na(get(params@outcome)), denominator := 1
                            ][, wt := numerator / denominator
                              ][, tmp := cumsum(ifelse(is.na(isExcused), 0, isExcused)), by = c(eval(params@id), "trial")
                                ][tmp > 0, wt := 1, by = c(eval(params@id), "trial")
                                  ][, weight := cumprod(ifelse(is.na(wt), 1, wt)), by = c(eval(params@id), "trial")
                                    ][, weight := weight[1], list(cumsum(!is.na(weight)))]
        } else {
          params@time <- "period"
          WDT <- DT[WT@weights, on = c(eval(params@id), eval(params@time), "trial"), nomatch = NULL
                    ][followup == 0, `:=`(numerator = 1,
                                          denominator = 1)
                      ][, wt := numerator / denominator
                        ][, weight := cumprod(wt), by = c(eval(params@id), "trial")]
        }
      }
      if (params@LTFU) WDT <- WDT[, weight := weight * cense1]

      percentile <- quantile(WDT[!is.na(get(params@outcome))]$weight, probs = c(.01, .25, .5, .75, .99), na.rm = TRUE)
      stats <- list(
        n0.coef = WT@coef.n0,
        n1.coef = WT@coef.n1,
        d0.coef = WT@coef.d0,
        d1.coef = WT@coef.d1,
        min = min(WDT[!is.na(get(params@outcome))]$weight, na.rm = TRUE),
        max = max(WDT[!is.na(get(params@outcome))]$weight, na.rm = TRUE),
        sd = sd(WDT[!is.na(get(params@outcome))]$weight, na.rm = TRUE),
        p01 = percentile[[1]],
        p25 = percentile[[2]],
        p50 = percentile[[3]],
        p75 = percentile[[4]],
        p99 = percentile[[5]]
      )
      if (params@p99.weight) {
        params@lower.weight <- stats$p01
        params@upper.weight <- stats$p99
      }
      model <- internal.model(WDT, params)
    }
    return(list(
      model = model,
      weighted_stats = if (params@weighted) stats else NA
    ))
  }

  if (params@bootstrap) cat("Bootstrapping with", params@boot.sample * 100, "% of data", params@nboot, "times\n")
  UIDs <- unique(params@DT[[params@id]])
  lnID <- length(UIDs)

  if (params@parallel) {
    setDTthreads(1)

    result <- future_lapply(1:params@nboot, function(x) {
      if (params@nboot > 1) {
        id.sample <- sample(UIDs, round(params@boot.sample * lnID), replace = TRUE)
      } else {
        id.sample <- UIDs
      }

      RMDT <- rbindlist(lapply(seq_along(id.sample), function(x) params@DT[get(params@id) == id.sample[x],
                                                                           ][, eval(params@id) := paste0(get(params@id), "_", x)]))
      RMdata <- rbindlist(lapply(seq_along(id.sample), function(x) params@data[get(params@id) == id.sample[x],
                                                                               ][, eval(params@id) := paste0(get(params@id), "_", x)]))

      model <- handler(RMDT, RMdata, params)

      return(list(
        model = model$model,
        weight_info = model$weighted_stats
      ))
    }, future.seed = params@seed)
  } else {
    result <- lapply(1:params@nboot, function(x) {
      if (params@bootstrap) {
        if (params@nboot > 1) {
          id.sample <- sample(UIDs, round(params@boot.sample * lnID), replace = TRUE)
        } else {
          id.sample <- UIDs
        }
        RMDT <- rbindlist(lapply(seq_along(id.sample), function(x) params@DT[get(params@id) == id.sample[x],
                                                                             ][, eval(params@id) := paste0(get(params@id), "_", x)]))
        RMdata <- rbindlist(lapply(seq_along(id.sample), function(x) params@data[get(params@id) == id.sample[x],
                                                                                 ][, eval(params@id) := paste0(get(params@id), "_", x)]))
      } else {
        RMDT <- params@DT
        RMdata <- params@data
      }

      model <- handler(RMDT, RMdata, params)
      return(list(
        model = model$model,
        weight_info = model$weighted_stats
      ))
    })
  }
  return(result)
}
