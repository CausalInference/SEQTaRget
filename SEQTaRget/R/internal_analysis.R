#' Internal analysis tool for handling parallelization/bootstrapping on multiple OS types
#'
#'
#' @import data.table future doFuture doRNG future.apply
#' @keywords internal
internal.analysis <- function(params) {
  result <- local({
    on.exit({
      rm(list = setdiff(ls(), "result"))
      gc()
    }, add = TRUE)

    trial <- trial.first <- NULL
    numerator <- denominator <- NULL
    wt <- weight <- cense1 <- NULL
    tmp <- NULL
    followup <- NULL
    isExcused <- NULL

    handler <- function(DT, data, params) {
      if (!params@weighted) {
        model <- internal.model(DT, params)
        WDT <- data.table()
      } else if (params@weighted) {
        WT <- internal.weights(DT, data, params)

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
        if (params@LTFU) WDT <- WDT[, weight := weight * cense1]

        percentile <- quantile(WDT[!is.na(get(params@outcome))]$weight, probs = c(.01, .25, .5, .75, .99), na.rm = TRUE)
        stats <- list(
          coef.numerator = WT@coef.numerator,
          coef.denominator = WT@coef.denominator,
          ncense.coef = WT@coef.ncense,
          dcense.coef = WT@coef.dcense,
          min = min(WDT[!is.na(get(params@outcome))]$weight, na.rm = TRUE),
          max = max(WDT[!is.na(get(params@outcome))]$weight, na.rm = TRUE),
          sd = sd(WDT[!is.na(get(params@outcome))]$weight, na.rm = TRUE),
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

    full <- handler(copy(params@DT), copy(params@data), params)

    if (params@bootstrap & params@verbose) cat("Bootstrapping with", params@bootstrap.sample * 100, "% of data", params@bootstrap.nboot, "times\n")
    UIDs <- unique(params@DT[[params@id]])
    lnID <- length(UIDs)

    bootstrap <- if (params@bootstrap) {
      if (params@parallel) {
        setDTthreads(1)
        future_lapply(1:params@bootstrap.nboot, function(x) {
          id.sample <- sample(UIDs, round(params@bootstrap.sample * lnID), replace = TRUE)
          RMDT <- rbindlist(lapply(seq_along(id.sample), function(x) params@DT[get(params@id) == id.sample[x],
                                                                               ][, eval(params@id) := paste0(get(params@id), "_", x)]))
          RMdata <- rbindlist(lapply(seq_along(id.sample), function(x) params@data[get(params@id) == id.sample[x],
                                                                                   ][, eval(params@id) := paste0(get(params@id), "_", x)]))
          out <- handler(RMDT, RMdata, params)
          out$WDT <- NULL
          
          return(out)
        }, future.seed = if (length(params@seed) > 1) params@seed[1] else params@seed)
      } else {
        lapply(1:params@bootstrap.nboot, function(x) {
          id.sample <- sample(UIDs, round(params@bootstrap.sample * lnID), replace = TRUE)
          RMDT <- rbindlist(lapply(seq_along(id.sample), function(x) params@DT[get(params@id) == id.sample[x],
                                                                               ][, eval(params@id) := paste0(get(params@id), "_", x)]))
          RMdata <- rbindlist(lapply(seq_along(id.sample), function(x) params@data[get(params@id) == id.sample[x],
                                                                                   ][, eval(params@id) := paste0(get(params@id), "_", x)]))
          out <- handler(RMDT, RMdata, params)
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
