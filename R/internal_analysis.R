#' Internal analysis tool for handling parallelization/bootstrapping on multiple OS types
#'
#'
#' @import data.table future doFuture doRNG future.apply
#' @keywords internal
internal.analysis <- function(DT, data, method, id.col, time.col, eligible.col, outcome.col, treatment.col, opts){
        handler <- function(DT, data, id.col, time.col, eligible.col, outcome.col, treatment.col, opts){
          if(!opts$weighted){
            model <- internal.model(DT, method, outcome.col, opts)

          } else if(opts$weighted){
            WT <- internal.weights(DT, data, id.col, time.col, eligible.col, outcome.col, treatment.col, opts)

            if(opts$pre.expansion){
              if(opts$expand) time.col <- "period"
              WDT <- DT[WT$weighted_data, on = c(id.col, time.col)
                        ][get(time.col) == 0 & trial == 0, `:=` (numerator = 1,
                                                                 denominator = 1)
                          ][, `:=` (cprod.Numerator = cumprod(numerator),
                                  cprod.Denominator = cumprod(denominator)), by = c(id.col, "trial")
                            ][, weight := cprod.Numerator/cprod.Denominator]

              model <- internal.model(WDT, method, outcome.col, opts)
            } else {
              if(opts$expand) time.col <- "period"
              WDT <- DT[WT$weighted_data, on = c(id.col, time.col, "trial")
                        ][followup == 0, `:=` (numerator = 1,
                                               denominator = 1)
                          ][, `:=` (cprod.Numerator = cumprod(numerator),
                                  cprod.Denominator = cumprod(denominator)), by = c(id.col, "trial")
                            ][, weight := cprod.Numerator/cprod.Denominator]

              model <- internal.model(WDT, method, outcome.col, opts)
            }

          percentile <- quantile(WDT$weight, probs = c(.01, .25, .5, .75, .99))
          stats <- list(n0.coef = WT$coef.n0,
                        n1.coef = WT$coef.n1,
                        d0.coef = WT$coef.d0,
                        d1.coef = WT$coef.d1,
                        min = min(WDT$weight),
                        max = max(WDT$weight),
                        sd = sd(WDT$weight),
                        p01 = percentile[[1]],
                        p25 = percentile[[2]],
                        p50 = percentile[[3]],
                        p75 = percentile[[4]],
                        p99 = percentile[[5]])
          }
          return(list(model = model,
                 weighted_stats = if(opts$weighted){
                   stats
                 } else NA
                 ))
        }
  if(opts$bootstrap){
    cat("Bootstrapping", opts$boot.sample*100, "% of data", opts$nboot, "times\n")
    UIDs <- unique(DT[[id.col]])
    lnID <- length(UIDs)
    if(opts$parallel){
      setDTthreads(0)

      result <- future.apply::future_lapply(1:opts$nboot, function(x) {
        id.sample <- sample(UIDs,
                            round(opts$boot.sample*lnID), replace = FALSE)

        RMDT <- DT[get(id.col) %in% id.sample, ]
        RMdata <- data[get(id.col) %in% id.sample, ]

        model <- handler(RMDT, RMdata, id.col, time.col, eligible.col, outcome.col, treatment.col, opts)
        if(opts$boot.return == "coef") output <- coef(model$model)
        if(opts$boot.return == "full") output <- model$model
        if(opts$boot.return == "summary") output <- summary(model$model)

        return(list(output = output,
                    weighted_stats = model$weighted_stats))
      }, future.seed = opts$seed)

      cat("Bootstrap Successful\n")
      return(list(result = lapply(1:opts$nboot, function(x) result[[x]][[1]]),
                  weighted_stats = if(!opts$weighted){
                    NA
                  } else {
                    list(
                      covariates = opts$weight.covariates,
                      weighted_stats = lapply(1:opts$nboot, function(x) result[[x]][[2]])
                    )
                  })
      )
    }
    # Non Parallel Bootstrapping ===============================================
    result <- lapply(1:opts$nboot, function(x) {
      set.seed(opts$seed + x)

      id.sample <- sample(UIDs,
                          round(opts$boot.sample*lnID), replace = FALSE)

      RMdata <- data[get(id.col) %in% id.sample, ]
      RMDT <- DT[get(id.col) %in% id.sample, ]

      model <- handler(RMDT, RMdata, id.col, time.col, eligible.col, outcome.col, treatment.col, opts)
      if(opts$boot.return == "coef") output <- coef(model$model)
      if(opts$boot.return == "full") output <- model$model
      if(opts$boot.return == "summary") output <- summary(model$model)

      return(list(output = output,
                  weighted_stats = model$weighted_stats))
    })

    cat("Bootstrap Successful\n")
    return(list(result = lapply(1:opts$nboot, function(x) result[[x]][[1]]),
                weighted_stats = if(!opts$weighted){
                  NA
                } else {
                  lapply(1:opts$nboot, function(x) result[[x]][[2]])
                })
    )

  } else {
    result <- handler(DT, data, id.col, time.col, eligible.col, outcome.col, treatment.col, opts)

    return(list(result = summary(result$model),
                weighted_stats = if(!opts$weighted){
                  NA
                } else {
                  result$weighted_stats
                })
           )
  }
}

