#' Internal analysis tool for handling parallelization/bootstrapping on multiple OS types
#'
#'
#' @import data.table future doFuture doRNG future.apply
#' @keywords internal
internal.analysis <- function(DT, data, method, id.col, time.col, eligible.col, outcome.col, treatment.col, opts){
        handler <- function(DT, data, id.col, time.col, eligible.col, outcome.col, treatment.col, opts){

          if(!opts$weighted){
            model <- internal.model(DT, method, outcome.col, opts)
          } else if (opts$weighted){
            if(!opts$stabilized && opts$pre.expansion){
              WT <- internal.weights(DT, data, id.col, time.col, eligible.col, outcome.col, treatment.col, opts)
              if(!opts$expand) {
                WDT <- DT[WT$weighted_data, on = c(id.col, time.col)]
              } else {
                time.col <- "followup"
                WDT <- DT[WT$weighted_data, on = c(id.col, time.col)]
              }
            }
            if(opts$pre.expansion) {

            }
          }
        return(model = model,
               weighted_stats = WT$weighted_stats)
      }
  if(opts$bootstrap){
    cat("Bootstrapping", opts$boot.sample*100, "% of data", opts$nboot, "times\n")
    if(opts$parallel){
      UIDs <- unique(DT[[id.col]])
      lnID <- length(UIDs)
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
      return(list(result = summary(result$model),
                  weighted_stats = if(!opts$weighted){
                    NA
                  } else {
                    list(
                      stabilized = opts$stabilized,
                      covariates = opts$weight.covariates,
                      weighted_stats = result$weighted_stats
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

      output <- handler(RMDT, RMdata, id.col, time.col, eligible.col, outcome.col, treatment.col, opts)
      if(opts$boot.return == "coef") output <- coef(model$model)
      if(opts$boot.return == "full") output <- model$model
      if(opts$boot.return == "summary") output <- summary(model$model)

      return(list(output = output,
                  weighted_stats = model$weighted_stats))
    })

    cat("Bootstrap Successful\n")
    return(list(result = summary(result$model),
                weighted_stats = if(!opts$weighted){
                  NA
                } else {
                  list(
                    stabilized = opts$stabilized,
                    covariates = opts$weight.covariates,
                    weighted_stats = result$weighted_stats
                  )
                })
    )

  } else if(!opts$bootstrap){
    result <- handler(DT, data, id.col, time.col, eligible.col, outcome.col, treatment.col, opts)
    return(list(result = summary(result$model),
                weighted_stats = if(!opts$weighted){
                  NA
                } else {
                  list(
                    stabilized = opts$stabilized,
                    covariates = opts$weight.covariates,
                    weighted_stats = result$weighted_stats
                  )
                })
           )
  }
}
