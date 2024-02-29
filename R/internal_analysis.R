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
            if(!opts$stabilized && opts$weight.time == "pre"){
              WT <- internal.weights(DT, data, id.col, time.col, eligible.col, outcome.col, treatment.col, opts)
              if(!opts$expand) {
                WDT <- DT[WT$weighted_data, on = c(id.col, time.col)
                          ][wt := ifelse(get(time.col) == 0, 1, wt), by = id.col]
              } else {
                WDT <- DT[WT$weighted_data, on = c(id.col, time.col)
                          ][, wt := ifelse(followup == 0, 1, wt), by = id.col]
              }
            }
            if(opts$weight.time == "pre") {

            }
            if(opts$weight.time == "post" ||  opts$stabilized){

            }
          }
        return(model)
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

        RMDT <- copy(DT[get(id.col) %in% id.sample, ])
        RMdata <- copy(data[get(id.col) %in% id.sample, ])

        model <- handler(RMDT, RMdata, id.col, time.col, eligible.col, outcome.col, treatment.col, opts)
        if(opts$boot.return == "coef") output <- coef(model)
        if(opts$boot.return == "full") output <- model
        if(opts$boot.return == "summary") output <- summary(model)

        return(output)
      }, future.seed = opts$seed)

      cat("Bootstrap Successful\n")
      return(result)
    }
    # Non Parallel Bootstrapping ===============================================
    result <- lapply(1:opts$nboot, function(x) {
      set.seed(opts$seed + x)

      id.sample <- sample(UIDs,
                          round(opts$boot.sample*lnID), replace = FALSE)

      data <- copy(data)[get(id.col) %in% id.sample, ]
      DT <- copy(DT)[get(id.col) %in% id.sample, ]

      output <- handler(DT, data, id.col, time.col, eligible.col, outcome.col, treatment.col, opts)
      if(opts$boot.return == "coef") output <- coef(model)
      if(opts$boot.return == "full") output <- model
      if(opts$boot.return == "summary") output <- list(
        coefficients = coef(model),
        standard_errors = sqrt(diag(vcov(model))),
        t_values = coef(model) / sqrt(diag(vcov(model))),
        p_values = 2 * pt(abs(coef(model) / sqrt(diag(vcov(model)))), df.residual(model), lower.tail = FALSE)
      )
    })

    cat("Bootstrap Successful\n")
    return(result)

  } else if(!opts$bootstrap){
    result <- summary(handler(DT, data, id.col, time.col, eligible.col, outcome.col, treatment.col, opts))
    return(result)
  }
}
