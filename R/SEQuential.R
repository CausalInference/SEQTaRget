#' SEQuential processing per (paper here?)
#'
#' @param data data.frame or data.table, if not already expanded with \code{SEQexpand}, will preform expansion according to arguments passed to either \code{params} or \code{...}
#' @param id.col String: column name of the id column
#' @param time.col String: column name of the time column
#' @param eligible.col String: column name of the eligibility column
#' @param treatment.col String: column name of the treatment column
#' @param outcome.col String: column name of the outcome column
#' @param method String: method of analysis to preform
#' @param params List: optional list of parameters from \code{SEQOpts}
#' @param ... another option for passing parameters from \code{SEQOpts}
#'
#' @import data.table
#'
#' @export
SEQuential <- function(data, id.col, time.col, eligible.col, treatment.col, outcome.col, method, params, ...){
  # Error Throwing ============================================
  errorData(data, id.col, time.col, eligible.col, treatment.col, outcome.col, method)

  # Parameter Space building ==================================
  opts <- SEQopts(); dots <- list(...)
  if(!missing(params)) errorParams(params, dots)
  if(!missing(params)) opts[names(params)] <- params
  if(length(dots > 0)) opts[names(dots)] <- dots
  errorOpts(opts)

  if(is.na(opts$covariates)){
    #Default covariates created, dependent on method selection
    opts$covariates <- create.default.covariates(data, id.col, time.col, eligible.col, treatment.col, outcome.col, method)
  }

  # Expansion ==================================================
  if(opts$expand == TRUE){
    cat("Expanding Data...\n")
    DT <- SEQexpand(data, id.col, time.col, eligible.col, outcome.col, opts)

    if(method == "none"){
      cat("Returning expanded data per 'method = 'none''")
      return(DT)
    }

    cat(paste("Expansion Successful\nMoving forward with", method, "analysis"))
  } else if(opts$expand == FALSE){
    cat("Skipping expansion per 'expand = FALSE'\n")
    cat(paste("Moving forward with", method, "analysis\n"))
    DT <- as.data.table(data)
  }


  #Model Dispersion ===========================================
  if(!opts$bootstrap) opts$nboot <- 1; opts$boot.sample <- 1
  if(!opts$parallel) opts$ncores <- 1;

  ids <- unique(DT[[id.col]])
  sample_lists <- mclapply(1:200, function(i) {
    set.seed(123 + i)
    sample_ids <- dt$unique_ids[sample(nrow(dt), 0.8 * nrow(dt))]
    return(list(sample_ids))
  }, mc.cores = num_cores)

  id.sample <- sample(unique(DT[[id.col]]),
                      opts$boot.sample*length(unique(DT[[id.col]])), replace = FALSE)

  if(!opts$weighted){
    model <- internal.model(DT, method, outcome.col, opts)
  } else if (opts$weighted){
    if(opts$stabilized && opts$weight.time == "pre"){
      WT <- internal.weights(DT, data, id.col, time.col, eligible.col, outcome.col, treatment.col, opts)
      if(!opts$expand){
        #In the case of no expansion, weights bind to the original data on defined time.col and id.col
        WT_data <- DT[WT$weighted_data, on = c(time.col, id.col)
                      ][, wt := ifelse(get(time.col) == 0, 1, wt), by = id.col]
      }
      if(opts$expand){
        #If expanded, time.col has been written to 'followup' and 'period'
        WT_data <- DT[WT$weighted_data, on = c(id.col, "followup")
                      ][, wt := ifelse(followup == 0, 1, wt)]
      }
      if(opts$weight.time == "pre"){
        weightModel <- 'MODEL USING WEIGHTS FIT PRE-EXPANSION'
      }
    } else if(opts$stabilized == FALSE || weight.time == "post"){
      weightModel <- 'MODEL USING WEIGHTS FIT POST-EXPANSION'
    }
  }
  cat(paste0("\n", method, " model successfully created\nCreating survival curves"))
  surv <- internal.survival(DT, id.col, time.col, outcome.col, treatment.col, opts)

  return(list(model, surv))
}
