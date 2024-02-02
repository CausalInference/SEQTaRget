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

  if(opts$parallel) opts$sys.type <- Sys.info()['sysname'][1]
  if(opts$parallel && opts$sys.type == "Windows"){
    cat("Starting parallel cluster with", opts$ncores, "cores\n")
    cl <- makeCluster(opts$ncores)
    registerDoParallel(cl)
  }

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

    cat("Expansion Successful\nMoving forward with", method, "analysis")
  } else if(opts$expand == FALSE){
    cat("Skipping expansion per 'expand = FALSE'\n")
    cat("Moving forward with", method, "analysis\n")
    DT <- as.data.table(data)
  }
  opts$data.size <- object.size(DT)

  #Model Dispersion ===========================================
  model <- internal.analysis(DT, data, method, id.col, time.col, eligible.col, outcome.col, treatment.col, opts)

  cat(method, "model successfully created\nCreating survival curves\n")
  surv <- internal.survival(DT, id.col, time.col, outcome.col, treatment.col, opts)

  if(opts$parallel && opts$sys.type == "Windows"){
    cat("Stopping Cluster")
    stopCluster(cl)
  }
  return(list(model, surv))
}
