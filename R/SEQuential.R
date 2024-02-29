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
  time.start <- Sys.time()
  # Error Throwing ============================================
  errorData(data, id.col, time.col, eligible.col, treatment.col, outcome.col, method)

  # Parameter Space building ==================================
  opts <- SEQopts(); dots <- list(...)
  if(!missing(params)) errorParams(params, dots)
  if(!missing(params)) opts[names(params)] <- params
  if(length(dots > 0)) opts[names(dots)] <- dots
  errorOpts(opts)

  ncores <<- opts$ncores

  if(opts$parallel){
    evalq({
    doFuture::registerDoFuture()
    doRNG::registerDoRNG()
    future::plan(future::multisession(workers = ncores), gc = TRUE)}, envir = .GlobalEnv)
  }
  rm(ncores, envir = .GlobalEnv)
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

    cat("Expansion Successful\nMoving forward with", method, "analysis\n")
  } else if(opts$expand == FALSE){
    cat("Skipping expansion per 'expand = FALSE'\n")
    cat("Moving forward with", method, "analysis\n")
    DT <- as.data.table(data)
  }

  #Model Dispersion ===========================================
  model <- internal.analysis(DT, data, method, id.col, time.col, eligible.col, outcome.col, treatment.col, opts)
  if(opts$bootstrap){
    if(opts$boot.return != "coef"){
      TDT <- rbindlist(lapply(model,
                              function(x) as.list(coef(x))))
    } else {
      TDT <- rbindlist(lapply(model, function(x) as.list(x)))
    }
      model_summary <- lapply(TDT, function(col){
        stats <- list(
          mean = mean(col, na.rm = TRUE),
          sd = sd(col, na.rm = TRUE),
          min = min(col, na.rm = TRUE),
          max = max(col, na.rm = TRUE),
          CI_95 = quantile(col, c(0.025, 0.975), na.rm = TRUE)
        )
        return(stats)
      })
    }

  cat(method, "model successfully created\nCreating survival curves\n")
  surv <- internal.survival(DT, id.col, time.col, outcome.col, treatment.col, opts)

  return_list <- list(
    boot_params = if(!opts$bootstrap){
      NA
      } else {
        list(
          nboot = opts$nboot,
          seed = opts$seed,
          sample = opts$boot.sample,
          return = opts$boot.return
        )
    },
    boot_stats = if(!opts$bootstrap){
      NA
    } else {
      model_summary
    },
    model = model,
    survival_curve = surv,
    survival_data = surv$data,
    elapsed_time = paste(round(as.numeric(difftime(Sys.time(), time.start, units = "mins")), 2), "minutes")
    )
  gc()
  future:::ClusterRegistry("stop")
  return(return_list)
}
