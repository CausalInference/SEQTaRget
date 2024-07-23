#' SEQuential processing per (paper here?)
#'
#' @param data data.frame or data.table, if not already expanded with \code{SEQexpand}, will preform expansion according to arguments passed to either \code{params} or \code{...}
#' @param id.col String: column name of the id column
#' @param time.col String: column name of the time column
#' @param eligible.col String: column name of the eligibility column
#' @param treatment.col String: column name of the treatment column
#' @param outcome.col String: column name of the outcome column
#' @param method String: method of analysis to preform
#' @param options List: optional list of parameters from \code{SEQOpts}
#'
#' @import data.table
#'
#' @export
SEQuential <- function(data, id.col, time.col, eligible.col, treatment.col, outcome.col, time.cols, fixed.cols, method, options){
  setDT(data); setorderv(data, c(id.col, time.col))
  time.start <- Sys.time()

  # Error Throwing ============================================
  errorData(data, id.col, time.col, eligible.col, treatment.col, outcome.col, time.cols, fixed.cols, method)
  if(!is(options, "SEQopts")) stop("Options should be built from SEQopts()")
  time_varying.cols <- as.list(time_varying.cols)
  fixed.cols <- as.list(fixed.cols)

  # Parallel Setup ==================================
  if(opts$parallel){
    ncores <<- options@ncores
    evalq({
    doFuture::registerDoFuture()
    doRNG::registerDoRNG()
    future::plan(future::multisession(workers = ncores), gc = TRUE)}, envir = .GlobalEnv)
    rm(ncores, envir = .GlobalEnv)
  }

  # Parameter Setup ==================================
  params <- parameter.setter(data, DT = data.table(), id.col, time.col, eligible.col, outcome.col, treatment.col,
                             as.list(time_varying.cols), as.list(fixed.cols),
                             method, options)

  if(is.na(params@covariates)) params@covariates <- create.default.covariates(params)
  if(params@weighted){
    if(is.na(params@numerator)) params@numerator <- create.default.weight.covariates(params, "numerator")
    if(is.na(params@denominator)) params@denominator <- create.default.weight.covariates(params, "denominator")
  }
  params <- parameter.simplifier(params)

  # Expansion ==================================================
    cat("Expanding Data...\n")
    params@DT <- SEQexpand(params)
    cat("Expansion Successful\nMoving forward with", params@method, "analysis\n")

  #Model Dispersion ===========================================
  internal.analysis(params)

  cat(method, "model successfully created\nCreating survival curves\n")
  survival <- internal.survival(DT, id.col, time.col, outcome.col, treatment.col, method, opts)
  risk <- create.risk(survival$data)

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
    model = model,
    survival_curve = survival,
    survival_data = survival$data,
    risk_difference = risk$rd,
    risk_ratio = risk$rr,
    elapsed_time = paste(round(as.numeric(difftime(Sys.time(), time.start, units = "mins")), 2), "minutes")
    )
  gc()
  future:::ClusterRegistry("stop")
  return(return_list)
}
