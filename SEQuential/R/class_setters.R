#' Parameter Helper
#'
#' @importFrom methods new
#' @keywords internal
parameter.setter <- function(data, DT,
                             id.col, time.col, eligible.col, outcome.col, treatment.col,
                             time_varying.cols, fixed.cols, method,
                             opts, verbose) {
  new("SEQparams",
    data = data,
    DT = DT,
    id = id.col,
    time = time.col,
    eligible = eligible.col,
    outcome = outcome.col,
    treatment = treatment.col,
    time_varying = time_varying.cols,
    fixed = fixed.cols,
    method = method,
    verbose = verbose,
    parallel = opts@parallel,
    nthreads = opts@nthreads,
    ncores = opts@ncores,
    bootstrap.nboot = opts@bootstrap.nboot,
    bootstrap = opts@bootstrap,
    bootstrap.sample = opts@bootstrap.sample,
    seed = opts@seed,
    followup.include = opts@followup.include,
    trial.include = opts@trial.include,
    followup.min = opts@followup.min,
    followup.max = opts@followup.max,
    survival.max = opts@survival.max,
    weighted = opts@weighted,
    weight.preexpansion = opts@weight.preexpansion,
    excused = opts@excused,
    cense = opts@cense,
    hazard = opts@hazard,
    compevent = opts@compevent,
    cense.eligible = opts@cense.eligible,
    excused.cols = opts@excused.cols,
    covariates = opts@covariates,
    numerator = opts@numerator,
    denominator = opts@denominator,
    cense.numerator = opts@cense.numerator,
    cense.denominator = opts@cense.denominator,
    km.curves = opts@km.curves,
    indicator.baseline = opts@indicator.baseline,
    indicator.squared = opts@indicator.squared,
    fastglm.method = opts@fastglm.method,
    multinomial = opts@multinomial,
    treat.level = opts@treat.level,
    followup.class = opts@followup.class,
    followup.spline = opts@followup.spline,
    weight.eligible_cols = opts@weight.eligible_cols,
    plot.type = opts@plot.type,
    plot.title = opts@plot.title,
    plot.subtitle = opts@plot.subtitle,
    plot.labels = opts@plot.labels,
    plot.colors = opts@plot.colors,
    subgroup = opts@subgroup,
    data.return = opts@data.return,
    weight.lag_condition = opts@weight.lag_condition,
    weight.lower = opts@weight.lower,
    weight.upper = opts@weight.upper,
    weight.p99 = opts@weight.p99,
    selection.first_trial = opts@selection.first_trial,
    selection.prob = opts@selection.prob,
    deviation = opts@deviation,
    deviation.excused = opts@deviation.excused,
    deviation.col = opts@deviation.col,
    deviation.conditions = opts@deviation.conditions,
    deviation.excused_cols = opts@deviation.excused_cols
  )
}

#' Simplifies parameters down for later use
#'
#' @keywords internal
parameter.simplifier <- function(params) {
  if (!params@bootstrap) {
    params@bootstrap.nboot <- 1L
    params@bootstrap.sample <- 1
    params@parallel <- FALSE
  }
  
  if (!is.na(params@subgroup) && !params@subgroup %in% params@fixed) stop("subgroup not found in provided fixed cols")
  
  if (params@survival.max > params@followup.max) {
    warning("Maximum followup for survival curves cannot be greater than the maximum for followup")
    params@survival.max <- params@followup.max
  }
  if (is.infinite(params@followup.max)) params@followup.max <- max(params@data[[params@time]])
  if (is.infinite(params@survival.max)) params@survival.max <- params@followup.max

  if (allNA(params@excused.cols) & params@excused & params@method == "censoring") {
    warning("No excused variables provided for excused censoring, automatically changed to excused = FALSE")
    params@excused <- FALSE
  }
  if (!allNA(params@excused.cols) & !params@excused) {
    warning("Excused variables given, but excused was set to FALSE, automatically changed to excused = TRUE")
    params@excused <- TRUE
  }
  
  params@excused.cols <- equalizer(params@excused.cols, params@treat.level)
  if (params@deviation) if (length(params@deviation.conditions) != length(params@treat.level)) stop("Deviation conditions should encompass all treatment levels")
  params@deviation.excused_cols <- equalizer(params@deviation.excused_cols, params@treat.level)
  params@weight.eligible_cols <- equalizer(params@weight.eligible_cols, params@treat.level)

  if (params@km.curves & params@hazard) stop("Kaplan-Meier Curves and Hazard Ratio or Robust Standard Errors are not compatible. Please select one.")
  if (sum(params@followup.include, params@followup.class, params@followup.spline) > 1) stop("followup.include, followup.class, and followup.spline are exclusive. Please select one")

  if (!is.na(params@cense)) {
    params@LTFU <- TRUE
    params@weighted <- TRUE
  }

  if (params@method == "ITT" & params@weighted & !params@LTFU) {
    warning("Without LTFU, weighted ITT model is not supported, automatically changed to weighted = FALSE")
    params@weighted <- FALSE
  }
  if (params@followup.class & params@followup.spline) stop("Followup cannot be both a class and a spline, please select one.")
  if (!params@plot.type %in% c("survival", "risk", "inc")) stop("Supported plot types are 'survival', 'risk', and 'inc' (in the case of censoring), please select one.")
  
  if (params@multinomial & params@method == "dose-response") stop("Multinomial dose-response is not supported")
  
  if (params@excused & params@deviation.excused) stop("Must select either excused from deviation or excused from treatment swap")

  return(params)
}

#' Output constructor
#'
#' @importFrom methods new
#' @keywords internal
prepare.output <- function(params, WDT, outcome, weights, hazard, survival.plot, survival.data, survival.ce, risk, runtime, info) {
  risk.comparison <- lapply(risk, \(x) x$risk.comparison)
  risk.data <- lapply(risk, \(x) x$risk.data)
  
  DT <- if (params@data.return) if (params@weighted) WDT else copy(params@DT) else data.table()
  params@DT <- params@data <- data.table()
  
  new("SEQoutput",
      params = params,
      DT = DT,
      outcome = paste0(params@outcome, "~", params@covariates),
      numerator = if (!params@weighted) NA_character_ else 
        if (!params@weight.preexpansion && (params@excused | params@deviation.excused)) paste0("censored", "~", params@numerator) else 
          paste0(params@treatment, "~", params@numerator),
      denominator = if (!params@weighted) NA_character_ else 
        if (!params@weight.preexpansion && (params@excused | params@deviation.excused)) paste0("censored", "~", params@denominator) else 
          paste0(params@treatment, "~", params@denominator),
      outcome.model = outcome,
      hazard = if (!params@hazard) list() else hazard,
      weight.statistics = weights,
      survival.curve = if (!params@km.curves) list() else survival.plot,
      survival.data = if (!params@km.curves) list() else survival.data,
      risk.comparison = if (!params@km.curves) list() else risk.comparison,
      risk.data = if (!params@km.curves) list() else risk.data,
      time = runtime,
      info = info,
      ce.model = survival.ce
  )
}
