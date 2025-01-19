#' Parameter Helper
#'
#' @importFrom methods new
#' @keywords internal
parameter.setter <- function(data, DT,
                             id.col, time.col, eligible.col, outcome.col, treatment.col,
                             time_varying.cols, fixed.cols, method,
                             opts) {
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
    calculate.var = opts@calculate.var,
    compevent = opts@compevent,
    cense.eligible = opts@cense.eligible,
    excused.col0 = opts@excused.col0,
    excused.col1 = opts@excused.col1,
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
    weight.eligible1 = opts@weight.eligible1,
    weight.eligible0 = opts@weight.eligible0,
    plot.type = opts@plot.type,
    plot.title = opts@plot.title,
    plot.subtitle = opts@plot.subtitle,
    plot.labels = opts@plot.labels,
    plot.colors = opts@plot.colors
  )
}

#' Simplifies parameters down for later use
#'
#' @keywords internal
parameter.simplifier <- function(params) {
  # Variable pre-definition ===================================
  tmp1 <- NULL
  tmp0 <- NULL

  if (!params@bootstrap) {
    params@bootstrap.nboot <- 1L
    params@bootstrap.sample <- 1
    params@parallel <- FALSE
  }
  if (params@survival.max > params@followup.max) {
    warning("Maximum followup for survival curves cannot be greater than the maximum for followup")
    params@survival.max <- params@followup.max
  }
  if (is.infinite(params@followup.max)) params@followup.max <- max(params@data[[params@time]])
  if (is.infinite(params@survival.max)) params@survival.max <- params@followup.max

  if (is.na(params@excused.col0) & is.na(params@excused.col1) & params@excused & params@method == "censoring") {
    warning("No excused variables provided for excused censoring, automatically changed to excused = FALSE")
    params@excused <- FALSE
  }

  if (params@km.curves & (params@calculate.var | params@hazard)) stop("Kaplan-Meier Curves and Hazard Ratio or Robust Standard Errors are not compatible. Please select one.")
  if (sum(params@followup.include, params@followup.class, params@followup.spline) > 1) stop("followup.include, followup.class, and followup.spline are exclusive. Please select one")

  if (is.na(params@excused.col0) & params@excused & params@method == "censoring") {
    params@excused.col0 <- "tmp0"
    params@data <- params@data[, tmp0 := 0]
  }
  if (is.na(params@excused.col1) & params@excused & params@method == "censoring") {
    params@excused.col1 <- "tmp1"
    params@data <- params@data[, tmp1 := 0]
  }

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

  return(params)
}

#' Output constructor
#'
#' @importFrom methods new
#' @keywords internal
prepare.output <- function(params, outcome_model, hazard, robustSE, survival_plot, survival_data, risk, elapsed_time, info) {
  if (!missing(outcome_model)) {
    outcome <- lapply(1:params@bootstrap.nboot, function(x) outcome_model[[x]]$model)
    weight.stats <- lapply(1:params@bootstrap.nboot, function(x) outcome_model[[x]]$weight_info)
  }

  new("SEQoutput",
      params = params,
      outcome = paste0(params@outcome, "~", params@covariates),
      numerator = if (!params@weighted) NA_character_ else paste0(params@treatment, "~", params@numerator),
      denominator = if (!params@weighted) NA_character_ else paste0(params@treatment, "~", params@denominator),
      outcome.model = outcome,
      hazard = if (!params@hazard) NA_real_ else hazard,
      robust.se = if (!params@calculate.var) list() else robustSE,
      weight.statistics = weight.stats,
      survival.curve = if (!params@km.curves) NA else survival_plot,
      survival.data = if (!params@km.curves) NA else survival_data,
      risk.difference = if(length(risk) > 1) risk$difference else NA_real_,
      risk.ratio = if(length(risk) > 1) risk$ratio else NA_real_,
      time = elapsed_time,
      info = info
  )
}
