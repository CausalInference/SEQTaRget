#' Parameter Builder for SEQuential Model and Estimates
#'
#' @param bootstrap Logical: defines if SEQuential should run bootstrapping, default is FALSE
#' @param bootstrap.CI Numeric: defines the confidence interval after bootstrapping, default is 0.95 (95\% CI)
#' @param bootstrap.CI_method Character: selects which way to calculate bootstraps confidence intervals ("se", "percentile")
#' @param bootstrap.nboot Integer: number of bootstraps
#' @param bootstrap.sample Numeric: percentage of data to use when bootstrapping, should in [0, 1], default is 0.8
#' @param cense String: column name for additional censoring variable, e.g. loss-to-follow-up
#' @param cense.denominator String: censoring denominator covariates to the right hand side of a formula object
#' @param cense.eligible String: column name for indicator column defining which rows to use for censoring model
#' @param cense.numerator String: censoring numerator covariates to the right hand side of a formula object
#' @param compevent String: column name for competing event indicator
#' @param covariates String: covariates to the right hand side of a formula object
#' @param data.return Logical: whether to return the expanded dataframe with weighting information
#' @param denominator String: denominator covariates to the right hand side of a  to formula object
#' @param deviation Logical: create switch based on deviation from column \code{deviation.col}
#' @param deviation.col Character: column name for deviation
#' @param deviation.conditions Character list: RHS evaluations of the same length as \code{treat.levels}
#' @param deviation.excused Logical: whether deviations should be excused by \code{deviation.excused_cols}
#' @param deviation.excused_cols Character list: excused columns for deviation switches
#' @param excused Logical: in the case of censoring, whether there is an excused condition
#' @param excused.cols List: list of column names for treatment switch excuses - should be the same length, and ordered the same as \code{treat.level}
#' @param fastglm.method Integer: decomposition method for fastglm (1-QR, 2-Cholesky, 3-LDLT, 4-QR.FPIV)
#' @param followup.class Logical: treat followup as a class, e.g. expands every time to it's own indicator column
#' @param followup.include Logical: whether or not to include 'followup' and 'followup_squared' in the outcome model
#' @param followup.max Numeric: maximum time to expand about, default is Inf (no maximum)
#' @param followup.min Numeric: minimum time to expand aboud, default is -Inf (no minimum)
#' @param followup.spline Logical: treat followup as a cubic spline
#' @param hazard Logical: hazard error calculation instead of survival estimation
#' @param indicator.baseline String: identifier for baseline variables in \code{covariates, numerator, denominator} - intended as an override
#' @param indicator.squared String: identifier for squared variables in \code{covariates, numerator, denominator} - intended as an override
#' @param km.curves Logical: Kaplan-Meier survival curve creation and data return
#' @param multinomial Logical: whether to expect multilevel treatment values
#' @param ncores Integer: number of cores to use in parallel processing, default is one less than system max
#' @param nthreads Integer: number of threads to use for data.table processing
#' @param numerator String: numerator covariates to the right hand side of a  to formula object
#' @param parallel Logical: define if the SEQuential process is run in parallel, default is FALSE
#' @param plot.colors Character: Colors for output plot if \code{km.curves = TRUE}, defaulted to ggplot2 defaults
#' @param plot.labels Character: Color labels for output plot if \code{km.curves = TRUE} in order e.g. \code{c("risk.0", "risk.1")}
#' @param plot.subtitle Character: Subtitle for output plot if \code{km.curves = TRUE}
#' @param plot.title Character: Title for output plot if \code{km.curves = TRUE}
#' @param plot.type Character: Type of plot to create if \code{km.curves = TRUE}, available options are 'survival', 'risk', and 'inc' (in the case of censoring)
#' @param seed Integer: starting seed
#' @param selection.first_trial Logical: selects only the first eligible trial in the expanded dataset
#' @param selection.prob Numeric: percent of total IDs to select for \code{selection.random}, should be bound [0, 1]
#' @param selection.random Logical: randomly selects IDs with replacement to run analysis
#' @param subgroup Character: Column name to stratify outcome models on
#' @param survival.max Numeric: maximum time for survival curves, default is Inf (no maximum)
#' @param treat.level List: treatment levels to compare
#' @param trial.include Logical: whether or not to include 'trial' and 'trial_squared' in the outcome model
#' @param weight.eligible_cols List: list of column names for indicator columns defining which weights are eligible for weight models - in order of \code{treat.level}
#' @param weight.lower Numeric: weights truncated at lower end at this weight
#' @param weight.lag_condition Logical: whether weights should be conditioned on treatment lag value
#' @param weight.p99 Logical: forces weight truncation at 1st and 99th percentile weights, will override provided \code{weight.upper} and \code{weight.lower}
#' @param weight.preexpansion Logical: whether weighting should be done on pre-expanded data
#' @param weight.upper Numeric: weights truncated at upper end at this weight
#' @param weighted Logical: whether or not to preform weighted analysis, default is FALSE
#' @returns An object of class 'SEQopts'
#' @export
#' @importFrom stats runif
#' @importFrom parallelly availableCores
#' @import data.table
SEQopts <- function(bootstrap = FALSE, bootstrap.nboot = 100, bootstrap.sample = 0.8, bootstrap.CI = 0.95, bootstrap.CI_method = "se",
                    cense = NA, cense.denominator = NA, cense.eligible = NA, cense.numerator = NA,
                    compevent = NA, covariates = NA, data.return = FALSE, denominator = NA,
                    deviation = FALSE, deviation.col = NA, deviation.conditions = c(NA, NA), deviation.excused = FALSE, deviation.excused_cols = c(NA, NA),
                    excused = FALSE, excused.cols = c(NA, NA), fastglm.method = 2L,
                    followup.class = FALSE, followup.include = TRUE, followup.max = Inf, followup.min = -Inf, followup.spline = FALSE,
                    hazard = FALSE, indicator.baseline = "_bas", indicator.squared = "_sq",
                    km.curves = FALSE, multinomial = FALSE, ncores = availableCores() - 1, nthreads = getDTthreads(),
                    numerator = NA, parallel = FALSE, plot.colors = c("#F8766D", "#00BFC4", "#555555"), plot.labels = NA, plot.subtitle = NA, plot.title = NA, plot.type = "survival",
                    seed = NULL, selection.first_trial = FALSE, selection.prob = 0.8, selection.random = FALSE, subgroup = NA, survival.max = Inf,
                    treat.level = c(0, 1), trial.include = TRUE, weight.eligible_cols = c(),
                    weight.lower = -Inf, weight.lag_condition = TRUE, weight.p99 = FALSE, weight.preexpansion = TRUE, weight.upper = Inf, weighted = FALSE) {
  # Standardization =============================================================
  parallel <- as.logical(parallel)
  nthreads <- as.integer(nthreads)
  ncores <- as.integer(ncores)
  bootstrap <- as.logical(bootstrap)
  bootstrap.nboot <- as.integer(bootstrap.nboot)
  runif(1)
  seed <- if (is.null(seed)) .Random.seed else as.integer(seed)
  followup.min <- as.numeric(followup.min)
  followup.max <- as.numeric(followup.max)
  survival.max <- as.numeric(survival.max)
  weight.lower <- as.numeric(weight.lower)
  weight.upper <- as.numeric(weight.upper)
  weight.eligible_cols <- as.list(weight.eligible_cols)
  
  # Temporarily disabling deviation - work in progress method
  if (deviation) stop("Deviation is currently under further development and is currently not suitable for analysis.
                      We apologize for this inconvenience")
  deviation.col <- as.character(deviation.col)
  deviation.conditions <- as.list(deviation.conditions)
  deviation.excused_cols <- as.list(deviation.excused_cols)

  hazard <- as.logical(hazard)
  
  subgroup <- as.character(subgroup)

  selection.prob <- as.numeric(selection.prob)
  selection.random <- as.logical(selection.random)

  trial.include <- as.logical(trial.include)
  followup.include <- as.logical(followup.include)

  covariates <- gsub("\\s", "", covariates)
  numerator <- gsub("\\s", "", numerator)
  denominator <- gsub("\\s", "", denominator)
  cense.numerator <- gsub("\\s", "", cense.numerator)
  cense.denominator <- gsub("\\s", "", cense.denominator)

  weighted <- as.logical(weighted)
  weight.preexpansion <- as.logical(weight.preexpansion)

  excused <- as.logical(excused)
  excused.cols <- as.list(excused.cols)

  cense <- as.character(cense)
  cense.eligible <- as.character(cense.eligible)
  compevent <- as.character(compevent)
  treat.level <- as.list(treat.level)

  indicator.baseline <- as.character(indicator.baseline)
  indicator.squared <- as.character(indicator.squared)

  fastglm.method <- as.integer(fastglm.method)

  plot.title <- as.character(plot.title)
  plot.subtitle <- as.character(plot.subtitle)
  plot.labels <- as.character(plot.labels)
  plot.colors <- as.character(plot.colors)
  plot.type <- as.character(plot.type)


  new("SEQopts",
      deviation = deviation,
      deviation.col = deviation.col,
      deviation.excused = deviation.excused,
      deviation.excused_cols = deviation.excused_cols,
      deviation.conditions = deviation.conditions,
      parallel = parallel,
      nthreads = nthreads,
      ncores = ncores,
      bootstrap = bootstrap,
      bootstrap.nboot = bootstrap.nboot,
      bootstrap.sample = bootstrap.sample,
      bootstrap.CI = bootstrap.CI,
      bootstrap.CI_method = bootstrap.CI_method,
      seed = seed,
      followup.min = followup.min,
      followup.max = followup.max,
      survival.max = survival.max,
      trial.include = trial.include,
      followup.include = followup.include,
      weighted = weighted,
      weight.lower = weight.lower,
      weight.lag_condition = weight.lag_condition,
      weight.upper = weight.upper,
      weight.p99 = weight.p99,
      weight.preexpansion = weight.preexpansion,
      excused = excused,
      cense = cense,
      compevent = compevent,
      cense.eligible = cense.eligible,
      excused.cols = excused.cols,
      km.curves = km.curves,
      covariates = covariates,
      numerator = numerator,
      denominator = denominator,
      indicator.baseline = indicator.baseline,
      indicator.squared = indicator.squared,
      fastglm.method = fastglm.method,
      treat.level = treat.level,
      multinomial = multinomial,
      hazard = hazard,
      weight.eligible_cols = weight.eligible_cols,
      followup.class = followup.class,
      followup.spline = followup.spline,
      plot.title = plot.title,
      plot.subtitle = plot.subtitle,
      plot.labels = plot.labels,
      plot.colors = plot.colors,
      plot.type = plot.type,
      subgroup = subgroup,
      data.return = data.return,
      selection.first_trial = selection.first_trial,
      cense.denominator = cense.denominator,
      cense.numerator = cense.numerator,
      selection.prob = selection.prob,
      selection.random = selection.random
  )
}
