#' Parameter Builder for SEQuential Model and Estimates
#'
#' @param parallel Logical: define if the SEQuential process is run in parallel, default is FALSE
#' @param nthreads Integer: number of threads to use for data.table processing
#' @param ncores Integer: number of cores to use in parallel processing, default is one less than system max
#' @param bootstrap Logical: defines if SEQuential should run bootstrapping, default is FALSE
#' @param bootstrap.nboot Integer: number of bootstraps
#' @param bootstrap.sample Numeric: percentage of data to use when bootstrapping, should in [0, 1], default is 0.8
#' @param seed Integer: starting seed
#' @param followup.min Numeric: minimum time to expand aboud, default is -Inf (no minimum)
#' @param followup.max Numeric: maximum time to expand about, default is Inf (no maximum)
#' @param survival.max Numeric: maximum time for survival curves, default is Inf (no maximum)
#' @param followup.include Logical: whether or not to include 'followup' and 'followup_squared' in the outcome model
#' @param trial.include Logical: whether or not to include 'trial' and 'trial_squared' in the outcome model
#' @param covariates String: covariates to the right hand side of a formula object
#' @param numerator String: numerator covariates to the right hand side of a  to formula object
#' @param denominator String: denominator covariates to the right hand side of a  to formula object
#' @param cense.numerator String: censoring numerator covariates to the right hand side of a formula object
#' @param cense.denominator String: censoring denominator covariates to the right hand side of a formula object
#' @param weighted Logical: whether or not to preform weighted analysis, default is FALSE
#' @param weight.lower Numeric: weights truncated at lower end at this weight
#' @param weight.upper Numeric: weights truncated at upper end at this weight
#' @param weight.p99 Logical: forces weight truncation at 1st and 99th percentile weights, will override provided \code{weight.upper} and \code{weight.lower}
#' @param weight.eligible0 String: column name for indicator column defining which weights are eligible for the 'zero' model
#' @param weight.eligible1 String: column name for indicator column defining which weights are eligible for the 'one' model
#' @param weight.preexpansion Logical: whether weighting should be done on pre-expanded data
#' @param hazard Logical: hazard error calculation instead of survival estimation
#' @param selection.random Logical: randomly selects IDs with replacement to run analysis
#' @param selection.prob Numeric: percent of total IDs to select for \code{selection.random}, should be bound [0, 1]
#' @param excused Logical: in the case of censoring, whether there is an excused condition
#' @param cense String: column name for additional censoring variable, e.g. loss-to-follow-up
#' @param cense.eligible String: column name for indicator column defining which rows to use for censoring model
#' @param multinomial Logical:
#' @param treat.level List: treatment levels to compare
#' @param compevent String: column name for competing event indicator
#' @param excused.cols List: list of column names for treatment switch excuses - should be the same length, and ordered the same as \code{treat.level}
#' @param km.curves Logical: Kaplan-Meier survival curve creation and data return
#' @param indicator.baseline String: identifier for baseline variables in \code{covariates, numerator, denominator} - intended as an override
#' @param indicator.squared String: identifier for squared variables in \code{covariates, numerator, denominator} - intended as an override
#' @param fastglm.method Integer: decomposition method for fastglm (1-QR, 2-Cholesky, 3-LDLT, 4-QR.FPIV)
#' @param followup.class Logical: treat followup as a class, e.g. expands every time to it's own indicator column
#' @param followup.spline Logical: treat followup as a cubic spline
#' @param plot.title Character: Title for output plot if \code{km.curves = TRUE}
#' @param plot.subtitle Character: Subtitle for output plot if \code{km.curves = TRUE}
#' @param plot.labels Character: Color labels for output plot if \code{km.curves = TRUE} in order e.g. \code{c("risk.0", "risk.1")}
#' @param plot.colors Character: Colors for output plot if \code{km.curves = TRUE}, defaulted to ggplot2 defaults
#' @param plot.type Character: Type of plot to create if \code{km.curves = TRUE}, available options are 'survival', 'risk', and 'inc' (in the case of censoring)
#' @param subgroup Character: Column name to stratify outcome models on
#'
#' @export
#' @returns An object of class 'SEQOpts'
SEQopts <- function(parallel = FALSE, nthreads = data.table::getDTthreads(), ncores = parallel::detectCores() - 1,
                    bootstrap = FALSE, bootstrap.nboot = 100, bootstrap.sample = 0.8, seed = 1636,
                    followup.min = -Inf, followup.max = Inf, survival.max = Inf, followup.include = TRUE, trial.include = TRUE,
                    covariates = NA, weighted = FALSE, weight.upper = Inf, weight.lower = -Inf, weight.p99 = FALSE,
                    numerator = NA, denominator = NA, weight.preexpansion = TRUE, weight.eligible0 = NA, weight.eligible1 = NA, 
                    hazard = FALSE, subgroup = NA,
                    selection.random = FALSE, selection.prob = 0.8, followup.class = FALSE, followup.spline = FALSE,
                    cense.numerator = NA, cense.denominator = NA, cense = NA, cense.eligible = NA,
                    compevent = NA, multinomial = FALSE, treat.level = c(0, 1),
                    excused = FALSE, excused.cols = c(NA, NA), km.curves = FALSE,
                    indicator.baseline = "_bas", indicator.squared = "_sq",
                    fastglm.method = 2L,
                    plot.title = NA, plot.subtitle = NA, plot.labels = NA, plot.colors = c("#F8766D", "#00BFC4", "#555555"), plot.type = "survival") {
  # Standardization =============================================================
  parallel <- as.logical(parallel)
  nthreads <- as.integer(nthreads)
  ncores <- as.integer(ncores)
  bootstrap <- as.logical(bootstrap)
  bootstrap.nboot <- as.integer(bootstrap.nboot)
  seed <- as.integer(seed)
  followup.min <- as.numeric(followup.min)
  followup.max <- as.numeric(followup.max)
  survival.max <- as.numeric(survival.max)
  weight.lower <- as.numeric(weight.lower)
  weight.upper <- as.numeric(weight.upper)
  weight.eligible0 <- as.character(weight.eligible0)
  weight.eligible1 <- as.character(weight.eligible1)

  hazard <- as.logical(hazard)
  
  subgroup <- as.character(subgroup)

  selection.prob <- as.logical(selection.prob)
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
    parallel = parallel,
    nthreads = nthreads,
    ncores = ncores,
    bootstrap = bootstrap,
    bootstrap.nboot = bootstrap.nboot,
    bootstrap.sample = bootstrap.sample,
    seed = seed,
    followup.min = followup.min,
    followup.max = followup.max,
    survival.max = survival.max,
    trial.include = trial.include,
    followup.include = followup.include,
    weighted = weighted,
    weight.lower = weight.lower,
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
    weight.eligible1 = weight.eligible1,
    weight.eligible0 = weight.eligible0,
    followup.class = followup.class,
    followup.spline = followup.spline,
    plot.title = plot.title,
    plot.subtitle = plot.subtitle,
    plot.labels = plot.labels,
    plot.colors = plot.colors,
    plot.type = plot.type,
    subgroup = subgroup
  )
}
