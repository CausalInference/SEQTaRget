setOldClass(c("gg", "ggplot"))

setClass("SEQopts",
         slots = c(
           parallel = "logical",
           nthreads = "numeric",
           ncores = "integer",
           nboot = "integer",
           bootstrap = "logical",
           boot.sample = "numeric",
           seed = "integer",
           max.followup = "numeric",
           max.survival = "numeric",
           include.trial = "logical",
           include.period = "logical",
           weighted = "logical",
           pre.expansion = "logical",
           excused = "logical",
           excused.col0 = "character",
           excused.col1 = "character",
           covariates = "character",
           numerator = "character",
           denominator = "character",
           baseline.indicator = "character",
           squared.indicator = "character"
         ), prototype = list(
           parallel = FALSE,
           nthreads = data.table::getDTthreads(),
           ncores = parallel::detectCores(),
           bootstrap = FALSE,
           boot.sample = 0.8,
           seed = 1636L,
           max.followup = Inf,
           max.survival = Inf,
           include.trial = TRUE,
           include.period = TRUE,
           weighted = FALSE,
           pre.expansion = TRUE,
           excused = FALSE,
           excused.col0 = NA_character_,
           excused.col1 = NA_character_,
           covariates = NA_character_,
           numerator = NA_character_,
           denominator = NA_character_,
           baseline.indicator = "_bas",
           squared.indicator = "_sq"
         ))

setClass("SEQparams",
         contains = "SEQopts",
         slots = c(
           data = "data.table",
           DT = "data.table",
           id = "character",
           time = "character",
           eligible = "character",
           treatment = "character",
           outcome = "character",
           time_varying = "list",
           fixed = "list",
           method = "character"
         ), prototype = list(
           data = NA,
           DT = NA,
           id = NA_character_,
           time = NA_character_,
           eligible = NA_character_,
           treatment = NA_character_,
           outcome = NA_character_,
           time_varying = list(),
           fixed = list(),
           method = NA_character_
         ))

setClass("SEQweights",
         slots = c(
           weights = "data.table",
           coef.n0 = "numeric",
           coef.n1 = "numeric",
           coef.d0 = "numeric",
           coef.d1 = "numeric"
         ), prototype = c(
           weights = NA,
           coef.n0 = NA_real_,
           coef.n1 = NA_real_,
           coef.d0 = NA_real_,
           coef.d1 = NA_real_
         ))

setClass("SEQuential",
         contains = c("ggplot"),
         slots = c(
           bootstrap = "logical",
           boot.sample = "numeric",
           seed = "integer",
           nboot = "integer",
           outcome = "character",
           numerator = "character",
           denominator = "character",
           outcome_model = "list",
           survival_curve = "gg",
           survival_data = "data.table",
           risk_difference = "numeric",
           risk_ratio = "numeric",
           elapsed_time = "character",
           weight_statistics = "list"

         ), prototype = c(
           bootstrap = FALSE,
           boot.sample = NA_real_,
           seed = NA_integer_,
           nboot = NA_integer_,
           outcome = NA_character_,
           numerator = NA_character_,
           denominator = NA_character_,
           outcome_model = list(),
           survival_curve = NA,
           survival_data = data.table(),
           risk_difference = NA_real_,
           risk_ratio = NA_real_,
           elapsed_time = NA_character_,
           weight_statistics = list()
         ))
