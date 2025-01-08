#' Creates an expanded dataset for use with \code{SEQuential}
#'
#' @param params SEQparams object built in the SEQuential function
#'
#' @import data.table
#'
#' @keywords internal
SEQexpand <- function(params) {
  # Variable pre-definition ===================================
  sum_elig <- NULL
  followup <- NULL
  dose <- NULL
  trial <- NULL
  isExcused <- NULL
  excused_tmp <- NULL
  firstSwitch <- NULL
  trialID <- NULL
  DT <- copy(params@data)

  # Expansion =======================================================
  if (!params@weighted) {
    vars.intake <- c(params@covariates, params@surv)
  } else {
    vars.intake <- c(params@covariates, params@numerator, params@denominator, params@surv,
                     params@cense.denominator, params@cense.numerator)
    if (params@excused) vars.intake <- c(vars.intake, paste0(params@treatment, params@indicator.baseline), params@surv)
  }
  vars <- unique(c(unlist(strsplit(vars.intake, "\\+|\\*|\\:")),
                   params@treatment, params@cense, params@cense.eligible,
                   params@compevent, params@weight.eligible0, params@weight.eligible1))
  vars.nin <- c("dose", "dose_sq", params@time, paste0(params@time, params@indicator.squared), "tx_lag")
  vars <- vars[!is.na(vars)]
  vars <- vars[!vars %in% vars.nin]
  vars.base <- vars[grep(params@indicator.baseline, vars)]
  vars.sq <- vars[grep(params@indicator.squared, vars)]
  vars.time <- c(vars[!vars %in% vars.base], params@excused.col0, params@excused.col1)
  vars.time <- vars.time[!is.na(vars.time)]
  vars.base <- unique(gsub(params@indicator.baseline, "", vars.base))
  vars.base <- c(vars.base[!vars.base %in% params@time], params@eligible)
  vars.sq <- unique(sub(params@indicator.squared, "", vars.sq))
  vars.kept <- c(vars, params@id, "trial", "period", "followup")

  data <- DT[, list(period = Map(seq, get(params@time), table(DT[[params@id]])[.GRP] - 1)), by = eval(params@id),
             ][, cbind(.SD, trial = rowid(get(params@id)) - 1)
               ][, list(period = unlist(.SD)), by = c(eval(params@id), "trial")
                 ][, followup := as.integer(seq_len(.N) - 1), by = c(eval(params@id), "trial")
                   ][followup <= params@followup.max,
                     ][followup >= params@followup.min, ]

  data_list <- list()
  if (length(c(vars.time, vars.sq)) > 0) {
    data.time <- data[DT, on = c(eval(params@id), "period" = eval(params@time)), .SDcols = vars.time
                      ][, (paste0(vars.sq, params@indicator.squared)) := lapply(.SD, function(x) x^2), .SDcols = vars.sq]

    vars.found <- unique(c(vars.time, vars.sq, "period", "trial", params@id, params@outcome))
    data_list[["time"]] <- data.time[, vars.found, with = FALSE]
  }
  if (length(vars.base) > 0) {
    data.base <- data[DT, on = c(eval(params@id), "trial" = eval(params@time)), .SDcols = vars.base, nomatch = 0
                      ]

    vars.found <- unique(c(paste0(vars.base, params@indicator.baseline), "period", "trial", params@id))
    setnames(data.base, old = vars.base, new = paste0(vars.base, params@indicator.baseline))
    data_list[["base"]] <- data.base[, vars.found, with = FALSE]
  }
  if (length(data_list) > 1) {
    out <- Reduce(function(x, y) merge(x, y, by = c(params@id, "trial", "period"), all = TRUE), data_list)
  } else if (length(data_list) == 1) {
    out <- data_list[[1]]
  }

  out <- out[get(paste0(params@eligible, params@indicator.baseline)) == 1,
             ][, paste0(params@eligible, params@indicator.baseline) := NULL]

  if (params@method == "dose-response") {
    out <- out[, dose := cumsum(get(params@treatment)), by = c(eval(params@id), "trial")][, `:=`(
      dose_sq = dose^2,
      trial_sq = trial^2
    )]
    return(out)
  }

  if (params@method == "censoring") {
    if (params@excused) {
      out <- out[, switch := (get(params@treatment) != shift(get(params@treatment), fill = get(params@treatment)[1])), by = c(eval(params@id), "trial")
                 ][(switch) & get(params@treatment) == 0, isExcused := ifelse(get(params@excused.col1) == 1, 1, 0)
                   ][(switch) & get(params@treatment) == 1, isExcused := ifelse(get(params@excused.col0) == 1, 1, 0)
                     ][!is.na(isExcused), excused_tmp := cumsum(isExcused), by = c(eval(params@id), "trial")
                       ][(excused_tmp) > 0, switch := FALSE, by = c(eval(params@id), "trial")
                         ][, firstSwitch := if (any(switch)) which(switch)[1] else .N, by = c(eval(params@id), "trial")
                           ][, excused_tmp := NULL]
    } else {
      out <- out[, `:=`(
        trial_sq = trial^2,
        switch = get(params@treatment) != shift(get(params@treatment), fill = get(params@treatment)[1])), by = c(eval(params@id), "trial")
        ][, firstSwitch := if (any(switch)) which(switch)[1] else .N, by = c(eval(params@id), "trial")]
    }
    out <- out[out[, .I[seq_len(firstSwitch[1])], by = c(eval(params@id), "trial")]$V1
               ][, paste0(params@outcome) := ifelse(switch, NA, get(params@outcome))
                 ][, `:=`(switch = NULL)]
  }
  if (params@selection.random) {
    set.seed(params@seed)
    out <- out[, "trialID" := paste0(params@id, "-", trial)]
    IDs <- unique(out[get(paste0(params@treatment, params@indicator.baseline)) != 0, ][["trialID"]])
    set <- unique(out[get(paste0(params@treatment, params@indicator.baseline)) == 0, ][["trialID"]])
    subset <- sample(set, round(length(set) * params@selection.prob))
    out <- out[trialID %in% c(IDs, subset),
               ][, trialID := NULL]
  }
  return(out)
}
