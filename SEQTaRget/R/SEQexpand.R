#' Creates an expanded dataset for use with [SEQuential()]
#'
#' @param params SEQparams object built in the SEQuential function
#'
#' @import data.table
#'
#' @keywords internal
SEQexpand <- function(params) {
  out <- local({
    on.exit({
      rm(list = setdiff(ls(), "out"))
      gc()
    }, add = TRUE)

    # Variable pre-definition ===================================
    sum_elig <- NULL
    followup <- NULL
    dose <- NULL
    trial <- NULL
    isExcused <- NULL
    excused_tmp <- NULL
    firstSwitch <- NULL
    trialID <- NULL
    lag <- NULL
    tx_bas <- paste0(params@treatment, params@indicator.baseline)
    DT <- copy(params@data)

    # Expansion =======================================================
    if (!params@weighted) {
      vars.intake <- c(params@covariates, params@deviation.col, tx_bas)
    } else {
      vars.intake <- c(params@covariates, params@numerator, params@denominator,
                       params@cense.denominator, params@cense.numerator, 
                       params@visit.denominator, params@visit.numerator,
                       params@deviation.col, tx_bas)
    }
    vars <- unique(c(unlist(strsplit(vars.intake, "\\+|\\*|\\:")),
                     params@treatment, params@cense, params@cense.eligible, params@visit,
                     params@compevent, unlist(params@weight.eligible_cols), params@subgroup))
    vars.nin <- c("dose", "dose_sq", params@time, paste0(params@time, params@indicator.squared), "tx_lag", "censored")
    vars <- vars[!is.na(vars)]
    vars <- vars[!vars %in% vars.nin]
    vars.base <- vars[grep(params@indicator.baseline, vars)]
    vars.sq <- vars[grep(params@indicator.squared, vars)]
    vars.time <- c(vars[!vars %in% vars.base], unlist(params@excused.cols), unlist(params@deviation.excused_cols))
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
    }

    if (params@method == "censoring") {
      out[, switch := FALSE]
      if (params@deviation) {
        # Censoring on deviation condition
        for (i in seq_along(params@treat.level)) {
          conditional <- paste0(paste0(params@treatment, params@indicator.baseline), "==", params@treat.level[[i]],
                                " & ", params@deviation.col, params@deviation.conditions[[i]])
          
          out[eval(parse(text = conditional)), switch := TRUE]
        }
        if (params@deviation.excused) {
          # Excusing deviation conditions
          for (i in seq_along(params@treat.level)) {
            if (!is.na(params@deviation.excused_cols[[i]])) {
              out[(switch), isExcused := ifelse(get(params@deviation.excused_cols[[i]]) == 1, 1, 0)]
            }
          }
          out[!is.na(isExcused), excused_tmp := cumsum(isExcused), by = c(params@id, "trial")
              ][(excused_tmp) > 0, switch := FALSE, by = c(params@id, "trial")
                ][, excused_tmp := FALSE]
        } 
      }  else {
        # Automatic switch definition (based on treatment and treatment lag)
        out[, lag := shift(get(params@treatment), fill = get(params@treatment)[1]), by = c(params@id, "trial")]
        
        if (params@excused) {
          # Excused treatment lag switches
          out[, switch := (get(params@treatment) != lag)]
          
          for (i in seq_along(params@treat.level)) {
            if (!is.na(params@excused.cols[[i]])) {
              out[(switch) & 
                    get(params@treatment) != lag & 
                    get(params@treatment) == params@treat.level[[i]], isExcused := ifelse(get(params@excused.cols[[i]]) == 1, 1, 0)]
            }
          }
          setorderv(out, c(params@id, "trial", "followup"))
          out[!is.na(isExcused), excused_tmp := cumsum(isExcused), by = c(params@id, "trial")
              ][(excused_tmp) > 0, switch := FALSE, by = c(params@id, "trial")
                ][, excused_tmp := NULL]
        } else {
          # Non-excused treatment lag switches
          out[, `:=`(
            trial_sq = trial^2,
            switch = get(params@treatment) != shift(get(params@treatment), fill = get(params@treatment)[1])), by = c(params@id, "trial")]
        }
      }
      out[, firstSwitch := if (any(switch)) which(switch)[1] else .N, by = c(params@id, "trial")]
      out <- out[out[, .I[seq_len(firstSwitch[1])], by = c(params@id, "trial")]$V1
                 ][, paste0(params@outcome) := ifelse(switch, NA, get(params@outcome))
                   ][, `:=`(firstSwitch = NULL)
                     ][, "censored" := ifelse(switch, 1, 0)]
    }
    
    if (params@selection.first_trial) {
      out <- out[, "trial.first" := min(get("trial")), by = c(params@id)
                 ][get("trial") == get("trial.first"), .SD
                   ][, "trial.first" := NULL]
    }
    
    if (params@selection.random) {
      set.seed(params@seed)
      out[, "trialID" := paste0(params@id, "-", trial)]
      IDs <- unique(out[get(paste0(params@treatment, params@indicator.baseline)) != params@treat.level[[1]], ][["trialID"]])
      set <- unique(out[get(paste0(params@treatment, params@indicator.baseline)) == params@treat.level[[1]], ][["trialID"]])
      subset <- sample(set, round(length(set) * params@selection.prob))
      out <- out[trialID %in% c(IDs, subset),
                 ][, trialID := NULL]
    }
    return(out)
  })
  return(out)
}
