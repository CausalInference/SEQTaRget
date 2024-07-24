#' Creates an expanded dataset for use with \code{SEQuential}
#'
#' @param params SEQparams object built in the SEQuential function
#'
#' @import data.table
#'
#' @keywords internal
SEQexpand <- function(params) {
  # Pre-Processing =============================================
  cols <- c(params@id, params@eligible)
  eligible_ids <- unique(params@data[, ..cols][, sum_elig := sum(.SD[[params@eligible]]), by = eval(params@id)
                                               ][sum_elig != 0,
                                                 ][[params@id]])

  DT <- params@data[params@data[[params@id]] %in% eligible_ids, ]
  # Expansion =======================================================
  if(!params@weighted) {
    vars.intake <- c(params@covariates)
  } else {
    vars.intake <- c(params@covariates, params@numerator, params@denominator)
    if(params@excused) vars.intake <- c(vars.intake, paste0(params@treatment, params@baseline.indicator))
  }
  vars <- unique(c(unlist(strsplit(vars.intake, "\\+|\\*")), params@treatment))
  vars.nin <- c("dose", "dose_sq")
  vars <- vars[!is.na(vars)][!vars %in% vars.nin]
  vars.base <- vars[grep(params@baseline.indicator, vars)]
  vars.sq <- vars[grep(params@squared.indicator, vars)]
  vars.time <- c(vars[!vars %in% vars.base], params@excused.col0, params@excused.col1)
  vars.base <- unique(gsub(params@baseline.indicator, "", vars.base))
  vars.base <- vars.base[!vars.base %in% params@time]
  vars.sq <- unique(sub(params@squared.indicator, "", vars.sq))
  vars.kept <- c(vars, params@id, "trial", "period", "followup")

  data <- DT[get(params@eligible) == 1, .(period = Map(seq, get(params@time), table(DT[[params@id]])[.GRP] - 1)), by = eval(params@id)
             ][, cbind(.SD, trial = rowid(get(params@id)) - 1)
               ][, .(period = unlist(.SD)), by = c(eval(params@id), "trial")
                 ][, followup := as.integer(seq_len(.N)-1), by = c(eval(params@id), "trial")
                   ][followup <= params@max.followup,
                     ]

  data_list <- list()
  if(length(c(vars.time, vars.sq)) > 0){
    data.time <- data[DT, on = c(eval(params@id), "period" = eval(params@time)), .SDcols = vars.time
                      ][, eval(params@eligible) := NULL
                        ][, (paste0(vars.sq, params@squared.indicator)) := lapply(.SD, function(x) x^2), .SDcols = vars.sq]

    vars.found <- unique(c(vars.time, vars.sq, "period", "trial", params@id, params@outcome))
    data_list[["time"]] <- data.time[, ..vars.found]
  }
  if(length(vars.base) > 0){
    data.base <- data[DT, on = c(eval(params@id), "trial" = eval(params@time)), .SDcols = vars.base, nomatch = 0
                      ][, eval(params@eligible) := NULL]

    vars.found <- unique(c(paste0(vars.base, params@baseline.indicator), "period", "trial", params@id))
    setnames(data.base, old = vars.base, new = paste0(vars.base, params@baseline.indicator))
    data_list[["base"]] <- data.base[, ..vars.found]
  }
  if(length(data_list) > 1){
    out <- Reduce(function(x, y) merge(x, y, by = c(params@id, "trial", "period"), all = TRUE), data_list)
  } else if(length(data_list) == 1){
    out <- data_list[[1]]
  }

  if(params@method == "dose-response"){
    out <- out[, dose := cumsum(get(params@treatment)), by = c(eval(params@id), "trial")
               ][, `:=` (dose_sq = dose^2,
                         trial_sq = trial^2)]
  }

  if(params@method == "censoring"){
    if(params@excused) {
      out <- out[, switch := (get(params@treatment) != shift(get(params@treatment), fill = get(params@treatment)[1])), by = c(eval(params@id), "trial")]

      if(is.na(params@excused.col0)){params@excused.col0 <- "tmp0"; out <- out[, tmp0 := 0]}
      if(is.na(params@excused.col1)){params@excused.col1 <- "tmp1"; out <- out[, tmp1 := 0]}

      out <- out[(switch) & get(params@treatment) == 0, isExcused := ifelse(get(params@excused.col1) == 1, 1, 0)
                   ][(switch) & get(params@treatment) == 1, isExcused := ifelse(get(params@excused.col0) == 1, 1, 0)
                     ][!is.na(isExcused), excused_tmp := cumsum(isExcused), by = c(eval(params@id), "trial")
                       ][(excused_tmp) > 0, switch := FALSE, by = c(eval(params@id), "trial")
                         ][, firstSwitch := if(any(switch)) which(switch)[1] else .N, by = c(eval(params@id), "trial")
                           ][, excused_tmp := NULL]
    } else {
      out <- out[, `:=` (trial_sq = trial^2,
                       switch = get(params@treatment) != shift(get(params@treatment), fill = get(params@treatment)[1])), by = c(eval(params@id), "trial")
               ][, firstSwitch := if(any(switch)) which(switch)[1] else .N, by = c(eval(params@id), "trial")]
    }
      out <- out[out[, .I[seq_len(firstSwitch[1])], by = c(eval(params@id), "trial")]$V1
                 ][, paste0(params@outcome) := ifelse(switch, NA, get(params@outcome))
                   ][, `:=` (firstSwitch = NULL,
                             switch = NULL)]
  }
  return(out)
}
