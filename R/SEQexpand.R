#' Creates an expanded dataset for use with \code{SEQuential}
#'
#' @param data Dataframe or DataTable: data to expand
#' @param id.col String: column name of the id column
#' @param time.col String: colum name of the time column
#' @param eligible.col String: column name of the eligibility column
#' @param outcome.col String: column name of the outcome column
#' @param opts List: optional list of parameters from \code{SEQOpts}
#'
#' @import data.table parallel foreach doParallel
#'
#' @export
SEQexpand <- function(data, id.col, time.col, treatment.col, eligible.col, outcome.col, method, opts) {
  # Pre-Processing =============================================
  cols <- c(id.col, eligible.col)
  eligible_ids <- unique(data[, ..cols][, sum_elig := sum(.SD[[eligible.col]]), by = id.col
                                        ][sum_elig != 0,
                                          ][[id.col]])

  DT <- data[data[[id.col]] %in% eligible_ids, ]
  # Expansion =======================================================
  if(!opts$weighted | opts$pre.expansion) {
    vars.intake <- c(opts$covariates)
  } else {
    vars.intake <- c(opts$covariates, opts$numerator, opts$denominator)
    if(opts$excused) vars.intake <- c(vars.intake, paste0(treatment.col, opts$baseline.indicator))
  }
  vars <- unique(c(unlist(strsplit(vars.intake,
                                   "\\+|\\*")), treatment.col,
                   names(DT)[!names(DT) %in% c(eligible.col, time.col)]))
  vars.nin <- c("dose", "dose_sq")
  vars <- vars[!is.na(vars)][!vars %in% vars.nin]

  vars.base <- vars[grep(opts$baseline.indicator, vars)]
  vars.sq <- vars[grep(opts$sq.indicator, vars)]
  vars.time <- vars[!vars %in% vars.base]
  vars.base <- unique(gsub(opts$baseline.indicator, "", vars.base))
  vars.base <- vars.base[!vars.base %in% time.col]
  vars.sq <- unique(sub(opts$sq.indicator, "", vars.sq))
  vars.kept <- c(vars, id.col, "trial", "period", "followup")

  data <- DT[(get(eligible.col)) == 1, .(period = Map(seq, get(time.col), table(DT[[id.col]])[.GRP] - 1)), by = eval(id.col)
             ][, cbind(.SD, trial = rowid(get(id.col)) - 1)
               ][, .(period = unlist(.SD)), by = c(eval(id.col), "trial")
                 ][, followup := as.integer(seq_len(.N)-1), by = c(eval(id.col), "trial")
                   ][followup <= opts$max.followup,
                     ]

  data_list <- list()
  if(length(c(vars.time, vars.sq)) > 0){
    data.time <- data[DT, on = c(id.col, "period" = time.col), .SDcols = vars.time
                      ][, eval(eligible.col) := NULL
                        ][, (paste0(vars.sq, opts$sq.indicator)) := lapply(.SD, function(x) x^2), .SDcols = vars.sq]

    vars.found <- unique(c(vars.time, vars.sq, "period", "trial", id.col, outcome.col))
    data_list[["time"]] <- data.time[, ..vars.found]
  }
  if(length(vars.base) > 0){
    data.base <- data[DT, on = c(id.col, "trial" = time.col), .SDcols = vars.base, nomatch = 0
                      ][, eval(eligible.col) := NULL]

    vars.found <- unique(c(paste0(vars.base, opts$baseline.indicator), "period", "trial", id.col))
    setnames(data.base, old = vars.base, new = paste0(vars.base, opts$baseline.indicator))
    data_list[["base"]] <- data.base[, ..vars.found]
  }
  if(length(data_list) > 1){
    out <- Reduce(function(x, y) merge(x, y, by = c(id.col, "trial", "period"), all = TRUE), data_list)
  } else if(length(data_list) == 1){
    out <- data_list[[1]]
  }

  if(method == "dose-response"){
    out <- out[, dose := cumsum(get(treatment.col)), by = c(id.col, "trial")
               ][, `:=` (dose_sq = dose^2,
                         trial_sq = trial^2)]
  }

  if(method == "censoring"){
    if(opts$excused) {
      out <- out[, switch := (get(treatment.col) != shift(get(treatment.col), fill = get(treatment.col)[1])), by = c(id.col, "trial")]

      if(is.na(opts$excused.col0)){opts$excused.col0 <- "tmp0"; out <- out[, tmp0 := 0]}
      if(is.na(opts$excused.col1)){opts$excused.col0 <- "tmp1"; out <- out[, tmp1 := 0]}

      out <- out[(switch) & get(treatment.col) == 0, isExcused := ifelse(get(opts$excused.col1) == 1, 1, 0)
                   ][(switch) & get(treatment.col) == 1, isExcused := ifelse(get(opts$excused.col0) == 1, 1, 0)
                     ][!is.na(isExcused), isExcused := cumsum(isExcused), by = c(id.col, "trial")
                       ][(isExcused) > 0, switch := FALSE, by = c(id.col, "trial")
                         ][, firstSwitch := if(any(switch)) which(switch)[1] else .N, by = c(id.col, "trial")]
    } else {
      out <- out[, `:=` (trial_sq = trial^2,
                       switch = get(treatment.col) != shift(get(treatment.col), fill = get(treatment.col)[1])), by = c(id.col, "trial")
               ][, firstSwitch := if(any(switch)) which(switch)[1] else .N, by = c(id.col, "trial")]
    }
      out <- out[out[, .I[seq_len(firstSwitch[1])], by = c(id.col, "trial")]$V1
                 ][, paste0(outcome.col) := ifelse(switch, NA, get(outcome.col))
                   ][, `:=` (firstSwitch = NULL,
                             switch = NULL)]
  }
  return(out)
}
