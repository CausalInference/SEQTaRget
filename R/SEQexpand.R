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
  # Pre-Processing ==================================================
  cols <- c(id.col, eligible.col)
  eligible_ids <- unique(data[, ..cols][, sum_elig := sum(.SD[[eligible.col]]), by = id.col
                                        ][sum_elig != 0,
                                          ][[id.col]])

  DT <- data[data[[id.col]] %in% eligible_ids, ]

  # Expansion =======================================================
  vars <- unique(c(unlist(strsplit(c(opts$covariates, opts$numerator, opts$denominator),
                            "\\+|\\*")), treatment.col, names(DT)[!names(DT) %in% c(eligible.col, time.col)]))
  vars <- vars[!is.na(vars)][!vars %in% c("dose", "dose_sq")]

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
               ][, dose_sq := dose^2]
  }

  return(out)
}
