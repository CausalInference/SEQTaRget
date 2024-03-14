#' Internal function that handles the expansion of a data.table (DT).
#' If \code{ID} is specified, it subsets per ID (for use in parallelizing operations by ID)
#'
#' @import data.table
#'
#' @keywords internal
internal.expansion <- function(DT, id.col, time.col, treatment.col, eligible.col, outcome.col, opts){
  vars <- c(unlist(strsplit(opts$covariates, "\\+|\\*")), treatment.col)
  vars.base <- vars[grep(opts$baseline.indicator, vars)]
  vars.sq <- vars[grep(opts$sq.indicator, vars)]

  vars.time <- vars[!vars %in% vars.base]
  vars.base <- unique(gsub(opts$baseline.indicator, "", vars.base))
  vars.base <- vars.base[!vars.base %in% time.col]
  vars.sq <- unique(sub(opts$sq.indicator, "", vars.sq))
  vars.kept <- c(vars, id.col, "trial", "period", "followup")

  data <- DT[(get(eligible.col)), .(period = Map(seq, get(time.col), table(DT[[id.col]])[.GRP] - 1)), by = eval(id.col)
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

return(out)
}
