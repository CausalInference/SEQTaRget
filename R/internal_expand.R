#' Internal function to extract only IDs that are eligible and convert any binary columns to type logical
#'
#' @import data.table
#'
#' @keywords internal
expansion.preprocess <- function(data, id.col, eligible.col, outcome.col){
  cols <- c(id.col, eligible.col)
  binary.cols <- names(data)[sapply(data, function(col) all(unique(col) %in% c(0, 1)))]

  eligible_ids <- unique(data[, ..cols][, sum_elig := sum(.SD[[eligible.col]]), by = id.col
                                      ][sum_elig != 0,
                                        ][[id.col]])

  DT <- data[data[[id.col]] %in% eligible_ids,
           ][, (binary.cols) := lapply(.SD, as.logical), .SDcols = binary.cols]

  return(DT)
}

#' Internal function that handles the expansion of a data.table (DT).
#' If \code{ID} is specified, it subsets per ID (for use in parallelizing operations by ID)
#'
#' @import data.table
#'
#' @keywords internal
internal.expansion <- function(DT, id.col, time.col, eligible.col, outcome.col, opts, id){
  if(!missing(id)) DT[get(id.col) == id,]

  vars <- unlist(strsplit(opts$covariates, "\\+|\\*"))
  vars.base <- vars[grep(opts$baseline.indicator, vars)]
  vars.sq <- vars[grep(opts$sq.indicator, vars)]

  vars.time <- vars[!vars %in% vars.base]
  vars.base <- unique(sub(opts$baseline.indicator, "", vars.base))
  vars.sq <- unique(sub(opts$sq.indicator, "", vars.sq))
  vars.kept <- c(vars, id.col, "trial", "period")

  data <- DT[(get(eligible.col)), .(period = Map(seq, get(time.col), table(DT[[id.col]])[.GRP] - 1)), by = eval(id.col)
             ][, cbind(.SD, trial = rowid(get(id.col)) - 1)
               ][, .(period = unlist(.SD)), by = c(eval(id.col), "trial")
                 ][period <= opts$max,
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

  attr(out, "SEQexpanded") <- TRUE

return(out)
}

