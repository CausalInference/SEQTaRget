#' Internal function to extract only IDs that are eligible and convert any binary columns to type logical
#'
#' @import data.table
#'
#' @keywords internal
expansion.preprocess <- function(data, id.col, eligible.col){
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
internal.expansion <- function(DT, id.col, time.col, eligible.col, covariates, opts, id){
  if(!missing(id)) DT[get(id.col) == id,]

  vars <- unlist(strsplit(covariates, "\\+|\\*"))
  vars.base <- vars[grep(opts$baseline.indicator, vars)]
  vars.time <- vars[!vars %in% vars.base]
  vars.base <- sub(opts$baseline.indicator, "", vars.base)
  vars.kept <- c(vars, id.col, "trial", "period")

  data <- DT[(get(eligible.col)), .(period = Map(seq, get(time.col), table(DT[[id.col]])[.GRP] - 1)), by = eval(id.col)
             ][, cbind(.SD, trial = rowid(get(id.col)) - 1)
               ][, .(period = unlist(.SD)), by = c(eval(id.col), "trial")
                 ][period <= opts$max,
                   ]

  data_list <- list()
  if(length(vars.base) > 0){
    data.base <- data[DT, on = c(id.col, "trial" = time.col), .SDcols = vars.base, nomatch = 0
                      ][, eval(eligible.col) := NULL]

    setnames(data.base, old = vars.base, new = paste0(vars.base, opts$baseline.indicator))
    vars.found <- vars.kept[vars.kept %in% names(data.base)]

    data_list[["base"]] <- data.base[, ..vars.found]
  }
  if(length(vars.time) > 0){
    data.time <- data[DT, on = c(id.col, "period" = time.col), .SDcols = vars.time
                      ][, eval(eligible.col) := NULL]

    vars.found <- vars.kept[vars.kept %in% names(data.time)]
    data_list[["time"]] <- data.time
  }
  if(length(data_list) > 1){
    out <- Reduce(function(x, y) merge(x, y, by = c(id.col, "trial", "period"), all = TRUE), data_list)
  } else if(length(data_list) == 1){
    out <- data_list[[1]]
  }
  vars <- c(vars, id.col, "period", "trial")
  out <- out[, ..vars]

  attr(out, "SEQexpanded") <- TRUE

return(out)
}


  #idea - if length vars.base > 0, merge on trial = time.col
  # if length vars.time > 0, merge on period = time.col
  # how to merge back together after these two seperate joins? Merge again with _bas on 'period'?
  #non _bas vars should be "period = time.col" _bas vars should be "trial = time.col"
  #non _bas vars should be "period = time.col" _bas vars should be "trial = time.col"
#  out <- data[DT[..vars.base], on = c(id.col, "trial" = time.col)
#              ][, eval(eligible.col) := NULL]

#  out2 <- data[DT[..vars.time], on = c(id.col, "period" = time.col)
#               ][, eval(eligible.col) := NULL]

#  attr(out, "SEQexpanded") <- TRUE
#}
