#' Internal function to extract only IDs that are eligible and convert any binary columns to type logical
#'
#' @import data.table
#'
#' @keywords internal
expansion.preprocess <- function(DT){
  cols <- c(id.col, eligible.col)
  binary.cols <- names(DT)[sapply(DT, function(col) all(unique(col) %in% c(0, 1)))]

  eligible_ids <- unique(DT[, ..cols][, sum_elig := sum(.SD[[eligible.col]]), by = id.col
                                      ][sum_elig != 0,
                                        ][[id.col]])

  DT <- DT[DT[[id.col]] %in% eligible_ids,
           ][, (binary.cols) := lapply(.SD, as.logical), .SDcols = binary.cols]

  return(DT)
}

#' Internal function that handles the expansion of a data.table (DT).
#' If \code{ID} is specified, it subsets per ID (for use in parallelizing operations by ID)
#'
#' @import data.table
#'
#' @keywords internal
internal.expansion <- function(id){
  if(!missing(id)) DT[get(id.col) == id,]

  data <- DT[(get(eligible.col)), .(period = Map(seq, get(time.col), table(DT[[id.col]])[.GRP] - 1)), by = eval(id.col)
             ][, cbind(.SD, trial = rowid(get(id.col)) - 1)
               ][, .(period = unlist(.SD)), by = c(eval(id.col), "trial")
                 ][period <= opts$max, ]

  out <- data[DT, on = c(id.col, "period" = time.col)
              ][, eval(eligible.col) := NULL]

  attr(out, "SEQexpanded") <- TRUE
  return(out)
}
