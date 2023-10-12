#' Internal function that handles the expansion of a data.table (DT).
#' If \code{ID} is specified, it subsets per ID to parallelize operations by ID.
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

  out <- data[DT[, eval(eligible.col) := NULL],
                on = c(id.col, "period" = time.col)]

  return(out)
}
