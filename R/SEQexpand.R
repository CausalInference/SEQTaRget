#' Creates an expanded dataset for use with \code{SEQuential}
#'
#' @param data Dataframe or DataTable: data to expand
#' @param id.col String: column name of the id column
#' @param time.col String: colum name of the time column
#' @param eligible.col String: column name of the eligibility column
#' @param outcome.col String: column name of the outcome column
#' @param opts List: optional list of parameters from \code{SEQOpts}
#' @param ... Other parameters, as passed to \code{SEQOpts.expansion}
#'
#' @import data.table
#'
#' @export

SEQexpand <- function(data, id.col, time.col, eligible.col, outcome.col, opts) {
  # Pre-Processing ==================================================
  cols <- c(id.col, eligible.col)
  binary.cols <- names(data)[sapply(data, function(col) all(unique(col) %in% c(0, 1)))]
  eligible_ids <- unique(data[, ..cols][, sum_elig := sum(.SD[[eligible.col]]), by = id.col
                                        ][sum_elig != 0,
                                          ][[id.col]])

  DT <- data[data[[id.col]] %in% eligible_ids,
             ][, (binary.cols) := lapply(.SD, as.logical), .SDcols = binary.cols]

  #Expansion =========================================================
  if(!parallel){
    result <- internal.expansion(DT, id.col, time.col, eligible.col, outcome.col, opts)
  } else {
    ID.unique <- unique(DT[[id.col]])
    ID.split <- split(ID.unique, cut(ID.unique, opts$nthreads))

    result.list <- mclapply(ID.split,
                            function(x) internal.expansion(DT, id.col, time.col, eligible.col, outcome.col, opts, x),
                            mc.cores <- opts$ncores)
    result <- rbindlist(result.list)
  }
  return(result)
}
