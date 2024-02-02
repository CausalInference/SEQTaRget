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
  if(!opts$parallel){
    result <- internal.expansion(DT, id.col, time.col, eligible.col, outcome.col, opts)
  } else {
    ID.unique <- unique(DT[[id.col]])
    if(opts$nthreads > 1) ID.split <- split(ID.unique, cut(ID.unique, opts$nthreads)) else ID.split <- ID.unique

    if(opts$sys.type %in% c("Unix", "Darwin")){
      result.list <- mclapply(ID.split,
                              function(x) internal.expansion(DT[get(id.col) %in% x, ],
                                                             id.col, time.col, eligible.col, outcome.col, opts),
                              mc.cores = opts$ncores)
      result <- rbindlist(result.list)
    } else if(opts$sys.type == "Windows"){
      result <- foreach(x = ID.split, .combine = "rbind", .packages = c("SEQuential", "data.table")) %dopar% {
        out <- internal.expansion(DT[get(id.col) %in% x, ], id.col, time.col, eligible.col, outcome.col, opts)
      }
    }
  }
  return(result)
}
