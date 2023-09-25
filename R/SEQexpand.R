#' Internal handler for expanding the input dataset
#'
#' @param data Dataframe: data to expand
#' @param id_col String: columnname of the ID
#' @param time_col String: columname of the time column
#' @param max Integer: maximum in \code{time_col} to expand about
#' @param min Integer: minimum in \code{time_col} to expand about
#' @param parallel Logical; default is TRUE
#' @param cores Integer: number of cores for parallelization
#'
#' @import data.table foreach doParallel
#'
#' @export
SEQexpand <- function(data, id.col, time.col, eligible.col, min = NULL, max = NULL, parallel = TRUE, cores = parallel::detectCores()) {
  data <- as.data.table(data)
  unique_ids <- unique(data[[id.col]])

  if(!is.null(min) && !is.null(max)){
    if(max < min) stop("The maximum is less than the minimum for expansion")
  }
  if(!is.null(min)) data <- data[get(time.col) >= min]
  if(!is.null(max)) data <- data[get(time.col) <= max]
  if(parallel == TRUE){cl <- makeCluster(cores); registerDoParallel(cl)}

  process_id <- function(id) {
    DT_sub <- data[get(id.col) == id]

    DT1_sub <- DT_sub[, `:=`(rev_row = .N - seq_len(.N)), by = get(id.col)
                      ][get(eligible.col) == 1
                        ][, rev_row := sort(rev_row), by = get(id.col)
                          ][, .(time = seq.int(get(time.col), rev_row),
                                trial = rep(.GRP - 1, rev_row - get(time.col) + 1)),
                            by = c(id.col, 'rev_row')
                            ][, setnames(.SD, old = "time", new = time.col)
                              ][, -c('rev_row')
                                ][, period := seq_len(.N) - 1, by = trial]
    DT2_sub <- DT1_sub[DT_sub, on = c(id.col, time.col)
                       ][, c('rev_row', eligible.col) := NULL]

    return(DT2_sub)
  }

  resultDT <- foreach(id = unique_ids, .combine = 'rbind', .packages = 'data.table') %dopar% {
    process_id(id)
  }

  stopCluster(cl)

  return(resultDT)
}
