#' Internal handler for expanding the input dataset
#'
#' @param data Dataframe: data to expand
#' @param id_col String: columnname of the ID
#' @param time_col String: columname of the time column
#' @param max Integer: maximum in \code{time_col} to expand about
#' @param min Integer: minimum in \code{time_col} to expand about
#' @param parallel Logical; default is TRUE
#' @param ncores Integer: number of cores for parallelization
#' @param memory String: String to translate to bytes, eg. "6kb", "10mb", "3gb"
#'
#' @import data.table foreach doParallel
#'
#' @export

SEQexpand <- function(data, id.col, time.col, eligible.col, min = NULL, max = NULL, parallel = TRUE, ncores = parallel::detectCores(), memory = "1gb") {
  # Data manip ====
  data <- as.data.table(data)
  memory <- translate_memory(memory)
  unique_ids <- unique(data[[id.col]])

  #Spark Setting ====
  sc <- spark_connect(master = "local")
  sdf <- copy(sc, data.frame(), "SEQexpanded_data", overwrite = TRUE)

  #Parallelization ====
  if(parallel == FALSE) ncores = 1
  cl <- makeCluster(ncores); registerDoParallel(cl)

  #Immediate error checking ====
  if(!is.null(min) && !is.null(max)){
    if(max < min) stop("The maximum is less than the minimum for expansion")
  }
  if(!is.null(min)) data <- data[get(time.col) >= min]
  if(!is.null(max)) data <- data[get(time.col) <= max]

  #Function to Parallelize ====
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

  #Bind Results to Spark table when they reach the memory limit ====
  aggregated <- data.table()
  resultDT <- foreach(id = unique_ids, .combine = 'rbind', .packages = c('data.table', 'sparklyr')) %dopar% {
    DT <- process_id(id)
    aggregated <- rbind(aggregated, DT)

    if(object.size(aggregated) >= memory){
      sdf_temp <- copy_to(sdf$sc, aggregated, name = "SEQexpanded_data", overwrite = FALSE)
      aggregated <- data.table()
      invisible(sdf_temp)
    }
  }
  #Bind any leftovers ====
  if(nrow(aggregated) > 0){
    sdf_temp <- copy_to(sdf$sc, aggregated, name = "SEQexpanded_data", overwrite = FALSE)
    invisible(sdf_temp)
  }

  #Close the connection and stop the clusters ====
  stopCluster(cl)
  spark_disconnect(sc)

  return(resultDT)
}
