#' Internal handler for expanding the input dataset
#'
#' @param data Dataframe or DataTable: data to expand
#' @param id String: column name of the id.col
#' @param time String: colum name of the time column
#' @param eligible String: column name of the eligibility column
#' @param params List: optional list of parameters from \code{SEQOpts}
#' @param ...
#'
#' @import data.table foreach doParallel sparklyr
#'
#' @export
SEQexpand <- function(data, id.col, time.col, eligible.col, params, ...) {
  # Coercion ==================================================
  DT <- as.data.table(data)
  opts <- buildParam(); dots <- list(...); errorParams(params, dots)
  memory <- translate_memory(opts$memory)

  #Parameter Space ============================================
  if(!missing(params)) opts[names(params)] <- params
  if(length(dots > 0)) opts[names(dots)] <- dots

  #Parallelization and Spark Setup ============================
  if(opts$parallel == TRUE){
    cl <- makeCluster(opts$ncores)
    registerDoParallel(cl)
  }

  if(opts$spark == TRUE){
    sc <- spark_connect(master = opts$spark.connection)
    sdf <- copy(sc, data.frame(), "SEQexpanded_data", overwrite = TRUE)
  }

  if(opts$parallel == FALSE){
    if(opts$spark == TRUE) stop("SEQuential implementation of Spark requires parallelization")
    out <- internal.expansion()
    return(out)
  }

  if(opts$parallel == TRUE){
    if(opts$spark == FALSE){
      out <- foreach(id = unique_id, .combine = "rbind", .packages = "data.table") %dopar% {
        DT <- DT[get(id.col) == unique_id]
        out <- internal.expansion()
      }
    }
  }



  unique_ids <- unique(data[[id.col]])

  #Bind Results to Spark table when they reach the memory limit ====
  aggregated <- data.table()
  resultDT <- foreach(id.col = unique_id, .combine = 'rbind', .packages = c('data.table', 'sparklyr')) %dopar% {
    DT <- process_id.col(id.col)
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
