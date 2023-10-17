#' Internal handler for expanding the input dataset
#'
#' @param data Dataframe or DataTable: data to expand
#' @param id String: column name of the id.col
#' @param time String: colum name of the time column
#' @param eligible String: column name of the eligibility column
#' @param params List: optional list of parameters from \code{SEQOpts}
#' @param ... Other parameters, as passed to \code{SEQOpts.expansion}
#'
#' @import data.table foreach doFuture future sparklyr
#'
#' @export

SEQexpand <- function(data, id.col, time.col, eligible.col, params, ...) {
  # Coercion ==================================================
  DT <- expansion.preprocess(as.data.table(data))
  unique_id <- unique(data[[id.col]])
  opts <- buildParam(); dots <- list(...); errorParams(params, dots)
  memory <- translate_memory(opts$memory)

  #Parameter Space ============================================
  if(!missing(params)) opts[names(params)] <- params
  if(length(dots > 0)) opts[names(dots)] <- dots

  #Parallelization and Spark Setup ============================
  if(opts$parallel == FALSE){
    if(opts$spark == TRUE) stop("SEQuential implementation of Spark requires parallelization")
    result <- internal.expansion()
    return(result)
  }

  if(opts$parallel == TRUE){
    cl <- parallel::makeCluster(opts$ncores)
    future::plan("cluster", workers = cl)

    if(opts$spark == FALSE){
      result <- foreach(id = unique_id, .combine = "rbind", .packages = "data.table") %dopar% {
        outputDT <- internal.expansion(id)
      }
      return(result)
    }
    if(opts$spark == TRUE){
      sc <- spark_connect(master = opts$spark.connection)
      sdf <- copy(sc, data.frame(), "SEQexpanded_data", overwrite = TRUE)
      aggregated <- data.table()

      result <- foreach(id = unique_id, .combine = "rbind", .packages = c("data.table", "sparklyr")) %dopar% {
        outputDT <- internal.expansion(id)

        aggregated <- rbind(aggregated, outputDT)

        if(object.size(aggregated) >= memory){
          sdf_temp <- copy_to(sdf$sc, aggregated, name = "SEQexpanded_data", overwrite = FALSE)
          aggregated <- data.table()
          invisible(sdf_temp)
        }
      }
      if(nrow(aggregated) > 0){
        sdf_temp <- copy_to(sdf$sc, aggregated, name = "SEQexpanded_data", overwrite = FALSE)
        invisible(sdf_temp)
        aggregated <- data.table()
      }
    }
    print("Spark Data: SEQexpanded_data successfully created")
    parallel::stopCluster(cl)
  }
}
