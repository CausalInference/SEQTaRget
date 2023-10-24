#' Internal handler for expanding the input dataset
#'
#' @param data Dataframe or DataTable: data to expand
#' @param id.col String: column name of the id column
#' @param time.col String: colum name of the time column
#' @param eligible.col String: column name of the eligibility column
#' @param params List: optional list of parameters from \code{SEQOpts}
#' @param ... Other parameters, as passed to \code{SEQOpts.expansion}
#'
#' @import data.table foreach doFuture future
#'
#' @export

SEQexpand <- function(data, id.col, time.col, eligible.col, params, ...) {
  # Coercion ==================================================
  DT <- expansion.preprocess(as.data.table(data))
  unique_id <- unique(DT[[id.col]])
  opts <- buildParam(); dots <- list(...) #errorParams(params, dots)
  memory <- if(is.character(opts$memory)) translate_memory(opts$memory) else opts$memory

  #Parameter Space ============================================
  if(!missing(params)) opts[names(params)] <- params
  if(length(dots > 0)) opts[names(dots)] <- dots

  #Parallelization Setup ============================
  if(opts$parallel == FALSE){
    result <- internal.expansion(DT, opts)
    return(result)
  }

  if(opts$parallel == TRUE){
    cl <- parallel::makeCluster(opts$ncores)
    future::plan(cluster, workers = cl)

    result <- foreach(id = unique_id, .combine = "rbind", .packages = "data.table") %dopar% {
      outputDT <- internal.expansion(DT, opts, id)
    }
    return(result)
  }
}
