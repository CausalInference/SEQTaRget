warningWeights <- function(params){
}

warningOpts <- function(opts){
  if(opts$parallel == TRUE){
    if(opts$ncores >= opts$nthreads) warning("Operations are less efficient when allocated threads are less than allocated cores")
  }
}
