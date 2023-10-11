convert.to.bool <- function(DT){
  binary.cols <- names(DT)[sapply(DT, function(col) all(unique(col) %in% c(0, 1)))]
  DT[, (binary.cols) := lapply(.SD, as.logical), .SDcols = binary.cols]

  return(DT)
}
