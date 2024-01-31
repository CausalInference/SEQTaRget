#' Internal analysis tool for handling parallelization/bootstrapping on multiple OS types
#'
#'
#' @import parallel doParallel foreach data.table
internal.analysis <- function(DT, data, id.col, time.col, eligible.col, outcome.col, treatment.col, opts){
  if(opts$bootstrap){

    subsample <- lapply(1:opts$nboot, function(x){
      set.seed(opts$seed + x)
      id.sample <- sample(unique(DT[[id.col]]),
                          round(opts$boot.sample*length(unique(DT[[id.col]]))), replace = FALSE)
      return(id.sample)
    })

    if(opts$parallel){
      if(opts$sys.type %in% c("Darwin", "Unix")){

        result <- parallel::mclapply(subsample, function(x){

          data <- data[get(id.col) %in% x, ]
          DT <- DT[get(id.col) %in% x, ]

          if(!opts$weighted){
            model <- internal.model(DT, method, outcome.col, opts)
          } else if (opts$weighted){
            if(!opts$stabilized && opts$weight.time == "pre"){
              WT <- internal.weights(DT, data, id.col, time.col, eligible.col, outcome.col, treatment.col, opts)
              if(!opts$expand) {
                WDT <- DT[WT$weighted_data, on = c(id.col, time.col)
                          ][wt := ifelse(get(time.col) == 0, 1, wt), by = id.col]
              } else {
                WDT <- DT[WT$weighted_data, on = c(id.col, time.col)
                          ][, wt := ifelse(followup == 0, 1, wt), by = id.col]
              }
            }
            if(opts$weight.time == "pre") {

            }
            if(opts$weight.time == "post" ||  opts$stabilized){

            }
          }
          #Weight models here
        }, mc.cores = opts$ncores)

      } else if(opts$sys.type == "Windows"){
        stop("Windows is sad right now")

        winPar <- function(i){

        }

      }
    }
  }
}
