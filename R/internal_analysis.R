internal.analysis <- function(data, DT, id.col, time.col, outcome.col, treatment.col, opts){
  if(opts$bootstrap){

    subsample <- lapply(1:opts$nboot, function(x){
      set.seed(opts$seed + x)
      id.sample <- sample(unique(DT[[id.col]]),
                          opts$boot.sample*length(unique(DT[[id.col]])), replace = FALSE)
      return(id.sample)
    })

    if(opts$parallel){
      sys.type <- Sys.info()['sysname']
      if(sys.type %in% c("Darwin", "Unix")){

        #MCLAPPLY LOOPING

      } else if(sys.type == "Windows"){
        #FOREACH LOOPING
      }
    }
  }
}


#Model Dispersion ===========================================
if(!opts$weighted){
  model <- internal.model(DT, method, outcome.col, opts)
} else if (opts$weighted){
  if(opts$stabilized && opts$weight.time == "pre"){
    WT <- internal.weights(DT, data, id.col, time.col, eligible.col, outcome.col, treatment.col, opts)
    if(!opts$expand){
      #In the case of no expansion, weights bind to the original data on defined time.col and id.col
      WT_data <- DT[WT$weighted_data, on = c(time.col, id.col)
      ][, wt := ifelse(get(time.col) == 0, 1, wt), by = id.col]
    }
    if(opts$expand){
      #If expanded, time.col has been written to 'followup' and 'period'
      WT_data <- DT[WT$weighted_data, on = c(id.col, "followup")
      ][, wt := ifelse(followup == 0, 1, wt)]
    }
    if(opts$weight.time == "pre"){
      weightModel <- 'MODEL USING WEIGHTS FIT PRE-EXPANSION'
    }
  } else if(opts$stabilized == FALSE || weight.time == "post"){
    weightModel <- 'MODEL USING WEIGHTS FIT POST-EXPANSION'
  }
}
