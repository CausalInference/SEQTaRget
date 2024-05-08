if(!require(pacman)) install.packages(pacman); pacman::p_load(data.table, foreach, doParallel, doRNG, SEQuential, speedglm)

gen_data <- function(){
  n_patients <- 1e3; max_time <- 59; ncores <- parallel::detectCores() - 1; cl <- makeCluster(ncores); registerDoParallel(cl)
  output <- foreach(i = 1:n_patients, .combine = "rbind", .packages = c("data.table")) %dorng% {
    set.seed(1636+i)
    sex <- as.integer(rbinom(1, 1, 0.5))
    outcome <- as.integer(rbinom(1, 1, .07))
    tx_time <- as.integer(sample(0:max_time, 1))

    if(outcome == 1) outcome_time <- as.integer(sample(0:max_time, 1)) else outcome_time <- NA
    if(is.na(outcome_time)) eligible_time <- tx_time else eligible_time <- outcome_time
    if(is.na(outcome_time)){
      outcome_vector <- rep(0, max_time+1)
    } else {
      outcome_vector <- c(rep(0, outcome_time), 1, rep(0, max_time-outcome_time))
    }

    tx_vector <- numeric(max_time+1)
    for(j in 1:max_time+1){
      if(j == 1) tx_vector[j] <- 0
      if(tx_vector[j-1] == 0) tx_vector[j] <- sample(c(0,1), prob = c(0.8, 0.2))
      if(tx_vector[j-1] == 1) tx_vector[j] <- sample(c(0,1), prob = c(0.05, 0.95))
    }
    excused0_vector <- numeric(max_time+1)
    excused1_vector <- numeric(max_time+1)
    for(j in 1:max_time+1){
      if(j == 1){
        excused0_vector[j] <- 0
        excused1_vector[j] <- 0
      }
      if(excused0_vector[j-1] == 0) excused0_vector[j] <- rbinom(1, 1, 0.05)
      if(excused1_vector[j-1] == 0) excused1_vector[j] <- rbinom(1, 1, 0.05)

      if(excused0_vector[j-1] == 1) excused0_vector[j] <- 1
      if(excused1_vector[j-1] == 1) excused1_vector[j] <- 1
    }

    ID <- data.table(ID = rep(i, max_time+1),
                     time = 0:max_time,
                     eligible = c(rep(1, eligible_time+1), rep(0, max_time-eligible_time)),
                     outcome = outcome_vector,
                     tx_init = tx_vector,
                     sex = rep(sex, max_time+1),
                     N = rnorm(length(0:max_time), 10, 5),
                     L = runif(1),
                     P = runif(1, 9, 10),
                     excused0 = excused0_vector,
                     excused1 = excused1_vector)

    ID[, `:=`(L = L[1] * cumprod(c(1, rep(1.04, .N-1))),
              P = P[1] * cumprod(c(1, rep(0.98, .N-1))))]

    if(outcome == 1) ID <- ID[time <= outcome_time]

    return(ID)
  }
  stopCluster(cl)
  return(output)
}
data <- gen_data()
#write.csv(data, "datagenExcused.csv", row.names = FALSE)
#setDTthreads(0)
#doFuture::registerDoFuture()
#doRNG::registerDoRNG()
#future::plan(future::multisession)
#test <- SEQuential::SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", method = "dose-response", fixed.cols = "sex", time.cols = c("N", "L", "P"),
#                               weighted = TRUE, pre.expansion = TRUE)
#print(test$coefficients)
#Debugging Junk ==========
id.col = "ID"; time.col = "time"; eligible.col = "eligible"; outcome.col = "outcome"; treatment.col = "tx_init"; method = "censoring"; time.cols = c("N", "L", "P"); fixed.cols = "sex"
opts <- SEQopts(parallel = TRUE, pre.expansion = TRUE, weighted = TRUE, excused = TRUE, excused.col0 = "excused0", excused.col1 = "excused1")
#opts$covariates = "tx_init_bas+tx_init_bas*period+tx_init_base*period_sq+period+period_sq+sex+N_bas+L_bas+P_bas"
#autoplot(test$surv)



#v <- listenv::listenv()  # requires listenv package
#library(future)
#for (ii in 1:ncores) {
#  v[[ii]] %<-% {
#    Sys.getpid()
#  }
#}
#for (i in 1:ncores) {
  #For windows
#  system(sprintf("taskkill /F /PID %s", v[[i]]))
#}
#For Linux
#system(sprintf("kill -9 %s", v[[i]]))
