#if(!require(pacman)) install.packages(pacman); pacman::p_load(data.table, foreach, doParallel, doRNG, SEQuential)

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

    ID <- data.table(ID = rep(i, max_time+1),
                     time = 0:max_time,
                     eligible = c(rep(1, eligible_time+1), rep(0, max_time-eligible_time)),
                     outcome = outcome_vector,
                     tx_init = c(rep(0, tx_time), 1, rep(1, max_time-tx_time)),
                     sex = rep(sex, max_time+1),
                     N = rnorm(length(0:max_time), 10, 5),
                     L = runif(1),
                     P = runif(1, 9, 10))

    ID[, `:=`(L = L[1] * cumprod(c(1, rep(1.04, .N-1))),
              P = P[1] * cumprod(c(1, rep(0.98, .N-1))))]

    if(outcome == 1) ID <- ID[time <= outcome_time]

    return(ID)
  }
  stopCluster(cl)
  return(output)
}
#myparams <- SEQopts(covariates = "tx_init_bas+tx_init_bas*followup+tx_init_bas*followup_sq + followup + followup_sq + sex + N_bas + L_bas + P_bas")
#data <- gen_data()
#test <- SEQuential::SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", method = "ITT", myparams)
#print(test$coefficients)

#Debugging Junk ==========
#id.col = "ID"; time.col = "time"; eligible.col = "eligible"; outcome.col = "outcome"; treatment.col = "tx_init"; method = "ITT"
#opts <- SEQuential::SEQopts()
#opts$covariates = "tx_init_bas+tx_init_bas*period+tx_init_base*period_sq+period+period_sq+sex+N_bas+L_bas+P_bas"
#autoplot(test$surv)
