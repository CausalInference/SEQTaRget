# if(!require(pacman)) install.packages(pacman); pacman::p_load(data.table, foreach, doParallel)

gen_data <- function(){
  n_patients <- 1e4; max_time <- 59; ncores <- parallel::detectCores() - 1; cl <- makeCluster(ncores); registerDoParallel(cl)
  tictoc::tic()
  output <- foreach(i = 1:n_patients, .combine = "rbind", .packages = c("data.table")) %dopar% {
    set.seed(1636+i)
    eligible_time <- sample(0:max_time, 1)
    sex <- rbinom(1, 1, 0.5)

    ID <- data.table(ID = rep(i, max_time+1),
                     time = 0:max_time,
                     eligible = c(rep(1, eligible_time), rep(0, max_time-eligible_time+1)),
                     sex = rep(sex, max_time+1),
                     N = rnorm(length(0:max_time), 10, 5),
                     L = runif(1),
                     P = runif(1, 9, 10))

    ID[, `:=`(L = L[1] * cumprod(c(1, rep(1.04, .N-1))),
              P = P[1] * cumprod(c(1, rep(0.98, .N-1))),
              tx_init = as.integer(eligible == 1 &
                                     shift(eligible, type = "lag", fill = 1) == 1 &
                                     shift(eligible, type = "lead", fill = 1) == 0))]

    return(ID)
  }
  stopCluster(cl)
  tictoc::toc()
  return(output)
}
# data <- gen_data()
# id.col = "ID"; eligible.col = "eligible"; time.col = "time"; outcome.col = "tx_init"
#tictoc::tic()
#test <- SEQexpand(data, "ID", "time", "eligible")
#test2 <- SEQuential(data, "ID", "time", "eligible", "tx_init", method = "ITT")
#tictoc::toc()
