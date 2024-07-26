## code to prepare `SEQdata` dataset goes here
#' Generation function for example data
#'
#' @param n Numeric: number of 'individuals' to simulate
#' @param max.time Integer: max followup time per individual
#'
#' @keywords internal
generate_data <- function(n = 300, max.time = 59){
  output <- future.apply::future_lapply(1:n, function(x){
    sex <- as.integer(rbinom(1, 1, 0.5))
    outcome <- as.integer(rbinom(1, 1, 0.7))
    tx_time <- as.integer(sample(0:max.time, 1))


    if(outcome == 1) outcome_time <- as.integer(sample(0:max.time, 1)) else outcome_time <- NA
    if(is.na(outcome_time)){
      eligible_time <- tx_time
      outcome_vector <- rep(0, max.time + 1)
    } else {
      eligible_time <- outcome_time
      outcome_vector <- c(rep(0, outcome_time), 1, rep(0, max.time - outcome_time))
    }

    tx_vector <- numeric(max.time + 1)
    for(j in 1:max.time+1){
      if(j == 1) tx_vector[j] <- 0
      if(tx_vector[j-1] == 0) tx_vector[j] <- sample(c(0,1), prob = c(0.8, 0.2))
      if(tx_vector[j-1] == 1) tx_vector[j] <- sample(c(0,1), prob = c(0.05, 0.95))
    }

    excused0_vector <- numeric(max.time + 1)
    excused1_vector <- numeric(max.time + 1)
    for(j in 1:max.time+1){
      if(j == 1){
        excused0_vector[j] <- 0
        excused1_vector[j] <- 0
      }
      if(excused0_vector[j-1] == 0) excused0_vector[j] <- rbinom(1, 1, 0.05)
      if(excused1_vector[j-1] == 0) excused1_vector[j] <- rbinom(1, 1, 0.05)

      if(excused0_vector[j-1] == 1) excused0_vector[j] <- 1
      if(excused1_vector[j-1] == 1) excused1_vector[j] <- 1
    }

    ID <- data.table::data.table(ID = rep(x, max.time+1),
                                 time = 0:max.time,
                                 eligible = c(rep(1, eligible_time+1),
                                              rep(0, max.time-eligible_time)),
                                 outcome = outcome_vector,
                                 tx_init = tx_vector,
                                 sex = rep(sex, max.time+1),
                                 N = rnorm(length(0:max.time), 10, 5),
                                 L = runif(1),
                                 P = runif(1, 9, 10),
                                 excusedZero = excused0_vector,
                                 excusedOne = excused1_vector)
    ID[, `:=`(L = L[1] * cumprod(c(1, rep(1.04, .N-1))),
              P = P[1] * cumprod(c(1, rep(0.98, .N-1))))]

    if(outcome == 1) ID <- ID[time <= outcome_time]
    return(ID)
  }, future.seed = 1636)
  return(data.table::rbindlist(output))
}
SEQdata <- generate_data()
usethis::use_data(SEQdata, overwrite = TRUE)
