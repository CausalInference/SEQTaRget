## code to prepare `SEQdata` dataset goes here
#' Generation function for example data
#'
#' @param n Numeric: number of 'individuals' to simulate
#' @param max.time Integer: max followup time per individual
#'
#' @keywords internal
generate_data <- function(n = 1e3, max.time = 59, LTFU = TRUE, n_treatments = 1) {
  output <- future.apply::future_lapply(1:n, function(x) {
    sex <- as.integer(rbinom(1, 1, 0.5))
    outcome <- as.integer(rbinom(1, 1, 0.02))
    tx_time <- as.integer(sample(0:max.time, 1))

    if (LTFU){
      LTFU.ind <- rbinom(1, 1, .2)
      if (LTFU.ind == 1) {
        LTFU.time <- sample(1:max.time, 1)
        LTFU_vector <- c(rep(0, LTFU.time), 1)
      } else {
        LTFU_vector <- rep(0, max.time+1)
      }
    }

    if (outcome == 1) outcome_time <- as.integer(sample(0:max.time, 1)) else outcome_time <- NA
    if (is.na(outcome_time)) {
      eligible_time <- tx_time
      outcome_vector <- rep(0, max.time + 1)
    } else {
      eligible_time <- outcome_time
      outcome_vector <- c(rep(0, outcome_time), 1, rep(0, max.time - outcome_time))
    }

    tx_vector <- numeric(max.time + 1)
    if(n_treatments == 1) {
      for (j in 1:max.time + 1) {
        if (j == 1) tx_vector[j] <- 0
        if (tx_vector[j - 1] == 0) tx_vector[j] <- sample(c(0, 1), prob = c(0.8, 0.2))
        if (tx_vector[j - 1] == 1) tx_vector[j] <- sample(c(0, 1), prob = c(0.05, 0.95))
      }
    } else {
      for (j in 1:max.time + 1) {
        if (j == 1) tx_vector[j] <- 0
        if (tx_vector[j - 1] == 0) tx_vector[j] <- sample(0:n_treatments, 1, prob = c(0.8, rep((1 - 0.8) / n_treatments, n_treatments)))
        if (tx_vector[j - 1] != 0) {
          switch <- as.logical(rbinom(1, 1, 0.05))
          if(switch) {
            valid <- setdiff(0:n_treatments, tx_vector[j - 1])
            tx_vector[j] <- sample(valid)
          } else tx_vector[j] <- tx_vector[j-1]
        }
      }
    }

    excused0_vector <- numeric(max.time + 1)
    excused1_vector <- numeric(max.time + 1)
    for (j in 1:max.time + 1) {
      if (j == 1) {
        excused0_vector[j] <- 0
        excused1_vector[j] <- 0
      }
      if (excused0_vector[j - 1] == 0) excused0_vector[j] <- rbinom(1, 1, 0.05)
      if (excused1_vector[j - 1] == 0) excused1_vector[j] <- rbinom(1, 1, 0.05)

      if (excused0_vector[j - 1] == 1) excused0_vector[j] <- 1
      if (excused1_vector[j - 1] == 1) excused1_vector[j] <- 1
    }

    ID <- data.table::data.table(
      ID = rep(x, max.time + 1),
      time = 0:max.time,
      eligible = c(
        rep(1, eligible_time + 1),
        rep(0, max.time - eligible_time)
      ),
      outcome = outcome_vector,
      tx_init = tx_vector,
      sex = rep(sex, max.time + 1),
      N = rnorm(length(0:max.time), 10, 5),
      L = runif(1),
      P = runif(1, 9, 10),
      excusedZero = excused0_vector,
      excusedOne = excused1_vector
    )
    ID[, `:=`(
      L = L[1] * cumprod(c(1, rep(1.04, .N - 1))),
      P = P[1] * cumprod(c(1, rep(0.98, .N - 1)))
    )]

    if (LTFU) {
      if (LTFU.ind == 1) ID <- ID[time <= LTFU.time, ]
      ID <- cbind(ID, LTFU = LTFU_vector)
      ID <- cbind(ID, eligible_cense = rep(1, nrow(ID)))
#      ID <- ID[outcome == 1, LTFU := NA]
    }
    if (outcome == 1) ID <- ID[time <= outcome_time]

    return(ID)
  }, future.seed = 1636)
  return(data.table::rbindlist(output))
}
#SEQdata.multitreatment <- generate_data(1e3, 59, FALSE, 2)
#write.csv(SEQdata.multitreatment, "SEQdata_multitreatment.csv", row.names = FALSE)
#usethis::use_data(SEQdata.multitreatment, overwrite = TRUE)
