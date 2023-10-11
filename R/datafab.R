library(tidyverse)

set.seed(123)

data <- data.frame(
  ID = rep(1:10000, each = 37),
  month = rep(0:36, 10000)
)

data <- data %>%
  group_by(ID) %>%
  mutate(
    max_month = sample(10:36, 1),
    sex = sample(c(1, 2), 1),
    censor = if_else(month == max_month, 1, 0)
  ) %>%
  ungroup() %>%
  mutate(
    L = ave(runif(n()), ID, FUN = cumsum), # Strictly increasing random process
    eligible_end_month = ave(month, ID, FUN = function(x) sample(x, 1))
  ) %>%
  group_by(ID) %>%
  mutate(
    eligible = if_else(month < eligible_end_month, 1, 0),
    treat_initiation = if_else(eligible == 1 & lead(eligible, default = 0) == 0, 1, 0)
  ) %>%
  filter(month <= max_month) %>%
  select(-max_month, -eligible_end_month)
library(data.table)
test <- SEQexpand(data, "ID", "month", "eligible")


library(tidyverse)
library(data.table)
data1 <- data.frame(
  ID = rep(1, 26),
  eligible = c(rep(1, 3), rep(0, 23)),
  month = 0:25,
  treat_initiation = c(0, 0, 1, rep(0, 23)),
  sex = rep(1, 26),
  L_bas = rep(2.47, 26),
  L = c(rep(2.47, 2), 2.77, rep(2.77, 2), rep(seq(from = 2.78, to = 2.83, length.out = 20), each = 1), 2.84),
  censor = c(rep(0, 25), 1),
  Event = rep(0, 26)
) %>%
  mutate(eligible = as.logical(eligible))
data2 <- data.frame(
  ID = rep(2, 20),
  eligible = c(rep(1, 3), rep(0, 17)),
  month = 0:19,  # Fix here: changing 0:20 to 0:19
  treat_initiation = c(0, 0, 1, rep(0, 17)),
  sex = rep(1, 20),
  L_bas = rep(2.47, 20),
  L = c(rep(2.47, 2), 2.77, rep(2.77, 2), rep(seq(from = 2.78, to = 2.83, length.out = 14), each = 1), 2.84),
  censor = c(rep(0, 19), 1),
  Event = rep(0, 20)
)

data3 <- rbind(data1, data2)

library(data.table)

N_PATIENTS <- 10000
MAX_TIME <- 59

# Create a data.table structure
dt <- data.table()

set.seed(123) # Setting seed for reproducibility

for (patient_id in 1:N_PATIENTS) {
  # Decide randomly when this patient "falls off"
  drop_time <- sample(0:MAX_TIME, 1)

  # Assign sexM for this patient
  sexM <- rbinom(1, 1, 0.5)

  # Initial value for L for this patient
  L_value <- rpois(1, 5)

  treat_init_flag <- FALSE

  for (t in 0:MAX_TIME) {
    if (t > drop_time) {
      break
    }

    N_value <- max(rnorm(1, 10, 5), 0)

    # Decide eligibility
    if (t == drop_time || (t != 0 && eligible == 0)) {
      eligible <- 0
    } else {
      eligible <- 1
    }

    # Determine treat_init
    if (eligible == 1 && !treat_init_flag) {
      treat_init <- 1
      treat_init_flag <- TRUE
    } else {
      treat_init <- 0
    }

    # Determine L value
    if (treat_init == 1) {
      L_value <- L_value * 0.99
    } else {
      L_value <- L_value * 1.05
    }

    dt <- rbindlist(list(dt, data.table(id = patient_id, time = t, eligible = eligible,
                                        treat_init = treat_init, censor = 0, sexM = sexM,
                                        N = N_value, L = L_value)), use.names = TRUE)
  }

  # Mark censor for the last time point of this patient
  dt[id == patient_id & time == max(time), censor := 1]
}

print(dt)

