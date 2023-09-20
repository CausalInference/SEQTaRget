#' Internal handler for expanding the input dataset
#'
#' @param data Dataframe: data to expand
#' @param id_col String: columnname of the ID
#' @param time_col String: columname of the time column
#' @param max Integer: maximum in \code{time_col} to expand about
#' @param min Integer: minimum in \code{time_col} to expand about
#'
#' @import data.table
#'
#' @export
SEQexpand <- function(data, id_col, time_col, max = NULL, min = NULL){
  data <- as.data.table(data)
  original_data <- copy(data)

  if (!is.null(min)) data <- data[get(time_col) >= min, ]
  if (!is.null(max)) data <- data[get(time_col) <= max, ]

  expanded_data <- data[rep(1:.N, get(time_col)), .SD]
  expanded_data[, period := get(time_col) - seq_len(.N), by = .(time = get(time_col), id = get(id_col))]

  excluded_rows <- original_data[!get(time_col) %in% expanded_data$time | !get(id_col) %in% expanded_data$id, .SD]
  excluded_rows[, period := get(time_col) - 1]

  rbind(excluded_rows, expanded_data)
}





data <- data.frame(
  ID = rep(1, 26),
  eligible = c(rep(1, 3), rep(0, 23)),
  month = 0:25,
  treat_initiation = c(0, 0, 1, rep(0, 23)),
  sex = rep(1, 26),
  L_bas = rep(2.47, 26),
  L = c(rep(2.47, 2), 2.77, rep(2.77, 2), rep(seq(from = 2.78, to = 2.83, length.out = 20), each = 1), 2.84),
  censor = c(rep(0, 25), 1),
  Event = rep(0, 26)
)

library(dtplyr)
library(tidyverse)
values <- data %>% select(ID, month, L)
max_time <- data %>% group_by(ID) %>% mutate(rev_row = n() - row_number()) %>% filter(eligible == 1)
max_time$rev_row <- sort(max_time$rev_row)
DT <- max_time %>% select(ID, month, rev_row)
result <- data.table::as.data.table(DT)[, .(month = seq.int(month, rev_row), increment = rep(.I - 1, rev_row - month + 1)), by = .(ID, rev_row)]
output <- as.data.frame(left_join(result, values, by = c("ID", "month")))

library(data.table)

SEQ.expand <- function(data, id_col, time_col, eligible_col) {

  # Convert data to data.table
  data <- as.data.table(data)

  # Compute the reverse row number and filter by the eligible column in one step
  max_time <- data[data[[eligible_col]] == 1, .(rev_row = .N - .I), by = id_col]

  # Sort rev_row within each group
  max_time <- max_time[, .(rev_row = sort(rev_row)), by = id_col]

  # Generate a sequence from the time column to rev_row and compute the increment
  result <- max_time[, .({
    time_col_val = .SD[[time_col]][1]
    rev_row_val = rev_row[1]
    .(
      seq = seq.int(time_col_val, rev_row_val),
      increment = rep(.I - 1, rev_row_val - time_col_val + 1)
    )
  }, by = .(id_col, rev_row)
  ]

  # Perform a left join with the original data table
  output <- result[data, on = c(id_col, time_col)]

  return(output)
}


SEQ.expand(data, "ID", "month", "eligible")
