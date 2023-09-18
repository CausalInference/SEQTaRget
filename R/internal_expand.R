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
  id = rep(c(1, 2, 3, 4, 5), each = 5),
  time = as.integer(rep(1:5, 5)),
  other_var = c("A", "B", "C", "D", "E")
)

SEQexpand(data, "id", "time")
