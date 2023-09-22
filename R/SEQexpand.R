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
SEQexpand <- function(data, id.col, time.col, eligible.col, min, max) {
  data <- as.data.table(data)
  if(max < min) stop("The maximum is less than the minimum")

  DT1 <- data[, `:=`(rev_row = .N - seq_len(.N)), by = get(id.col)
              ][get(eligible.col) == 1
                ][, rev_row := sort(rev_row), by = get(id.col)
                  ][, .(time = seq.int(get(time.col), rev_row),
                        trial = rep(.GRP - 1, rev_row - get(time.col) + 1)),
                    by = c(id.col, 'rev_row')
                    ][, setnames(.SD, old = "time", new = time.col)
                      ][, -c('rev_row')
                        ][, period := seq_len(.N) - 1, by = trial]

  if(!missing(min)) DT1 <- DT1[get(time.col) >= min]
  if(!missing(max)) DT1 <- DT1[get(time.col) <= max]

  DT2 <- DT1[data, on = c(id.col, time.col)
             ][, c('rev_row', eligible.col) := NULL]

  return(DT2)
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
