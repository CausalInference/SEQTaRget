#' Internal function for defining weights
#'
#' @param DT data.table after expansion
#' @param data data.table for data before expansion
#' @param params object of class SEQparams (defined in SEQuential)
#'
#' @import data.table
#' @importFrom stats quasibinomial coef predict qnorm quantile sd
#' @importFrom fastglm fastglm
#'
#' @keywords internal
internal.weights <- function(DT, data, params) {
  # Variable pre-definition ===================================
  tx_lag <- NULL
  numerator <- denominator <- NULL
  cense1.numerator <- cense1.denominator <- NULL
  followup <- NULL
  isExcused <- NULL

  # Setting up weight data ====================================
  if (!params@pre.expansion) {
    subtable.kept <- c(params@treatment, params@id, params@time)
    params@time <- "period"

    baseline.lag <- data[, subtable.kept, with = FALSE
                         ][, tx_lag := shift(get(params@treatment)), by = eval(params@id)
                           ][data[, .I[1L], by = eval(params@id)]$V1, tx_lag := 0
                             ][, eval(params@treatment) := NULL]

    setnames(baseline.lag, 2, params@time)
    # conditional join
    weight <- rbind(
      DT[followup == 0,
         ][baseline.lag, on = c(params@id, params@time), nomatch = 0],
      DT[, tx_lag := shift(get(params@treatment)), by = c(eval(params@id), "trial")
         ][followup != 0, ])[, paste0(params@time, params@squared.indicator) := get(params@time)^2
                             ]

    if (params@excused) weight <- weight[, isExcused := cumsum(ifelse(is.na(isExcused), 0, isExcused)), by = c(eval(params@id), "trial")]

  } else {
    weight <- data[, tx_lag := shift(get(params@treatment)), by = eval(params@id)
                   ][get(params@time) == 0, tx_lag := 0
                     ][, paste0(params@time, "_sq") := get(params@time)^2]
  }
  # Modeling ======================================================
  if (params@method == "ITT" | params@LTFU) {
    ltfu.numerator.data <- prepare.data(weight, params, type = "numerator", model = NA, case = "LTFU")
    ltfu.denominator.data <- prepare.data(weight, params, type = "denominator", model = NA, case = "LTFU")

    ltfu.numerator <- fastglm::fastglm(ltfu.numerator.data$X, ltfu.numerator.data$y, family = quasibinomial(), method = 2)
    ltfu.denominator <- fastglm::fastglm(ltfu.denominator.data$X, ltfu.denominator.data$y, family = quasibinomial(), method = 2)

  } else {
    if (!(params@excused & params@pre.expansion)){
      n0.data <- prepare.data(weight, params, type = "numerator", model = 0, case = "default")
      n1.data <- prepare.data(weight, params, type = "numerator", model = 1, case = "default")

      numerator0 <- fastglm::fastglm(n0.data$X, n0.data$y, family = quasibinomial(), method = 2)
      numerator1 <- fastglm::fastglm(n1.data$X, n1.data$y, family = quasibinomial(), method = 2)
    }
    d0.data <- prepare.data(weight, params, type = "denominator", model = 0, case = "default")
    d1.data <- prepare.data(weight, params, type = "denominator", model = 1, case = "default")

    denominator0 <- fastglm::fastglm(d0.data$X, d0.data$y, family = quasibinomial(), method = 2)
    denominator1 <- fastglm::fastglm(d1.data$X, d1.data$y, family = quasibinomial(), method = 2)
  }
    # Estimating ====================================================
  if(params@method != "ITT") {
    if (!params@excused) {
      out <- weight[tx_lag == 0, `:=`(numerator = inline.pred(numerator0, .SD, params, "numerator"),
                                      denominator = inline.pred(denominator0, .SD, params, "denominator"))
                    ][tx_lag == 0 & get(params@treatment) == 0, `:=`(numerator = 1 - numerator,
                                                                     denominator = 1 - denominator)
                      ][tx_lag == 1, `:=`(numerator = inline.pred(numerator1, .SD, params, "numerator"),
                                          denominator = inline.pred(denominator1, .SD, params, "denominator"))
                        ][tx_lag == 1 & get(params@treatment) == 0, `:=`(numerator = 1 - numerator,
                                                                         denominator = 1 - denominator)]
      setnames(out, params@time, "period")
    } else {
          out <- weight[tx_lag == 0 & get(params@excused.col0) != 1, denominator := inline.pred(denominator0, .SD, params, "denominator")
                        ][tx_lag == 0 & get(params@treatment) == 0 & get(params@excused.col0) != 1, denominator := 1 - denominator
                          ][tx_lag == 1 & get(params@excused.col1) != 1, denominator := inline.pred(denominator1, .SD, params, "denominator")
                            ][tx_lag == 1 & get(params@treatment) == 0 & get(params@excused.col1) != 1, denominator := 1 - denominator]

      if (params@pre.expansion) {
        out <- out[, numerator := 1]
      } else {
        out <- out[get(params@treatment) == 1 & get(params@excused.col0) == 0, numerator := inline.pred(numerator0, .SD, params, "numerator")
                   ][get(params@treatment) == 1 & get(params@excused.col1) == 0, numerator := inline.pred(numerator1, .SD, params, "numerator")
                     ][get(params@treatment) == 0, numerator := 1 - numerator]
      }
      setnames(out, params@time, "period")
    }
  } else out <- weight

  if (params@LTFU) {
    if (params@method == "ITT") {
      out <- out[, `:=` (numerator = 1,
                         denominator = 1)]
    }
    out <- out[, `:=` (cense1.numerator = inline.pred(ltfu.numerator, .SD, params, "LTFU"),
                       cense2.denominator = inline.pred(ltfu.denominator, .SD, params, "LTFU"))]
  }

  kept <- c("numerator", "denominator", "period", "trial", params@id, "cense1.denominator", "cense1.numerator")
  kept <- kept[kept %in% names(out)]
  out <- out[, kept, with = FALSE]

  weight.info <- new("SEQweights",
    weights = out,
    coef.n0 = if (!(params@excused & params@pre.expansion)) coef(numerator0) else NA_real_,
    coef.n1 = if (!(params@excused & params@pre.expansion)) coef(numerator1) else NA_real_,
    coef.d0 = coef(denominator0),
    coef.d1 = coef(denominator1)
  )
  return(weight.info)
}
