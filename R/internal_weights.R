#' Internal function for defining weights
#'
#' @param DT data.table after expansion
#' @param data data.table for data before expansion
#' @param params object of class SEQparams (defined in SEQuential)
#'
#' @import data.table
#' @importFrom speedglm speedglm
#' @importFrom stats binomial coef predict qnorm quantile sd
#'
#' @keywords internal
internal.weights <- function(DT, data, params){
  if(!params@pre.expansion){
    subtable.kept <- c(params@treatment, params@id, params@time)
    params@time <- "period"

    baseline.lag <- data[, ..subtable.kept
                                ][, tx_lag := shift(get(params@treatment)), by = eval(params@id)
                                  ][data[, .I[1L], by = eval(params@id)]$V1, tx_lag := 0
                                    ][, eval(params@treatment) := NULL]

    setnames(baseline.lag, 2, params@time)
    #conditional join
    weight <- rbind(DT[followup == 0,
                       ][baseline.lag, on = c(params@id, params@time), nomatch = 0],
                    DT[, tx_lag := shift(get(params@treatment)), by = c(eval(params@id), "trial")
                       ][followup != 0, ]
                    )[, paste0(params@time, params@squared.indicator) := get(params@time)^2]
  } else {
    weight <- data[, tx_lag := shift(get(params@treatment)), by = eval(params@id)
                         ][get(params@time) == 0, tx_lag := 0
                           ][, paste0(params@time, "_sq") := get(params@time)^2]
  }
  if(!params@excused){
    numerator0 <- speedglm::speedglm(paste0(params@treatment, "==1~", params@numerator),
                                     data = weight[tx_lag == 0, ],
                                     family = binomial("logit"))

    numerator1 <- speedglm::speedglm(paste0(params@treatment, "==1~", params@numerator),
                                     data = weight[tx_lag == 1, ],
                                     family = binomial("logit"))

    denominator0 <- speedglm::speedglm(paste0(params@treatment, "==1~", params@denominator),
                                         data = weight[tx_lag == 0, ],
                                         family = binomial("logit"))

    denominator1 <- speedglm::speedglm(paste0(params@treatment, "==1~", params@denominator),
                                       data = weight[tx_lag == 1, ],
                                       family = binomial("logit"))

    kept <- c("numerator", "denominator", params@time, params@id)
    out <- weight[tx_lag == 0, `:=` (numerator = predict(numerator0, newdata = .SD, type = "response"),
                                     denominator = predict(denominator0, newdata = .SD, type = "response"))
                  ][tx_lag == 0 & get(params@treatment) == 0, `:=` (numerator = 1 - numerator,
                                                                 denominator = 1 - denominator)
                    ][tx_lag == 1, `:=` (numerator = predict(numerator1, newdata = .SD, type = "response"),
                                         denominator = predict(denominator1, newdata = .SD, type = "response"))
                      ][tx_lag == 1 & get(params@treatment) == 0, `:=` (numerator = 1 - numerator,
                                                                     denominator = 1 - denominator)
                        ][, ..kept]
    setnames(out, params@time, "period")
  } else {
    if(!params@pre.expansion){
      kept <- c("numerator", "denominator", params@time, params@id, "trial")
      numerator0 <- speedglm::speedglm(paste0(params@treatment, "~", params@numerator),
                                       data = weight[get(paste0(params@treatment, params@baseline.indicator)) == 0 & get(params@excused.col0) == 0, ],
                                       family = binomial("logit"))

      numerator1 <- speedglm::speedglm(paste0(params@treatment, "~", params@numerator),
                                       data = weight[get(paste0(params@treatment, params@baseline.indicator)) == 1 & get(params@excused.col1) == 0, ],
                                       family = binomial("logit"))
    }

    denominator0 <- speedglm::speedglm(paste0(params@treatment, "~", params@denominator),
                                       data = weight[tx_lag == 0 & get(params@excused.col0) ==0, ],
                                       family = binomial("logit"))

    denominator1 <- speedglm::speedglm(paste0(params@treatment, "~", params@denominator),
                                       data = weight[tx_lag == 1 & get(params@excused.col1) == 0, ],
                                       family = binomial("logit"))

    out <- weight[tx_lag == 0 & get(params@excused.col0) != 1, denominator := predict(denominator0, newdata = .SD, type = "response")
                  ][tx_lag == 0 & get(params@treatment) == 0 & get(params@excused.col0) != 1, denominator := 1 - denominator
                    ][tx_lag == 1 & get(params@excused.col1) != 1, denominator := predict(denominator1, newdata = .SD, type = "response")
                      ][tx_lag == 1 & get(params@treatment) == 0 & get(params@excused.col1) != 1, denominator := 1 - denominator
                        ]

    if(!params@pre.expansion){
      out <- out[get(params@treatment) == 1 & get(params@excused.col0) == 0, numerator := predict(numerator0, newdata = .SD, type = "response")
                 ][get(params@treatment) == 1 & get(params@excused.col1) == 0, numerator := predict(numerator1, newdata = .SD, type = "response")
                   ][get(params@treatment) == 0, numerator := 1 - numerator
                     ][, ..kept]
    } else {
      kept <- c("numerator", "denominator", params@time, params@id)
      out <- out[, numerator := 1
                 ][, ..kept]
    }
    setnames(out, params@time, "period")
  }
  weight.info <- new("SEQweights",
                     weights = out,
                     coef.n0 = if(!(params@excused & params@pre.expansion)) coef(numerator0) else NA_real_,
                     coef.n1 = if(!(params@excused & params@pre.expansion)) coef(numerator1) else NA_real_,
                     coef.d0 = coef(denominator0),
                     coef.d1 = coef(denominator1))
  return(weight.info)
}
