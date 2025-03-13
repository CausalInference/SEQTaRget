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
  result <- local({
    on.exit({
      rm(list = setdiff(ls(), "result"))
      gc()
    }, add = TRUE)

    # Variable pre-definition ===================================
    tx_lag <- NULL
    numerator <- denominator <- NULL
    cense1 <- cense1.numerator <- cense1.denominator <- NULL
    followup <- NULL
    isExcused <- NULL

    # Setting up weight data ====================================
    if (!params@weight.preexpansion) {
      subtable.kept <- c(params@treatment, params@id, params@time)
      params@time <- "period"

      baseline.lag <- data[, subtable.kept, with = FALSE
                           ][, tx_lag := shift(get(params@treatment)), by = eval(params@id)
                             ][data[, .I[1L], by = eval(params@id)]$V1, tx_lag := 0
                               ][, eval(params@treatment) := NULL]

      setnames(baseline.lag, 2, params@time)
      # Conditional join
      weight <- rbind(
        DT[followup == 0,
           ][baseline.lag, on = c(params@id, params@time), nomatch = 0],
        DT[, tx_lag := shift(get(params@treatment)), by = c(eval(params@id), "trial")
           ][followup != 0, ])[, paste0(params@time, params@indicator.squared) := get(params@time)^2]

      if (params@excused) weight <- weight[, isExcused := cumsum(ifelse(is.na(isExcused), 0, isExcused)), by = c(eval(params@id), "trial")]

    } else {
      weight <- copy(data)[, tx_lag := shift(get(params@treatment)), by = eval(params@id)
                     ][get(params@time) == 0, tx_lag := 0
                       ][, paste0(params@time, "_sq") := get(params@time)^2]
    }

    # Modeling ======================================================
    if (params@method == "ITT" | params@LTFU) {
      model.data <- copy(weight)
      if (!is.na(params@cense.eligible)) model.data <- model.data[get(params@cense.eligible) == 1, ]

      cense.numerator.data <- prepare.data(model.data, params, type = "numerator", model = NA, case = "LTFU")
      cense.denominator.data <- prepare.data(model.data, params, type = "denominator", model = NA, case = "LTFU")

      cense.numerator <- fastglm(cense.numerator.data$X, cense.numerator.data$y, family = quasibinomial(), method = params@fastglm.method)
      cense.denominator <- fastglm(cense.denominator.data$X, cense.denominator.data$y, family = quasibinomial(), method = params@fastglm.method)

      rm(cense.numerator.data, cense.denominator.data)
    }

      if (params@method != "ITT") {
        model.data <- copy(weight)
        if (!params@weight.preexpansion & !params@excused) model.data <- model.data[followup > 0, ]
        if (!is.na(params@weight.eligible0)) model.data <- model.data[get(params@weight.eligible0) == 1 & get(params@treatment) == 0, ]
        if (!is.na(params@weight.eligible1)) model.data <- model.data[get(params@weight.eligible1) == 1 & get(params@treatment) == 1, ]

        if (!(params@excused & params@weight.preexpansion)) {
          n0.data <- prepare.data(model.data, params, type = "numerator", model = 0, case = "default")
          n1.data <- prepare.data(model.data, params, type = "numerator", model = 1, case = "default")

          numerator0 <- fastglm(n0.data$X, n0.data$y, family = quasibinomial(), method = params@fastglm.method)
          numerator1 <- fastglm(n1.data$X, n1.data$y, family = quasibinomial(), method = params@fastglm.method)

          rm(n0.data, n1.data)
        }

        d0.data <- prepare.data(model.data, params, type = "denominator", model = 0, case = "default")
        d1.data <- prepare.data(model.data, params, type = "denominator", model = 1, case = "default")

        denominator0 <- fastglm(d0.data$X, d0.data$y, family = quasibinomial(), method = params@fastglm.method)
        denominator1 <- fastglm(d1.data$X, d1.data$y, family = quasibinomial(), method = params@fastglm.method)

        rm(model.data, d0.data, d1.data)
      }
      gc()

      # Estimating ====================================================
      if (params@method != "ITT") {
        if (!params@excused) {
          out <- weight[tx_lag == 0, `:=`(numerator = inline.pred(numerator0, .SD, params, "numerator"),
                                          denominator = inline.pred(denominator0, .SD, params, "denominator"))
                        ][tx_lag == 0 & get(params@treatment) == 0, `:=`(numerator = 1 - numerator,
                                                                         denominator = 1 - denominator)
                          ][tx_lag == 1, `:=`(numerator = inline.pred(numerator1, .SD, params, "numerator"),
                                              denominator = inline.pred(denominator1, .SD, params, "denominator"))
                            ][tx_lag == 1 & get(params@treatment) == 0, `:=`(numerator = 1 - numerator,
                                                                             denominator = 1 - denominator)]
        } else {
          out <- weight[tx_lag == 0 & get(params@excused.col0) != 1, denominator := inline.pred(denominator0, .SD, params, "denominator")
                        ][tx_lag == 0 & get(params@treatment) == 0 & get(params@excused.col0) != 1, denominator := 1 - denominator
                          ][tx_lag == 1 & get(params@excused.col1) != 1, denominator := inline.pred(denominator1, .SD, params, "denominator")
                            ][tx_lag == 1 & get(params@treatment) == 0 & get(params@excused.col1) != 1, denominator := 1 - denominator]

          if (params@weight.preexpansion) {
            out <- out[, numerator := 1]
          } else {
            out <- out[get(params@treatment) == 1 & get(params@excused.col0) == 0, numerator := inline.pred(numerator0, .SD, params, "numerator")
                       ][get(params@treatment) == 1 & get(params@excused.col1) == 0, numerator := inline.pred(numerator1, .SD, params, "numerator")
                         ][get(params@treatment) == 0, numerator := 1 - numerator]
          }
        }
      } else out <- weight

    if (params@LTFU) {
      if (params@method == "ITT") out <- out[, `:=` (numerator = 1, denominator = 1)]
      out <- out[, `:=` (cense1.numerator = inline.pred(cense.numerator, .SD, params, "numerator", "LTFU"),
                         cense1.denominator = inline.pred(cense.denominator, .SD, params, "denominator", "LTFU"))
                 ][, cense1 := cense1.numerator / cense1.denominator]
    }

    if (params@time %in% names(out)) setnames(out, params@time, "period")
    kept <- c("numerator", "denominator", "period", "trial", params@id, "cense1", "cense2")
    kept <- kept[kept %in% names(out)]
    out <- out[, kept, with = FALSE]

    weight.info <- new("SEQweights",
      weights = out,
      coef.n0 = if (!(params@excused & params@weight.preexpansion) & params@method != "ITT") fastglm.clean(numerator0) else NA_real_,
      coef.n1 = if (!(params@excused & params@weight.preexpansion) & params@method != "ITT") fastglm.clean(numerator1) else NA_real_,
      coef.d0 = if (params@method != "ITT") fastglm.clean(denominator0) else NA_real_,
      coef.d1 = if (params@method != "ITT") fastglm.clean(denominator1) else NA_real_,
      coef.ncense = if (params@LTFU) fastglm.clean(cense.numerator) else NA_real_,
      coef.dcense = if (params@LTFU) fastglm.clean(cense.denominator) else NA_real_
    )

    return(weight.info)
  })

  return(result)
}

