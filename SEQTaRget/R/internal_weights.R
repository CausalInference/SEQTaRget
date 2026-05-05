#' Internal function for defining weights
#'
#' @param DT data.table after expansion
#' @param data data.table for data before expansion
#' @param params object of class SEQparams (defined in SEQuential)
#' @param cache cache
#' @import data.table
#' @importFrom stats quasibinomial coef predict qnorm quantile sd
#' @keywords internal
internal.weights <- function(DT, data, params, cache) {
  # Variable pre-definition ===================================
    tx_lag <- i.tx_lag <- NULL
    numerator <- denominator <- NULL
    cense1 <- cense1.numerator <- cense1.denominator <- NULL
    followup <- NULL
    isExcused <- NULL
    visit <- visit.numerator <- visit.denominator <- NULL
    
    # Setting up weight data ====================================
    if (!params@weight.preexpansion) {
      subtable.kept <- c(params@treatment, params@id, params@time)
      params@time <- "period"
      # DT is passed by reference; a previous handler call (e.g. main analysis before
      # bootstrap) may have added tx_lag in-place. Remove it so the join below does not
      # produce an i.tx_lag duplicate column that causes rbind to fail.
      if ("tx_lag" %in% names(DT)) DT[, tx_lag := NULL]

      baseline.lag <- data[, subtable.kept, with = FALSE
                           ][, tx_lag := shift(get(params@treatment)), by = eval(params@id)
                             ][data[, .I[1L], by = eval(params@id)]$V1, tx_lag := as.character(params@treat.level[[1]])
                               ][, eval(params@treatment) := NULL]

      setnames(baseline.lag, 2, params@time)
      weight <- copy(DT)
      weight[, tx_lag := shift(get(params@treatment)), by = c(eval(params@id), "trial")]
      weight[baseline.lag, on = c(params@id, params@time),
             tx_lag := fifelse(followup == 0L, i.tx_lag, tx_lag)]
      rm(baseline.lag)
      weight[, paste0(params@time, params@indicator.squared) := get(params@time)^2]

      if (params@excused | params@deviation.excused) weight[, isExcused := cumsum(ifelse(is.na(isExcused), 0, isExcused)), by = c(eval(params@id), "trial")]

    } else {
      weight <- copy(data)[, tx_lag := shift(get(params@treatment)), by = eval(params@id)
                     ][get(params@time) == 0, tx_lag := as.character(params@treat.level[[1]])
                       ][, paste0(params@time, "_sq") := get(params@time)^2]
    }

    # Modeling ======================================================
    if (params@method == "ITT" || params@LTFU || !is.na(params@visit)) {
      model.data <- weight
      if (params@LTFU) {
        if (!is.na(params@cense.eligible)) model.data <- model.data[get(params@cense.eligible) == 1, ]
        
        cense.numerator.data <- prepare.data_cached(model.data, params, type = "numerator", model = NA, case = "LTFU", cache)
        cense.denominator.data <- prepare.data_cached(model.data, params, type = "denominator", model = NA, case = "LTFU", cache)
        
        cense.numerator <- fit_glm(cense.numerator.data$X, cense.numerator.data$y, family = quasibinomial(), params = params)
        check_separation(cense.numerator, label = "censoring numerator")
        cense.denominator <- fit_glm(cense.denominator.data$X, cense.denominator.data$y, family = quasibinomial(), params = params)
        check_separation(cense.denominator, label = "censoring denominator")
        
        rm(cense.numerator.data, cense.denominator.data)
      }
      
      if (!is.na(params@visit)) {
        visit.numerator.data <- prepare.data_cached(model.data, params, type = "numerator", model = NA, case = "visit", cache)
        visit.denominator.data <- prepare.data_cached(model.data, params, type = "denominator", model = NA, case = "visit", cache)
        
        visit.numerator <- fit_glm(visit.numerator.data$X, visit.numerator.data$y, family = quasibinomial(), params = params)
        check_separation(visit.numerator, label = "visit numerator")
        visit.denominator <- fit_glm(visit.denominator.data$X, visit.denominator.data$y, family = quasibinomial(), params = params)
        check_separation(visit.denominator, label = "visit denominator")
        
        rm(visit.numerator.data, visit.denominator.data)
      }
      
    }

    # Initialize storage for all models
    numerator_models <- list()
    denominator_models <- list()
    
    if (params@method != "ITT") {
      model.data <- weight
      if (!params@weight.preexpansion & !(params@excused | params@deviation.excused)) model.data <- model.data[followup > 0, ]
      
      # Fit models for each treatment level - combined loop to avoid redundant filtering
      for (i in seq_along(params@treat.level)) {
        level <- params@treat.level[[i]]
        eligible_col <- params@weight.eligible_cols[[i]]
        level_data <- if (!is.na(eligible_col)) model.data[get(eligible_col) == 1, ] else model.data

        # Fit numerator model (skip if excused & preexpansion)
        if (!((params@excused | params@deviation.excused) & params@weight.preexpansion)) {
          n.data <- prepare.data_cached(level_data, params, type = "numerator", model = level, case = "default", cache)
          if (length(unique(n.data$y)) < 2L) {
            numerator_models[[i]] <- list(skip = TRUE)
          } else {
            numerator_models[[i]] <- model.passer(n.data$X, n.data$y, params)
          }
          rm(n.data)
        }

        # Fit denominator model
        d.data <- prepare.data_cached(level_data, params, type = "denominator", level, case = "default", cache)
        if (length(unique(d.data$y)) < 2L) {
          denominator_models[[i]] <- list(skip = TRUE)
        } else {
          denominator_models[[i]] <- model.passer(d.data$X, d.data$y, params)
        }
        rm(d.data)
      }

      rm(model.data)
    }

    # Estimating ====================================================
    if (params@method != "ITT") {
      out <- weight[, `:=`(numerator = NA_real_, denominator = NA_real_)]
      
      if (!(params@excused | params@deviation.excused)) {
        for (i in seq_along(params@treat.level)) {
          level <- params@treat.level[[i]]
          if (isTRUE(numerator_models[[i]]$skip) || isTRUE(denominator_models[[i]]$skip)) {
            out[tx_lag == level, `:=`(numerator = 1, denominator = 1)]
          } else {
            out[tx_lag == level, `:=`(
              numerator = inline.pred(numerator_models[[i]], .SD, params, "numerator", multi = params@multinomial, target = level),
              denominator = inline.pred(denominator_models[[i]], .SD, params, "denominator", multi = params@multinomial, target = level))]

            if (i == 1) {
              out[tx_lag == level & get(params@treatment) == params@treat.level[[i]],
                  `:=` (numerator = 1 - numerator, denominator = 1 - denominator)]
            } else {
              out[tx_lag == level & get(params@treatment) != params@treat.level[[i]],
                  `:=` (numerator = 1 - numerator, denominator = 1 - denominator)]
            }
          }
        }
      } else {
        if (params@multinomial & !params@weight.preexpansion) multi <- FALSE else multi <- params@multinomial
        for (i in seq_along(params@treat.level)) {
          level <- params@treat.level[[i]]
          col <- if (params@excused) params@excused.cols[[i]] else params@deviation.excused_cols[[i]]
          
          if (!is.na(col)) {
            out[tx_lag == level & get(col) != 1, 
                denominator := inline.pred(denominator_models[[i]], .SD, params, "denominator", multi = multi, target = level)]
            if (i == 1) {
              out[tx_lag == level & 
                    get(params@treatment) == params@treat.level[[i]] & 
                    get(col) == 0, 
                  denominator := 1 - denominator]
            } else {
              out[tx_lag == level & 
                    get(params@treatment) != params@treat.level[[i]] & 
                    get(col) == 0, 
                  denominator := 1 - denominator]
            }
          }
        }

        if (params@weight.preexpansion) {
          out[, numerator := 1]
        } else {
          if (params@multinomial & !params@weight.preexpansion) multi <- FALSE else multi <- params@multinomial
          for (i in seq_along(params@treat.level)) {
            level <- params@treat.level[[i]]
            col <- if (params@excused) params@excused.cols[[i]] else params@deviation.excused_cols[[i]]
            if (!is.na(col)) {
              out[get(params@treatment) == level & get(col) == 0, 
                  numerator := inline.pred(numerator_models[[i]], .SD, params, "numerator", multi = multi, target = level)
                  ]
            }
          }
          out[get(params@treatment) == params@treat.level[[1]], numerator := 1 - numerator]
        }
      }
    } else out <- weight

    if (params@LTFU) {
      if (params@method == "ITT") out <- out[, `:=` (numerator = 1, denominator = 1)]
      out <- out[, `:=` (cense1.numerator = inline.pred(cense.numerator, .SD, params, "numerator", "LTFU"),
                         cense1.denominator = inline.pred(cense.denominator, .SD, params, "denominator", "LTFU"))
                 ][, cense1 := cense1.numerator / cense1.denominator]
    }

    if (!is.na(params@visit)) {
      if (params@method == "ITT") out <- out[, `:=` (numerator = 1, denominator = 1)]
      out <- out[, `:=` (visit.numerator = inline.pred(visit.numerator, .SD, params, "numerator", "visit"),
                         visit.denominator = inline.pred(visit.denominator, .SD, params, "denominator", "visit"))
      ][, visit := visit.numerator / visit.denominator]
    }

    if (params@time %in% names(out)) setnames(out, params@time, "period")
    kept <- c("numerator", "denominator", "period", "trial", params@id, "cense1", "visit")
    kept <- kept[kept %in% names(out)]
    out <- out[, kept, with = FALSE]

    weight.info <- new("SEQweights", weights = out)
    
    if (!((params@excused | params@deviation.excused) & params@weight.preexpansion) & params@method != "ITT") {
      coef.numerator <- vector("list", length(params@treat.level))
      for (i in seq_along(params@treat.level)) {
        coef.numerator[[i]] <- if (!isTRUE(numerator_models[[i]]$skip)) clean_fastglm(numerator_models[[i]]) else NULL
      }
      weight.info@coef.numerator <- coef.numerator
    }

    if (params@method != "ITT") {
      coef.denominator <- vector("list", length(params@treat.level))
      for (i in seq_along(params@treat.level)) {
        coef.denominator[[i]] <- if (!isTRUE(denominator_models[[i]]$skip)) clean_fastglm(denominator_models[[i]]) else NULL
      }
      weight.info@coef.denominator <- coef.denominator
    }
    
    if (params@LTFU) {
      slot(weight.info, "coef.ncense") <- clean_fastglm(cense.numerator)
      slot(weight.info, "coef.dcense") <- clean_fastglm(cense.denominator)
    }
  return(weight.info)
}
