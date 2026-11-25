#' Creates an expanded dataset for use with [SEQuential()]
#'
#' @param params SEQparams object built in the SEQuential function
#'
#' @import data.table
#'
#' @keywords internal
SEQexpand <- function(params) {
  # Variable pre-definition ===================================
    trial <- last <- eligible <- treatment_val <- n <- batch <- NULL
    tx_bas <- paste0(params@treatment, params@indicator.baseline)
    DT <- params@data

    # Expansion =======================================================
    if (!params@weighted) {
      vars.intake <- c(params@covariates, params@deviation.col, tx_bas)
    } else {
      vars.intake <- c(params@covariates, params@numerator, params@denominator,
                       params@cense.denominator, params@cense.numerator, 
                       params@visit.denominator, params@visit.numerator,
                       params@deviation.col, tx_bas)
    }
    vars <- unique(c(formula_vars(vars.intake),
                     params@treatment, params@cense, params@cense.eligible, params@visit,
                     params@compevent, unlist(params@weight.eligible_cols), params@subgroup))
    vars.nin <- c("dose", paste0("dose", params@indicator.squared),
                  params@time, paste0(params@time, params@indicator.squared), "tx_lag", "censored")
    vars <- vars[!is.na(vars)]
    vars <- vars[!vars %in% vars.nin]
    # The indicators are suffixes: match them literally and only at the end of
    # the name, so columns such as "bmi_baseline" or "bmi_sqrt" are left alone
    strip_suffix <- function(x, suffix) substr(x, 1L, nchar(x) - nchar(suffix))
    vars.base <- vars[endsWith(vars, params@indicator.baseline)]
    vars.sq <- vars[endsWith(vars, params@indicator.squared)]
    vars.time <- c(vars[!vars %in% vars.base], unlist(params@excused.cols), unlist(params@deviation.excused_cols))
    vars.time <- vars.time[!is.na(vars.time)]
    vars.base <- unique(strip_suffix(vars.base, params@indicator.baseline))
    vars.base <- c(vars.base[!vars.base %in% params@time], params@eligible)
    vars.sq <- unique(strip_suffix(vars.sq, params@indicator.squared))

    # One trial starts at each eligible row. Time is zero-indexed and contiguous
    # within each ID (checked in SEQuential()), so a trial's follow-up can reach
    # the ID's last row, or followup.max periods if that comes first
    starts <- DT[, list(trial = as.numeric(get(params@time)),
                        last = pmin(.N - 1, get(params@time) + params@followup.max),
                        eligible = get(params@eligible) == 1,
                        treatment_val = get(params@treatment)),
                 by = eval(params@id)]
    # Follow-up rows of each trial after the followup.min cut
    followup.first <- if (params@followup.min > 0) ceiling(params@followup.min) else 0L
    starts[, n := pmax(0, floor(last - trial) + 1 - followup.first)]
    # Reported as if every row started a trial, before the eligibility filter
    n_expanded <- sum(starts$n)
    starts <- starts[(eligible)][, c("last", "eligible") := NULL]

    if (params@selection.random && !params@selection.first_trial) {
      # Keep every treated trial and a sample of the untreated ones
      set.seed(params@seed)
      control <- which(starts$treatment_val == params@treat.level[[1]])
      keep <- rep(TRUE, nrow(starts))
      keep[control] <- FALSE
      keep[control[sample.int(length(control), round(length(control) * params@selection.prob))]] <- TRUE
      starts <- starts[keep]
    }
    starts[, treatment_val := NULL]

    # Every step below works within an ID, so IDs are expanded a batch at a time
    # and the batches stacked, which keeps the working memory to one batch
    id_rows <- starts[, list(n = sum(n)), by = eval(params@id)]
    id_rows[, batch := if (is.na(params@expand.batch_size)) {
      # About this many follow-up rows per batch
      cumsum(n) %/% 1e6
    } else {
      (seq_len(.N) - 1L) %/% params@expand.batch_size
    }]
    data_rows <- split(seq_len(nrow(DT)), id_rows$batch[match(DT[[params@id]], id_rows[[params@id]])])
    start_rows <- split(seq_len(nrow(starts)), id_rows$batch[match(starts[[params@id]], id_rows[[params@id]])])
    # With no trials, a single empty batch still gives the columns of the output
    if (length(start_rows) == 0) start_rows <- data_rows <- list(`0` = integer())

    pieces <- vector("list", length(start_rows))
    n_filtered <- 0
    for (b in seq_along(start_rows)) {
      piece <- expand.batch(DT[data_rows[[names(start_rows)[b]]]], starts[start_rows[[b]]], followup.first,
                            vars.time, vars.base, vars.sq, params)
      n_filtered <- n_filtered + piece$n_filtered
      pieces[[b]] <- piece$out
    }
    rm(starts, data_rows, start_rows)
    out <- rbindlist(pieces, use.names = TRUE, fill = TRUE)
    rm(pieces)

    if (params@verbose) {
      n_cols <- ncol(out)
      cat("\nPre-filter expansion:", format(n_expanded, big.mark = ","), "observations\n")
      sampled <- params@selection.random && !params@selection.first_trial
      censored <- params@method == "censoring" && !params@expand.only
      quals <- c(if (sampled) "post-selection.random", if (censored) "pre-censoring")
      expanded_label <- if (length(quals) > 0)
        paste0("Expanded dataset (", paste(quals, collapse = ", "), ")") else "Expanded dataset"
      cat("\n", expanded_label, ": ", format(n_filtered, big.mark = ","), " observations, ", n_cols, " variables\n", sep = "")
      if (censored) {
        cat("\nExpanded dataset (post-censoring): ", format(nrow(out), big.mark = ","), " observations, ", n_cols, " variables\n", sep = "")
        # The outcome model is fit only on the un-censored rows (censored == 0);
        # the artificially-censored rows are retained in the dataset. Report the
        # split so the count lines up with implementations that print only the
        # modelled rows (e.g. Stata seqtte).
        n_censored <- sum(out[["censored"]] == 1)
        cat("  entering outcome model (uncensored): ", format(nrow(out) - n_censored, big.mark = ","), "\n", sep = "")
        cat("  artificially censored (treatment switch): ", format(n_censored, big.mark = ","), "\n", sep = "")
      }
    }

    return(out)
}

#' Expands one batch of IDs
#'
#' @param DT Rows of the person-period data for the batch's IDs
#' @param starts The batch's trial starts, with the number of follow-up rows of each in `n`
#' @param followup.first First follow-up time kept
#' @param vars.time,vars.base,vars.sq Time-varying, baseline and squared variables
#' @param params SEQparams object built in the SEQuential function
#'
#' @returns A list of the expanded rows, `out`, and their number before any
#'   method-specific reduction, `n_filtered`
#'
#' @keywords internal
expand.batch <- function(DT, starts, followup.first, vars.time, vars.base, vars.sq, params) {
    followup <- period <- dose <- trial <- isExcused <- excused_tmp <- firstSwitch <- lag <- NULL

    row <- rep.int(seq_len(nrow(starts)), starts$n)
    data <- data.table(starts[[params@id]][row], starts$trial[row])
    setnames(data, c(params@id, "trial"))
    data[, followup := sequence(starts$n, from = followup.first)
         ][, period := as.integer(trial + followup)]
    setcolorder(data, c(params@id, "trial", "period", "followup"))
    rm(row)

    data_list <- list()
    if (length(c(vars.time, vars.sq)) > 0) {
      # nomatch = NULL: original-data rows with no expansion-grid match (possible
      # under followup.min > 0 or selection.random) would otherwise be carried as
      # NA-trial rows through the squared-column computation only to be dropped by
      # the inner join with the baseline table below.
      data.time <- data[DT, on = c(eval(params@id), "period" = eval(params@time)), nomatch = NULL
                        ][, (paste0(vars.sq, params@indicator.squared)) := lapply(.SD, function(x) x^2), .SDcols = vars.sq]

      vars.found <- unique(c(vars.time, vars.sq, "period", "trial", params@id, params@outcome))
      data_list[["time"]] <- data.time[, vars.found, with = FALSE]
      rm(data.time)
    }
    if (length(vars.base) > 0) {
      data.base <- data[DT, on = c(eval(params@id), "trial" = eval(params@time)), .SDcols = vars.base, nomatch = 0
                        ]

      vars.found <- unique(c(paste0(vars.base, params@indicator.baseline), "period", "trial", params@id))
      setnames(data.base, old = vars.base, new = paste0(vars.base, params@indicator.baseline))
      data_list[["base"]] <- data.base[, vars.found, with = FALSE]
      rm(data.base)
    }
    if (length(data_list) > 1) {
      out <- Reduce(function(x, y) x[y, on = c(params@id, "trial", "period"), nomatch = NULL], data_list)
    } else if (length(data_list) == 1) {
      out <- data_list[[1]]
    }
    rm(data_list)

    out <- out[get(paste0(params@eligible, params@indicator.baseline)) == 1,
               ][, paste0(params@eligible, params@indicator.baseline) := NULL]

    # Truncate each trial at (and including) the first outcome event row, so that
    # subjects who experience the outcome early are not carried forward with outcome=0
    # from subsequent periods in the original data. The row indices are computed
    # outside the i-expression: inside DT[i], columns shadow local variables, so a
    # user column named "out" would otherwise shadow the table itself and error.
    # end_of_fup outcomes are measurements, not events - truncating would discard
    # the rows the estimate is read from
    if (!params@end_of_fup) {
      keep_rows <- out[, .I[seq_len(match(1L, .SD[[1L]], nomatch = .N))],
                       by = c(params@id, "trial"), .SDcols = params@outcome]$V1
      out <- out[keep_rows]
    }

    # Row count after eligibility filtering and outcome truncation, before any
    # method-specific reduction (dose-response, PP censoring, first-trial selection).
    # Matches the stage at which pySEQTarget reports "Expanded dataset".
    n_filtered <- nrow(out)

    if (params@method == "dose-response") {
      out <- out[, dose := cumsum(get(params@treatment)), by = c(eval(params@id), "trial")
                 ][, paste0(c("dose", "trial"), params@indicator.squared) := list(dose^2, trial^2)]
    }

    if (params@method == "censoring" && !params@expand.only) {
      out[, switch := FALSE]
      if (params@deviation) {
        # Censoring on deviation condition
        for (i in seq_along(params@treat.level)) {
          conditional <- paste0(paste0(params@treatment, params@indicator.baseline), "==", params@treat.level[[i]],
                                " & ", params@deviation.col, params@deviation.conditions[[i]])
          
          out[eval(parse(text = conditional)), switch := TRUE]
        }
        if (params@deviation.excused) {
          # deviation.excused_cols[[i]] excuses deviations by people following treat.level[[i]]
          for (i in seq_along(params@treat.level)) {
            if (!is.na(params@deviation.excused_cols[[i]])) {
              out[(switch) &
                    get(paste0(params@treatment, params@indicator.baseline)) == params@treat.level[[i]],
                  isExcused := ifelse(get(params@deviation.excused_cols[[i]]) == 1, 1, 0)]
            }
          }
          out[!is.na(isExcused), excused_tmp := cumsum(isExcused), by = c(params@id, "trial")
              ][(excused_tmp) > 0, switch := FALSE, by = c(params@id, "trial")
                ][, excused_tmp := NULL]
          if ("isExcused" %in% names(out)) out[, isExcused := NULL]
        }
      }  else {
        # Automatic switch definition (based on treatment and treatment lag)
        out[, lag := shift(get(params@treatment), fill = get(params@treatment)[1]), by = c(params@id, "trial")]
        
        if (params@excused) {
          # Excused treatment lag switches
          out[, switch := (get(params@treatment) != lag)]
          
          # excused.cols[[i]] excuses a switch away from treat.level[[i]], as in the weight models
          for (i in seq_along(params@treat.level)) {
            if (!is.na(params@excused.cols[[i]])) {
              out[(switch) & 
                    lag == params@treat.level[[i]], isExcused := ifelse(get(params@excused.cols[[i]]) == 1, 1, 0)]
            }
          }
          setorderv(out, c(params@id, "trial", "followup"))
          out[!is.na(isExcused), excused_tmp := cumsum(isExcused), by = c(params@id, "trial")
              ][(excused_tmp) > 0, switch := FALSE, by = c(params@id, "trial")
                ][, excused_tmp := NULL]
        } else {
          # Non-excused treatment lag switches
          out[, paste0("trial", params@indicator.squared) := trial^2
              ][, switch := get(params@treatment) != shift(get(params@treatment), fill = get(params@treatment)[1]),
                by = c(params@id, "trial")]
        }
        out[, lag := NULL]
      }
      out[, firstSwitch := { m <- match(TRUE, switch); if (is.na(m)) .N else m }, by = c(params@id, "trial")]
      keep_rows <- out[, .I[seq_len(firstSwitch[1])], by = c(params@id, "trial")]$V1
      out <- out[keep_rows
                 ][, paste0(params@outcome) := ifelse(switch, NA, get(params@outcome))
                   ][, `:=`(firstSwitch = NULL)
                     ][, "censored" := ifelse(switch, 1, 0)]
    }
    
    if (params@selection.first_trial) {
      out <- out[, "trial.first" := min(get("trial")), by = c(params@id)
                 ][get("trial") == get("trial.first"), .SD
                   ][, "trial.first" := NULL]
    }
    
    list(out = out, n_filtered = n_filtered)
}
