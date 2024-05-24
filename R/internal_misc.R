#' Internal Function to translate \code{x} (covariates) and \code{y} to a formula object
#'
#' @keywords internal
prepare.data <- function(data, method, opts){
    if(method == "censoring" & opts$excused){
      data <- data[, switch := (get(treatment.col) != shift(get(treatment.col), fill = get(treatment.col)[1])), by = id.col]

      if(is.na(opts$excused.col0)){opts$excused.col0 <- "tmp0"; data <- data[, tmp0 := 0]}
      if(is.na(opts$excused.col1)){opts$excused.col0 <- "tmp1"; data <- data[, tmp1 := 0]}

      out <- data[(switch) & get(treatment.col) == 0, isExcused := ifelse(get(opts$excused.col0) == 1, TRUE, FALSE)
                   ][(switch) & get(treatment.col) == 1, isExcused := ifelse(get(opts$excused.col1) == 1, TRUE, FALSE)
                     ][(isExcused), switch := FALSE
                       ]
      return(out)
    }
  return(data)
}
#' Internal Function to create 'default' formula
#' Assumes every column not explicitly given in \code{SEQuential} is a covariate, concatenating them with '+'
#'
#' @keywords internal
create.default.covariates <- function(data, id.col, time.col, eligible.col, treatment.col, outcome.col, time.cols, fixed.cols, method, opts){
  if(method == "ITT"){
    baseline.cols <- paste0(time.cols, "_bas", collapse = "+")
    fixed.cols <- paste0(fixed.cols, collapse = "+")
    cols <- paste0(fixed.cols, "+", baseline.cols)
    interactions <- paste0(treatment.col, "_bas*", "followup", collapse = "+")

    string <- paste0(interactions, "+", cols, "+", "followup+followup_sq")

  } else if(method %in% c("dose-response", "censoring") & !opts$excused){
    if(opts$pre.expansion){
      cols <- paste0(fixed.cols, collapse="+")
    } else {
      baseline.cols <- paste0(time.cols, "_bas", collapse = "+")
      fixed.cols <- paste0(fixed.cols, collapse = "+")
      cols <- paste0(fixed.cols, "+", baseline.cols)
    }
    string <- paste0(cols, "+followup+followup_sq+trial+trial_sq")

    if(method == "dose-resonse") string <- paste0(string, "+dose+dose_sq")
    if(method == "censoring") string <- paste0(treatment.col, "+", string, "+", paste0(treatment.col, "*followup"))
  } else if(opts$excused){
    string <- paste(treatment.col, "followup", "trial", paste0(c("followup", "trial"), opts$sq.indicator, collapse = "+"), sep = "+")
  }

  return(string)
}

create.default.weight.covariates <- function(data, id.col, time.col, eligible.col, treatment.col, outcome.col, time.cols, fixed.cols, type, method, opts){
  if(opts$pre.expansion){
    if(type == "numerator"){
      string <- paste0(paste0(fixed.cols, collapse = "+"), "+", time.col, "+", time.col, "_sq")
    } else {
      string <- paste0(paste0(c(fixed.cols, time.cols), collapse = "+"), "+", time.col, "+", time.col, "_sq")
    }
  } else {
    if(type == "numerator"){
      baseline.cols <- paste0(time.cols, "_bas", collapse = "+")
      fixed.cols <- paste0(fixed.cols, collapse = "+")
      string <- paste0(fixed.cols, "+", baseline.cols, "+followup+followup_sq+trial+trial_sq")
    } else {
      baseline.cols <- paste0(time.cols, "_bas", collapse = "+")
      fixed.cols <- paste0(fixed.cols, collapse = "+")
      tv.cols <- paste0(time.cols, collapse = "+")
      string <- paste0(tv.cols, "+", fixed.cols, "+", baseline.cols, "+followup+followup_sq+trial+trial_sq")
    }
  }
  return(string)
}

create.risk <- function(data){
  table <- data[, .SD[.N], by = variable]
  rd <- round(as.numeric(table[2, 3] - table[1, 3]), 4)
  rr <- round(as.numeric(table[2, 3]/table[1, 3]), 4)

  return(list(rd = rd,
              rr = rr))
}
