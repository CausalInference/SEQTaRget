#' Internal Function to translate \code{x} (covariates) and \code{y} to a formula object
#'
#' @keywords internal
create.formula <- function(y, x){
  paste0(y, "~", x)
}
#' Internal Function to create 'default' formula
#' Assumes every column not explicitly given in \code{SEQuential} is a covariate, concatenating them with '+'
#'
#' @keywords internal
create.default.covariates <- function(data, id.col, time.col, eligible.col, treatment.col, outcome.col, time.cols, fixed.cols, method){
  if(method == "ITT"){
    baseline.cols <- paste0(time.cols, "_bas", collapse = "+")
    fixed.cols <- paste0(fixed.cols, collapse = "+")
    cols <- paste0(fixed.cols, "+", baseline.cols)
    interactions <- paste0(treatment.col, "_bas*", "followup", collapse = "+")

    string <- paste0(interactions, "+", cols, "+", "followup+followup_sq")
  }
  return(string)
}

create.default.weight.covariates <- function(DT, data, id.col, time.col, eligible.col, treatment.col, outcome.col, time.cols, fixed.cols, method){
  if(!opts$stabilized){
    cols <- paste0(names(data)[!names(data) %in% c(id.col, eligible.col, treatment.col, time.col, outcome.col)])
    string <- paste0(cols, collapse = "+")
  } else if (opts$stabilized){
    cols <-
  }



  if(!opts$stabilized){
    if(opts$pre.expansion){
      cols <- paste0(names(data)[!names(data) %in% c(id.col, eligible.col, treatment.col, time.col, outcome.col)])
      string <- paste0(cols, collapse = "+")
    }
  } else if(opts$stabilized){
    if(opts$pre.expansion){

    } else if (!opts$pre.expansion){
      cols <- paste0(names(DT)[!names(DT) %in% c(id.col, eligible.col, outcome.col,
                                                 names(DT)[grep("followup", names(DT))],
                                                 "period", "trial",
                                                 names(DT)[grep(treatment.col, names(DT))])])
      string <- paste0(cols, collapse="+")
    }
  }

  return(string)
}
DT <- copy(data)
