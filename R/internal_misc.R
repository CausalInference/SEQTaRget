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
create.default.covariates <- function(data, id.col, time.col, eligible.col, treatment.col, outcome.col, method){
  if(method == "ITT"){
    cols <- paste0(names(data)[!names(data) %in% c(id.col, eligible.col, outcome.col, time.col)], "_bas", collapse = "+")
    interactions <- paste0(treatment.col, "_bas*", "followup", collapse = "+")

    string <- paste0(interactions, "+", cols, "+", "followup+followup_sq")
  }
  return(string)
}

create.default.weight.covariates <- function(DT, data, id.col, time.col, eligible.col, treatment.col, opts){
  cols <- paste0(names(data)[!names(data) %in% c(id.col, eligible.col, treatment.col, time.col)])
  string <- paste0(cols, collapse = "+")

  return(string)
}

memory.check <- function(opts){
  gc()

  if(opts$sys.type == "Windows") free <- memory.size()
  if(opts$sys.type %in% c("Darwin", "Unix")) free <-

  if(opts$parallel){
    expected <- opts$data.size * opts$ncores
  }
}



# fROM https://stackoverflow.com/questions/6457290/how-to-check-the-amount-of-ram
available_memory <- function()
{

  # Get operating system
  OS <- tolower(Sys.info()["sysname"])

  # Branch based on OS
  if(OS == "windows"){ # Windows

    # System information
    system_info <- system("systeminfo", intern = TRUE)

    # Get available memory
    value <- system_info[
      grep("Available Physical Memory", system_info)
    ]

    # Remove extraneous information
    value <- gsub("Available Physical Memory: ", "", value)
    value <- gsub("\\,", "", value)

    # Convert to bytes
    value_split <- unlist(strsplit(value, split = " "))

    # Check for second value
    bytes <- as.numeric(value_split[1]) * switch(
      value_split[2],
      "KB" = 1e03,
      "MB" = 1e06,
      "GB" = 1e09
    )

  }else if(OS == "linux"){ # Linux

    # Split system information
    info_split <- strsplit(system("free", intern = TRUE), split = " ")

    # Remove "Mem:" and "Swap:"
    info_split <- lapply(info_split, function(x){gsub("Mem:", "", x)})
    info_split <- lapply(info_split, function(x){gsub("Swap:", "", x)})

    # Get actual values
    info_split <- lapply(info_split, function(x){x[x != ""]})

    # Bind values
    info_split <- do.call(rbind, info_split[1:2])

    # Get free values
    bytes <- as.numeric(info_split[2, info_split[1,] == "free"])

  }else{ # Mac

    # System information
    system_info <- system("top -l 1 -s 0 | grep PhysMem", intern = TRUE)

    # Get everything after comma
    unused <- gsub(" .*,", "", system_info)

    # Get values only
    value <- gsub("PhysMem: ", "", unused)
    value <- gsub(" unused.", "", value)

    # Check for bytes
    if(grepl("M", value)){
      bytes <- as.numeric(gsub("M", "", value)) * 1e06
    }else if(grepl("G", value)){
      bytes <- as.numeric(gsub("G", "", value)) * 1e09
    }else if(grepl("K", value)){
      bytes <- as.numeric(gsub("K", "", value)) * 1e03
    }

  }

  # Return bytes
  return(bytes)

}
