#' Internal helper function to translate memory allocation for maximum size allowed in memory before shifted to spark
#' @param size String: string to translate to bytes, eg. "6kb", "10mb", "3gb"
#' @keywords internal

translate_memory <- function(size_str) {
  matches <- regmatches(size_str, regexec("([0-9.]+)([a-zA-Z]+)", size_str))[[1]]
  if (length(matches) < 3) stop("Invalid memory size string format")

  size <- as.numeric(matches[2])
  unit <- tolower(matches[3])

  switch(unit,
         b = size,               # Bytes
         kb = size * 2^10,       # Kilobytes
         mb = size * 2^20,       # Megabytes
         gb = size * 2^30,       # Gigabytes
         stop(paste("Invalid or unsupported unit:", unit))
  )
}

get_ram <- function() {
  os <- .Platform$OS.type

  if (os == 'windows') {
    cmd <- "wmic OS get FreePhysicalMemory"
    ram_in_kb <- system(cmd, intern = TRUE)
    bytes <- as.numeric(gsub("[^0-9]", "", ram_in_kb[2])) * 1024
  }

  if (os == 'unix' && Sys.info()["sysname"] != "Darwin") {
    cmd <- "free | awk 'NR==2{print $4}'"
    bytes <- as.numeric(system(cmd, intern = TRUE)) * 1024
  }

  if (Sys.info()["sysname"] == "Darwin") {
    cmd <- "vm_stat | grep 'Pages free' | awk '{print $3}'"
    bytes <- as.numeric(gsub("[^0-9]", "", system(cmd, intern = TRUE))) * 1e6
  }

  return(bytes)
}
