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
