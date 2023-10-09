internal.expansion <- function(){
    data <- DT[, N := .N, by = get(opts$id.col)
               ][, sequence := .(list(seq(get(opts$time.col), N - 1))), by = 1:nrow(DT)
                 ][get(opts$eligible.col) == 1, .(period = unlist(sequence)),
                   by = c(opts$id.col, opts$time.col, "N", opts$eligible.col)
                   ][, setnames(.SD, old = opts$time.col, new = "trial")
                     ][, c("N", opts$eligible.col) := NULL]

    if(!is.null(opts$max)) data <- data[get(opts$time.col) <= opts$max]

    out <- data[DT, on = c(opts$id.col, "period" = opts$time.col)
                ][, c("sequence", "N") := NULL]

    return(out)
}
