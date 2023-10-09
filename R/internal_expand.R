internal.expansion <- function(){
    data <- DT[, N := .N, by = get(id.col)
               ][, sequence := .(list(seq(get(time.col), N - 1))), by = 1:nrow(DT)
                 ][get(eligible.col) == 1, .(period = unlist(sequence)),
                   by = c(id.col, time.col, "N", eligible.col)
                   ][, setnames(.SD, old = time.col, new = "trial")
                     ][, c("N", eligible.col) := NULL]

    if(!is.null(opts$max)) data <- data[get(opts$time.col) <= opts$max]

    out <- data[DT, on = c(id.col, "period" = time.col)
                ][, c("sequence", "N", eligible.col) := NULL]

    return(out)
}
