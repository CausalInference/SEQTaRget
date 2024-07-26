data <- fread("datagenExcused.csv")
test <- SEQuential::SEQuential(data, "ID", "time", "eligible", "tx_init", "outcome", method = "dose-response", fixed.cols = "sex", time_varying.cols = c("N", "L", "P"), options)
#dose-response failing - creating variable 'tx_initsex'
#Debugging Junk ==========
id.col = "ID"; time.col = "time"; eligible.col = "eligible"; outcome.col = "outcome"; treatment.col = "tx_init"; method = "censoring"; time_varying.cols = c("N", "L", "P"); fixed.cols = "sex"
options <- SEQuential::SEQopts(pre.expansion = TRUE, weighted = TRUE, excused = FALSE, excused.col0 = "excusedZero", excused.col1 = "excusedOne")
#rm(id.col, time.col, eligible.col, outcome.col, treatment.col, method, time_varying.cols, fixed.cols)

