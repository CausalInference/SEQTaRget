#' Simulated observational example data for SEQuential
#'
#' @format A data frame with 54,687 rows and 13 columns:
#' \describe{
#'   \item{ID}{Integer: Unique ID emulating individual patients}
#'   \item{time}{Integer: Time of observation, always begins at 0, max time of 59. Should be continuous}
#'   \item{eligible}{Binary: eligibility criteria for timepoints}
#'   \item{outcome}{Binary: If an outcome is observed at this time point}
#'   \item{tx_init}{Binary: If treatment is observed at this time point}
#'   \item{sex}{Binary: Sex of the emulated patient}
#'   \item{N}{Numeric: Normal random variable from N\(10,5\)}
#'   \item{L}{Numeric: 4\% continuously increase from U\(0, 1\)}
#'   \item{P}{Numeric: 2\% continuously decrease from U\(9, 10\)}
#'   \item{excusedOne}{Binary: Once one, always one variable emulating an excuse for treatment switch}
#'   \item{excusedZero}{Binary: Once zero, always zero variable emulating an excuse for treatment switch}
#' }
#' @usage SEQdata
#' @description Simulated observational example data for SEQuential
#' @keywords internal
"SEQdata"

#' Simulated Lost-to-followup example data for SEQuential
#' @format A dataframe with 4,139 rows and 13 columns:
#' \describe{
#'   \item{ID}{Integer: Unique ID emulating individual patients}
#'   \item{time}{Integer: Time of observation, always begins at 0, max time of 59; however, if lost-to-followup, time is truncated at a random point}
#'   \item{eligible}{Binary: eligibility criteria for timepoints}
#'   \item{outcome}{Binary: If an outcome is observed at this time point}
#'   \item{tx_init}{Binary: If treatment is observed at this time point}
#'   \item{sex}{Binary: Sex of the emulated patient}
#'   \item{N}{Numeric: Normal random variable from N\(10,5\)}
#'   \item{L}{Numeric: 4\% continuously increase from U\(0, 1\)}
#'   \item{P}{Numeric: 2\% continuously decrease from U\(9, 10\)}
#'   \item{excusedOne}{Binary: Once one, always one variable emulating an excuse for treatment switch}
#'   \item{excusedZero}{Binary: Once zero, always zero variable emulating an excuse for treatment switch}
#'   \item{LTFU}{Binary: Flag for losing a simulated ID to followup, if 1 there are no more records of the ID afterwards}
#' }
"SEQdata.LTFU"

#' Simulated multitreatment example data for SEQuential multinomial models
#' @format A dataframe with 5,976 rows and 11 columns:
#' \describe{
#'   \item{ID}{Integer: Unique ID emulating individual patients}
#'   \item{time}{Integer: Time of observation, always begins at 0, max time of 59; however, if lost-to-followup, time is truncated at a random point}
#'   \item{eligible}{Binary: eligibility criteria for timepoints}
#'   \item{outcome}{Binary: If an outcome is observed at this time point}
#'   \item{tx_init}{Integer: Which treatment is observed at this time point}
#'   \item{sex}{Binary: Sex of the emulated patient}
#'   \item{N}{Numeric: Normal random variable from N\(10,5\)}
#'   \item{L}{Numeric: 4\% continuously increase from U\(0, 1\)}
#'   \item{P}{Numeric: 2\% continuously decrease from U\(9, 10\)}
#'   \item{excusedOne}{Binary: Once one, always one variable emulating an excuse for treatment switch}
#'   \item{excusedZero}{Binary: Once zero, always zero variable emulating an excuse for treatment switch}
#' }
"SEQdata.multitreatment"
