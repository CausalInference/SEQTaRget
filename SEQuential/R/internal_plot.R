#' Plotting for survival curves
#'
#' @param survival.data Dataframe containing survival information
#' @param params Params passed around SEQuential
#'
#'
#' @importFrom stringr str_to_title
#' @import ggplot2
#' @keywords internal

internal.plot <- function(survival.data, params) {
  variable <- followup <- NULL
  lb <- ub <- NULL

  if (params@plot.type == "survival") subset <- "surv" else subset <- params@plot.type
  if (all(!is.na(params@plot.labels))) guide <- params@plot.labels else guide <- waiver()

  data <- survival.data[variable %like% subset, ]
  p <- ggplot(data,
              aes(x = followup, y = value, col = variable)) +
    geom_line() +
    theme_bw() +
    labs(x = "Followup",
         y = stringr::str_to_title(params@plot.type),
         col = "")
  if (params@bootstrap) {
    p <- p + geom_ribbon(aes(ymax = ub, ymin = lb, fill = variable), alpha = 0.5) +
      scale_fill_manual(labels = guide, values = params@plot.colors) +
      labs(fill = "") +
      guides(col = "none")
  }

  if (!is.na(params@plot.title)) p <- p + labs(title = params@plot.title)
  if (!is.na(params@plot.subtitle)) p <- p + labs(title = params@plot.subtitle)
  p <- p + scale_color_manual(labels = guide, values = params@plot.colors)

  return(p)
}
