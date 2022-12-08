#' Violin Plot Statistics
#'
#' This function is typically called by another function to gather the statistics necessary
#' for producing box plots, but may be invoked separately. See: \code{\link[grDevices]{boxplot.stats}}
#'
#' @aliases violin.stats violinplot.stats
#' @rdname violin.stats
#' @param x	a numeric vector for which the violin plot will be constructed \code{NA}s and \code{NaN}s are allowed and omitted).
#' @param coef	this determines how far the plot ‘whiskers’ extend out from the box. If coef is positive, the
#' whiskers extend to the most extreme data point which is no more than coef times the length of the box away
#' from the box. A value of zero causes the whiskers to extend to the data extremes (and no outliers be returned).
#' @param do.conf,do.out	logicals; if FALSE, the conf or out component respectively will be empty in the result.
#' @param ... arguments passed to \code{\link[vioplot]{vioplot}}.
#' @importFrom grDevices boxplot.stats
#' @export
vioplot.stats <- function(x, coef = 1.5, do.conf = TRUE, do.out = TRUE, ...){
  boxplot.stats(x, coef = coef, do.conf =  do.conf, do.out = do.out)
}
