#' violin plot
#'
#' Produce violin plot(s) of the given (grouped) values with enhanced annotation and colour per group. Builds upon \code{\link[vioplot]{vioplot}} (0.2) with added customisation possible with colours for each aspect of the violin, boxplot, and separate violins. This supports input of data as a list or formula, being backwards compatible with \code{\link[vioplot]{vioplot}} (0.2) and taking input in a formula as used for \code{\link[graphics]{boxplot}}.
#'
#' New function  \code{\link[vioplot]{vioplot}} has been developed with different default values. Otherwise, they function identically. This version kept for complete backwards-compatibility.
#'
#' @param x for specifying data from which the boxplots are to be produced. Either a numeric vector, or a single list containing such vectors. Additional unnamed arguments specify further data as separate vectors (each corresponding to a component boxplot). NAs are allowed in the data.
#' @param ... additional data vectors or forumla parameters.
#' @param formula a formula, such as y ~ grp, where y is a numeric vector of data values to be split into groups according to the grouping variable grp (usually a factor).
#' @param data a data.frame (or list) from which the variables in formula should be taken.
#' @param range a factor to calculate the upper/lower adjacent values
#' @param h the height for the density estimator, if omit as explained in sm.density, h will be set to an optimum
#' @param ylim y limits
#' @param yaxt A character which specifies the y axis type. Specifying "n" suppresses plotting.
#' @param ylog A logical value (see log in \code{\link[graphics]{plot.default}}). If TRUE, a logarithmic scale is in use (e.g., after plot(*, log = "y")). For a new device, it defaults to FALSE, i.e., linear scale.
#' @param log  Logarithmic scale if log = "y" or TRUE. Invokes ylog = TRUE.
#' @param logLab Increments for labelling y-axis on log-scale, defaults to numbers starting with 1, 2, 5, and 10.
#' @param names one label, or a vector of labels for the datas must match the number of datas given
#' @param col Graphical parameter for fill colour of the violin(s) polygon. NA for no fill colour. If col is a vector, it specifies the colour per violin, and colours are reused if necessary.
#' @param border Graphical parameters for the colour of the violin border passed to lines. NA for no border. If border is a vector, it specifies the colour per violin, and colours are reused if necessary.
#' @param lty,lwd Graphical parameters for the violin passed to lines and polygon
#' @param rectCol Graphical parameters to control fill colour of the box. NA for no fill colour. If col is a vector, it specifies the colour per violin, and colours are reused if necessary.
#' @param lineCol Graphical parameters to control colour of the box outline and whiskers. NA for no border. If lineCol is a vector, it specifies the colour per violin, and colours are reused if necessary.
#' @param pchMed Graphical parameters to control shape of the median point. If pchMed is a vector, it specifies the shape per violin.
#' @param colMed,colMed2 Graphical parameters to control colour of the median point. If colMed is a vector, it specifies the colour per violin. colMed specifies the fill colour in all cases unless pchMed is 21:25 in which case colMed is the border colour and colMed2 is the fill colour.
#' @param drawRect logical. The box is drawn if TRUE.
#' @param areaEqual logical. Density plots checked for equal area if TRUE. wex must be scalar, relative widths of violins depend on area.
#' @param at position of each violin. Default to 1:n
#' @param add logical. if FALSE (default) a new plot is created
#' @param wex relative expansion of the violin.  If wex is a vector, it specifies the area/width size per violin and sizes are reused if necessarydocu.
#' @param horizontal logical. horizontal or vertical violins
#' @param main,sub,xlab,ylab graphical parameters passed to plot.
#' @param cex A numerical value giving the amount by which plotting text should be magnified relative to the default.
#' @param cex.axis The magnification to be used for y axis annotation relative to the current setting of cex.
#' @param cex.names The magnification to be used for x axis annotation relative to the current setting of cex. Takes the value of cex.axis if not given.
#' @param cex.lab The magnification to be used for x and y labels relative to the current setting of cex.
#' @param cex.main The magnification to be used for main titles relative to the current setting of cex.
#' @param cex.sub The magnification to be used for sub-titles relative to the current setting of cex.
#' @param na.action a function which indicates what should happen when the data contain NAs. The default is to ignore missing values in either the response or the group.
#' @param na.rm logical value indicating whether NA values should be stripped before the computation proceeds. Defaults to TRUE.
#' @param side defaults to "both". Assigning "left" or "right" enables one sided plotting of violins. May be applied as a scalar across all groups.
#' @param plotCentre defaults to "points", plotting a central point at the median. If "line" is given a median line is plotted (subject to side) alternatively.
#' @keywords plot graphics violin
#' @import sm
#' @importFrom zoo rollmean
#' @importFrom stats median na.omit quantile
#' @importFrom graphics Axis axis box lines par plot.new plot.window plot.xy points polygon rect title
#' @importFrom grDevices boxplot.stats dev.flush dev.hold dev.interactive devAskNewPage xy.coords
#' @export
#' @examples
#'
#' #generate example data
#' data_one <- rnorm(100)
#' data_two <- rnorm(50, 1, 2)
#'
#' #generate violin plot with similar functionality to vioplot
#' vioplot(data_one, data_two, col="magenta")
#'
#' #note vioplox defaults to a greyscale plot
#' vioplot(data_one, data_two)
#'
#' #colours can be customised separately, with axis labels, legends, and titles
#' vioplot(data_one, data_two, col=c("red","blue"), names=c("data one", "data two"),
#'    main="data violin", xlab="data class", ylab="data read")
#' legend("topleft", fill=c("red","blue"), legend=c("data one", "data two"))
#'
#' #colours can be customised for the violin fill and border separately
#' vioplot(data_one, data_two, col="grey85", border="purple", names=c("data one", "data two"),
#'    main="data violin", xlab="data class", ylab="data read")
#'
#' #colours can also be customised for the boxplot rectange and lines (border and whiskers)
#' vioplot(data_one, data_two, col="grey85", rectCol="lightblue", lineCol="blue",
#'    border="purple", names=c("data one", "data two"),
#'    main="data violin", xlab="data class", ylab="data read")
#'
#' #these colours can also be customised separately for each violin
#' vioplot(data_one, data_two, col=c("skyblue", "plum"), rectCol=c("lightblue", "palevioletred"),
#'    lineCol="blue", border=c("royalblue", "purple"), names=c("data one", "data two"),
#'    main="data violin", xlab="data class", ylab="data read")
#'
#' #this applies to any number of violins, given that colours are provided for each
#' vioplot(data_one, data_two, rnorm(200, 3, 0.5), rpois(200, 2.5),  rbinom(100, 10, 0.4),
#'    col=c("red", "orange", "green", "blue", "violet"),
#'    rectCol=c("palevioletred", "peachpuff", "lightgreen", "lightblue", "plum"),
#'    lineCol=c("red4", "orangered", "forestgreen", "royalblue", "mediumorchid"),
#'    border=c("red4", "orangered", "forestgreen", "royalblue", "mediumorchid"),
#'    names=c("data one", "data two", "data three", "data four", "data five"),
#'    main="data violin", xlab="data class", ylab="data read")
#'
#' #The areaEqual parameter scales with width of violins
#' #Violins will have equal density area (including missing tails) rather than equal maximum width
#' vioplot(data_one, data_two, areaEqual=TRUE)
#'
#' vioplot(data_one, data_two, areaEqual=TRUE,
#'    col=c("skyblue", "plum"), rectCol=c("lightblue", "palevioletred"),
#'    lineCol="blue", border=c("royalblue", "purple"), names=c("data one", "data two"),
#'    main="data violin", xlab="data class", ylab="data read")
#'
#' vioplot(data_one, data_two, rnorm(200, 3, 0.5), rpois(200, 2.5),  rbinom(100, 10, 0.4),
#'    areaEqual=TRUE, col=c("red", "orange", "green", "blue", "violet"),
#'    rectCol=c("palevioletred", "peachpuff", "lightgreen", "lightblue", "plum"),
#'    lineCol=c("red4", "orangered", "forestgreen", "royalblue", "mediumorchid"),
#'    border=c("red4", "orangered", "forestgreen", "royalblue", "mediumorchid"),
#'    names=c("data one", "data two", "data three", "data four", "data five"),
#'    main="data violin", xlab="data class", ylab="data read")

#' @export
#' @usage NULL
vioplot <- function(x, col = "magenta", ...) {
  .Deprecated("vioplot")
  vioplot(x, col = col, ...)
}
