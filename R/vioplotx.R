#' Enhanced Violin Plot
#'
#' Produce violin plot(s) of the given (grouped) values with enhanced annotation and colour per group. Builds upon \code{\link[vioplot]{vioplot}} and intended to function in largely the same way, with added customisation possible with colours for each aspect of the violin, boxplot, and separate violins. This supports input of data as a list or formula, being backwards compatible with \code{\link[vioplot]{vioplot}} and taking input in a formula as used for \code{\link[graphics]{boxplot}}.
#'
#' @param x for specifying data from which the boxplots are to be produced. Either a numeric vector, or a single list containing such vectors. Additional unnamed arguments specify further data as separate vectors (each corresponding to a component boxplot). NAs are allowed in the data.
#' @param ... additional data vectors or forumla parameters.
#' @param formula a formula, such as y ~ grp, where y is a numeric vector of data values to be split into groups according to the grouping variable grp (usually a factor).
#' @param data a data.frame (or list) from which the variables in formula should be taken.
#' @param range a factor to calculate the upper/lower adjacent values
#' @param h the height for the density estimator, if omit as explained in sm.density, h will be set to an optimum
#' @param ylim y limits
#' @param names one label, or a vector of labels for the datas must match the number of datas given
#' @param col Graphical parameter for fill colour of the violin(s) polygon. NA for no fill colour. If col is a vector, it specifies the colour per violin, and colours are reused if necessary.
#' @param border Graphical parameters for the colour of the violin border passed to lines. NA for no border. If border is a vector, it specifies the colour per violin, and colours are reused if necessary.
#' @param lty,lwd Graphical parameters for the violin passed to lines and polygon
#' @param rectCol Graphical parameters to control fill colour of the box. NA for no fill colour. If col is a vector, it specifies the colour per violin, and colours are reused if necessary.
#' @param lineCol Graphical parameters to control fill colour of the box. NA for no border. If border is a vector, it specifies the colour per violin, and colours are reused if necessary.
#' @param pchMed Graphical parameters to control shape of the median point. If pchMed is a vector, it specifies the shape per violin.
#' @param colMed,colMed2 Graphical parameters to control colour of the median point. If colMed is a vector, it specifies the colour per violin. colMed specifies the fill colour in all cases unless pchMed is 21:25 in which case colMed is the border colour and colMed2 is the fill colour.
#' @param drawRect logical. The box is drawn if TRUE.
#' @param areaEqual logical. Density plots checked for equal area if TRUE. wex must be scalar, relative widths of violins depend on area.
#' @param at position of each violin. Default to 1:n
#' @param add logical. if FALSE (default) a new plot is created
#' @param wex relative expansion of the violin.  If wex is a vector, it specifies the area/width size per violin and sizes are reused if necessary.
#' @param horizontal logical. horizontal or vertical violins
#' @param main,sub,xlab,ylab graphical parameters passed to plot.
#' @param yaxt A character which specifies the y axis type. Specifying "n" suppresses plotting.
#' @param na.action a function which indicates what should happen when the data contain NAs. The default is to ignore missing values in either the response or the group.
#' @param na.rm logical value indicating whether NA values should be stripped before the computation proceeds. Defaults to TRUE.
#' @param side defaults to "both". Assigning "left" or "right" enables one sided plotting of violins. May be applied as a scalar across all groups.
#' @param plotCentre defaults to "points", plotting a central point at the median. If "line" is given a median line is plotted (subject to side) alternatively.
#' @keywords plot graphics violin
#' @import sm
#' @importFrom zoo rollmean
#' @export
#' @examples
#'
#' #generate example data
#' data_one <- rnorm(100)
#' data_two <- rnorm(50, 1, 2)
#'
#' #generate violin plot with similar functionality to vioplot
#' vioplotx(data_one, data_two, col="magenta")
#'
#' #note vioplox defaults to a greyscale plot
#' vioplotx(data_one, data_two)
#'
#' #colours can be customised separately, with axis labels, legends, and titles
#' vioplotx(data_one, data_two, col=c("red","blue"), names=c("data one", "data two"), main="data violin", xlab="data class", ylab="data read")
#' legend("topleft", fill=c("red","blue"), legend=c("data one", "data two"))
#'
#' #colours can be customised for the violin fill and border separately
#' vioplotx(data_one, data_two, col="grey85", border="purple", names=c("data one", "data two"), main="data violin", xlab="data class", ylab="data read")
#'
#' #colours can also be customised for the boxplot rectange and lines (border and whiskers)
#' vioplotx(data_one, data_two, col="grey85", rectCol="lightblue", lineCol="blue", border="purple", names=c("data one", "data two"), main="data violin", xlab="data class", ylab="data read")
#'
#' #these colours can also be customised separately for each violin
#' vioplotx(data_one, data_two, col=c("skyblue", "plum"), rectCol=c("lightblue", "palevioletred"), lineCol="blue", border=c("royalblue", "purple"), names=c("data one", "data two"), main="data violin", xlab="data class", ylab="data read")
#'
#' #this applies to any number of violins, given that colours are provided for each
#' vioplotx(data_one, data_two, rnorm(200, 3, 0.5), rpois(200, 2.5),  rbinom(100, 10, 0.4), col=c("red", "orange", "green", "blue", "violet"), rectCol=c("palevioletred", "peachpuff", "lightgreen", "lightblue", "plum"), lineCol=c("red4", "orangered", "forestgreen", "royalblue", "mediumorchid"), border=c("red4", "orangered", "forestgreen", "royalblue", "mediumorchid"), names=c("data one", "data two", "data three", "data four", "data five"), main="data violin", xlab="data class", ylab="data read")
#'
#' #The areaEqual parameter scales with width of violins so that they have equal density area (including missing tails) rather than equal maximum width
#' vioplotx(data_one, data_two, areaEqual=T)
#' vioplotx(data_one, data_two, areaEqual=T, col=c("skyblue", "plum"), rectCol=c("lightblue", "palevioletred"), lineCol="blue", border=c("royalblue", "purple"), names=c("data one", "data two"), main="data violin", xlab="data class", ylab="data read")
#' vioplotx(data_one, data_two, rnorm(200, 3, 0.5), rpois(200, 2.5),  rbinom(100, 10, 0.4), areaEqual=T, col=c("red", "orange", "green", "blue", "violet"), rectCol=c("palevioletred", "peachpuff", "lightgreen", "lightblue", "plum"), lineCol=c("red4", "orangered", "forestgreen", "royalblue", "mediumorchid"), border=c("red4", "orangered", "forestgreen", "royalblue", "mediumorchid"), names=c("data one", "data two", "data three", "data four", "data five"), main="data violin", xlab="data class", ylab="data read")

#' @export
#' @usage NULL
vioplotx <- function(x, ...) {
  UseMethod("vioplotx")
}

#' @rdname vioplotx
#' @export
vioplotx.formula <-
  function (formula, data = NULL, ..., names=NULL, na.action = NULL)
  {
    if (missing(formula) || (length(formula) != 3L))
      stop("'formula' missing or incorrect")
    m <- match.call(expand.dots = FALSE)
    if (is.matrix(eval(m$data, parent.frame())))
      m$data <- data.frame(as.numeric(data))
    m$... <- NULL
    m$names <- NULL
    m$na.action <- na.action
    m[[1L]] <- quote(stats::model.frame)
    mf <- eval(m, parent.frame())
    response <- attr(attr(mf, "terms"), "response")
    datas <- split(mf[[response]], mf[-response])
    attach(datas)
    if(is.null(names)) names <- names(datas)
    eval(parse(text=paste("vioplotx(", paste(names(datas), collapse = ", "), ", names = names, ...)")))
    detach(datas)
  }


#' @rdname vioplotx
#' @export
vioplotx.default <-
  function (x, ..., data = NULL, range = 1.5, h = NULL, ylim = NULL, names = NULL,
            horizontal = FALSE, col = "grey50", border = "black", lty = 1,
            lwd = 1, rectCol = "black", lineCol = "black", pchMed = 19, colMed = "white", colMed2 = "grey 75",
            at, add = FALSE, wex = 1, drawRect = TRUE, areaEqual=FALSE, main=NA, sub=NA, xlab=NA, ylab=NA, yaxt="s",
            na.action = NULL, na.rm = T, side = "both", plotCentre = "point")
  {
    if(!is.list(x)){
      datas <- list(x, ...)
    } else{
      datas<-lapply(x, unlist)
    }
    if(is.null(na.action)) na.action <- na.omit
    lapply(datas, function(data) data <- data[!sapply(data, is.infinite)])
    if(na.rm) datas <- lapply(datas, na.action)
    n <- length(datas)
    #if(is.list(datas)) datas <- as.data.frame(datas)
    if (missing(at))
      at <- 1:n
    upper <- vector(mode = "numeric", length = n)
    lower <- vector(mode = "numeric", length = n)
    q1 <- vector(mode = "numeric", length = n)
    q2 <- vector(mode = "numeric", length = n)
    q3 <- vector(mode = "numeric", length = n)
    med <- vector(mode = "numeric", length = n)
    base <- vector(mode = "list", length = n)
    height <- vector(mode = "list", length = n)
    area_check <- vector(mode = "list", length = n)
    baserange <- c(Inf, -Inf)
    args <- list(display = "none")
    radj <- ifelse(side == "right", 0, 1)
    ladj <- ifelse(side == "left", 0, 1)
    boxwex <- wex
    if (!(is.null(h)))
      args <- c(args, h = h)
    if(plotCentre == "line") med.dens <- rep(NA, n)
    if(areaEqual){
      for (i in 1:n) {
        data <- unlist(datas[[i]])
        data.min <- min(data, na.rm = na.rm)
        data.max <- max(data, na.rm = na.rm)
        q1[i] <- quantile(data, 0.25)
        q2[i] <- quantile(data, 0.5)
        q3[i] <- quantile(data, 0.75)
        med[i] <- median(data)
        iqd <- q3[i] - q1[i]
        upper[i] <- min(q3[i] + range * iqd, data.max)
        lower[i] <- max(q1[i] - range * iqd, data.min)
        est.xlim <- c(min(lower[i], data.min), max(upper[i],
                                                   data.max))
        smout <- do.call("sm.density", c(list(data, xlim = est.xlim),
                                         args))
        if(plotCentre == "line"){
          med.dat <- do.call("sm.density",
                             c(list(data, xlim=est.xlim,
                                    eval.points=med[i], display = "none")))
          med.dens[i] <- med.dat$estimate
        }
        Avg.pos <- mean(smout$eval.points)
        xt <- diff(smout$eval.points[smout$eval.points<Avg.pos])
        yt <- rollmean(smout$eval.points[smout$eval.points<Avg.pos],2)
        area_check[[i]] <- sum(xt*yt)
      }
      if(length(wex)>1){
        warning("wex may not be a vector if areaEqual is TRUE")
        print("using first element of wex")
        wex<-wex[i]
      }
      wex <-unlist(area_check)/max(unlist(area_check))*wex
    }
    for (i in 1:n) {
      data <- unlist(datas[[i]])
      data.min <- min(data, na.rm = na.rm)
      data.max <- max(data, na.rm = na.rm)
      q1[i] <- quantile(data, 0.25)
      q2[i] <- quantile(data, 0.5)
      q3[i] <- quantile(data, 0.75)
      med[i] <- median(data)
      iqd <- q3[i] - q1[i]
      upper[i] <- min(q3[i] + range * iqd, data.max)
      lower[i] <- max(q1[i] - range * iqd, data.min)
      est.xlim <- c(min(lower[i], data.min), max(upper[i],
                                                 data.max))
      smout <- do.call("sm.density", c(list(data, xlim = est.xlim),
                                       args))
      hscale <- 0.4/max(smout$estimate) * ifelse(length(wex)>1, wex[i], wex)
      base[[i]] <- smout$eval.points
      height[[i]] <- smout$estimate * hscale
      t <- range(base[[i]])
      baserange[1] <- min(baserange[1], t[1])
      baserange[2] <- max(baserange[2], t[2])
      if(plotCentre == "line"){
        med.dat <- do.call("sm.density",
                           c(list(data, xlim=est.xlim,
                                  eval.points=med[i], display = "none")))
        med.dens[i] <- med.dat$estimate *hscale
      }
    }
    if (!add) {
      xlim <- if (n == 1)
        at + c(-0.5, 0.5)
      else range(at) + min(diff(at))/2 * c(-1, 1)
      if (is.null(ylim)) {
        ylim <- baserange
      }
    }
    if (is.null(names)) {
      label <- 1:n
    }
    else {
      label <- names
    }
    boxwidth <- 0.05 * ifelse(length(boxwex)>1, boxwex[i], boxwex)
    if (!add)
      plot.new()
    if (!horizontal) {
      if (!add) {
        plot.window(xlim = xlim, ylim = ylim)
        if(yaxt !="n")  axis(2)
        axis(1, at = at, label = label)
      }
      box()
      for (i in 1:n) {
        polygon(c(at[i] - radj*height[[i]], rev(at[i] + ladj*height[[i]])),
                c(base[[i]], rev(base[[i]])), col = ifelse(length(col)>1, col[i], col), border = ifelse(length(border)>1, border[i], border),
                lty = lty, lwd = lwd)
        if (drawRect) {
          lines(at[c(i, i)], c(lower[i], upper[i]), lwd = lwd,
                lty = lty, col = ifelse(length(lineCol)>1, lineCol[i], lineCol))
          rect(at[i] - radj*ifelse(length(boxwidth)>1, boxwidth[i], boxwidth)/2, q1[i], at[i] + ladj*ifelse(length(boxwidth)>1, boxwidth[i], boxwidth)/2,
               q3[i], col = ifelse(length(rectCol)>1, rectCol[i], rectCol), border = ifelse(length(lineCol)>1, lineCol[i], lineCol))
          if(plotCentre == "line"){
            lines(x = c(at[i] - radj*med.dens[i],
                        at[i],
                        at[i] + ladj*med.dens[i]),
                  y = rep(med[i],3))
          } else {
            points(at[i], med[i], pch = ifelse(length(pchMed)>1, pchMed[i], pchMed), col = ifelse(length(colMed)>1, colMed[i], colMed), bg = ifelse(length(colMed2)>1, colMed2[i], colMed2))
          }
        }
      }
    }
    else {
      if (!add) {
        plot.window(xlim = ylim, ylim = xlim)
        axis(1)
        if(yaxt !="n")  axis(2, at = at, label = label)
      }
      box()
      for (i in 1:n) {
        polygon(c(base[[i]], rev(base[[i]])), c(at[i] - radj*height[[i]],
                                                rev(at[i] + ladj*height[[i]])), col = ifelse(length(col)>1, col[i], col), border = ifelse(length(border)>1, border[i], border),
                lty = lty, lwd = lwd)
        if (drawRect) {
          lines(c(lower[i], upper[i]), at[c(i, i)], lwd = lwd,
                lty = lty, col = ifelse(length(lineCol)>1, lineCol[i], lineCol))
          rect(q1[i], at[i] - radj*ifelse(length(boxwidth)>1, boxwidth[i], boxwidth)/2, q3[i], at[i] +
                 ladj*ifelse(length(boxwidth)>1, boxwidth[i], boxwidth)/2, col = ifelse(length(rectCol)>1, rectCol[i], rectCol), border = ifelse(length(lineCol)>1, lineCol[i], lineCol))
          if(plotCentre == "line"){
            lines(y = c(at[i] - radj*med.dens[i],
                        at[i],
                        at[i] + ladj*med.dens[i]),
                  x = rep(med[i],3))
          } else {
            points(med[i], at[i], pch = ifelse(length(pchMed)>1, pchMed[i], pchMed), col = ifelse(length(colMed)>1, colMed[i], colMed), , bg = ifelse(length(colMed2)>1, colMed2[i], colMed2))
          }
        }
      }
    }
    invisible(list(upper = upper, lower = lower, median = med,
                   q1 = q1, q3 = q3))
    if(is.na(xlab)==F){
      if(is.na(ylab)==F){
        if(is.na(main)==F){
          if(is.na(sub)==T){
            title(main=main, ylab=ylab, xlab=xlab)
          } else{
            title(main=main, sub=sub, ylab=ylab, xlab=xlab)
          }
        }
      } else{
        if(is.na(main)==F){
          if(is.na(sub)==T){
            title(main=main, xlab=xlab)
          } else{
            title(main=main, sub=sub, xlab=xlab)
          }
        }
      }
    } else {
      if(is.na(ylab)==F){
        if(is.na(main)==F){
          if(is.na(sub)==T){
            title(main=main, ylab=ylab)
          } else{
            title(main=main, sub=sub, ylab=ylab)
          }
        }
      } else{
        if(is.na(main)==F){
          if(is.na(sub)==T){
            title(main=main)
          } else{
            title(main=main, sub=sub)
          }
        }
      }
    }
  }

