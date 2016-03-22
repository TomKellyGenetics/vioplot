#' Enhanced Violin Plot
#'
#' Produce violin plot(s) of the given (grouped) values with enhanced annotation and colour per group.
#' @param x data vector
#' @param ... additional data vectors
#' @param range a factor to calculate the upper/lower adjacent values
#' @param h the height for the density estimator, if omit as explained in sm.density, h will be set to an optimum
#' @param ylim y limits
#' @param names one label, or a vector of labels for the datas must match the number of datas given
#' @param col Graphical parameter for fill colour of the violin(s) polygon. NA for no fill colour. If col is a vector, it specifies the colour per violin, and colours are reused if necessary.
#' @param border Graphical parameters for the colour of the violin border passed to lines. NA for no border. If border is a vector, it specifies the colour per violin, and colours are reused if necessary.
#' @param lty,lwd Graphical parameters for the violin passed to lines and polygon
#' @param rectCol Graphical parameters to control fill colour of the box. NA for no fill colour. If col is a vector, it specifies the colour per violin, and colours are reused if necessary.
#' @param lineCol Graphical parameters to control fill colour of the box. NA for no border. If border is a vector, it specifies the colour per violin, and colours are reused if necessary.
#' @param colMed Graphical parameters to control colour of the median point
#' @param pchMed Graphical parameters to control shape of the median point
#' @param drawRect logical. the box is drawn if TRUE.
#' @param at position of each violin. Default to 1:n
#' @param add logical. if FALSE (default) a new plot is created
#' @param wex relative expansion of the violin.  If wex is a vector, it specifies the area/width size per violin and sizes are reused if necessary.
#' @param horizontal logical. horizontal or vertical violins
#' @param main,sub,xlab,ylab graphical parameters passed to plot.
#' @keywords plot graphics violin
#' @export
#' @examples
#' #vioplotx()
vioplotx <-
function (x, ..., range = 1.5, h = NULL, ylim = NULL, names = NULL, 
          horizontal = FALSE, col = "grey50", border = "black", lty = 1, 
          lwd = 1, rectCol = "black", lineCol = "black", colMed = "white", pchMed = 19, 
          at, add = FALSE, wex = 1, drawRect = TRUE, main=NA, sub=NA, xlab=NA, ylab=NA) 
{
  datas <- list(x, ...)
  n <- length(datas)
  if (missing(at)) 
    at <- 1:n
  upper <- vector(mode = "numeric", length = n)
  lower <- vector(mode = "numeric", length = n)
  q1 <- vector(mode = "numeric", length = n)
  q3 <- vector(mode = "numeric", length = n)
  med <- vector(mode = "numeric", length = n)
  base <- vector(mode = "list", length = n)
  height <- vector(mode = "list", length = n)
  baserange <- c(Inf, -Inf)
  args <- list(display = "none")
  if (!(is.null(h))) 
    args <- c(args, h = h)
  for (i in 1:n) {
    data <- datas[[i]]
    data.min <- min(data)
    data.max <- max(data)
    q1[i] <- quantile(data, 0.25)
    q3[i] <- quantile(data, 0.75)
    med[i] <- median(data)
    iqd <- q3[i] - q1[i]
    upper[i] <- min(q3[i] + range * iqd, data.max)
    lower[i] <- max(q1[i] - range * iqd, data.min)
    est.xlim <- c(min(lower[i], data.min), max(upper[i], 
                                               data.max))
    smout <- do.call("sm.density", c(list(data, xlim = est.xlim), 
                                     args))
    hscale <- 0.4/max(smout$estimate) * ifelse(is.vector(wex), wex[i], wex)
    base[[i]] <- smout$eval.points
    height[[i]] <- smout$estimate * hscale
    t <- range(base[[i]])
    baserange[1] <- min(baserange[1], t[1])
    baserange[2] <- max(baserange[2], t[2])
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
  boxwidth <- 0.05 * ifelse(is.vector(wex), wex[i], wex)
  if (!add) 
    plot.new()
  if (!horizontal) {
    if (!add) {
      plot.window(xlim = xlim, ylim = ylim)
      axis(2)
      axis(1, at = at, label = label)
    }
    box()
    for (i in 1:n) {
      polygon(c(at[i] - height[[i]], rev(at[i] + height[[i]])), 
              c(base[[i]], rev(base[[i]])), col = ifelse(is.vector(col), col[i], col), border = ifelse(is.vector(border), border[i], border), 
              lty = lty, lwd = lwd)
      if (drawRect) {
        lines(at[c(i, i)], c(lower[i], upper[i]), lwd = lwd, 
              lty = lty, col = ifelse(is.vector(lineCol), lineCol[i], lineCol))
        rect(at[i] - ifelse(is.vector(wex), boxwidth[i], boxwidth)/2, q1[i], at[i] + ifelse(is.vector(wex), boxwidth[i], boxwidth)/2, 
             q3[i], col = ifelse(is.vector(rectCol), rectCol[i], rectCol), border = ifelse(is.vector(lineCol), lineCol[i], lineCol))
        points(at[i], med[i], pch = pchMed, col = colMed)
      }
    }
  }
  else {
    if (!add) {
      plot.window(xlim = ylim, ylim = xlim)
      axis(1)
      axis(2, at = at, label = label)
    }
    box()
    for (i in 1:n) {
      polygon(c(base[[i]], rev(base[[i]])), c(at[i] - height[[i]], 
                                              rev(at[i] + height[[i]])), col = ifelse(is.vector(col), col[i], col), border = ifelse(is.vector(border), border[i], border), 
              lty = lty, lwd = lwd)
      if (drawRect) {
        lines(c(lower[i], upper[i]), at[c(i, i)], lwd = lwd, 
              lty = lty, col = ifelse(is.vector(lineCol), lineCol[i], lineCol))
        rect(q1[i], at[i] - ifelse(is.vector(wex), boxwidth[i], boxwidth)/2, q3[i], at[i] + 
              ifelse(is.vector(wex), boxwidth[i], boxwidth)/2, col = ifelse(is.vector(rectCol), rectCol[i], rectCol), border = ifelse(is.vector(lineCol), lineCol[i], lineCol))
        points(med[i], at[i], pch = pchMed, col = colMed)
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
