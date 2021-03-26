#' Violin Plot
#'
#' Produce violin plot(s) of the given (grouped) values with enhanced annotation and colour per group. Includes customisation of colours for each aspect of the violin, boxplot, and separate violins. This supports input of data as a list or formula, being backwards compatible with \code{\link[vioplot]{vioplot}} (0.2) and taking input in a formula as used for \code{\link[graphics]{boxplot}}.
#'
#' @name vioplot
#' @aliases violinplot
#' @param x for specifying data from which the boxplots are to be produced. Either a numeric vector, or a single list containing such vectors. Additional unnamed arguments specify further data as separate vectors (each corresponding to a component boxplot). NAs are allowed in the data.
#' @param ... additional data vectors or formula parameters. For the formula method, named arguments to be passed to the default method.
#' @param formula a formula, such as y ~ grp, where y is a numeric vector of data values to be split into groups according to the grouping variable grp (usually a factor).
#' @param data a data.frame (or list) from which the variables in formula should be taken.
#' @param use.cols logical indicating if columns (by default) or rows (use.cols = FALSE) should be plotted.
#' @param subset	an optional vector specifying a subset of observations to be used for plotting.
#' @param drop,sep,lex.order defines groups to plot from formula, passed to \code{split.default}, see there.
#' @param range a factor to calculate the upper/lower adjacent values
#' @param h the height for the density estimator, if omit as explained in sm.density, h will be set to an optimum
#' @param xlim,ylim	 numeric vectors of length 2, giving the x and y coordinates ranges.
#' @param yaxt A character which specifies the y axis type. Specifying "n" suppresses plotting.
#' @param ylog,xlog A logical value (see log in \code{\link[graphics]{plot.default}}). If ylog is TRUE, a logarithmic scale is in use (e.g., after plot(*, log = "y")). For horizontal = TRUE then, if xlog is TRUE, a logarithmic scale is in use (e.g., after plot(*, log = "x")). For a new device, it defaults to FALSE, i.e., linear scale.
#' @param log  Logarithmic scale if log = "y" or TRUE. Invokes ylog = TRUE. If horizontal is TRUE then invokes xlog = TRUE.
#' @param logLab Increments for labelling y-axis on log-scale, defaults to numbers starting with 1, 2, 5, and 10.
#' @param names one label, or a vector of labels for the data must match the number of data given
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
#' @param wex relative expansion of the violin.  If wex is a vector, it specifies the area/width size per violin and sizes are reused if necessary.
#' @param horizontal logical. To use horizontal or vertical violins. Note that log scale can only be used on the x-axis for horizontal violins, and on the y-axis otherwise.
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
#' @param axes,frame.plot,panel.first,panel.last,asp,line,outer,adj,ann,ask,bg,bty,cin,col.axis,col.lab,col.main,col.sub,cra,crt,csi,cxy,din,err,family,fg,fig,fin,font,font.axis,font.lab,font.main,font.sub,lab,las,lend,lheight,ljoin,lmitre,mai,mar,mex,mfcol,mfg,mfrow,mgp,mkh,new,oma,omd,omi,page,pch,pin,plt,ps,pty,smo,srt,tck,tcl,usr,xaxp,xaxs,xaxt,xpd,yaxp,yaxs,ylbias Arguments to be passed to methods, such as graphical parameters (see \code{\link[graphics]{par}})).
#' @keywords plot graphics violin
#' @import sm
#' @importFrom zoo rollmean
#' @importFrom stats median na.omit quantile
#' @importFrom graphics Axis axis box lines par plot.new plot.window plot.xy points polygon rect title
#' @importFrom grDevices boxplot.stats dev.flush dev.hold dev.interactive devAskNewPage xy.coords
#' @export
#' @examples
#'
#' # box- vs violin-plot
#' par(mfrow=c(2,1))
#' mu<-2
#' si<-0.6
#' bimodal<-c(rnorm(1000,-mu,si),rnorm(1000,mu,si))
#' uniform<-runif(2000,-4,4)
#' normal<-rnorm(2000,0,3)
#' vioplot(bimodal,uniform,normal)
#' boxplot(bimodal,uniform,normal)
#'
#' # add to an existing plot
#' x <- rnorm(100)
#' y <- rnorm(100)
#' plot(x, y, xlim=c(-5,5), ylim=c(-5,5))
#' vioplot(x, col="tomato", horizontal=TRUE, at=-4, add=TRUE,lty=2, rectCol="gray")
#' vioplot(y, col="cyan", horizontal=FALSE, at=-4, add=TRUE,lty=2)
#'
#' # formula input
#' data("iris")
#' vioplot(Sepal.Length~Species, data = iris, main = "Sepal Length",
#'         col=c("lightgreen", "lightblue", "palevioletred"))
#' legend("topleft", legend=c("setosa", "versicolor", "virginica"),
#'        fill=c("lightgreen", "lightblue", "palevioletred"), cex = 0.5)
#'
#' data("diamonds", package = "ggplot2")
#' palette <- RColorBrewer::brewer.pal(9, "Pastel1")
#' par(mfrow=c(3, 1))
#' vioplot(price ~ cut, data = diamonds, las = 1, col = palette)
#' vioplot(price ~ clarity, data = diamonds, las = 2, col = palette)
#' vioplot(price ~ color, data = diamonds, las = 2, col = palette)
#' par(mfrow=c(3, 1))
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
vioplot <- function(x, ...) {
  UseMethod("vioplot")
}

#' Draw a Violin plot for each Column (Row) of a Matrix
#'
#' Interpreting the columns (or rows) of a matrix as different groups, draw a boxplot for each.
#'
#' @aliases violin.matrix violinplot.matrix
#' @param x	a numeric matrix.
#' @param use.cols logical indicating if columns (by default) or rows (use.cols = FALSE) should be plotted.
#' @param ...	Further arguments to \code{\link[vioplot]{vioplot}}.
#' @rdname vioplot
#' @export
vioplot.matrix <- function (x, use.cols = TRUE, ...)
{
  groups <- if (use.cols) {
    split(c(x), rep.int(1L:ncol(x), rep.int(nrow(x), ncol(x))))
  }
  else split(c(x), seq(nrow(x)))
  if (length(nam <- dimnames(x)[[1 + use.cols]]))
    names(groups) <- nam
  invisible(vioplot(groups, ...))
}

#' @rdname vioplot
#' @export
vioplot.list <- function (x, ...){
  ind <- sapply(x, is.numeric)
  if(all(!ind)){
    stop(paste("elements are not numeric: ", names(x)[!sapply(x, is.numeric)]))
  }
  if(any(!ind)){
    warning(paste("some elements are not numeric: ", names(x)[!sapply(x, is.numeric)]))
    x <- x[sapply(x, is.numeric)]
  }
  invisible(vioplot.default(x, ...))
}
#' @rdname vioplot
#' @export
vioplot.data.frame <- vioplot.list

#' @rdname vioplot
#' @export
vioplot.matrix <- vioplot.matrix

#' @rdname vioplot
#' @export
vioplot.formula <-
  function (formula, data = NULL, ..., subset,  na.action = NULL,
            add = FALSE, ann = !add, horizontal = FALSE, side = "both",
            xlab = mklab(y_var = horizontal), ylab = mklab(y_var = !horizontal), names=NULL,
            drop = FALSE, sep = ".", lex.order = FALSE)
  {
    if (missing(formula) || (length(formula) != 3L)){
      stop("'formula' missing or incorrect")
    }
    if(add && side != "both"){
      if(!is.null(names)) warning("Warning: names can only be changed on first call of vioplot (when add = FALSE)
")
      if(!missing(xlab)) warning("Warning: x-axis labels can only be changed on first call of vioplot (when add = FALSE)
")
      if(!missing(ylab)) warning("Warning: y-axis labels can only be changed on first call of vioplot (when add = FALSE)
")
    }
    if (missing(xlab) || missing(ylab)){
      mklab <- function(y_var){
        if(y_var){
          names(mf)[response]
        } else {
          paste(names(mf)[-response], collapse = " : ")
        }
      }
    }
    m <- match.call(expand.dots = FALSE)
    if (is.matrix(eval(m$data, parent.frame())))
      m$data <- as.data.frame(data)
    m$... <- m$drop <- m$sep <- m$lex.order <- NULL
    m$xlab <- m$ylab <- m$add <- m$ann <- m$horizontal <- NULL
    m$names <-  m$side <- NULL
    m$na.action <- na.action
    m[[1L]] <- quote(stats::model.frame.default)
    mf <- eval(m, parent.frame())
    response <- attr(attr(mf, "terms"), "response")
    if(add){
      xlab <- ylab <- NA
    }
    vioplot(split(mf[[response]], mf[-response], drop = drop,
                  sep = sep, lex.order = lex.order), xlab = xlab, ylab = ylab, names = names,
            add = add, ann = ann, horizontal = horizontal, side = side, ...)
  }

#' @rdname vioplot
#' @export
vioplot.default <-
  function (x, ..., data = NULL, range = 1.5, h = NULL, xlim = NULL, ylim = NULL, names = NULL,
            horizontal = FALSE, col = "grey50", border = par()$fg, lty = 1,
            lwd = 1, rectCol = par()$fg, lineCol = par()$fg, pchMed = 19, colMed = "white", colMed2 = "grey 75",
            at, add = FALSE, wex = 1, drawRect = TRUE, areaEqual=FALSE,
            axes = TRUE, frame.plot = axes, panel.first = NULL, panel.last = NULL, asp = NA,
            main="", sub="", xlab=NA, ylab=NA, line = NA, outer = FALSE,
            xlog = NA, ylog=NA, adj=NA, ann = NA, ask=NA, bg=NA, bty=NA,
            cex=NA, cex.axis=NA, cex.lab=NA, cex.main=NA, cex.names=NULL, cex.sub=NA,
            cin=NA, col.axis=NA, col.lab=NA, col.main=NA, col.sub=NA,
            cra=NA, crt=NA, csi=NA,cxy=NA, din=NA, err=NA, family=NA, fg=NA,
            fig=NA, fin=NA, font=NA, font.axis=NA, font.lab=NA, font.main=NA, font.sub=NA,
            lab=NA, las=NA, lend=NA, lheight=NA, ljoin=NA, lmitre=NA, mai=NA, mar=NA, mex=NA,
            mfcol=NA, mfg=NA, mfrow=NA, mgp=NA, mkh=NA, new=NA, oma=NA, omd=NA, omi=NA,
            page=NA, pch=NA, pin=NA, plt=NA, ps=NA, pty=NA, smo=NA, srt=NA, tck=NA, tcl=NA,
            usr=NA, xaxp=NA, xaxs=NA, xaxt=NA, xpd=NA, yaxp=NA, yaxs=NA, yaxt=NA, ylbias=NA,
            log="", logLab=c(1,2,5),
            na.action = NULL, na.rm = T, side = "both", plotCentre = "point")
  {
    #assign graphical parameters if not given
    for(ii in 1:length(names(par()))){
      if(is.na(get(names(par())[ii])[1])) assign(names(par()[ii]), unlist(par()[[ii]]))
    }
    if(add && side != "both"){
      if(!is.null(names)) warning("Warning: names can only be changed on first call of vioplot (when add = FALSE)
")
      if(!is.na(xlab)) warning("Warning: x-axis labels can only be changed on first call of vioplot (when add = FALSE)
")
      if(!is.na(ylab)) warning("vy-axis labels can only be changed on first call of vioplot (when add = FALSE)
")
      if(!missing(main)) warning("Warning: main title can only be changed on first call of vioplot (when add = FALSE)
")
      if(!missing(sub)) warning("Warning: subtitle can only be changed on first call of vioplot (when add = FALSE)
 ")
    }
    if(!is.list(x)){
      datas <- list(x, ...)
    } else{
      datas <- lapply(x, unlist)
      if(is.null(names)){
        names <- names(datas)
      }
    }
    datas <- lapply(datas, function(x){
      if(all(x == unique(x)[1]) & length(x) > 100){
        unique(x)[1]
      } else {
        x
      }
    })
    if(is.character(log)) if("y" %in% unlist(strsplit(log, ""))) log <- TRUE
    if(is.na(xlog) | (horizontal == TRUE & (log == FALSE | log == ""))) xlog <- FALSE
    log <- ifelse(log == TRUE, "y", "")
    if(log == 'x' | log == 'xy' | xlog == TRUE){
      if(horizontal | log == "xy"){
        log <- TRUE
      } else {
        log <- FALSE
        ylog <- FALSE
      }
      xlog <- FALSE
    }
    if(log == TRUE | ylog == TRUE){
      ylog <- TRUE
      log <- "y"
    } else {
      log <- ""
    }
    if(ylog){
      #check data is compatible with log scale
      if(all(unlist(datas) <= 0)){
        ylog <- FALSE
        warning("log scale cannot be used with non-positive data")
      } else {
        #log-scale data
        datas <- datas #lapply(datas, function(x) log(unlist(x)))
      }
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
      if (is.null(xlim)) {
        xlim <- if (n == 1){
          at + c(-0.5, 0.5)
        } else {
          range(at) + min(diff(at))/2 * c(-1, 1)
        }
      } else {
        xlim.default <- if (n == 1){
          at + c(-0.5, 0.5)
        } else {
          range(at) + min(diff(at))/2 * c(-1, 1)
        }
        print(paste0("Using c(", xlim[1],",", xlim[2], ") as input for xlim, note that default values for these dimensions are c(", xlim.default[1],",", xlim.default[2], ")"))
      }
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
    if (!add){
      plot.new()
      if(!horizontal){
        plot.window(xlim, ylim, log = log, asp = asp, bty = bty, cex = cex, xaxs = xaxs, yaxs = yaxs, lab = lab, mai = mai, mar = mar, mex = mex, mfcol = mfcol, mfrow = mfrow, mfg = mfg, xlog = xlog, ylog = ylog)
      } else {
        plot.window(ylim, xlim, log = ifelse(log == "y", "x", ""), asp = asp, bty = bty, cex = cex, xaxs = xaxs, yaxs = yaxs, lab = lab, mai = mai, mar = mar, mex = mex, mfcol = mfcol, mfrow = mfrow, mfg = mfg, xlog = ylog, ylog = xlog)
      }
    }
    panel.first
    if (!horizontal) {
      if (!add) {
        plot.window(xlim, ylim, log = log, asp = asp, bty = bty, cex = cex, xaxs = xaxs, yaxs = yaxs, lab = lab, mai = mai, mar = mar, mex = mex, mfcol = mfcol, mfrow = mfrow, mfg = mfg, xlog = xlog, ylog = ylog)
        xaxp <- par()$xaxp
        yaxp <- par()$yaxp
        if(yaxt !="n"){
          if(ylog){
            #log_axis_label <- log_axis_label[log_axis >= exp(par("usr")[3])]
            #log_axis <- log_axis[log_axis >= exp(par("usr")[3])]
            #log_axis_label <- log_axis_label[log_axis <= exp(par("usr")[4])]
            #log_axis <- log_axis[log_axis <= exp(par("usr")[4])]
            Axis(unlist(datas), side = 2, cex.axis = cex.axis, col.axis = col.axis, font.axis = font.axis, mgp = mgp, tck = tck, tcl = tcl, las = las) # xaxp = xaxp, yaxp = yaxp disabled for log
            if(is.null(cex.names)) cex.names <- cex.axis
            if(xaxt !="n"){
              Axis(1:length(datas), at = at, labels = label, side = 1, cex.axis = cex.names, col.axis = col.axis, font.axis = font.axis, mgp = mgp, tck = tck, tcl = tcl, las = las) # xaxp = xaxp, yaxp = yaxp disabled for log
            }
          } else {
            Axis(unlist(datas), side = 2, cex.axis = cex.axis, col.axis = col.axis, font.axis = font.axis, mgp = mgp, yaxp = yaxp, tck = tck, tcl = tcl, las = las)
            if(is.null(cex.names)) cex.names <- cex.axis
            if(xaxt !="n"){
              Axis(1:length(datas), at = at, labels = label, side = 1, cex.axis = cex.names, col.axis = col.axis, font.axis = font.axis, mgp = mgp, xaxp = xaxp, tck = tck, tcl = tcl, las = las)
            }
          }
        }
      }
      if (frame.plot) {
        box(lty = lty, lwd = lwd)
      }
      for (i in 1:n) {
        polygon(c(at[i] - radj*height[[i]], rev(at[i] + ladj*height[[i]])),
                c(base[[i]], rev(base[[i]])), col = ifelse(length(col)>1,col[1+(i-1)%%length(col)], col), border = ifelse(length(border)>1, border[1+(i-1)%%length(border)], border),
                lty = lty, lwd = lwd, xpd = xpd, lend = lend, ljoin = ljoin, lmitre = lmitre)
        if (drawRect) {
          lines(at[c(i, i)], c(lower[i], upper[i]), lwd = lwd,
                lty = lty, col = ifelse(length(lineCol)>1, lineCol[1+(i-1)%%length(lineCol)], lineCol), lend = lend, ljoin = ljoin, lmitre = lmitre)
          rect(at[i] - radj*ifelse(length(boxwidth)>1, boxwidth[i], boxwidth)/2, q1[i], at[i] + ladj*ifelse(length(boxwidth)>1, boxwidth[i], boxwidth)/2,
               q3[i], col = ifelse(length(rectCol)>1, rectCol[1+(i-1)%%length(rectCol)], rectCol), border = ifelse(length(lineCol)>1, lineCol[1+(i-1)%%length(lineCol)], lineCol), xpd = xpd, lend = lend, ljoin = ljoin, lmitre = lmitre)
          if(plotCentre == "line"){
            lines(x = c(at[i] - radj*med.dens[i],
                        at[i],
                        at[i] + ladj*med.dens[i]),
                  y = rep(med[i],3))
          } else {
            points(at[i], med[i], pch = ifelse(length(pchMed)>1, pchMed[1+(i-1)%%length(pchMed)], pchMed), col = ifelse(length(colMed)>1, colMed[1+(i-1)%%length(colMed)], colMed), bg = ifelse(length(colMed2)>1, colMed2[1+(i-1)%%length(colMed2)], colMed2), cex = cex, lwd = lwd, lty = lty)
          }
        }
      }
    }
    else {
      if(log == "y" || ylog == TRUE){
        log <- "x"
        xlog <- TRUE
        ylog <- FALSE
      }
      if (!add) {
        plot.window(ylim, xlim, log = log, asp = asp, bty = bty, cex = cex, xaxs = xaxs, yaxs = yaxs, lab = lab, mai = mai, mar = mar, mex = mex, mfcol = mfcol, mfrow = mfrow, mfg = mfg, xlog = xlog, ylog = ylog)
        xaxp <- par()$xaxp
        yaxp <- par()$yaxp
        if(yaxt !="n"){
          if(xlog){
            #log_axis_label <- log_axis_label[log_axis >= exp(par("usr")[3])]
            #log_axis <- log_axis[log_axis >= exp(par("usr")[3])]
            #log_axis_label <- log_axis_label[log_axis <= exp(par("usr")[4])]
            #log_axis <- log_axis[log_axis <= exp(par("usr")[4])]
            Axis(unlist(datas), side = 1, cex.axis = cex.names, col.axis = col.axis, font.axis = font.axis, mgp = mgp, tck = tck, tcl = tcl, las = las) # xaxp = xaxp, yaxp = yaxp disabled for log
            if(is.null(cex.names)) cex.names <- cex.axis
            if(xaxt !="n"){
              Axis(1:length(datas), at = at, labels = label, side = 2, cex.axis = cex.axis, col.axis = col.axis, font.axis = font.axis, mgp = mgp, tck = tck, tcl = tcl, las = las) # xaxp = xaxp, yaxp = yaxp disabled for log
            }
          } else {
            Axis(unlist(datas), side = 1, cex.axis = cex.names, col.axis = col.axis, font.axis = font.axis, mgp = mgp, xaxp = xaxp, tck = tck, tcl = tcl, las = las)
            if(is.null(cex.names)) cex.names <- cex.axis
            if(xaxt !="n"){
              Axis(1:length(datas), at = at, labels = label, side = 2, cex.axis = cex.axis, col.axis = col.axis, font.axis = font.axis, mgp = mgp, yaxp = yaxp, tck = tck, tcl = tcl, las = las)
            }
          }
        }
      }
      if (frame.plot) {
        box(lty = lty, lwd = lwd)
      }
      for (i in 1:n) {
        polygon(c(base[[i]], rev(base[[i]])), c(at[i] - radj*height[[i]],
                                                rev(at[i] + ladj*height[[i]])), col = ifelse(length(col)>1,col[1+(i-1)%%length(col)], col), border = ifelse(length(border)>1, border[1+(i-1)%%length(border)], border),
                lty = lty, lwd = lwd, xpd = xpd, lend = lend, ljoin = ljoin, lmitre = lmitre)
        if (drawRect) {
          lines(c(lower[i], upper[i]), at[c(i, i)], lwd = lwd,
                lty = lty, col = ifelse(length(lineCol)>1, lineCol[1+(i-1)%%length(lineCol)], lineCol), lend = lend, ljoin = ljoin, lmitre = lmitre)
          rect(q1[i], at[i] - radj*ifelse(length(boxwidth)>1, boxwidth[i], boxwidth)/2, q3[i], at[i] +
                 ladj*ifelse(length(boxwidth)>1, boxwidth[i], boxwidth)/2, col = ifelse(length(rectCol)>1, rectCol[1+(i-1)%%length(rectCol)], rectCol), border = ifelse(length(lineCol)>1, lineCol[1+(i-1)%%length(lineCol)], lineCol), xpd = xpd, lend = lend, ljoin = ljoin, lmitre = lmitre)
          if(plotCentre == "line"){
            lines(y = c(at[i] - radj*med.dens[i],
                        at[i],
                        at[i] + ladj*med.dens[i]),
                  x = rep(med[i],3))
          } else {
            points(med[i], at[i], pch = ifelse(length(pchMed)>1, pchMed[1+(i-1)%%length(pchMed)], pchMed), col = ifelse(length(colMed)>1, colMed[1+(i-1)%%length(colMed)], colMed), , bg = ifelse(length(colMed2)>1, colMed2[1+(i-1)%%length(colMed2)], colMed2), cex = cex, lwd = lwd, lty = lty)
          }
        }
      }
    }
    panel.last
    if (ann) {
      title(main = main, sub = sub, xlab = xlab, ylab = ylab, line = line, outer = outer, xpd = xpd, cex.main = cex.main, col.main = col.main, font.main = font.main)
    }
    invisible(list(upper = upper, lower = lower, median = med,
                   q1 = q1, q3 = q3))
  }
