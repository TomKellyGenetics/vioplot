#' Annotated Violin Plot
#'
#' @name add_labels
#' @description
#'  Annotate violin plots with custom labels
#' @aliases add_labels
#' @param variable continuous variable to to plot on y-axis (numeric or integer)
#' @param categories discrete variable to break down groups (factor or string).
#' @param cex size of text.
#' @param col colour of text
#' @param height adjust placement of text.
#' @keywords plot graphics violin annotation.
#' @examples
#'
#' # box- vs violin-plot
#' par(mfrow=c(2,1))
#' mu<-2
#' si<-0.6
#' bimodal<-c(rnorm(1000,-mu,si),rnorm(1000,mu,si))
#' uniform<-runif(2000,-4,4)
#' normal<-rnorm(2000,0,3)
#'
#' # annotate a violin plot
#' group <- rep(c("bimodal", "uniform", "normal"),
#'              sapply(list(bimodal, uniform, normal), length))
#' table(group)
#' vioplot(bimodal,uniform,normal)
#' add_labels(unlist(bimodal,uniform,normal), group, height = 3, cex = 0.8)
#'
#' # boxplots are also supported
#' boxplot(bimodal,uniform,normal)
#' add_labels(unlist(bimodal,uniform,normal), group, height = 3, cex = 0.8)
#'
#'
#' # formula input
#' data("iris")
#' vioplot(Sepal.Length~Species, data = iris, main = "Sepal Length",
#'         col=c("lightgreen", "lightblue", "palevioletred"))
#' legend("bottomright", legend=c("setosa", "versicolor", "virginica"),
#'        fill=c("lightgreen", "lightblue", "palevioletred"), cex = 0.6)
#' add_labels(unlist(iris$Sepal.Length), iris$Species, height = 0, cex = 0.8)
#'
#' # demo with outliers
#' iris2 <- iris
#' iris2 <- rbind(iris2, c(7, 0, 0, 0, "setosa"))
#' iris2 <- rbind(iris2, c(0, 0, 0, 0, "setosa"))
#' iris2 <- rbind(iris2, c(9, 0, 0, 0, "versicolor"))
#' iris2 <- rbind(iris2, c(2, 0, 0, 0, "versicolor"))
#' iris2 <- rbind(iris2, c(10, 0, 0, 0, "virginica"))
#' iris2 <- rbind(iris2, c(12, 0, 0, 0, "virginica"))
#' iris2$Species <- factor(iris2$Species)
#' iris2$Sepal.Length <- as.numeric(iris2$Sepal.Length)
#'
#' vioplot(Sepal.Length~Species, data = iris2, main = "Sepal Length",
#'         col=c("lightgreen", "lightblue", "palevioletred"))
#' add_outliers(unlist(iris2$Sepal.Length), iris2$Species,
#'              col = "grey50", fill = "red", bars = "grey85")
#' legend("bottomright", legend=c("setosa", "versicolor", "virginica"),
#'        fill=c("lightgreen", "lightblue", "palevioletred"), cex = 0.6)
#' add_labels(unlist(iris2$Sepal.Length), iris2$Species, height = 0, cex = 0.8)
#'
#' @usage add_labels(variable, categories, cex = par()$cex, col = par()$fg, height = 0.5)
#' @rdname add_labels
#' @export
#'
add_labels <- function(variable, categories, cex = par()$cex, col = par()$fg, height = 0.5){
  text(x =  seq_along(table(categories)), y = rep(max(variable)+height, length(categories)), sapply(table(categories), function(nn) paste0(c("n=", nn), collapse = "")), col = col, cex = cex)
}


#' Annotated Violin Plot
#'
#' Annotate violin plots with outliers
#'
#' @name add_outliers
#' @description
#' Annotation to highlight outliers.
#' @param variable continuous variable to to plot on y-axis (numeric or integer).
#' @param categories discrete variable to break down groups (factor or string).
#' @param col colour of rings or borders. Scalar applied to all columns or a vector for each category.
#' @param fill colour of spots. Scalar applied to all columns or a vector for each category.
#' @param bars colour of horizontal bars. Scalar applied to all columns or a vector for each category.
#' @param cutoff minimum number (default 3L) of standard deviations to report.
#' @param verbose to print logs (defaults to FALSE).
#' @keywords plot graphics violin annotation
#'
#' @usage add_outliers(variable, categories, cutoff = 3,
#' fill = par()$bg, col = par()$fg, bars = par()$fg,
#' verbose = FALSE)
#' @rdname add_outliers
#' @importFrom stats sd
#' @importFrom graphics abline text
#' @export
add_outliers <- function(variable, categories, cutoff = 3, fill = par()$bg, col = par()$fg, bars = par()$fg, verbose = FALSE){
  if(length(col) == 1) col <- rep(col, length(unique(categories)))
  if(length(fill) == 1) fill <- rep(fill, length(unique(categories)))
  for(category in unique(categories)){
    ii <- which(sort(unique(categories)) == category)
    if(verbose){
      print(category)
      print(ii)
    }
    y <- variable[categories == category]
    y <- na.omit(y)
    if(verbose) print(length(y[y < mean(y, na.rm = TRUE) - cutoff * sd(y, na.rm = TRUE)]))
    points(rep(ii, length(y[y < mean(y, na.rm = TRUE) - cutoff * sd(y, na.rm = TRUE)])),
           y[y < mean(y, na.rm = TRUE) - cutoff * sd(y, na.rm = TRUE)],
           cex = 1.25, pch = 21, bg = fill[ii], col = col[ii])
    if(verbose) print(length(y[y > mean(y, na.rm = TRUE) + cutoff * sd(y, na.rm = TRUE)]))
    points(rep(ii, length(y[y > mean(y, na.rm = TRUE) + cutoff * sd(y, na.rm = TRUE)])),
           y[y > mean(y, na.rm = TRUE) + cutoff * sd(y, na.rm = TRUE)],
           cex = 1.25, pch = 21, bg = fill[ii], col = col[ii])
  }
  abline(h=0, lwd = 1.5, lty = 2, col = bars)
  return(0)
}
