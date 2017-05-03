library("vioplotx")
context("log-scale")

test_that("plot defaults", {
  data(iris)
  boxplot(iris$Sepal.Length[iris$Species=="setosa"], iris$Sepal.Length[iris$Species=="versicolor"], iris$Sepal.Length[iris$Species=="virginica"], names=c("setosa", "versicolor", "virginica"))
  vioplotx(iris$Sepal.Length[iris$Species=="setosa"], iris$Sepal.Length[iris$Species=="versicolor"], iris$Sepal.Length[iris$Species=="virginica"], names=c("setosa", "versicolor", "virginica"))
})

##y-axis log scale (ylog)
test_that("log-scale y-axis with ylog TRUE", {
  vioplotx(iris$Sepal.Length[iris$Species=="setosa"], iris$Sepal.Length[iris$Species=="versicolor"], iris$Sepal.Length[iris$Species=="virginica"], names=c("setosa", "versicolor", "virginica"), main="Sepal Length", ylog = T, ylim=c(log(1), log(10)))
})
test_that("linear y-axis with ylog FALSE", {
  vioplotx(iris$Sepal.Length[iris$Species=="setosa"], iris$Sepal.Length[iris$Species=="versicolor"], iris$Sepal.Length[iris$Species=="virginica"], names=c("setosa", "versicolor", "virginica"), main="Sepal Length", ylog = F)
})

##y-axis log scale (log=TRUE)
test_that("log-scale y-axis with log TRUE", {
  vioplotx(iris$Sepal.Length[iris$Species=="setosa"], iris$Sepal.Length[iris$Species=="versicolor"], iris$Sepal.Length[iris$Species=="virginica"], names=c("setosa", "versicolor", "virginica"), main="Sepal Length", log = T, ylim=c(log(1), log(10)))
})
test_that("linear y-axis with log FALSE", {
  vioplotx(iris$Sepal.Length[iris$Species=="setosa"], iris$Sepal.Length[iris$Species=="versicolor"], iris$Sepal.Length[iris$Species=="virginica"], names=c("setosa", "versicolor", "virginica"), main="Sepal Length", log = F)
})
test_that("override with ylog TRUE", {
  vioplotx(iris$Sepal.Length[iris$Species=="setosa"], iris$Sepal.Length[iris$Species=="versicolor"], iris$Sepal.Length[iris$Species=="virginica"], names=c("setosa", "versicolor", "virginica"), main="Sepal Length", log = F, ylog=T, ylim=c(log(1), log(10)))
})

##y-axis log scale (log="y")
test_that("log-scale y-axis with log='y'", {
  vioplotx(iris$Sepal.Length[iris$Species=="setosa"], iris$Sepal.Length[iris$Species=="versicolor"], iris$Sepal.Length[iris$Species=="virginica"], names=c("setosa", "versicolor", "virginica"), main="Sepal Length", log = 'y', ylim=c(log(1), log(10)))
})
test_that("log-scale y-axis with log='xy'", {
  vioplotx(iris$Sepal.Length[iris$Species=="setosa"], iris$Sepal.Length[iris$Species=="versicolor"], iris$Sepal.Length[iris$Species=="virginica"], names=c("setosa", "versicolor", "virginica"), main="Sepal Length", log = 'xy', ylim=c(log(1), log(10)))
})
test_that("linear y-axis with log=''", {
  vioplotx(iris$Sepal.Length[iris$Species=="setosa"], iris$Sepal.Length[iris$Species=="versicolor"], iris$Sepal.Length[iris$Species=="virginica"], names=c("setosa", "versicolor", "virginica"), main="Sepal Length", log = '')
})
test_that("linear y-axis with log='x'", {
  vioplotx(iris$Sepal.Length[iris$Species=="setosa"], iris$Sepal.Length[iris$Species=="versicolor"], iris$Sepal.Length[iris$Species=="virginica"], names=c("setosa", "versicolor", "virginica"), main="Sepal Length", log = 'x')
})
test_that("override with ylog TRUE", {
  vioplotx(iris$Sepal.Length[iris$Species=="setosa"], iris$Sepal.Length[iris$Species=="versicolor"], iris$Sepal.Length[iris$Species=="virginica"], names=c("setosa", "versicolor", "virginica"), main="Sepal Length", log = 'x', ylog=T, ylim=c(log(1), log(10)))
})

##y-axis removed
test_that("linear scale y-axis with labels removed", {
  vioplotx(iris$Sepal.Length[iris$Species=="setosa"], iris$Sepal.Length[iris$Species=="versicolor"], iris$Sepal.Length[iris$Species=="virginica"], names=c("setosa", "versicolor", "virginica"), main="Sepal Length", ylog = F, yaxt="n")
})
test_that("log-scale y-axis with labels removed", {
  vioplotx(iris$Sepal.Length[iris$Species=="setosa"], iris$Sepal.Length[iris$Species=="versicolor"], iris$Sepal.Length[iris$Species=="virginica"], names=c("setosa", "versicolor", "virginica"), main="Sepal Length", ylog = T, ylim=c(log(1), log(10)), yaxt="n")
})
