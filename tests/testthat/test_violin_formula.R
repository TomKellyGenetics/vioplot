library("vioplot")
context("formula input")

test_that("plot data list input", {
  data(iris)
  boxplot(iris$Sepal.Length[iris$Species=="setosa"], iris$Sepal.Length[iris$Species=="versicolor"], iris$Sepal.Length[iris$Species=="virginica"], names=c("setosa", "versicolor", "virginica"), main = "Sepal Length")
  vioplot(iris$Sepal.Length[iris$Species=="setosa"], iris$Sepal.Length[iris$Species=="versicolor"], iris$Sepal.Length[iris$Species=="virginica"], names=c("setosa", "versicolor", "virginica"), main = "Sepal Length")
})
test_that("plot vector formula input", {
  boxplot(iris$Sepal.Length~iris$Species)
  vioplot(iris$Sepal.Length~iris$Species)
})
test_that("plot column formula with dataframe input", {
  boxplot(Sepal.Length~Species, data=iris)
  vioplot(Sepal.Length~Species, data=iris)
})
test_that("plot formula with dataframe input and scalar colour", {
  vioplot(Sepal.Length~Species, data=iris, col="lightblue")
})
test_that("plot formula with dataframe input and vector colour", {
  vioplot(Sepal.Length~Species, data=iris, col=c("lightgreen", "lightblue", "palevioletred"))
})
# attach(iris)
# iris <- as.matrix(iris[,1:4])
# test_that("plot column formula with matrix input", {
#   vioplot(Sepal.Length~Species, data=iris)
# })
