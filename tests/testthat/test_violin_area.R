library("vioplot")
context("controlling area")

test_that("plot defaults", {
  data(iris)
  boxplot(iris$Sepal.Length[iris$Species=="setosa"], iris$Sepal.Length[iris$Species=="versicolor"], iris$Sepal.Length[iris$Species=="virginica"], names=c("setosa", "versicolor", "virginica"))
  vioplot(iris$Sepal.Length[iris$Species=="setosa"], iris$Sepal.Length[iris$Species=="versicolor"], iris$Sepal.Length[iris$Species=="virginica"], names=c("setosa", "versicolor", "virginica"))
})

##Violin Plot Area

test_that("equal area with areaEqual TRUE", {
  vioplot(iris$Sepal.Length[iris$Species=="setosa"], iris$Sepal.Length[iris$Species=="versicolor"], iris$Sepal.Length[iris$Species=="virginica"], names=c("setosa", "versicolor", "virginica"), main="Sepal Length", areaEqual = TRUE)
})
test_that("equal width with areaEqual FALSE", {
  vioplot(iris$Sepal.Length[iris$Species=="setosa"], iris$Sepal.Length[iris$Species=="versicolor"], iris$Sepal.Length[iris$Species=="virginica"], names=c("setosa", "versicolor", "virginica"), main="Sepal Length (Equal Width)", areaEqual = FALSE)
})
test_that("equal area with areaEqual with vector colours", {
  vioplot(iris$Sepal.Length[iris$Species=="setosa"], iris$Sepal.Length[iris$Species=="versicolor"], iris$Sepal.Length[iris$Species=="virginica"], names=c("setosa", "versicolor", "virginica"), main="Sepal Length (Equal Area)", areaEqual = TRUE, col=c("lightgreen", "lightblue", "palevioletred"), rectCol=c("green", "blue", "palevioletred3"), lineCol=c("darkolivegreen", "royalblue", "violetred4"), border=c("darkolivegreen4", "royalblue4", "violetred4"))
})
test_that("equal area with areaEqual and wex scaling", {
  vioplot(iris$Sepal.Length[iris$Species=="setosa"], iris$Sepal.Length[iris$Species=="versicolor"], iris$Sepal.Length[iris$Species=="virginica"], names=c("setosa", "versicolor", "virginica"), main="Sepal Length (Equal Area)", areaEqual = TRUE, col=c("lightgreen", "lightblue", "palevioletred"), rectCol=c("green", "blue", "palevioletred3"), lineCol=c("darkolivegreen", "royalblue", "violetred4"), border=c("darkolivegreen4", "royalblue4", "violetred4"), wex=1.25)
})
