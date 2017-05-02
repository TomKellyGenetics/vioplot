library("vioplotx")
context("controlling area")

test_that("plot defaults", {
  data(iris)
  boxplot(iris$Sepal.Length[iris$Species=="setosa"], iris$Sepal.Length[iris$Species=="versicolor"], iris$Sepal.Length[iris$Species=="virginica"], names=c("setosa", "versicolor", "virginica"))
  vioplotx(iris$Sepal.Length[iris$Species=="setosa"], iris$Sepal.Length[iris$Species=="versicolor"], iris$Sepal.Length[iris$Species=="virginica"], names=c("setosa", "versicolor", "virginica"))
})

##Violin Plot Area

test_that("equal area with areaEqual TRUE", {
  vioplotx(iris$Sepal.Length[iris$Species=="setosa"], iris$Sepal.Length[iris$Species=="versicolor"], iris$Sepal.Length[iris$Species=="virginica"], names=c("setosa", "versicolor", "virginica"), main="Sepal Length", areaEqual = T)
})
test_that("equal width with areaEqual FALSE", {
  vioplotx(iris$Sepal.Length[iris$Species=="setosa"], iris$Sepal.Length[iris$Species=="versicolor"], iris$Sepal.Length[iris$Species=="virginica"], names=c("setosa", "versicolor", "virginica"), main="Sepal Length (Equal Width)", areaEqual = F)
})
test_that("equal area with areaEqual with vector colours", {
  vioplotx(iris$Sepal.Length[iris$Species=="setosa"], iris$Sepal.Length[iris$Species=="versicolor"], iris$Sepal.Length[iris$Species=="virginica"], names=c("setosa", "versicolor", "virginica"), main="Sepal Length (Equal Area)", areaEqual = T, col=c("lightgreen", "lightblue", "palevioletred"), rectCol=c("green", "blue", "palevioletred3"), lineCol=c("darkolivegreen", "royalblue", "violetred4"), border=c("darkolivegreen4", "royalblue4", "violetred4"))
})
test_that("equal area with areaEqual and wex scaling", {
  vioplotx(iris$Sepal.Length[iris$Species=="setosa"], iris$Sepal.Length[iris$Species=="versicolor"], iris$Sepal.Length[iris$Species=="virginica"], names=c("setosa", "versicolor", "virginica"), main="Sepal Length (Equal Area)", areaEqual = T, col=c("lightgreen", "lightblue", "palevioletred"), rectCol=c("green", "blue", "palevioletred3"), lineCol=c("darkolivegreen", "royalblue", "violetred4"), border=c("darkolivegreen4", "royalblue4", "violetred4"), wex=1.25)
})
