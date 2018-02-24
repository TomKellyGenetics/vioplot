library("vioplot")
context("names input")

test_that("list input", {
  data(iris)
  vioplot(iris$Sepal.Length[iris$Species=="setosa"], iris$Sepal.Length[iris$Species=="versicolor"], iris$Sepal.Length[iris$Species=="virginica"], names=c("a", "b", "c"))
})


test_that("naming formulae", {
  data(iris)
  vioplot(iris$Sepal.Length~iris$Species, names=c("a", "b", "c"))
  vioplot(Sepal.Length~Species, data=iris, names=c("a", "b", "c"))
  })

