library("vioplotx")
context("unequal group size")

data(iris)
table(iris$Species)
identical(as.numeric(table(iris$Species)), c(50, 50, 50))

index <- sample(1:3,150,replace=T)
while(identical(as.numeric(table(index)), c(50, 50, 50))) index <- sample(1:3,150,replace=T)
table(index)
iris$Species <- factor(names(table(iris$Species))[index])

test_that("list input", {
  vioplotx(iris$Sepal.Length[iris$Species=="setosa"], iris$Sepal.Length[iris$Species=="versicolor"], iris$Sepal.Length[iris$Species=="virginica"])
})


test_that("formulae input", {
  vioplotx(iris$Sepal.Length~iris$Species)
  vioplotx(Sepal.Length~Species, data=iris)
})

