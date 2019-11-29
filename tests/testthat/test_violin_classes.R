library("vioplot")
context("different input classes")

test_that("input as data.frame", {
  data(iris)
  boxplot(as.data.frame(iris))
  vioplot(as.data.frame(iris)[sapply(as.list(iris), is.numeric)])
})

test_that("input as list", {
  data(iris)
  boxplot(as.list(iris))
  vioplot(as.list(iris)[sapply(as.list(iris), is.numeric)])
})

test_that("input as matrix (by col)", {
  data(iris)
  boxplot(as.matrix(iris[sapply(as.list(iris), is.numeric)]))
  vioplot(as.matrix(iris[sapply(as.list(iris), is.numeric)]))
})

test_that("input as matrix (by col)", {
  data(iris)
  boxplot(as.matrix(iris[sapply(as.list(iris), is.numeric)]), use.cols = FALSE)
  vioplot(as.matrix(iris[sapply(as.list(iris), is.numeric)]), use.cols = FALSE)
})
