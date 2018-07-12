context("forceArgs")

test_that("arguments are evaluated", {
  lazy_fun <- function(x, y) function() c(x, y)
  forceArgs_fun <- function(x, y) {
    forceArgs()
    function() c(x, y)
  }

  lazy_funList <- list()
  for (i in 1:2) {
    lazy_funList[[i]] <- lazy_fun(x = i, y = i)
  }
  lazy_result <- vapply(lazy_funList, function(x) x(), FUN.VALUE = numeric(2))


  forceArgs_funList <- list()
  for (j in 1:2) {
    forceArgs_funList[[j]] <- forceArgs_fun(x = j, y = j)
  }
  forceArgs_result <- vapply(forceArgs_funList, function(x) x(), FUN.VALUE = numeric(2))

  forceAndCall_funList <- list()
  for (k in 1:2) {
    forceAndCall_funList[[k]] <- forceAndCall(n = 2, lazy_fun, x = k, y = k)
  }
  forceAndCall_result <- vapply(forceAndCall_funList, function(x) x(), FUN.VALUE = numeric(2))

  expect_false(all(lazy_result == forceArgs_result))
  expect_equal(forceArgs_result, forceAndCall_result)
})
