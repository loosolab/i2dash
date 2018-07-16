context("Parse clarion file")

test_that("file is parsed correctly", {
  object <- parser(file = "wiki_example.clarion", dec = ",")

  expect_is(object, "Clarion")
  expect_equal(object$get_delimiter(), "|")
  expect_equal(object$get_factors(), object$metadata[, c("key", "factor1", "factor2")])
  expect_equal(object$get_id(), "id")
  expect_equal(object$get_name(), "name")
  expect_equal(object$is_delimited(names(object$data)), c(rep(FALSE, 3), TRUE, rep(FALSE, 8)))
})
