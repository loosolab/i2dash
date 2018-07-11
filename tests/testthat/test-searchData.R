context("orNumeric searchData function")

test_that("no selection returns all TRUE", {
  input <- c(5)
  choices <- c(seq_len(20), NA, NaN)
  selection <- wilson:::searchData(input = input, choices = choices, options = NULL)

  expect_length(selection, length(choices))
  expect_type(selection, "logical")
  expect_equal(selection, rep(TRUE, length(choices)))
})

test_that("ranged selection is correct", {
  input <- c(2, 5)
  choices <- c(seq_len(20), NA, NaN)
  selection_inner <- wilson:::searchData(input = input, choices = choices, options = "inner")
  selection_outer <- wilson:::searchData(input = input, choices = choices, options = "outer")

  expect_length(selection_inner, length(choices))
  expect_type(selection_inner, "logical")
  expect_equal(selection_inner, c(FALSE, rep(TRUE, 4), rep(FALSE, 17)))

  expect_length(selection_outer, length(choices))
  expect_type(selection_outer, "logical")
  expect_equal(selection_outer, c(TRUE, rep(FALSE, 4), rep(TRUE, 15), rep(FALSE, 2)))
})

test_that("single selection is correct", {
  input <- c(2)
  choices <- c(seq_len(20), NA, NaN)
  selection_equal <- wilson:::searchData(input = input, choices = choices, options = "=")
  selection_smaller <- wilson:::searchData(input = input, choices = choices, options = "<")
  selection_greater <- wilson:::searchData(input = input, choices = choices, options = ">")
  selection_all <- wilson:::searchData(input = input, choices = choices, options = c("=", "<", ">"))

  expect_length(selection_equal, length(choices))
  expect_type(selection_equal, "logical")
  expect_equal(selection_equal, c(FALSE, TRUE, rep(FALSE, 20)))

  expect_length(selection_smaller, length(choices))
  expect_type(selection_smaller, "logical")
  expect_equal(selection_smaller, c(TRUE, rep(FALSE, 21)))

  expect_length(selection_greater, length(choices))
  expect_type(selection_greater, "logical")
  expect_equal(selection_greater, c(rep(FALSE, 2), rep(TRUE, 18), rep(FALSE, 2)))

  expect_length(selection_all, length(choices))
  expect_type(selection_all, "logical")
  expect_equal(selection_all, c(rep(TRUE, 20), rep(FALSE, 2)))
})
