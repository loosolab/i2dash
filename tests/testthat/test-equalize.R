context("Equalize tables and vectors")

test_that("equalize works with tables", {
  table <- data.table::data.table(seq_len(10), seq(11, 20))
  eq <- wilson:::equalize(table)

  expect_length(eq, 2)
  expect_equal(eq, c(-20, 20))
})

test_that("equalize works with vectors", {
  vec <- seq_len(10)
  eq <- wilson:::equalize(vec)

  expect_length(eq, 2)
  expect_equal(eq, c(-10, 10))
})

test_that("non-numeric throws error", {
  expect_error(wilson:::equalize("a"))
})
