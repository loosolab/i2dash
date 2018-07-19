context("Create interactive plots")

test_that("scatterplot can be created", {
  data <- data.table::data.table(mtcars[, c("mpg", "hp", "vs")], keep.rownames = "id")

  out <- create_scatterplot(data, highlight.data = data[1:3,], highlight.color = "blue", color = c("red", "green"), categorized = TRUE, plot.method = "interactive")
  expect_is(out$plot, class = "plotly")
})

test_that("heatmap can be created", {
  data <- data.table::data.table(mtcars, keep.rownames = "id")

  out <- create_heatmap(data, colors = c("red", "green"), plot.method = "interactive")
  expect_is(out$plot, class = "plotly")
})

test_that("geneview can be created", {
  data <- data.table::data.table(mtcars, keep.rownames = "id")
  grouping <- grouping <- data.table::data.table(names(data)[-1], factor = rep(c("a", "b"), length.out = ncol(data) - 1))

  out <- create_geneview(data, grouping = grouping, colors = c("red", "green"), plot.method = "interactive")
  expect_is(out$plot, class = "plotly")
})
