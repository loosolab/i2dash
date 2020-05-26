context("Create static plots")

test_that("scatterplot can be created", {
  data <- data.table::data.table(mtcars[, c("mpg", "hp", "vs")], keep.rownames = "id")

  out <- create_scatterplot(data, highlight.data = data[1:3,], highlight.color = "blue", color = c("red", "green"), categorized = TRUE, density = FALSE)

  expect_s3_class(out$plot, "ggplot")
  expect_type(out$width, "double")
  expect_type(out$height, "double")
  expect_type(out$ppi, "double")
  expect_type(out$exceed_size, "logical")
})

test_that("pca can be created", {
  data <- data.table::data.table(mtcars, keep.rownames = "id")
  grouping <- rep(c("a", "b", "c"), length.out = ncol(data) - 1)

  out <- create_pca(data, color.group = grouping, shape.group = grouping, palette = c("red", "green", "blue"))

  expect_s3_class(out$plot, "ggplot")
  expect_type(out$width, "double")
  expect_type(out$height, "double")
  expect_type(out$ppi, "double")
  expect_type(out$exceed_size, "logical")
})

test_that("heatmap can be created", {
  data <- data.table::data.table(mtcars, keep.rownames = "id")

  out <- create_heatmap(data, colors = c("red", "green"))

  expect_s4_class(out$plot, "Heatmap")
  expect_type(out$width, "double")
  expect_type(out$height, "double")
  expect_type(out$ppi, "double")
  expect_type(out$exceed_size, "logical")
})

test_that("geneview can be created", {
  data <- data.table::data.table(mtcars, keep.rownames = "id")
  grouping <- grouping <- data.table::data.table(names(data)[-1], factor = rep(c("a", "b"), length.out = ncol(data) - 1))

  out <- create_geneview(data, grouping = grouping, colors = c("red", "green"))

  expect_s3_class(out$plot, "ggplot")
  expect_type(out$width, "double")
  expect_type(out$height, "double")
  expect_type(out$ppi, "double")
  expect_type(out$exceed_size, "logical")
})
