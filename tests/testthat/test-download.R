context("Download")

test_that("ggplot is downloadable", {
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, hp)) + ggplot2::geom_point()
  width <- 20
  height <- 20
  file <- tempfile(fileext = ".zip")
  filename <- "plot"

  expect_false(file.exists(file))
  expect_silent(download(file = file, filename = filename, plot = plot, width = width, height = height))
  expect_true(file.exists(file))
})

test_that("plotly is downloadable", {
  skip_on_cran()

  plot <- plotly::ggplotly(ggplot2::ggplot(mtcars, ggplot2::aes(mpg, hp)) + ggplot2::geom_point())
  width <- 20
  height <- 20
  file <- tempfile(fileext = ".zip")
  filename <- "plot"

  expect_false(file.exists(file))
  download(file = file, filename = filename, plot = plot, width = width, height = height)
  expect_true(file.exists(file))
})

test_that("complexHeatmap is downloadable", {
  plot <- ComplexHeatmap::Heatmap(as.matrix(mtcars))
  width <- 20
  height <- 20
  file <- tempfile(fileext = ".zip")
  filename <- "plot"

  expect_false(file.exists(file))
  expect_silent(download(file = file, filename = filename, plot = plot, width = width, height = height))
  expect_true(file.exists(file))
})
