context("Accessor methods")

test_that("general getter and setter methods work correctly",{
  t <- "Test string"
  i <- TRUE
  s <- "menu"

  dashboard <- i2dashboard()
  title(dashboard) <- t
  expect_equal(title(dashboard), t)

  author(dashboard) <- t
  expect_equal(author(dashboard), t)

  theme(dashboard) <- t
  expect_equal(theme(dashboard), t)

  datadir(dashboard) <- t
  expect_equal(datadir(dashboard), t)

  interactivity(dashboard) <- i
  expect_equal(interactivity(dashboard), i)
})

test_that("getting/setting the source code embedding works as expected", {
  dashboard <- i2dashboard()

  embed_source(dashboard) <- TRUE
  expect_equal(source(dashboard), "embed")

  source(dashboard) <- "http://url.ending"
  expect_equal(source(dashboard), "http://url.ending")

  embed_source(dashboard) <- FALSE
  expect_equal(source(dashboard), "")
})

test_that("getting/setting the share links for social media work as expected", {
  dashboard <- i2dashboard()

  share(dashboard) <- c("Facebook", "reddit")
  expect_equal(share(dashboard), c("facebook"))
})
