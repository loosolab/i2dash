context("Assemble dashboard")

test_that("Assembling dashboard is correct",{
  i2dashboard(datadir="input-data") %>%
    assemble() -> dashboard

  expect_s4_class(dashboard, "i2dashboard")

  # removing comment line containing the sys.time for generating correct hash value
  rmd <- readLines(dashboard@file)
  new_rmd <- rmd[-31]
  writeLines(new_rmd, dashboard@file)

  test_hash <- digest::digest(file = dashboard@file, serialize = F, seed = 100)
  ref_hash <- digest::digest(file = "input-data/i2dashboard.Rmd", serialize = F, seed = 100)
  expect_equal(test_hash, ref_hash)

  expect_warning(assemble(dashboard, pages = "page1"), "i2dashboard dashboard does not contain a page named 'page1'")

  # Delete dashboard Rmd file
  if (file.exists(dashboard@file)) {
    file.remove(dashboard@file)
  }
})
