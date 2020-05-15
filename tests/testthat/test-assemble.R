context("Assemble dashboard")

test_that("Assembling dashboard is correct",{
  temp <- tempfile(fileext = ".Rmd")
  i2dashboard(datadir = "input-data") %>%
    assemble(file = temp) -> dashboard

  expect_s4_class(dashboard, "i2dashboard")

  # removing comment line containing the sys.time for generating correct hash value
  rmd <- readLines(temp)
  new_rmd <- rmd[-31]
  writeLines(new_rmd, temp)

  ref_hash <- digest::digest(file = temp, serialize = F, seed = 100)

  expect_true(stringi::stri_detect_regex(ref_hash, "(1055ed29147446902eb224e7c17c52af|442bef06307bb6dea490435230f7e22e|11a5a0938212656d1fbb02496fb7cfa0)"))

  expect_warning(assemble(dashboard, file = temp, pages = "page1"), "i2dashboard dashboard does not contain a page named 'page1'")
})
