context("Logging")

test_that("logger can be created and deleted", {
  logger <- log4r::create.logger()
  token <- "test"

  expect_false(exists(paste0("logger", token), envir = wilson.globals))
  set_logger(logger = logger, token = token)
  expect_identical(logger, get(paste0("logger", token), envir = wilson.globals))
  set_logger(logger = NULL, token = token)
  expect_false(exists(paste0("logger", token), envir = wilson.globals))
})

test_that("message can be logged", {
  logfile <- tempfile()
  logger <- log4r::create.logger(logfile = logfile, level = "DEBUG")
  token <- "test_log"

  set_logger(logger = logger, token = token)
  expect_false(file.exists(logfile))
  log_message("test message", level = "DEBUG", token = token)
  expect_true(file.exists(logfile))
  file.remove(logfile)
  set_logger(logger = NULL, token = token)
  log_message("test message", level = "DEBUG", token = token)
  expect_false(file.exists(logfile))
})
