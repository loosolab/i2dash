context("MaxQuant parser")

testthat::test_that("all needed input parameteres are given", {
  expect_error(wilson::parse_MaxQuant(),"The proteinGroups file was not given")
  expect_error(wilson::parse_MaxQuant(proteinGroups_in = "/path/path/"),"The summary file was not given")
  expect_error(wilson::parse_MaxQuant(proteinGroups_in = "/path/path/",summary_in = "/path/path/"),
                                      "The output file was not given")
  expect_error(wilson::parse_MaxQuant(proteinGroups_in = "/path/path/",summary_in = "/path/path/",
                                      outfile = "/path/path/"),"The output_reduced file was not given")
})

testthat::test_that("mq_parser",{

  expect_error(wilson::parse_MaxQuant(proteinGroups_in = system.file("/tests/testthat", "proteinGroups_test.txt", package = "wilson"),
                                      summary_in = system.file("/tests/testthat", "summary_test_2.txt", package = "wilson"),
                                      outfile = "./out", outfile_reduced = "./outres" ),
               "wrong format on summary file: column \'Experiment\' misssing")
  expect_true(wilson::parse_MaxQuant(proteinGroups_in = system.file("/tests/testthat", "proteinGroups_test.txt", package = "wilson"),
                                      summary_in = system.file("/tests/testthat", "summary_test.txt", package = "wilson"),
                                      outfile = "./out", outfile_reduced = "./outres", config = system.file("/tests/testthat", "success_config.json", package = "wilson")))
  expect_error(wilson::parse_MaxQuant(proteinGroups_in = system.file("/tests/testthat", "proteinGroups_test.txt", package = "wilson"),
                                      summary_in = system.file("/tests/testthat", "summary_test.txt", package = "wilson"),
                                      outfile = "./out", outfile_reduced = "./outres", config = "" ),
               "Could not read config file")
  expect_error(wilson::parse_MaxQuant(proteinGroups_in = system.file("/tests/testthat", "proteinGroups_test.txt", package = "wilson"),
                                      summary_in = system.file("/tests/testthat", "summary_test.txt", package = "wilson"),
                                      outfile = "./out", outfile_reduced = "./outres", config = system.file("/tests/testthat", "fail_config.json", package = "wilson") ),
               "reduced_list is missing in config file")
})
