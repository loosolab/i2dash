context("MaxQuant parser")

test_that("all needed input parameteres are given", {
  expect_error(parse_MaxQuant(), "The proteinGroups file was not given")
  expect_error(parse_MaxQuant(proteinGroups_in = "./path/path/"), "The summary file was not given")
  expect_error(parse_MaxQuant(proteinGroups_in = "./path/path/", summary_in = "./path/path/"),
                                      "The output file was not given")
  expect_error(parse_MaxQuant(proteinGroups_in = "./path/path/", summary_in = "./path/path/",
                                      outfile = "./path/path/"), "The output_reduced file was not given")
})

test_that("mq_parser", {

  expect_error(parse_MaxQuant(proteinGroups_in = "proteinGroups_test.txt", summary_in = "summary_test_2.txt",
                                      outfile = "./out", outfile_reduced = "./outres" ),
               "wrong format on summary file: column \'Experiment\' misssing")
  expect_true(parse_MaxQuant(proteinGroups_in = "proteinGroups_test.txt", summary_in = "summary_test.txt",
                                      outfile = "./out", outfile_reduced = "./outres", config = "success_config.json"))
  expect_error(parse_MaxQuant(proteinGroups_in = "proteinGroups_test.txt", summary_in = "summary_test.txt",
                                      outfile = "./out", outfile_reduced = "./outres", config = "" ),
               "Could not read config file")
  expect_error(parse_MaxQuant(proteinGroups_in = "proteinGroups_test.txt", summary_in = "summary_test.txt",
                                      outfile = "./out", outfile_reduced = "./outres", config = "fail_config.json" ),
               "reduced_list is missing in config file")
})
