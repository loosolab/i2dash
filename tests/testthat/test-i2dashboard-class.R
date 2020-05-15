context("i2dashboard class")

test_that("construction of the i2dashboard works correctly",{
  dashboard <- i2dashboard()
  expect_s4_class(dashboard, "i2dashboard")
})

test_that("show function works",{
  dashboard <- i2dashboard()
  expect_output(show(dashboard),
"A flexdashboard with the title: i2dashboard
... containing  1 pages:
 ... the page 'default' with the title 'Default page' contains 0 components.")
})
