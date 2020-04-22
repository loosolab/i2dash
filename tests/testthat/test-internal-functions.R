context("Internal functions")

#
# add_vis_object
#
test_that("add_vis_object works correctly",{
  if(requireNamespace("plotly", quietly = TRUE)) {
    obj <- plotly::plot_ly(mtcars,x=~wt,y=~mpg)
    i2dashboard() %>% i2dash:::add_vis_object(obj, "plotly", title = "Test") -> dashboard

    expect_equal(length(dashboard@pages$default$components), 1)
    expect_match(dashboard@pages$default$components[[1]], "### Test")
    unlink(dashboard@datadir, recursive=TRUE)
  }
})

#
# .render_page
#
test_that(".render_page works correctly",{
  title <- "Test"
  comp <- "### Component\n\nContent\n\n"
  expect_match(i2dash:::.render_page(title, comp, "default"), "Test")
  expect_match(i2dash:::.render_page(title, comp, "default"), "### Component\n\nContent\n\n")
})

#
# render_image
#
test_that("render_image works correctly",{
  expect_equal(i2dash:::render_image("input-data/sample.jpg"), "### \n\n![input-data/sample.jpg](input-data/sample.jpg)\n")
})

#
# render_text
#
test_that("render_text works correctly",{
  expect_equal(i2dash:::render_text("input-data/sample.txt"), "### \n\nLorem ipsum dolor sit amet\n")
})

#
# .add_component
#
test_that(".add_component works correctly",{
  comp <- "### Component\n\nContent\n\n"
  i2dashboard() %>% i2dash:::.add_component("default", comp) -> dashboard

  expect_equal(length(dashboard@pages$default$components), 1)
  expect_match(dashboard@pages$default$components[[1]], "### Component\n\nContent\n\n")
})
