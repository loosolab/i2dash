context("Adding content to the dashboard")

#
# add_link
#
test_that("adding a link to the navigation works as expected",{
  l1 <- list(href = "sample_url", title = "Link", icon = "", align = "right", target = "")

  i2dashboard() %>%
    add_link(
      href = "sample_url",
      title = "Link") -> dashboard

  expect_s4_class(dashboard, "i2dashboard")
  expect_equal(dashboard@navbar[[1]], l1)
  expect_warning(add_link(dashboard, href = "sample_url"), "Both, title and icon, cannot be NULL when adding a link.")
})

#
# add_colormap
#
test_that("adding a colorbar to the dashboard works as expected",{
  colors <- c("l1" = "#F7FCFD", "l2" ="#E5F5F9", "l3" = "#CCECE6")

  i2dashboard() %>%
    add_colormap(map = colors, name = "test") -> dashboard

  expect_s4_class(dashboard, "i2dashboard")
  expect_equal(dashboard@colormaps$test, colors)
})

#
# add_to_sidebar
#
test_that("adding sidebar content to the dashboard works as expected",{
  text_generator <- function(dashboard) paste0("Lorem ipsum dolor sit amet\n")
  base_sidebar <- function(component, global=F) i2dashboard() %>% add_to_sidebar(component = component, global=global) -> dashboard

  # add text to local sidebar
  expect_s4_class(base_sidebar("input-data/sample.txt"), "i2dashboard")
  expect_equal(base_sidebar("input-data/sample.txt")@pages$default$sidebar, "### \n\nLorem ipsum dolor sit amet\n")

  # add image to local sidebar
  expect_s4_class(base_sidebar("input-data/sample.jpg"), "i2dashboard")
  expect_equal(base_sidebar("input-data/sample.jpg")@pages$default$sidebar, "### \n\n<img src=\"input-data/sample.jpg\" alt=\"input-data/sample.jpg\" style=\"height:auto;width:100%\"/><br/>\n")

  # use function for local sidebar
  expect_s4_class(base_sidebar(text_generator), "i2dashboard")
  expect_equal(base_sidebar(text_generator)@pages$default$sidebar, "Lorem ipsum dolor sit amet\n")

  # add text to global sidebar
  expect_s4_class(base_sidebar("input-data/sample.txt", global=TRUE), "i2dashboard")
  expect_equal(base_sidebar("input-data/sample.txt", global=TRUE)@sidebar, "### \n\nLorem ipsum dolor sit amet\n")

  # add image to global sidebar
  expect_s4_class(base_sidebar("input-data/sample.jpg", global=TRUE), "i2dashboard")
  expect_equal(base_sidebar("input-data/sample.jpg", global=TRUE)@sidebar, "### \n\n<img src=\"input-data/sample.jpg\" alt=\"input-data/sample.jpg\" style=\"height:auto;width:100%\"/><br/>\n")

  # use function for global sidebar
  expect_s4_class(base_sidebar(text_generator, global=TRUE), "i2dashboard")
  expect_equal(base_sidebar(text_generator, global=TRUE)@sidebar, "Lorem ipsum dolor sit amet\n")

  expect_warning(i2dashboard() %>% add_to_sidebar(component = "input-data/sample.txt", page = "page1"), "i2dashboard dashboard does not contain a page named 'page1'")
})

#
# add_component
#
test_that("adding components to a dashboard is correct",{


  # test signature 'i2dashboard,'function''
  text_generator <- function(dashboard) paste0("### Test\n\n", "Lorem ipsum dolor sit amet\n")
  i2dashboard() %>%
    add_component(component = text_generator) -> dashboard
  expect_equal(length(dashboard@pages$default$components), 1)
  expect_equal(dashboard@pages$default$components[[1]], "### Test\n\nLorem ipsum dolor sit amet\n")

  base_component <- function(component) i2dashboard() %>% add_component(component = component, title = "Test") -> dashboard

  # test signature 'i2dashboard,character'
  expect_equal(length(base_component("input-data/sample.txt")@pages$default$components), 1)
  expect_equal(base_component("input-data/sample.txt")@pages$default$components[[1]], "### Test\n\nLorem ipsum dolor sit amet\n")

  expect_equal(length(base_component("input-data/sample.jpg")@pages$default$components), 1)
  expect_equal(base_component("input-data/sample.jpg")@pages$default$components[[1]], "### Test\n\n![input-data/sample.jpg](input-data/sample.jpg)\n")

  # test signature 'i2dashboard,gg'
  if(requireNamespace("ggplot2", quietly = TRUE)){
    o1 <- ggplot2::ggplot(mtcars,ggplot2::aes(x=wt,y=mpg)) + ggplot2::geom_point()

    expect_equal(length(base_component(o1)@pages$default$components), 1)
    expect_match(base_component(o1)@pages$default$components[[1]], "### Test")
  }

  # test signature 'i2dashboard,gt_tbl'
  if(requireNamespace("gt", quietly = TRUE)){
    o2 <- gt::gt(mtcars)

    expect_equal(length(base_component(o2)@pages$default$components), 1)
    expect_match(base_component(o2)@pages$default$components[[1]], "### Test")
  }
  # test signature 'i2dashboard,knitr_kable'
  if(requireNamespace("kableExtra", quietly = TRUE)){
    o3 <- kableExtra::kable(mtcars) %>% kableExtra::kable_styling()

    expect_equal(length(base_component(o3)@pages$default$components), 1)
    expect_match(base_component(o3)@pages$default$components[[1]], "### Test")
  }

  # test signature 'i2dashboard,Heatmap'
  if(requireNamespace("ComplexHeatmap", quietly = TRUE)) {
    o4 <- ComplexHeatmap::Heatmap(scale(mtcars))

    expect_equal(length(base_component(o4)@pages$default$components), 1)
    expect_match(base_component(o4)@pages$default$components[[1]], "### Test")
  }

  # test signature 'i2dashboard,ANY'
  if(requireNamespace("plotly", quietly = TRUE)) {
    o5 <- plotly::plot_ly(mtcars,x=~wt,y=~mpg)
    #o6 <- lattice::xyplot(mpg ~ hp, data=mtcars)

    expect_equal(length(base_component(o5)@pages$default$components), 1)
    expect_match(base_component(o5)@pages$default$components[[1]], "### Test")
  }
  expect_warning(add_component(dashboard, component = text_generator, page = "page1"), "i2dashboard dashboard does not contain a page named 'page1'")
  #expect_warning(add_component(dashboard, component = o6), "The component did not inherit from any of the currently supported classes ('htmlwidget').")

  unlink(dashboard@datadir, recursive=TRUE)
})
