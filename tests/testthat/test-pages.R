context("Adding and removing pages")

p <- list(
  title = "New page",
  layout = "2x2_grid",
  menu = "A",
  components = list(),
  max_components = 4,
  sidebar = NULL
)

test_that("Adding new page is correct",{
  i2dashboard() %>%
    add_page(
      page = "new",
      title = "New page",
      layout = "2x2_grid",
      menu = "A") -> dashboard
  expect_equal(dashboard@pages$new, p)
  #expect_warning(add_page(dashboard, page = "new", title = "Title"), "The page new already exists and will be overwritten.")
})

test_that("Removing page is correct",{
  i2dashboard() %>%
    add_page(
      page = "new",
      title = "New page",
      layout = "2x2_grid",
      menu = "A") %>%
    remove_page(page = "new") -> dashboard
  expect_null(dashboard@pages$new)
})
