#' Methods to add and remove pages of an \linkS4class{i2dashboard} object.
#'
#' '\code{add_page()}' creates a page and adds it to the \linkS4class{i2dashboard} object.
#' '\code{remove_page()}' removes a page from the \linkS4class{i2dashboard} object.
#'
#' @param dashboard A \linkS4class{i2dashboard}.
#' @param page The name of the page to be added or removed.
#' @param title The title of the page to be added.
#' @param layout The page layout (see below).
#' @param menu The name of the menu, under which the page should appear.
#' @param sidebar A Markdown string. Preferably, use the function '\code{add_to_sidebar()}'.
#' @param ... Additional arguments.
#'
#' @return Returns the \linkS4class{i2dashboard} object with a modified 'pages' slot.
#' @rdname i2dashboard-pages
#' @examples
#' library(magrittr)
#' i2dashboard() %>%
#'      add_page(page="page1",
#'          title="Page Title",
#'          layout = "2x2_grid",
#'          menu="Menu A") -> dashboard
#' dashboard %<>% remove_page(page="page1")
setMethod("add_page", "i2dashboard", function(dashboard, page, title, layout = "default", menu = NULL, sidebar = NULL, ...) {
  name <- .create_page_name(page)

  max_components <- switch(layout,
                           "default" = Inf,
                           "storyboard" = Inf,
                           "focal_left" = 3,
                           "2x2_grid" = 4
  )

  if(name %in% names(dashboard@pages)) {
    warning(paste("The page", name, "already exists and will be overwritten."))
    if(base::interactive()) {
      overwrite_page <- menu(c("Yes, overwrite page", "Cancel"), title = "Do you want to overwrite this page?")
      skip <- switch(overwrite_page,
                     "1" = FALSE,
                     "2" = TRUE)
      if (skip) stop("Aborted.")
    }
  }

  dashboard@pages[[name]] <- list(title = title, layout = layout, menu = menu, components = list(), max_components = max_components, sidebar = sidebar)
  return(dashboard)
})

#' @rdname i2dashboard-pages
setMethod("remove_page", "i2dashboard", function(dashboard, page) {
  name <- .create_page_name(page)
  dashboard@pages[[name]] <- NULL
  return(dashboard)
})

#' Sanitize component names
#'
#' This function takes a character string, replaces spaces by underscores and runs make.names.
#'
#' @param x A character string.
#'
#' @return A sanitized string.
.create_page_name <- function(x) {
  . = NULL # workaround for R CMD check note: no visible binding for global variable '.'
  x %>% tolower %>% gsub(x = ., pattern = " ", replacement = "_") %>% make.names %>% return
}