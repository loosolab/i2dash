#' Sanitize component names
#'
#' This function takes a character string, replaces spaces by underscores and runs make.names.
#'
#' @param x A character string.
#'
#' @return A sanitized string.
.create_page_name <- function(x) {
  x %>% tolower %>% gsub(x = ., pattern = " ", replacement = "_") %>% make.names %>% return
}

setGeneric("add_page", function(object, ...) standardGeneric("add_page"))

#' Method to add a page to an i2dashboard object
#'
#' @param object A \linkS4class{i2dash::i2dashboard} object.
#' @param page The name of the page to be added.
#' @param title The title of the page to be added.
#' @param layout The page layout (see below).
#' @param menu The name of the menu, under which the page should appear.
#'
#' @rdname idashboard-class
#' @export
setMethod("add_page", "i2dashboard", function(object, page, title, layout = "default", menu = NULL, sidebar = NULL, ...) {
  name <- .create_page_name(page)

  max_components <- switch(layout,
                           "default" = Inf,
                           "storyboard" = Inf,
                           "focal_left" = 3,
                           "2x2_grid" = 4
  )

  if(name %in% names(object@pages)) {
    warning(paste("The page", name, "already exists and will be overidden."))
    if(base::interactive()) {
      overwrite_page <- menu(c("Yes, overwrite page", "Cancel"), title = "Do you want to overwrite this page?")
      skip <- switch(overwrite_page,
                     "1" = FALSE,
                     "2" = TRUE)
      if (skip) stop("Aborted.")
    }
  }

  object@pages[[name]] <- list(title = title, layout = layout, menu = menu, components = list(), max_components = max_components, sidebar = sidebar)
  return(object)
})

setGeneric("remove_page", function(object, ...) standardGeneric("remove_page"))

#' Method to remove a page to an i2dashboard object
#'
#' @param object A \linkS4class{i2dash::i2dashboard} object.
#' @param page The name of the page to be removed.
#'
#' @rdname idashboard-class
#' @export
setMethod("remove_page", "i2dashboard", function(object, page) {
  name <- .create_page_name(page)
  object@pages[[name]] <- NULL
  return(object)
})