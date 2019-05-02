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
setMethod("add_page", "i2dashboard", function(object, page, title, layout = "storyboard", menu = NULL, ...) {
  name <- .create_page_name(page)

  if (base::interactive()){
    if (name %in% names(object@pages)){
      print("A page with this 'page'-argument already exists.")
      answer1 <- menu(c("Yes", "No"), title="Do you want to overwrite this page? If 'No' you can input another 'page'-argument or cancel.")
      switch (answer1,
              "1"={
                print("The page was overwritten.")
                object@pages[[name]] <- list(title = title, layout = layout, menu = menu, components = list())
              },
              "2"={
                answer2 <- menu(c("Yes", "Cancel"), title="Do you want to provide another 'page'-argument?")
                switch (answer2,
                        "1"={
                          new_name <- readline("Please input a new 'page'-argument: ")
                          if (is.character(new_name)){
                            new_name <- .create_page_name(new_name)
                            print("Page with new 'page'-argument created.")
                            object@pages[[new_name]] <- list(title = title, layout = layout, menu = menu, components = list())
                          }
                        },
                        "2"={
                          print("Function 'add_page' canceled")
                        }
                )
              }
      )
    } else {
      object@pages[[name]] <- list(title = title, layout = layout, menu = menu, components = list())
    }
  } else {
    warning("A page with this 'page'-argument already exists. The existing page will be overwritten.")
    object@pages[[name]] <- list(title = title, layout = layout, menu = menu, components = list())
  }


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