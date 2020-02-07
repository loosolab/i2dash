#' Method to add a component to a page of an i2dashboard.
#'
#' Components can be created by evaluating a function, or by including an object, a text or image file.
#'
#' @section Adding content by evaluating a function:
#' If the argument \code{component} is a function, the function will be called and its return value is used as content.
#'
#' @section Adding plain text as content:
#' If the argument \code{component} is a character and ends with \code{.md} or \code{.txt}, the function will try to open a file and use its content.
#'
#' @section Adding images as content:
#' If the argument \code{component} is a character and its end matches \code{\\.[png|jpg|jpeg|gif]}, the function will try to include an image as the content.
#'
#' @section Adding a R object as content:
#' If the argument \code{component} is a supported R object (e.g. a htmlwidget), the function will include its representation as content.
#'
#' @param dashboard A \linkS4class{i2dash::i2dashboard}.
#' @param page The name of the page to add the component to.
#' @param component A R object, function, or character.
#' @param copy Whether or not to copy images to \code{dashboard@datadir}.
#' @param ... Additional parameters passed to the components render function.
#'
#' @rdname add-component
#' @export
setMethod("add_component",
          signature = signature(dashboard = "i2dashboard", component = "character"),
          function(dashboard, component, page = "default", copy = FALSE, ...) {
            # Logic to guess intended usage
            mode <- NULL
            if(stringr::str_detect(tolower(component), "\\.[md|txt]+$")) {
              mode <- "text"
            }
            if(stringr::str_detect(tolower(component), "\\.[png|jpg|jpeg|gif]+$")) {
              if(copy) {
                location <- file.path(dashboard@datadir, basename(component))
                file.copy(component, location)
                component <- location
              }
              mode <- "image"
            }

            # validate "page" input
            name <- .create_page_name(page)
            if (!(name %in% names(dashboard@pages))) {
              warning(sprintf("i2dashboard dashboard does not contain a page named '%s'", name))
              return(dashboard)
            }

            if(length(dashboard@pages[[name]]$components) + 1 > dashboard@pages[[name]]$max_components) {
              warning(sprintf("Not enough space left on page '%s'", name))
              return(dashboard)
            }

            component <- switch(mode,
              "text" = render_text(component, ...),
              "image" = render_image(component, ...))

            return(.add_component(dashboard, name, component))
          })

setMethod("add_component", signature(dashboard = "i2dashboard", component = "function"),
          function(dashboard, component, page = "default", title = NULL, ...) {
            # validate "page" input
            name <- .create_page_name(page)
            if (!(name %in% names(dashboard@pages))) {
              warning(sprintf("i2dashboard dashboard does not contain a page named '%s'", name))
              return(dashboard)
            }

            if(length(dashboard@pages[[name]]$components) + 1 > dashboard@pages[[name]]$max_components) {
              warning(sprintf("Not enough space left on page '%s'", name))
              return(dashboard)
            }

            content <- component(dashboard, ...)
            return(.add_component(dashboard, name, content))
          })

#' Method to download embed files into an Rmd-file
#'
#' @param x Data, which will be written to the embedded file.
#' @param ... Additional parameters.
#'
#' @export
embed_var <- function(x, ...) {
  f = tempfile(fileext = '.csv')
  write.csv(x, f)
  xfun::embed_file(f, text = 'Download data', ...)
}

#' Method to embed content from a text file in a component/sidebar
#'
#' @param file The file containing the text content.
#' @param title The components title.
#' @param raw Whether or not to emit raw file content
#'
#' @return A character string containing the evaluated component
render_text <- function(file, title = NULL, raw = FALSE) {
  readLines(con = file) %>%
    paste(collapse = "\n") -> content

  if(raw) return(content)
  knitr::knit_expand(file = system.file("templates", "component.Rmd", package = "i2dash"),
                     delim = c("<%", "%>"),
                     content = content,
                     title = title)
}

#' Method to embed an image file in a component
#'
#' @param image The path to the image file.
#' @param image_alt_text The alt text of the image.
#' @param title The components title.
#' @param raw Whether or not to emit solely the markdown image code.
#'
#' @return A character string containing the evaluated component
render_image <- function(image, image_alt_text = NULL, title = NULL, raw = FALSE) {
  if(is.null(image_alt_text)) {
    image_alt_text <- image
  }
  content <- glue::glue("![{image_alt_text}]({image})\n", image_alt_text = image_alt_text, image = image)

  if(raw) return(content)
  knitr::knit_expand(file = system.file("templates", "component.Rmd", package = "i2dash"),
                     delim = c("<%", "%>"),
                     content = content,
                     title = title)
}

#' Helper function to add components to the dashboard
#'
#' @param dashboard A \linkS4class{i2dash::i2dashboard}.
#' @param page The name of the page to add the component to.
#' @param component A string or list.
#'
#' @return The dashboard with added component.
.add_component <- function(dashboard, page, component) {
  if(is.list(component)) {
    assertive.sets::is_subset(c("appendix", "component", "sidebar"), names(component))
    dashboard@pages[[page]]$components <- append(dashboard@pages[[page]]$components, component$component)
    dashboard@pages[[page]]$sidebar <- paste0(dashboard@pages[[page]]$sidebar, component$sidebar)
    # ToDo: Handle appendix
  } else {
    dashboard@pages[[page]]$components <- append(dashboard@pages[[page]]$components, component)
  }
  return(dashboard)
}
