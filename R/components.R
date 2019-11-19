#' Method to add a component to a page of an i2dashboard.
#'
#' Components can be created by evaluating a function, or by including a text or image file.
#' The function tries to guess the intended usage by applying regular expressions to the argument \code{component} (see below for details).
#'
#' @section Adding a component by evaluating a function:
#' If the argument \code{component} is a valid function name, the function will be called and its return value is used as component content.
#'
#' @section Adding plain text as a component:
#' If the argument \code{component} ends with \code{.md} or \code{.txt}, the function will try to open a file and use its content as the components content.
#'
#' @section Adding images as a component:
#' If the argument \code{component} end matches \code{\\.[png|jpg|jpeg|gif]}, the function will try to include an image as the components content.
#'
#' @param dashboard A \linkS4class{i2dash::i2dashboard}.
#' @param page The name of the page to add the component to.
#' @param component The name of a function of a path to a file.
#' @param copy Whether or not to copy images to \code{dashboard@datadir}.
#' @param ... Additional parameters passed to the components render function.
#'
#' @rdname add-component
#' @export
setMethod("add_component",
          signature = signature(dashboard = "i2dashboard", component = "character"),
          function(dashboard, component, page = "default", copy = FALSE, ...) {
            # Logic to guess intended usage
            mode <- "function"
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

            # Create page
            name <- .create_page_name(page)
            if (!(name %in% names(dashboard@pages))) {
              warning(sprintf("i2dashboard dashboard does not contain a page named '%s'", name))
              return(dashboard)
            }

            if(length(dashboard@pages[[name]]$components) + 1 > dashboard@pages[[name]]$max_components) {
              warning(sprintf("Not enough space left on page '%s'", name))
              return(dashboard)
            }

            if(mode == "function") {
              pn <- strsplit(component, "::")[[1]]
              eval_function <- if(length(pn) == 1) {
                get(pn[[1]], envir = asNamespace("i2dash"), mode = "function")
              } else {
                get(pn[[2]], envir = asNamespace(pn[[1]]), mode = "function")
              }
            }

            component <- switch(mode,
              "function" = eval_function(dashboard, ...),
              "text" = render_text(component, ...),
              "image" = render_image(component, ...))

            if(is.list(component)) {
              assertive.sets::is_subset(c("appendix", "component", "sidebar"), names(component))
              dashboard@pages[[name]]$components <- append(dashboard@pages[[name]]$components, component$component)
              dashboard@pages[[name]]$sidebar <- paste0(dashboard@pages[[name]]$sidebar, component$sidebar)
              # ToDo: Handle appendix
            } else {
              dashboard@pages[[name]]$components <- append(dashboard@pages[[name]]$components, component)
            }
            return(dashboard)
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
  xfun::embed_file(f, text = 'Download full data as .csv', ...)
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
