#' Add content to a sidebar
#'
#' This method allows to add content either to the global sidebar, or to a sidebar of an existing page.
#'
#' @param dashboard A \linkS4class{i2dash::i2dashboard}.
#' @param content The name of a function of a path to a file.
#' @param page The name of the page to which add the sidebar.
#' @param global Whether or not to add the content to the global sidebar.
#' @param copy Whether or not to copy images to \code{dashboard@datadir}.
#' @param ... Additional parameters passed to the components render function.
#'
#' @rdname i2dashboard-methods
#' @export
setMethod("add_to_sidebar", "i2dashboard", function(dashboard, content, page = "default", global = FALSE, copy = FALSE, ...) {
  # Logic to guess intended usage
  mode <- "function"
  if(stringr::str_detect(tolower(content), "\\.[md|txt]+$")) {
    mode <- "text"
  }
  if(stringr::str_detect(tolower(content), "\\.[png|jpg|jpeg|gif]+$")) {
    if(copy) {
      location <- file.path(dashboard@datadir, basename(content))
      file.copy(content, location)
      content <- location
    }
    mode <- "image"
  }

  if(mode == "function") {
    pn <- strsplit(content, "::")[[1]]
    eval_function <- if(length(pn) == 1) {
      get(pn[[1]], envir = asNamespace("i2dash"), mode = "function")
    } else {
      get(pn[[2]], envir = asNamespace(pn[[1]]), mode = "function")
    }
  }

  content <- switch(mode,
                      "function" = do.call(eval_function, args = list(dashboard, ...)),
                      "text" = do.call("render_text", args = list(content, raw = TRUE)),
                      "image" = do.call("render_image", args = list(content, raw = TRUE)))


  if(is.list(content)) {
    warning(sprintf("Component function returned unsupported content for sidebar."))
    return(dashboard)
  }

  if(global) {
    dashboard@sidebar <- paste0(dashboard@sidebar, content)
  } else {
    name <- .create_page_name(page)
    if (!(name %in% names(dashboard@pages))) {
      warning(sprintf("i2dashboard dashboard does not contain a page named '%s'", name))
      return(dashboard)
    }
    dashboard@pages[[name]]$sidebar <- paste0(dashboard@pages[[name]]$sidebar, content)
  }
  return(dashboard)
})
