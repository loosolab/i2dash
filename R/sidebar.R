#' @include components.R
#' @param global Whether or not to add the content to the global sidebar.
#'
#' @rdname i2dashboard-content
setMethod("add_to_sidebar", "i2dashboard", function(dashboard, component, page = "default", global = FALSE, copy = FALSE, ...) {
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

  if(mode == "function") {
    pn <- strsplit(component, "::")[[1]]
    eval_function <- if(length(pn) == 1) {
      get(pn[[1]], envir = asNamespace("i2dash"), mode = "function")
    } else {
      get(pn[[2]], envir = asNamespace(pn[[1]]), mode = "function")
    }
  }

  component <- switch(mode,
                      "function" = do.call(eval_function, args = list(dashboard, ...)),
                      "text" = do.call("render_text", args = list(component, raw = TRUE)),
                      "image" = do.call("render_image", args = list(component, raw = TRUE)))


  if(is.list(component)) {
    warning(sprintf("Component function returned unsupported content for sidebar."))
    return(dashboard)
  }

  if(global) {
    dashboard@sidebar <- paste0(dashboard@sidebar, component)
  } else {
    name <- .create_page_name(page)
    if (!(name %in% names(dashboard@pages))) {
      warning(sprintf("i2dashboard dashboard does not contain a page named '%s'", name))
      return(dashboard)
    }
    dashboard@pages[[name]]$sidebar <- paste0(dashboard@pages[[name]]$sidebar, component)
  }
  return(dashboard)
})
