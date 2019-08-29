#' Method to add a component to a page of an i2dashboard
#'
#' @param dashboard A \linkS4class{i2dash::i2dashboard}.
#' @param page The name of the page to add the component to.
#' @param component The name of the component.
#' @param ... Additional parameters passed to the components render function.
#'
#' @rdname i2dashboard-methods
#' @export
setMethod("add_component",
          signature = signature(dashboard = "i2dashboard", component = "character"),
          function(dashboard, component, page = "default", ...) {
  name <- .create_page_name(page)
  if (!(name %in% names(dashboard@pages))) {
    warning(sprintf("i2dashboard dashboard does not contain a page named '%s'", name))
    return(dashboard)
  }

  if(length(dashboard@pages[[name]]$components) + 1 > dashboard@pages[[name]]$max_components) {
    warning(sprintf("Not enough space left on page '%s'", name))
    return(dashboard)
  }

  pn <- strsplit(component, "::")[[1]]
  eval_function <- if(length(pn) == 1) {
    get(pn[[1]], envir = asNamespace("i2dash"), mode = "function")
  } else {
    get(pn[[2]], envir = asNamespace(pn[[1]]), mode = "function")
  }

  component <- do.call(eval_function, args = list(dashboard, ...))

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
embed_var = function(x, ...) {
  f = tempfile(fileext = '.csv')
  write.csv(x, f)
  xfun::embed_file(f, text = 'Download full data as .csv', ...)
}
