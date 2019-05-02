setGeneric("add_component", function(object, ...) standardGeneric("add_component"))

#' Method to add a component to a page of an i2dashboard object
#'
#' @param object A \linkS4class{i2dash::i2dashboard} object.
#' @param page The name of the page to add the component to.
#' @param component The name of the component.
#' @param ... Additional parameters passed to the components render function.
#'
#' @rdname idashboard-class
#' @export
setMethod("add_component", "i2dashboard", function(object, page = "default", component, ...) {
  pn <- strsplit(component, "::")[[1]]
  eval_function <- if(length(pn) == 1) {
    get(paste0("render_", pn[[1]]), envir = asNamespace("i2dash"), mode = "function")
  } else {
    get(paste0("render_", pn[[2]]), envir = asNamespace(pn[[1]]), mode = "function")
  }

  component <- do.call(eval_function, args = list(object, ...))

  name <- .create_page_name(page)
  if (name %in% names(object@pages)){
    object@pages[[name]]$components <- append(object@pages[[name]]$components, component)
  } else {
    warning(sprintf("i2dashboard object does not contain Pagename %s", name))
  }
  return(object)
})
