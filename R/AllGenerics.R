#' @include i2dashboard.R
#' @export
setGeneric("assemble", function(dashboard, ...) standardGeneric("assemble"))

#' @export
setGeneric("add_page", function(dashboard, ...) standardGeneric("add_page"))

#' @export
setGeneric("remove_page", function(dashboard, ...) standardGeneric("remove_page"))

#' @export
setGeneric("add_component", function(dashboard, component, ...) standardGeneric("add_component"))

#' @export
setGeneric("add_to_sidebar", function(dashboard, component, ...) standardGeneric("add_to_sidebar"))

#' @export
setGeneric("interactivity", function(dashboard) standardGeneric("interactivity"))
setGeneric("interactivity<-", function(dashboard, value) standardGeneric("interactivity<-"))
