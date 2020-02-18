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
setGeneric("add_colormap", function(dashboard, ...) standardGeneric("add_colormap"))

#' @export
setGeneric("interactivity", function(dashboard) standardGeneric("interactivity"))

#' @export
setGeneric("interactivity<-", function(dashboard, value) standardGeneric("interactivity<-"))

#' @export
setGeneric("title", function(dashboard) standardGeneric("title"))

#' @export
setGeneric("title<-", function(dashboard, value) standardGeneric("title<-"))

#' @export
setGeneric("author", function(dashboard) standardGeneric("author"))

#' @export
setGeneric("author<-", function(dashboard, value) standardGeneric("author<-"))

#' @export
setGeneric("theme", function(dashboard) standardGeneric("theme"))

#' @export
setGeneric("theme<-", function(dashboard, value) standardGeneric("theme<-"))

#' @export
setGeneric("datadir", function(dashboard) standardGeneric("datadir"))

#' @export
setGeneric("datadir<-", function(dashboard, value) standardGeneric("datadir<-"))