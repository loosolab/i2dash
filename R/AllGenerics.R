#' @include i2dashboard.R
#' @export
#' @rdname assemble
setGeneric("assemble", function(dashboard, ...) standardGeneric("assemble"))

#' @export
#' @rdname i2dashboard-pages
setGeneric("add_page", function(dashboard, ...) standardGeneric("add_page"))

#' @export
#' @rdname i2dashboard-pages
setGeneric("remove_page", function(dashboard, page) standardGeneric("remove_page"))

#' @export
#' @rdname i2dashboard-content
setGeneric("add_component", function(dashboard, component, ...) standardGeneric("add_component"))

#' @export
#' @rdname i2dashboard-content
setGeneric("add_to_sidebar", function(dashboard, component, ...) standardGeneric("add_to_sidebar"))

#' @export
#' @rdname i2dashboard-content
setGeneric("add_colormap", function(dashboard, ...) standardGeneric("add_colormap"))

#' @export
#' @rdname i2dashboard-methods
setGeneric("interactivity", function(dashboard) standardGeneric("interactivity"))

#' @export
#' @rdname i2dashboard-methods
setGeneric("interactivity<-", function(dashboard, value) standardGeneric("interactivity<-"))

#' @export
#' @rdname i2dashboard-methods
setGeneric("title", function(dashboard) standardGeneric("title"))

#' @export
#' @rdname i2dashboard-methods
setGeneric("title<-", function(dashboard, value) standardGeneric("title<-"))

#' @export
#' @rdname i2dashboard-methods
setGeneric("author", function(dashboard) standardGeneric("author"))

#' @export
#' @rdname i2dashboard-methods
setGeneric("author<-", function(dashboard, value) standardGeneric("author<-"))

#' @export
#' @rdname i2dashboard-methods
setGeneric("theme", function(dashboard) standardGeneric("theme"))

#' @export
#' @rdname i2dashboard-methods
setGeneric("theme<-", function(dashboard, value) standardGeneric("theme<-"))

#' @export
#' @rdname i2dashboard-methods
setGeneric("datadir", function(dashboard) standardGeneric("datadir"))

#' @export
#' @rdname i2dashboard-methods
setGeneric("datadir<-", function(dashboard, value) standardGeneric("datadir<-"))

#' @export
#' @rdname i2dashboard-methods
setGeneric("social_links", function(dashboard) standardGeneric("social_links"))

#' @export
#' @rdname i2dashboard-methods
setGeneric("social_links<-", function(dashboard, value) standardGeneric("social_links<-"))

#' @export
#' @rdname i2dashboard-methods
setGeneric("embed_source<-", function(dashboard, value) standardGeneric("embed_source<-"))

#' @export
#' @rdname i2dashboard-methods
setGeneric("source", function(dashboard) standardGeneric("source"))

#' @export
#' @rdname i2dashboard-methods
setGeneric("source<-", function(dashboard, value) standardGeneric("source<-"))

#' @export
#' @rdname i2dashboard-content
setGeneric("add_link", function(dashboard, ...) standardGeneric("add_link"))