#' Accessor methods for slots of an \linkS4class{i2dashboard} object.
#'
#' Getter and Setter methods can be used to directly manipulate properties (slots) of an \linkS4class{i2dashboard} object. See \linkS4class{i2dashboard} for details.
#'
#' @param dashboard A \linkS4class{i2dashboard}.
#' @param value The value of the desired property. See \linkS4class{i2dashboard} for details.
#'
#' @examples
#' dashboard <- i2dashboard()
#'
#' title(dashboard) <- "New dashboard title"
#' author(dashboard) <- "John Doe"
#' theme(dashboard) <- "cosmo"
#' datadir(dashboard) <- "path/to/workdir/"
#' interactivity(dashboard) <- TRUE
#' share(dashboard) <- "menu"
#' embed_source(dashboard) <- TRUE
#' source(dashboard) <- "www.url_to_souce_code.net"
#'
#' @rdname i2dashboard-methods
setMethod("interactivity", "i2dashboard", function(dashboard) dashboard@interactive)

#' @rdname i2dashboard-methods
setMethod("interactivity<-", "i2dashboard", function(dashboard, value) {
  dashboard@interactive <- value
  dashboard
})

#' @rdname i2dashboard-methods
setMethod("title", "i2dashboard", function(dashboard) dashboard@title)

#' @rdname i2dashboard-methods
setMethod("title<-", "i2dashboard", function(dashboard, value) {
  dashboard@title <- value
  dashboard
})

#' @rdname i2dashboard-methods
setMethod("author", "i2dashboard", function(dashboard) dashboard@author)

#' @rdname i2dashboard-methods
setMethod("author<-", "i2dashboard", function(dashboard, value) {
  dashboard@author <- value
  dashboard
})

#' @rdname i2dashboard-methods
setMethod("theme", "i2dashboard", function(dashboard) dashboard@theme)

#' @rdname i2dashboard-methods
setMethod("theme<-", "i2dashboard", function(dashboard, value) {
  dashboard@theme <- value
  dashboard
})

#' @rdname i2dashboard-methods
setMethod("datadir", "i2dashboard", function(dashboard) dashboard@datadir)

#' @rdname i2dashboard-methods
setMethod("datadir<-", "i2dashboard", function(dashboard, value) {
  dashboard@datadir <- value
  dashboard
})

#' @rdname i2dashboard-methods
setMethod("share", "i2dashboard", function(dashboard) dashboard@share)

#' @rdname i2dashboard-methods
setMethod("share<-", "i2dashboard", function(dashboard, value) {
  i <- intersect(tolower(value), c("facebook", "twitter", "google-plus", "linkedin", "pinterest", "menu"))
  if (length(i) > 0) {
    dashboard@share <- i
  }
  dashboard
})

#' @rdname i2dashboard-methods
setMethod("source", "i2dashboard", function(dashboard) dashboard@source)

#' @rdname i2dashboard-methods
setMethod("source<-", "i2dashboard", function(dashboard, value) {
  dashboard@source <- as.character(value)
  dashboard
})

#' @rdname i2dashboard-methods
setMethod("embed_source<-", "i2dashboard", function(dashboard, value) {
  if(value) {
    dashboard@source <- "embed"
  } else {
    dashboard@source <- ""
  }
  dashboard
})

