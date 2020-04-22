#' Accessor methods for slots of an \linkS4class{i2dashboard} object.
#'
#' Getter and Setter methods can be used to directly manipulate properties (slots) of an \linkS4class{i2dashboard} object. See \linkS4class{i2dashboard} for details.
#'
#' @param dashboard A \linkS4class{i2dashboard}.
#' @param value The value of the desired property. See \linkS4class{i2dashboard} for details.
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
setMethod("social_links", "i2dashboard", function(dashboard) dashboard@social_links)

#' @rdname i2dashboard-methods
setMethod("social_links<-", "i2dashboard", function(dashboard, value) {
  i <- intersect(tolower(value), c("facebook", "twitter", "google-plus", "linkedin", "pinterest", "menu"))
  if (length(i) > 0) {
    dashboard@social <- i
  }
  dashboard
})

#' @rdname i2dashboard-methods
setMethod("source", "i2dashboard", function(dashboard) dashboard@source)

#' @rdname i2dashboard-methods
setMethod("source<-", "i2dashboard", function(dashboard, value) {
  dashboard@source <- tolower(as.character(value))
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

