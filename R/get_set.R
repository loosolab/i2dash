#' Get/set the interactivity of the i2dashboard.
#'
#' @param dashboard A \linkS4class{i2dash::i2dashboard}.
#' @param value The value of the desired property.
#'
#' @name i2dashboard-class
#' @rdname i2dashboard-class
setMethod("interactivity", "i2dashboard", function(dashboard) dashboard@interactive)

#' @name i2dashboard-class
#' @rdname i2dashboard-class
setMethod("interactivity<-", "i2dashboard", function(dashboard, value) {
  dashboard@interactive <- value
  dashboard
})

#' Get/set the title of the i2dashboard.
#'
#' @param dashboard A \linkS4class{i2dash::i2dashboard}.
#' @param value The value of the desired property.
#'
#' @name i2dashboard-class
#' @rdname i2dashboard-class
setMethod("title", "i2dashboard", function(dashboard) dashboard@title)

#' @name i2dashboard-class
#' @rdname i2dashboard-class
setMethod("title<-", "i2dashboard", function(dashboard, value) {
  dashboard@title <- value
  dashboard
})

#' Get/set the author of the i2dashboard.
#'
#' @param dashboard A \linkS4class{i2dash::i2dashboard}.
#' @param value The value of the desired property.
#'
#' @name i2dashboard-class
#' @rdname i2dashboard-class
setMethod("author", "i2dashboard", function(dashboard) dashboard@author)

#' @name i2dashboard-class
#' @rdname i2dashboard-class
setMethod("author<-", "i2dashboard", function(dashboard, value) {
  dashboard@author <- value
  dashboard
})

#' Get/set the theme of the i2dashboard.
#'
#' @param dashboard A \linkS4class{i2dash::i2dashboard}.
#' @param value The value of the desired property.
#'
#' @name i2dashboard-class
#' @rdname i2dashboard-class
setMethod("theme", "i2dashboard", function(dashboard) dashboard@theme)

#' @name i2dashboard-class
#' @rdname i2dashboard-class
setMethod("theme<-", "i2dashboard", function(dashboard, value) {
  dashboard@theme <- value
  dashboard
})

#' Get/set the datadir of the i2dashboard.
#'
#' @param dashboard A \linkS4class{i2dash::i2dashboard}.
#' @param value The value of the desired property.
#'
#' @name i2dashboard-class
#' @rdname i2dashboard-class
setMethod("datadir", "i2dashboard", function(dashboard) dashboard@datadir)

#' @name i2dashboard-class
#' @rdname i2dashboard-class
setMethod("datadir<-", "i2dashboard", function(dashboard, value) {
  dashboard@datadir <- value
  dashboard
})
