#' Get/set the interactivity of the i2dashboard object.
#'
#' @param .Object A \linkS4class{i2dash::i2dashboard} report.
#' @param value The value of the desired property.
#'
#' @name i2dashboard-class
#' @rdname i2dashboard-class
setMethod("interactivity", "i2dashboard", function(object) object@interactive)

#' @name i2dashboard-class
#' @rdname i2dashboard-class
setMethod("interactivity<-", "i2dashboard", function(object, value) {
  object@interactive <- value
  object
})
