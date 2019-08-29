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
