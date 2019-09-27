#' Add a color mapping to the dashboards colormaps.
#'
#' @param dashboard A \linkS4class{i2dash::i2dashboard}.
#' @param map A character vector containing colors and possible the levels they map to (as names).
#' @param name A name for the color mapping.
#'
#' @rdname i2dashboard-methods
#' @export
setMethod("add_colormap", "i2dashboard", function(dashboard, map, name) {
  dashboard@colormaps[[make.names(name)]] <- map
  return(dashboard)
})
