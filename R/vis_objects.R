#' General method to add an object as components of a page of an i2dashboard object.
#'
#' @param report The \linkS4class{i2dash::i2dashboard} object.
#' @param object The R visualization object to be addedd.
#' @param package The name of the R package that defines the class(object).
#' @param page The name of the page to add the object to.
#' @param title An optional component title.
add_vis_object <- function(report, object, package, page = "default", title = NULL, ...){
  sanitised_page <- i2dash:::.create_page_name(page)
  if (!(sanitised_page %in% names(report@pages))) {
    warning(sprintf("i2dashboard object does not contain a page named '%s'", sanitised_page))
    return(report)
  }

  if(length(report@pages[[sanitised_page]]$components) + 1 > report@pages[[sanitised_page]]$max_components) {
    warning(sprintf("Not enough space left on page '%s'", sanitised_page))
    return(report)
  }

  # Create random component for RDS filename
  component_id <- paste0("obj_", stringi::stri_rand_strings(1, 6))

  # Save plot as RDS
  saveRDS(object, file = file.path(report@datadir, paste0(component_id, ".rds")))

  # Expand template
  timestamp <- Sys.time()
  expanded_component <- knitr::knit_expand(file = system.file("templates", "vis_object.Rmd", package = "i2dash"),
                                           delim = c("<%", "%>"),
                                           title = title,
                                           package = package,
                                           class = class(object),
                                           component_id = component_id,
                                           timestamp = timestamp)

  # Add component to page
  report@pages[[sanitised_page]]$components <- append(report@pages[[sanitised_page]]$components, expanded_component)
  return(report)
}

#
# Methods to add common visualization objects
#
setMethod("add_component",
          signature = signature(object = "i2dashboard", component = "highchart"),
          definition = function(object, component, page = "default", title = NULL, ...) {
            add_vis_object(object, component, "highcharter", page, title, ...) })

setMethod("add_component",
          signature = signature(object = "i2dashboard", component = "plotly"),
          definition = function(object, component, page = "default", title = NULL, ...) {
            add_vis_object(object, component, "plotly", page, title, ...) })

setMethod("add_component",
          signature = signature(object = "i2dashboard", component = "leaflet"),
          definition = function(object, component, page = "default", title = NULL, ...) {
            add_vis_object(object, component, "leaflet", page, title, ...) })

setMethod("add_component",
          signature = signature(object = "i2dashboard", component = "dygraphs"),
          definition = function(object, component, page = "default", title = NULL, ...) {
            add_vis_object(object, component, "dygraphs", page, title, ...) })

setMethod("add_component",
          signature = signature(object = "i2dashboard", component = "rbokeh"),
          definition = function(object, component, page = "default", title = NULL, ...) {
            add_vis_object(object, component, "rbokeh", page, title, ...) })

setMethod("add_component",
          signature = signature(object = "i2dashboard", component = "visNetwork"),
          definition = function(object, component, page = "default", title = NULL, ...) {
            add_vis_object(object, component, "visNetwork", page, title, ...) })

setMethod("add_component",
          signature = signature(object = "i2dashboard", component = "d3heatmap"),
          definition = function(object, component, page = "default", title = NULL, ...) {
            add_vis_object(object, component, "d3heatmap", page, title, ...) })

setMethod("add_component",
          signature = signature(object = "i2dashboard", component = "metricsgraphics"),
          definition = function(object, component, page = "default", title = NULL, ...) {
            add_vis_object(object, component, "metricsgraphics", page, title, ...) })

setMethod("add_component",
          signature = signature(object = "i2dashboard", component = "gg"),
          definition = function(object, component, page = "default", title = NULL, ...) {
            add_vis_object(object, component, "ggplot2", page, title, ...) })

setMethod("add_component",
          signature = signature(object = "i2dashboard", component = "datatables"),
          definition = function(object, component, page = "default", title = NULL, ...) {
            add_vis_object(object, component, "DT", page, title, ...) })

setMethod("add_component",
          signature = signature(object = "i2dashboard", component = "grViz"),
          definition = function(object, component, page = "default", title = NULL, ...) {
            add_vis_object(object, component, "diagrammeR", page, title, ...) })

setMethod("add_component",
          signature = signature(object = "i2dashboard", component = "gt_tbl"),
          definition = function(object, component, page = "default", title = NULL, ...) {
            add_vis_object(object, component, "gt", page, title, ...) })
