#' General method to add an object as component to a page of an i2dashboard.
#'
#' @param dashboard The \linkS4class{i2dash::i2dashboard}.
#' @param object The R visualization object to be addedd.
#' @param package The name of the R package that defines the class(object).
#' @param page The name of the page to add the object to.
#' @param title An optional component title.
#'
#' @export
add_vis_object <- function(dashboard, object, package, page = "default", title = NULL, ...){
  sanitised_page <- i2dash:::.create_page_name(page)
  if (!(sanitised_page %in% names(dashboard@pages))) {
    warning(sprintf("i2dashboard does not contain a page named '%s'", sanitised_page))
    return(dashboard)
  }

  if(length(dashboard@pages[[sanitised_page]]$components) + 1 > dashboard@pages[[sanitised_page]]$max_components) {
    warning(sprintf("Not enough space left on page '%s'", sanitised_page))
    return(dashboard)
  }

  # Create random component for RDS filename
  component_id <- paste0("obj_", stringi::stri_rand_strings(1, 6))

  # Save plot as RDS
  saveRDS(object, file = file.path(dashboard@datadir, paste0(component_id, ".rds")))

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
  dashboard@pages[[sanitised_page]]$components <- append(dashboard@pages[[sanitised_page]]$components, expanded_component)
  return(dashboard)
}

#
# Methods to add common visualization objects
#
setMethod("add_component",
          signature = signature(dashboard = "i2dashboard", component = "highchart"),
          definition = function(dashboard, component, page = "default", title = NULL, ...) {
            add_vis_object(dashboard, component, "highcharter", page, title, ...) })

setMethod("add_component",
          signature = signature(dashboard = "i2dashboard", component = "plotly"),
          definition = function(dashboard, component, page = "default", title = NULL, ...) {
            add_vis_object(dashboard, component, "plotly", page, title, ...) })

setMethod("add_component",
          signature = signature(dashboard = "i2dashboard", component = "leaflet"),
          definition = function(dashboard, component, page = "default", title = NULL, ...) {
            add_vis_object(dashboard, component, "leaflet", page, title, ...) })

setMethod("add_component",
          signature = signature(dashboard = "i2dashboard", component = "dygraphs"),
          definition = function(dashboard, component, page = "default", title = NULL, ...) {
            add_vis_object(dashboard, component, "dygraphs", page, title, ...) })

setMethod("add_component",
          signature = signature(dashboard = "i2dashboard", component = "rbokeh"),
          definition = function(dashboard, component, page = "default", title = NULL, ...) {
            add_vis_object(dashboard, component, "rbokeh", page, title, ...) })

setMethod("add_component",
          signature = signature(dashboard = "i2dashboard", component = "visNetwork"),
          definition = function(dashboard, component, page = "default", title = NULL, ...) {
            add_vis_object(dashboard, component, "visNetwork", page, title, ...) })

setMethod("add_component",
          signature = signature(dashboard = "i2dashboard", component = "d3heatmap"),
          definition = function(dashboard, component, page = "default", title = NULL, ...) {
            add_vis_object(dashboard, component, "d3heatmap", page, title, ...) })

setMethod("add_component",
          signature = signature(dashboard = "i2dashboard", component = "metricsgraphics"),
          definition = function(dashboard, component, page = "default", title = NULL, ...) {
            add_vis_object(dashboard, component, "metricsgraphics", page, title, ...) })

setMethod("add_component",
          signature = signature(dashboard = "i2dashboard", component = "gg"),
          definition = function(dashboard, component, page = "default", title = NULL, ...) {
            add_vis_object(dashboard, component, "ggplot2", page, title, ...) })

setMethod("add_component",
          signature = signature(dashboard = "i2dashboard", component = "datatables"),
          definition = function(dashboard, component, page = "default", title = NULL, ...) {
            add_vis_object(dashboard, component, "DT", page, title, ...) })

setMethod("add_component",
          signature = signature(dashboard = "i2dashboard", component = "grViz"),
          definition = function(dashboard, component, page = "default", title = NULL, ...) {
            add_vis_object(dashboard, component, "DiagrammeR", page, title, ...) })

setMethod("add_component",
          signature = signature(dashboard = "i2dashboard", component = "gt_tbl"),
          definition = function(dashboard, component, page = "default", title = NULL, ...) {
            add_vis_object(dashboard, component, "gt", page, title, ...) })
