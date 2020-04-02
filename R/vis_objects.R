#' General method to add an object as component to a page of an i2dashboard.
#'
#' @param dashboard The \linkS4class{i2dash::i2dashboard}.
#' @param object The R visualization object to be addedd.
#' @param package The name of the R package that defines the class(object).
#' @param page The name of the page to add the object to.
#' @param title An optional component title.
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
                                           class = is(object),
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
          signature = signature(dashboard = "i2dashboard", component = "gg"),
          definition = function(dashboard, component, page = "default", title = NULL, ...) {
            add_vis_object(dashboard, component, "ggplot2", page, title, ...) })

setMethod("add_component",
          signature = signature(dashboard = "i2dashboard", component = "gt_tbl"),
          definition = function(dashboard, component, page = "default", title = NULL, ...) {
            add_vis_object(dashboard, component,"gt", page, title, ...) })

setMethod("add_component",
          signature = signature(dashboard = "i2dashboard", component = "knitr_kable"),
          definition = function(dashboard, component, page = "default", title = NULL, ...) {
            add_vis_object(dashboard, component, "kableExtra", page, title, ...) })

setMethod("add_component",
          signature = signature(dashboard = "i2dashboard", component = "Heatmap"),
          definition = function(dashboard, component, page = "default", title = NULL, ...) {
            add_vis_object(dashboard, component, "ComplexHeatmap", page, title, ...) })

setMethod("add_component",
          signature = signature(dashboard = "i2dashboard", component = "ANY"),
          definition = function(dashboard, component, page = "default", title = NULL, ...) {

            # HTMLWIDGETS
            if(inherits(component, "htmlwidget")) {
              package <- packageSlot(component)

              if(is.null(package)) {
                warning("No component added, since the package name of the HTML widget could not be determined.")
                return(dashboard)
              }

              return(add_vis_object(dashboard, component, package, page, title, ...))
            }

            # OTHER
            warning("The component did not inherit from any of the currently supported classes ('htmlwidget').")
            return(dashboard)
            })
