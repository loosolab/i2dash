#' Method to add highcharts as components of a page of an i2dashboard object.
#'
#' @param object The \linkS4class{i2dash::i2dashboard} object.
#' @param component The highcharts object.
#' @param page The name of the page to add the plot to.
#' @param title An optional component title.
#'
#' @export
setMethod("add_component",
          signature = signature(object = "i2dashboard", component = "highchart"),
          definition = function(object, component, page = "default", title = NULL, ...) {
            sanitised_page <- i2dash:::.create_page_name(page)
            if (!(sanitised_page %in% names(object@pages))) {
              warning(sprintf("i2dashboard object does not contain a page named '%s'", sanitised_page))
              return(object)
            }

            if(length(object@pages[[sanitised_page]]$components) + 1 > object@pages[[sanitised_page]]$max_components) {
              warning(sprintf("Not enough space left on page '%s'", sanitised_page))
              return(object)
            }

            # Create random component for RDS filename
            component_id <- paste0("hc_", stringi::stri_rand_strings(1, 6))

            # Save plot as RDS
            saveRDS(component, file = file.path(object@workdir, "envs", paste0(component_id, ".rds")))

            # Expand template
            timestamp <- Sys.time()
            expanded_component <- knitr::knit_expand(file = system.file("templates", "highcharts.Rmd", package = "i2dash"),
                                                     delim = c("<%", "%>"),
                                                     title = title,
                                                     component_id = component_id,
                                                     timestamp = timestamp)

            # Add component to page
            object@pages[[sanitised_page]]$components <- append(object@pages[[sanitised_page]]$components, expanded_component)
            return(object)
          })
