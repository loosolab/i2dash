#' @include components.R
#' @param global Whether or not to add the content to the global sidebar.
#' @rdname i2dashboard-content
setMethod("add_to_sidebar",
          signature = signature(dashboard = "i2dashboard", component = "character"),
          function(dashboard, component, page = "default", global = FALSE, copy = FALSE, ...) {
            # Logic to guess intended usage
            mode <- NULL
            if(stringr::str_detect(tolower(component), "\\.[md|txt]+$")) {
              mode <- "text"
            }
            if(stringr::str_detect(tolower(component), "\\.[png|jpg|jpeg|gif]+$")) {
              if(copy) {
                location <- file.path(dashboard@datadir, basename(component))
                file.copy(component, location)
                component <- location
              }
              mode <- "image"
            }

            component <- switch(mode,
                                "text" = render_text(component, ...),
                                "image" = render_image(component, in_component=FALSE, ...))

            if(global) {
              dashboard@sidebar <- paste0(dashboard@sidebar, component)
            } else {
              # validate "page" input
              name <- .create_page_name(page)
              if (!(name %in% names(dashboard@pages))) {
                warning(sprintf("i2dashboard dashboard does not contain a page named '%s'", name))
                return(dashboard)
              }
              dashboard@pages[[name]]$sidebar <- paste0(dashboard@pages[[name]]$sidebar, component)
            }
            return(dashboard)
          })

#' @rdname i2dashboard-content
setMethod("add_to_sidebar",
          signature = signature(dashboard = "i2dashboard", component = "function"),
          function(dashboard, component, page = "default", global = FALSE, copy = FALSE, ...) {
            content <- component(dashboard, ...)

            if(is.list(content)) {
              warning(sprintf("Component function returned unsupported content for a sidebar."))
              return(dashboard)
            }

            if(global) {
              dashboard@sidebar <- paste0(dashboard@sidebar, content)
            } else {
              # validate "page" input
              name <- .create_page_name(page)
              if (!(name %in% names(dashboard@pages))) {
                warning(sprintf("i2dashboard dashboard does not contain a page named '%s'", name))
                return(dashboard)
              }
              dashboard@pages[[name]]$sidebar <- paste0(dashboard@pages[[name]]$sidebar, content)
            }
            return(dashboard)
          })
