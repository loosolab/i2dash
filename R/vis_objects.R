#' @include components.R
NULL

#' General method to add an object as component to a page of an \linkS4class{i2dashboard} object.
#'
#' @param dashboard The \linkS4class{i2dashboard}.
#' @param object The R visualization object to be added.
#' @param package The name of the R package that defines the class(object).
#' @param page The name of the page to add the object to.
#' @param title An optional component title.
#' @return Returns the modified \linkS4class{i2dashboard} object, if the component was added, otherwise an unmodified \linkS4class{i2dashboard} object.
add_vis_object <- function(dashboard, object, package, page = "default", title = NULL){
  sanitised_page <- .create_page_name(page)
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
                                           class = methods::is(object),
                                           component_id = component_id,
                                           timestamp = timestamp)

  # Add component to page
  dashboard@pages[[sanitised_page]]$components <- append(dashboard@pages[[sanitised_page]]$components, expanded_component)
  return(dashboard)
}

