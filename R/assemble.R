setGeneric("assemble", function(object, ...) standardGeneric("assemble"))

#' Method to assemble a dashboard and write it to a file
#'
#' @param object A \linkS4class{i2dash::i2dashboard} object.
#' @param output_file The output filename (recommend that the suffix should be '.Rmd'). This file will be saved in the working directory.
#' @param pages A string or vector with the names of pages, which should be assemble to a report.
#' @param ... Additional parameters passed to the components render function.
#'
#' @rdname idashboard-class
#' @export
setMethod("assemble", "i2dashboard", function(object, output_file, pages, ...) {
  yaml_list <- list(title = object@title,
                    author = object@author,
                    output = list("flexdashboard::flex_dashboard" = list(theme = object@theme)))
  if (object@interactive){
    yaml_list[["runtime"]] <- "shiny"
  }
  yaml_part <- yaml::as.yaml(yaml_list)
  header_string <- paste0("---\n", yaml_part, "---\n")

  tmp_document <- tempfile()
  # create variable final_document
  final_document <- file.path(object@workdir, output_file)
  # write header to tempfile
  cat(header_string, file = tmp_document, append = FALSE, sep='')
  # write page to tempfile
  for (pagename in pages){
    name <- .create_page_name(pagename)
    if (name %in% names(object@pages)){
      # Create a content string from all components
      components <- paste0(object@pages[[name]]$components, sep = "")
      #components <- "here are the components"
      # Create variable "title" & "menu" & "layout" for readability
      title <- object@pages[[name]]$title
      menu <- object@pages[[name]]$menu
      layout <- object@pages[[name]]$layout
      # Check menu argument
      if (is.null(menu)){
        menu <- ""
      }
      # Check layout argument
      if (any(layout == "storyboard")){
        layout <- ".storyboard"
      } else {
        if(!is.null(layout)) warning("layout argument is not known.")
        layout <- ""
      }
      layout_with_menu <- sprintf('{%s data-navmenu="%s"}', layout, menu)
      timestamp <- Sys.time()
      full_content <- knitr::knit_expand(file = system.file("templates", "page_template.Rmd", package = "i2dash"), title = title, layout_with_menu = layout_with_menu, components = components, date = timestamp)
      cat(full_content, file = tmp_document, append = TRUE, sep='')

      #cat(object@pages[[name]]$header, file = tmp_document, append = TRUE, sep='')
      #cat(object@pages[[name]]$components, file = tmp_document, append = TRUE, sep='')
    } else {
      warning(sprintf("i2dashboard object does not contain a page named '%s'", pagename))
    }
  }
  # copy tempfile to final_document
  file.copy(from = tmp_document, to = final_document, overwrite = TRUE)
})