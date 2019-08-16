#' Method to assemble a dashboard to a Rmd file.
#'
#' @param object A \linkS4class{i2dash::i2dashboard} object.
#' @param pages A string or vector with the names of pages, which should be assemble to a report.
#' @param file The output filename (recommend that the suffix should be '.Rmd'). This file will be saved in the working directory.
#' @param render A logical indicating whether the assembled report should immediately be rendered with \code{rmarkdown::render}.
#' @param ... Additional arguments passed on to \code{rmarkdown::render}.
#'
#' @rdname i2dashboard-methods
#' @export
setMethod("assemble", "i2dashboard", function(object, pages = names(object@pages), file = object@file, render = FALSE, ...) {
  tmp_document <- tempfile()

  # Add YAML header
  knitr::knit_expand(file = system.file("templates", "yaml_header.Rmd", package = "i2dash"),
                     delim = c("<%", "%>"),
                     title = object@title,
                     author = object@author,
                     theme = object@theme,
                     interactive = object@interactive) %>%
    cat(file = tmp_document, append = FALSE, sep = "\n")

  # Add i2dash global setup
  knitr::knit_expand(file = system.file("templates", "i2dash-global-setup.Rmd", package = "i2dash"), delim = c("<%", "%>"), datadir = object@datadir) %>%
    cat(file = tmp_document, append = TRUE, sep = "\n")

  # write page to tempfile
  for (pagename in pages){
    name <- .create_page_name(pagename)
    if (name %in% names(object@pages)){
      # Create a content string from all components
      components <- paste0(object@pages[[name]]$components, sep = "")
      # Create variable "title" & "menu" & "layout" for readability
      title <- object@pages[[name]]$title
      menu <- object@pages[[name]]$menu
      layout <- object@pages[[name]]$layout
      sidebar <- object@pages[[name]]$sidebar

      .render_page(title = title, components = components, layout = layout,  menu = menu, sidebar = sidebar) %>%
        cat(file = tmp_document, append = TRUE, sep='')
    } else {
      warning(sprintf("i2dashboard object does not contain a page named '%s'", pagename))
    }
  }
  # copy tempfile to final_document
  file.copy(from = tmp_document, to = file, overwrite = TRUE)

  # Render, if requested
  if(render) {
    rmarkdown::render(file, ...)
  }

  invisible(object)
})


#' Method for rendering a page with a given layout and components
#'
#' @param title The page title.
#' @param components A list of page components.
#' @param layout The pages overall layout.
#' @param menu The menu under which the page will be filed.
#' @param sidebar Character string with sidebar content.
#'
#' @return A markdown string with the final page.
.render_page <- function(title, components, layout = c("default", "storyboard", "focal_left", "2x2_grid"), menu = NULL, sidebar = NULL) {
  if(!is.null(sidebar)) {
    sidebar <- knitr::knit_expand(file = system.file("templates", "sidebar_template.Rmd", package = "i2dash"),
                                  content = sidebar)
  }

  template <- switch(layout,
                     "default" = system.file("templates", "layout_default.Rmd", package = "i2dash"),
                     "storyboard" = system.file("templates", "layout_storyboard.Rmd", package = "i2dash"),
                     "focal_left" = system.file("templates", "layout_focal_left.Rmd", package = "i2dash"),
                     "2x2_grid" = system.file("templates", "layout_2x2_grid.Rmd", package = "i2dash"))

  knitr::knit_expand(file = template,
                     delim = c("<%", "%>"),
                     title = title,
                     menu = menu,
                     layout = layout,
                     sidebar = sidebar,
                     components = components,
                     date = Sys.time())
}