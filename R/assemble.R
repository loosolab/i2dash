#' Generate an RMarkdown file from an \linkS4class{i2dashboard} object.
#'
#' @param dashboard A \linkS4class{i2dashboard}.
#' @param pages A string or vector with the names of pages, which should be assembled into the resulting Rmd file.
#' @param file The filename of the resulting Rmd file (recommend that the suffix should be '.Rmd').
#' @param exclude A string or vector with the names of pages, which should be excluded from dashboard assembly.
#' @param render A logical indicating whether the assembled dashboard should immediately be rendered with '\code{rmarkdown::render()}' or run with '\code{rmarkdown::run()}'.
#' @param ... Additional arguments passed on to '\code{rmarkdown::render()}'.
#'
#' @return Invisibly returns the unmodified \linkS4class{i2dashboard} object.
#'
#' @rdname assemble
#' @examples
#' \dontrun{
#' i2dashboard() %>% assemble()
#' i2dashboard() %>%
#'     add_page("p1", "Title 1") %>%
#'     add_page("p2", "Title 2") %>%
#'     assemble(file="MyDashboard.Rmd", exclude="default", render=TRUE)
#' }
setMethod("assemble", "i2dashboard", function(dashboard, pages = names(dashboard@pages), file = dashboard@file, exclude = NULL, render = FALSE, ...) {
  . = NULL # workaround for R CMD check note: no visible binding for global variable '.'
  tmp_document <- tempfile()

  # Handle colormap
  colormap_id <- NULL
  if(length(dashboard@colormaps) > 0) {
    file %>% tolower() %>% gsub(".rmd", "", x = .) %>% paste0("colormap_", ., ".rds") -> colormap_id
    saveRDS(dashboard@colormaps, file = file.path(dashboard@datadir, colormap_id))
  }

  # Hack to proper source and social
  if (dashboard@source == "") {
    source <- NULL
  } else {
    source <- dashboard@source
  }
  if (dashboard@share == "") {
    social <- NULL
  } else {
    social <- dashboard@share
  }

  # Add YAML header
  options(ymlthis.rmd_body = "")
  ymlthis::yml(date = F) %>%
    ymlthis::yml_title(dashboard@title) %>%
    ymlthis::yml_author(dashboard@author) %>%
    ymlthis::yml_output(flexdashboard::flex_dashboard(theme = !!dashboard@theme, social = !!social, source = !!source, navbar = !!dashboard@navbar)) %>%
    {if(dashboard@interactive) ymlthis::yml_runtime(., runtime = "shiny") else .} %>%
    ymlthis::use_rmarkdown(path = tmp_document, include_body = FALSE, quiet = TRUE, open_doc = FALSE)

  # Add i2dash global setup
  knitr::knit_expand(file = system.file("templates", "i2dash-global-setup.Rmd", package = "i2dash"),
                     delim = c("<%", "%>"),
                     datadir = dashboard@datadir,
                     colormap = colormap_id) %>%
    cat(file = tmp_document, append = TRUE, sep = "\n")

  # Handle exclusion of pages
  if(!is.null(exclude)) {
    pages <- pages[-stats::na.omit(match(exclude, pages))]
  }

  # Handle global sidebar if it has content
  if(length(dashboard@sidebar) > 0) {
    knitr::knit_expand(file = system.file("templates", "global_sidebar.Rmd", package = "i2dash"),
                       delim = c("<%", "%>"),
                       content = dashboard@sidebar,
                       datawidth = 250) %>%
      cat(file = tmp_document, append = TRUE, sep="\n")
  }

  # write page to tempfile
  for (pagename in pages){
    name <- .create_page_name(pagename)
    if (name %in% names(dashboard@pages)){
      # Create a content string from all components
      components <- paste0(dashboard@pages[[name]]$components, sep = "")
      # Create variable "title" & "menu" & "layout" for readability
      title <- dashboard@pages[[name]]$title
      menu <- dashboard@pages[[name]]$menu
      layout <- dashboard@pages[[name]]$layout
      sidebar <- dashboard@pages[[name]]$sidebar

      .render_page(title = title, components = components, layout = layout,  menu = menu, sidebar = sidebar) %>%
        cat(file = tmp_document, append = TRUE, sep="\n")
    } else {
      warning(sprintf("i2dashboard dashboard does not contain a page named '%s'", pagename))
    }
  }

  # copy tempfile to final_document
  file.copy(from = tmp_document, to = file, overwrite = TRUE)

  # Render or run app, if requested
  if(render & !dashboard@interactive) {
    rmarkdown::render(file, ...)
  }

  if(render & dashboard@interactive) {
    rmarkdown::run(file, ...)
  }

  invisible(dashboard)
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
    sidebar <- knitr::knit_expand(file = system.file("templates", "local_sidebar.Rmd", package = "i2dash"),
                                  delim = c("<%", "%>"),
                                  content = sidebar, datawidth = 250)
  }

  template <- switch(layout,
                     "empty" = system.file("templates", "layout_empty.Rmd", package = "i2dash"),
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
