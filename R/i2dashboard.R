#' i2dash: A package for programmatic creation of interactive, web-based dashboards
#'
#' i2dash relies on the widely used R packages flexdashboard, knitr and rmarkdown. i2dash introduces a new class from R's S4 object system named \linkS4class{i2dashboard}, which by design provides the main functionality of the package. Besides global properties such as the dashboard title, author and theme, an instance of the \linkS4class{i2dashboard} class also stores individual dashboard pages and the navigation menu, as well as all components that make up the content of individual pages.
#'
#' @section Citation:
#'
#' When using the package in your work, please cite: tba.
#'
#' @docType package
#' @name i2dash
NULL

#' The \linkS4class{i2dashboard} S4 class.
#'
#' The \linkS4class{i2dashboard} S4 class provides main functionality of the package. Besides global properties such as the dashboard title, author and theme, an instance of the i2dashboard class also stores individual dashboard pages and the navigation menu, as well as all components that make up the content of individual pages. A new instance can be created using the \code{i2dashboard} function.
#'
#' @param object An object of class \linkS4class{i2dashboard}.
#' @param .Object An object of class \linkS4class{i2dashboard}.
#' @param ... Named slots of the \linkS4class{i2dashboard} object.
#'
#' @slot title The dashboards title
#' @slot author The author of the dashboard
#' @slot interactive If a shiny-based report should be created
#' @slot theme The theme of the dashboard (see the \href{https://rmarkdown.rstudio.com/flexdashboard/using.html#appearance}{documentation of flexdashboard} for available themes)
#' @slot datadir Path to the directory, where report data is stored.
#' @slot file The output filename (recommend that the suffix should be '.Rmd').
#' @slot pages A list of dashboard pages
#' @slot sidebar Content of the global sidebar
#' @slot colormaps A named list with color mappings.
#' @slot source Either a logical value describing whether the source code should be embeded through an item in the navigation bar or a link to a URL where the source code can be found online.
#' @slot social A vector with any number of the following services: “facebook”, “twitter”, “google-plus”, “linkedin”, and “pinterest”. You can also specify “menu” to provide a generic sharing drop-down menu that includes all of the services.
#' @slot navbar A list of links in the navigation bar (see the \href{https://rmarkdown.rstudio.com/flexdashboard/using.html#navigation_bar}{documentation of flexdashboard}).
#'
#' @return An \linkS4class{i2dashboard} object.
#'
#' @rdname i2dashboard-class
#' @exportClass i2dashboard
setClass("i2dashboard",
  slots = c(
    title = "character",
    author = "character",
    interactive = "logical",
    theme = "character",
    datadir = "character",
    file = "character",
    pages = "list",
    sidebar = "character",
    colormaps = "list",
    source = "character",
    social = "character",
    navbar = "list"
    ),
  prototype=list(
    title = "i2dashboard",
    interactive = FALSE,
    theme = "yeti",
    datadir = file.path(getwd(), "report-data"),
    pages = list(default = list(title = "Default page", layout = "default", menu = NULL, components = list(), sidebar = NULL, max_components = Inf)),
    source = "",
    social = ""
    )
  )

#'
#' @rdname i2dashboard-class
setMethod("initialize", "i2dashboard", function(.Object, ...) {
  # Do prototyping
  .Object <- methods::callNextMethod()

  # Create nice filename from title
  if(length(.Object@file) == 0 & length(.Object@title) > 0) {
    .Object@title %>% tolower %>% gsub(pattern = " ", replacement = "-") %>% gsub(pattern = '[^a-zA-Z0-9-]', replacement = '') %>% paste0(".Rmd") -> .Object@file
  }

  # Create working directory and directory for environments
  dir.create(.Object@datadir, showWarnings = FALSE, recursive = T)

  # Validate object - tbd
  return(.Object)
})

#' Show method of the \linkS4class{i2dashboard} class.
#'
#' @param object An \linkS4class{i2dashboard} class object.
#'
#' @rdname i2dashboard-class
setMethod("show", "i2dashboard", function(object) {
  cat("A flexdashboard with the title: ", object@title, "\n", sep = "")
  if(length(object@pages) > 0) {
    cat("... containing ", length(object@pages), "pages:\n")
    for (pagename in names(object@pages)){
      cat(sprintf(" ... the page '%s' with the title '%s' contains %i components.\n", pagename, object@pages[[pagename]]$title, length(object@pages[[pagename]]$components)))
    }
  } else {
    cat("... containing 0 pages.")
  }
})

#' @rdname i2dashboard-class
#' @export
i2dashboard <- function(...) methods::new("i2dashboard", ...)
