#' 'i2dash': A package for programmatic creation of interactive, web-based dashboards
#'
#' 'i2dash' relies on the widely used R packages 'flexdashboard', 'knitr' and 'rmarkdown'. 'i2dash' introduces a new class from R's S4 object system named \linkS4class{i2dashboard}, which by design provides the main functionality of the package. Besides global properties such as the dashboard title, author and theme, an instance of the \linkS4class{i2dashboard} class also stores individual dashboard pages and the navigation menu, as well as all components that make up the content of individual pages.
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
#' @slot title The dashboards title (default 'i2dashboard').
#' @slot author The author of the dashboard.
#' @slot interactive If a 'shiny'-based report should be created (default 'FALSE').
#' @slot theme The theme of the dashboard (see the \href{https://rmarkdown.rstudio.com/flexdashboard/using.html#appearance}{documentation of 'flexdashboard'} for available themes) (default 'yeti').
#' @slot datadir Path to the directory, where report data is stored (default 'tempdir()').
#' @slot pages A list of dashboard pages.
#' @slot sidebar Content of the global sidebar
#' @slot colormaps A named list with color mappings.
#' @slot source Either a logical value describing whether the source code should be embeded through an item in the navigation bar or a link to a URL where the source code can be found online.
#' @slot share A vector with any number of the following services: 'facebook', 'twitter', 'google-plus', 'linkedin', and 'pinterest'. You can also specify “menu” to provide a generic sharing drop-down menu that includes all of the services.
#' @slot navbar A list of links in the navigation bar (see the \href{https://rmarkdown.rstudio.com/flexdashboard/using.html#navigation_bar}{documentation of 'flexdashboard'}).
#'
#' @return '\code{i2dashboard()}' returns an \linkS4class{i2dashboard} object with a default title, theme, and an empty 'default' page.
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
    pages = "list",
    sidebar = "character",
    colormaps = "list",
    source = "character",
    share = "character",
    navbar = "list"
    ),
  prototype=list(
    title = "i2dashboard",
    interactive = FALSE,
    theme = "yeti",
    datadir = file.path(tempdir()),
    pages = list(default = list(title = "Default page", layout = "default", menu = NULL, components = list(), sidebar = NULL, max_components = Inf)),
    source = "",
    share = ""
    )
  )

#'
#' @rdname i2dashboard-class
setMethod("initialize", "i2dashboard", function(.Object, ...) {
  # Do prototyping
  .Object <- methods::callNextMethod()

  # Create working directory and directory for environments
  dir.create(.Object@datadir, showWarnings = FALSE, recursive = T)

  # Validate object - tbd
  return(.Object)
})

#' Show method of the \linkS4class{i2dashboard} class.
#'
#' @param object An \linkS4class{i2dashboard} class object.
#'
#' @return '\code{show()}' returns text, describing the structure of the \linkS4class{i2dashboard} object.
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
#' @examples
#' \donttest{
#' dashboard <- i2dashboard()
#' dashboard <- i2dashboard(
#'     title = "Dashboard title",
#'     author = "John Doe",
#'     interactive = TRUE,
#'     theme = "cosmo",
#'     datadir = "path/to/workdir",
#'     source = "embed"
#' )
#' # inspect dashboard:
#' show(dashboard)
#' dashboard
#' }
i2dashboard <- function(...) methods::new("i2dashboard", ...)
