#' The idashboard S4 class.
#'
#' @slot title The dashboards title
#' @slot author The author of the dashboard
#' @slot interactive If a shiny-based report should be created
#' @slot theme The theme of the dashboard
#' @slot datadir Path to the directory, where report data is stored.
#' @slot file The output filename (recommend that the suffix should be '.Rmd').
#' @slot pages A list of dashboard pages
#' @slot sidebar Content of the global sidebar
#' @slot colormaps A named list with color mappings.
#' @slot source Either a logical value describing whether the source code should be embeded through an item in the navigation bar or a link to a URL where the source code can be found online.
#' @slot social A vector with any number of the following services: “facebook”, “twitter”, “google-plus”, “linkedin”, and “pinterest”. You can also specify “menu” to provide a generic sharing drop-down menu that includes all of the services.
#' @slot navbar A list of links in the navigation bar (see https://rmarkdown.rstudio.com/flexdashboard/using.html#navigation_bar).
#'
#' @return An i2dashboard object.
#'
#' @name idashboard-class
#' @rdname idashboard-class
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

#' Constructor method of the i2dashboard class.
#'
#' @name idashboard-class
#' @rdname idashboard-class
setMethod("initialize", "i2dashboard", function(.Object, ...) {
  # Do prototyping
  .Object <- callNextMethod()

  # Create nice filename from title
  if(length(.Object@file) == 0 & length(.Object@title) > 0) {
    .Object@title %>% tolower %>% gsub(pattern = " ", replacement = "-") %>% gsub(pattern = '[^a-zA-Z0-9-]', replacement = '') %>% paste0(".Rmd") -> .Object@file
  }

  # Create working directory and directory for environments
  dir.create(.Object@datadir, showWarnings = FALSE, recursive = T)

  # Validate object - tbd
  return(.Object)
})

#' Show method of the i2dashboard class.
#'
#' @name idashboard-class
#' @rdname idashboard-class
setMethod("show", "i2dashboard", function(.Object) {
  cat("A flexdashboard with the title: ", .Object@title, "\n", sep = "")
  if(length(.Object@pages) > 0) {
    cat("... containing ", length(.Object@pages), "pages:\n")
    for (pagename in names(.Object@pages)){
      cat(sprintf(" ... the page '%s' with the title '%s' contains %i components.\n", pagename, .Object@pages[[pagename]]$title, length(.Object@pages[[pagename]]$components)))
    }
  } else {
    cat("... containing 0 pages.")
  }
})

#' Create a new i2dashboard object.
#'
#' @name idashboard-class
#' @rdname idashboard-class
#' @export
i2dashboard <- function(...) new("i2dashboard", ...)


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Validity.
###

# .valid.RmdReport <- function(object){
#   msg <- NULL
#   # Checking header
#   if (!is.character(object@header)){
#     msg <- c(msg, "'header' must be a character-like object")
#   }
#   # Checking pages
#   if (!is.list(object@pages)){
#     msg <- c(msg, "'pages' must be a list-like object")
#   }
#   if (length(msg)) { return(msg) }
#   return(TRUE)
# }
#
# setValidity("RmdReport", .valid.RmdReport)
