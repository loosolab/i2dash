#' The idashboard S4 class
#'
#' @slot title The dashboards title
#' @slot author The author of the dashboard
#' @slot interactive If a shiny-based report should be created
#' @slot theme The theme of the dashboard
#' @slot data.dir
#' @slot file
#' @slot pages A list of dashboard pages
#'
#' @name idashboard-class
#' @rdname idashboard-class
#' @export
setClass("i2dashboard",
  slots = c(
    title = "character",
    author = "character",
    interactive = "logical",
    theme = "character",
    datadir = "character",
    file = "character",
    pages = "list"
    ),
  prototype=list(
    title = "i2dashboard",
    interactive = FALSE,
    theme = "yeti",
    datadir = file.path(getwd(), "report-data"),
    pages = list(default = list(title = "Default page", layout = "default", menu = NULL, components = list(), sidebar = NULL, max_components = Inf))
    )
  )

setMethod("initialize", "i2dashboard", function(.Object, ...) {
  # Do prototyping
  .Object <- callNextMethod()

  # Create nice filename from title
  if(!is.null(.Object@title)) {
    .Object@title %>% tolower %>% gsub(pattern = " ", replacement = "-") %>% gsub(pattern = '[^a-zA-Z-]', replacement = '') %>% paste0(".Rmd") -> .Object@file
  }

  # Create working directory and directory for environments
  dir.create(.Object@datadir, showWarnings = FALSE, recursive = T)

  # Validate object - tbd
  return(.Object)
})

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
