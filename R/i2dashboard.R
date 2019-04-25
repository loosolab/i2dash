#' The idashboard S4 class
#'
#' @slot title The dashboards title
#' @slot pages A list of dashboard pages
#'
#' @name idashboard-class
#' @rdname idashboard-class
#' @export
i2dashboard <- setClass("i2dashboard",
                        slots = c(title = "character", pages = "list"))

setMethod("show", "i2dashboard", function(object) {
  cat("A flexdashboard with the title: ", object@title, "\n", sep = "")
  if(length(object@pages) > 0) {
    cat("... containing ", length(object@pages), "pages.")
  } else {
    cat("... containing 0 pages.")
  }
})
