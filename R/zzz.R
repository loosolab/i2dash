.onLoad <- function(libname = find.package("wilson"), pkgname = "wilson") {
  # make server aware of images
  shiny::addResourcePath(prefix = "wilson_www", directoryPath = system.file("www", package = "wilson"))
}

# hideous hack
# set variables otherwise noted by R cmd check 'no visible binding for...'
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      "Experiment",
      "col_name",
      "condition",
      "level",
      "type"
    )
  )
}
