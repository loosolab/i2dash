.onLoad <- function(libname = find.package("wilson"), pkgname = "wilson") {
  # make server aware of images
  shiny::addResourcePath(prefix = "wilson_www", directoryPath = system.file("www", package = "wilson"))
}
