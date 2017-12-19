#' colorPicker module UI representation
#'
#' The functions creates HTML tag definitions of its representation based on the parameters supplied.
#' Currently, two UI can be created for the user to choose either (a) colors from a given color scheme, or (b) choose one or more single colors.
#'
#' @param id The ID of the modules namespace.
#' @param label Either a character vector of length one with the label for the color scheme dropdown, or a character vector containing labels of the single colors.
#' @param choices A character vector with choices for the color scheme dropdown. See \code{\link[shiny]{selectInput}}.
#' @param selected.choice The initially selected value(s) of the dropdown. If NULL (default), the first value of schemes will be taken.
#' @param show.reverse Logical, whether or not to show the reverse switch.
#' @param show.transparency Logical, whether or not to show the transparency slider.
#' @param single.colors Logical, whether or not to make a single color chooser. (Only if length(label) == 1 needed)
#'
#' @return A list with HTML tags from \code{\link[shiny]{tag}}.
#'
#' @section Single color mode:
#' If one or more single colors can be chosen, the UI element names are prefix by \emph{color} followed by \code{make.names} ouput of \code{label}.
#'
#' @section To do:
#' Replace ordinary textInput for single colors by a real colorPicker, e.g. from https://github.com/daattali/colourpicker
#'
#' @export
colorPickerUI <- function(id, label = "Color scheme", choices = c("Blues", "Greens", "Greys", "Oranges", "Purples", "Reds"), selected.choice = NULL, show.reverse = TRUE, show.transparency = TRUE, single.colors = FALSE) {
  ns <- shiny::NS(id)

  if(is.null(selected.choice)) {
    selected.choice <- choices[[1]]
  }

  if(length(label) == 1 & !single.colors) {
    ret <- list(shiny::selectInput(ns("scheme"), label = label, choices = choices, selected = selected.choice))

    if(show.reverse) {
      ret <- c(ret, list(shiny::checkboxInput(ns("reverse"), label = "Reverse scheme")))
    }
  } else {
    ret <- list()
    for(name in make.names(label)) {
      ret <- c(ret, list(colourpicker::colourInput(ns(paste0("color", name)), name, value = "red")))
    }
  }

  if(show.transparency) {
    ret <- c(ret, list(shiny::sliderInput(ns("transparency"), label = "Transparency", min = 0, max = 1, value = 1)))
  }
  shiny::tagList(ret)
}

#' colorPicker module server logic
#'
#' Provides server logic for the colorPicker module.
#'
#' @param input Shiny's input object
#' @param output Shiny's output object
#' @param session Shiny's session object
#'
#' @return The \code{input} object.
#'
#' @section To do:
#' Implement transparency calculation in case of one or more single colors.
#'
#' @export
colorPicker <- function(input, output, session) {
  return(input)
}

