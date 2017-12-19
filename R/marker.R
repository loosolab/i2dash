#' marker module UI representation
#'
#' @param id The ID of the modules namespace
#' @param label Set label of first element.
#'
#' @return A list with HTML tags from \code{\link[shiny]{tag}}
#'
#' @export
markerUI <- function(id, label = "Highlight/ Label Selected Features"){
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::selectInput(ns("highlight"), label = label, choices = c("Disabled", "Highlight", "Exclusive")),
    colorPicker2UI(ns("color"), label = "Color", custom = TRUE),
    labelUI(ns("label"))

  )
}

#' marker module server logic
#'
#' @param input Shiny's input object.
#' @param output Shiny's output object.
#' @param session Shiny's session object.
#' @param highlight.labels Data.table from which labels are provided (Supports reactive).
#'
#' @return A reactive which contains a named list (highlight, color, labelColumn, label).
#'
#' @export
marker <- function(input, output, session, highlight.labels){
  highlight.labels.r <- shiny::reactive({
    if(shiny::is.reactive(highlight.labels)){
      highlight.labels()
    }else{
      highlight.labels
    }
  })

  color <- shiny::callModule(colorPicker2, "color")
  labeller <-  shiny::callModule(label, "label", data = shiny::reactive(highlight.labels.r()), unique = FALSE)

  shiny::reactive({
    shiny::req(input$highlight, color()$palette)

    list(highlight = input$highlight, color = color()$palette, labelColumn = labeller()$selected, label = labeller()$label)
  })
}
