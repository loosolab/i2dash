#' limit module UI representation
#'
#' @param id The ID of the modules namespace
#' @param label Set the modules label.
#'
#' @return A list with HTML tags from \code{\link[shiny]{tag}}
#'
#' @export
limitUI <- function(id, label = "Limit"){
  ns <- shiny::NS(id)

  shiny::tagList(
    list(shinyjs::useShinyjs(), shiny::checkboxInput(inputId = ns("enable"), label = label, value = FALSE)),
    shiny::fluidRow(shiny::column(shiny::numericInput(ns("lowerLimit"), label = "Lower limit", value = -2), width = 6),
                    shiny::column(shiny::numericInput(ns("upperLimit"), label = "Upper limit", value = 2), width = 6))
  )
}

#' limit module server logic
#'
#' @param input Shiny's input object.
#' @param output Shiny's output object.
#' @param session Shiny's session object.
#' @param lower Set lower limit (supports reactive).
#' @param upper Set upper limit (supports reactive).
#'
#' @return Reactive containing: list(lower, upper).
#'
#' @export
limit <- function(input, output, session, lower = NULL, upper = NULL){
  # reset on re-run
  shinyjs::reset("enable")
  shinyjs::reset("lowerLimit")
  shinyjs::reset("upperLimit")

  # evaluate reactive parameter
  lower.r <- shiny::reactive({
    if(shiny::is.reactive(lower)) {
      lower()
    } else {
      lower
    }
  })

  upper.r <- shiny::reactive({
    if(shiny::is.reactive(upper)) {
      upper()
    } else {
      upper
    }
  })

  # update ui
  shiny::observe({
    if(!is.null(input$enable) && input$enable) {
      shiny::isolate(shiny::updateNumericInput(session = session, inputId = "lowerLimit", value = lower.r()))
    }
  })

  shiny::observe({
    if(!is.null(input$enable) && input$enable) {
      shiny::isolate(shiny::updateNumericInput(session = session, inputId = "upperLimit", value = upper.r()))
    }
  })

  shiny::observe({
    # lowerLimit = smaller than upper
    shiny::updateNumericInput(session = session, inputId = "lowerLimit", max = input$upperLimit - 1)
    # upperLimit = greater than lower
    shiny::updateNumericInput(session = session, inputId = "upperLimit", min = input$lowerLimit + 1)
  })

  # enable ui if checkbox checked
  shiny::observeEvent(input$enable, {
    if(input$enable) {
      shinyjs::enable("lowerLimit")
      shinyjs::enable("upperLimit")
    } else {
      shinyjs::disable("lowerLimit")
      shinyjs::disable("upperLimit")
    }
  })

  # return values
  shiny::reactive({
    if(!is.null(input$enable) && !input$enable) {
      NULL
    } else {
     list(lower = input$lowerLimit,
         upper = input$upperLimit)
    }
  })
}
