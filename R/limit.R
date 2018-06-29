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
    shiny::fluidRow(shiny::column(shiny::numericInput(ns("lower_limit"), label = "Lower limit", value = -2), width = 6),
                    shiny::column(shiny::numericInput(ns("upper_limit"), label = "Upper limit", value = 2), width = 6))
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
  shinyjs::reset("lower_limit")
  shinyjs::reset("upper_limit")

  # evaluate reactive parameter
  lower_r <- shiny::reactive({
    if (shiny::is.reactive(lower)) {
      lower()
    } else {
      lower
    }
  })

  upper_r <- shiny::reactive({
    if (shiny::is.reactive(upper)) {
      upper()
    } else {
      upper
    }
  })

  # update ui
  shiny::observe({
    if (!is.null(input$enable) && input$enable) {
      shiny::isolate(shiny::updateNumericInput(session = session, inputId = "lower_limit", value = lower_r()))
    }
  })

  shiny::observe({
    if (!is.null(input$enable) && input$enable) {
      shiny::isolate(shiny::updateNumericInput(session = session, inputId = "upper_limit", value = upper_r()))
    }
  })

  shiny::observe({
    # lowerLimit = smaller than upper
    shiny::updateNumericInput(session = session, inputId = "lower_limit", max = input$upper_limit - 1)
    # upperLimit = greater than lower
    shiny::updateNumericInput(session = session, inputId = "upper_limit", min = input$lower_limit + 1)
  })

  # enable ui if checkbox checked
  shiny::observeEvent(input$enable, {
    if (input$enable) {
      shinyjs::enable("lower_limit")
      shinyjs::enable("upper_limit")
    } else {
      shinyjs::disable("lower_limit")
      shinyjs::disable("upper_limit")
    }
  })

  # return values
  shiny::reactive({
    if (!is.null(input$enable) && !input$enable) {
      NULL
    } else {
     list(lower = input$lower_limit,
         upper = input$upper_limit)
    }
  })
}
