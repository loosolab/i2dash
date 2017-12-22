#' orNumeric module UI representation
#'
#' This module allows to select value/range inputs from a \code{\link[shiny]{sliderInput}} element.
#' The functions creates HTML tag definitions of its representation based on the parameters supplied.
#'
#' @param id The ID of the modules namespace.
#'
#' @return A list with HTML tags from \code{\link[shiny]{tag}}.
#'
#' @export
orNumericUI <- function(id){
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tagList(shinyjs::useShinyjs(),
                   shiny::singleton(shiny::tags$head(shiny::tags$link(rel = "stylesheet", type = "text/css", href = "wilson_www/styles.css"))),
                   shiny::uiOutput(ns("label"))
    ),
    shiny::uiOutput(ns("options")),
    shiny::uiOutput(ns("slider")),
    shiny::uiOutput(ns("info"))
  )
}

#' orNumeric module server logic
#'
#' Provides server logic for the orNumeric module.
#'
#' @param input Shiny's input object.
#' @param output Shiny's output object.
#' @param session Shiny's session object.
#' @param choices A list or a numeric vector with the possible choices offered in the UI. See \code{\link[shiny]{sliderInput}} (Supports reactive).
#' @param value Initial value of the slider. Creates a ranged slider if numeric vector of two given (Supports reactive).
#' @param label Label of the entire module.
#' @param step Number of steps on interval (Default = 100).
#' @param stepsize Value defining interval size of the slider. Will be used instead of step (Default = NULL).
#' @param min. Minimum value that can be selected on slider (defaults to min(choices)) (Supports reactive).
#' @param max. Maximum value that can be selected on slider (defaults to max(choices)) (Supports reactive).
#' @param label.slider A character vector of length one with the label for the \code{\link[shiny]{sliderInput}}.
#' @param zoomable Boolean to enable zooming. Redefine the sliders range. Defaults to TRUE.
#' @param reset A reactive which will trigger a module reset on change.
#'
#' @return Returns a reactive containing a named list with the label, the selected choices as a character vector (text) and a boolean vector of length \code{length(choices)} (bool), indicating whether a item has been chosen. If no item has been chosen, the return is \code{TRUE} for items.
#'
#' @export
orNumeric <- function(input, output, session, choices, value, label = "Column", step = 100, stepsize = NULL, min. = shiny::reactive(min(choices.r(), na.rm = TRUE)), max. = shiny::reactive(max(choices.r(), na.rm = TRUE)), label.slider = NULL, zoomable = TRUE, reset = NULL){
  choices.r <- shiny::reactive({
    if(shiny::is.reactive(choices)) {
      choices()
    } else {
      choices
    }
  })

  value.r <- shiny::reactive({
    if(shiny::is.reactive(value)) {
      value()
    } else {
      value
    }
  })

  min.r <- shiny::reactive({
    if(shiny::is.reactive(min.)) {
      min.()
    } else {
      min.
    }
  })

  max.r <- shiny::reactive({
    if(shiny::is.reactive(max.)) {
      max.()
    } else {
      max.
    }
  })

  output$label <- shiny::renderUI({
    shiny::tags$b(label)
  })

  output$options <- shiny::renderUI({
    if(shiny::isolate(length(value.r()) > 1)){
      shiny::radioButtons(inputId = session$ns("options"), label = NULL, inline = FALSE,
                                           choices = list("inner", "outer"))
    }else{
      shiny::selectInput(inputId = session$ns("options"), label = NULL,
                                    choices = c("=", "<", ">"),
                                    selected = NULL,
                                    multiple = TRUE)
    }
  })

  # change css classes so slider visually matches options
  shiny::observe({
      # change css classes if slider is re-rendered
      min.r()
      max.r()

      if(length(value.r()) > 1){
        shiny::req(input$options)

        if(input$options == "inner") {
          shinyjs::runjs(paste0("$(document.getElementById('", session$ns("slider"),"')).find('span').removeClass('outer')"))
          shinyjs::runjs(paste0("$(document.getElementById('", session$ns("slider"),"')).find('span').addClass('inner')"))
        } else if(input$options == "outer") {
          shinyjs::runjs(paste0("$(document.getElementById('", session$ns("slider"),"')).find('span').removeClass('inner')"))
          shinyjs::runjs(paste0("$(document.getElementById('", session$ns("slider"),"')).find('span').addClass('outer')"))
        }
      } else {
        shinyjs::runjs(paste0("$(document.getElementById('", session$ns("slider"),"')).find('span').addClass('empty')"))

        if(shiny::isTruthy(input$options)) {
          if(any(input$options == ">")) {
            shinyjs::runjs(paste0("$(document.getElementById('", session$ns("slider"),"')).find('span').addClass('greater')"))
          } else {
            shinyjs::runjs(paste0("$(document.getElementById('", session$ns("slider"),"')).find('span').removeClass('greater')"))
          }

          if(any(input$options == "=")) {
            shinyjs::runjs(paste0("$(document.getElementById('", session$ns("slider"),"')).find('span').addClass('equal')"))
          } else {
            shinyjs::runjs(paste0("$(document.getElementById('", session$ns("slider"),"')).find('span').removeClass('equal')"))
          }

          if(any(input$options == "<")) {
            shinyjs::runjs(paste0("$(document.getElementById('", session$ns("slider"),"')).find('span').addClass('less')"))
          } else {
            shinyjs::runjs(paste0("$(document.getElementById('", session$ns("slider"),"')).find('span').removeClass('less')"))
          }
        } else {
          shinyjs::runjs(paste0("$(document.getElementById('", session$ns("slider"),"')).find('span').removeClass('greater')"))
          shinyjs::runjs(paste0("$(document.getElementById('", session$ns("slider"),"')).find('span').removeClass('equal')"))
          shinyjs::runjs(paste0("$(document.getElementById('", session$ns("slider"),"')).find('span').removeClass('less')"))
        }
      }
  })

  output$slider <- shiny::renderUI({
    min. <- min.r()
    max. <- max.r()
    value <- value.r()

    interval <- ifelse(is.null(stepsize), abs(min. - max.) / step, stepsize)

    out <- shiny::tagList(
      shiny::fluidRow(
        if(zoomable) shiny::column(width = 3, shiny::numericInput(session$ns("minVal"), min = min., max = max., value = min., label = NULL, width = "100%")),
        shiny::column(width = ifelse(zoomable, 6, 12), shiny::sliderInput(session$ns("slider"), label = label.slider, min = min., max = max., value = value, step = interval, width  = "100%", round = FALSE)),
        if(zoomable) shiny::column(width = 3, shiny::numericInput(session$ns("maxVal"), min = min., max = max., value = max., label = NULL, width = "100%"))
      )
    )

    return(out)
  })

  if(shiny::is.reactive(reset)) {
    shiny::observeEvent(reset(), {
      # require rendered UI
      shiny::req(input$slider)

      shinyjs::reset("minVal")
      shinyjs::reset("maxVal")
      shinyjs::reset("options")

      min. <- min.r()
      max. <- max.r()
      value <- value.r()
      interval <- ifelse(is.null(stepsize), abs(min. - max.) / step, stepsize)
      shiny::updateSliderInput(session = session, inputId = "slider", value = value, min = min., max = max., step = interval) # shinyjs::reset("slider") won't reset value if not in current range
    })
  }

  if(zoomable){
    #zoomable slider
    shiny::observe({
      shiny::req(input$minVal, input$maxVal)

      interval <- ifelse(is.null(stepsize), abs(input$minVal - input$maxVal) / step, stepsize)
      shiny::updateSliderInput(session, inputId = "slider", min = input$minVal, max = input$maxVal, step = interval)
    })

    #only useful values
    shiny::observe({
      shiny::req(input$minVal, input$maxVal)

      shiny::updateNumericInput(session, inputId = "minVal", max  = input$maxVal)
      shiny::updateNumericInput(session, inputId = "maxVal", min = input$minVal)
    })
  }


  output$info <- shiny::renderUI({
    shiny::tagList(
      #added css so that padding won't be added everytime (sums up) modal is shown
      shiny::tags$style(type="text/css", "body {padding-right: 0px !important;}"),
      shiny::actionLink(session$ns("infobutton"), label = NULL, icon = shiny::icon("question-circle"))
    )
  })

  #show right info
  shiny::observeEvent(input$infobutton, {
    if(length(value.r()) > 1){
      #ranged slider
      title <- "Numeric"
      content <- shiny::HTML("Use the slider to selected the desired range. Choose either 'inner' or 'outer' for the values in- or outside the range.")
    }else{
      #single slider
      title <- "Numeric"
      content <- shiny::HTML("Select one or more operations (\"<\", \">\", \"=\")<br/> Use slider to set value on which operations will be performed.<br/> E.g. If you choose \"<\" \"=\" 5 every value smaller or equal will be selected.")
    }

    if(zoomable){
      content <- shiny::HTML(content, paste("<hr> Use the numberfields on each side of the slider to set it's min/max value, allowing for a more precise selection. As the slider will always attempt to have", step, "steps."))
      path <- "wilson_www/orNumeric_help.png"
      if(length(value.r()) > 1) content <- shiny::tagList(content, shiny::div(shiny::img(src = path, width = "90%")))
    }

    shiny::showModal(
      shiny::modalDialog(
        title = title,
        footer = shiny::modalButton("close"),
        easyClose = TRUE,
        size = "l",
        content
      )
    )
  })

  selected <- shiny::reactive({
    searchData(input = input$slider, choices = choices.r(), options = input$options, min. = min.r(), max. = max.r())
  })

  out <- shiny::reactive({
    value <- value.r()

    text <- ""

    # ranged: return text if not in default selection; single: return if options are selected
    if(length(value) > 1) {
      if(shiny::isTruthy(input$slider) & shiny::isTruthy(input$options)) {
        if(!(input$slider[1] == min.r() & input$slider[2] == max.r() & input$options == "inner")) {
          text <- paste(input$options, paste(input$slider, collapse = "-"))
        }
      }
    } else {
      if(shiny::isTruthy(input$options)){
        text <- paste(paste(input$options, collapse = " "), input$slider)
      }
    }

    list(
      label = label,
      bool = selected(),
      text = text
    )
  })

  return(out)
}
