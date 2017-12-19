#' orTextual module UI representation
#'
#' This module allows to select (multiple) inputs from a \code{\link[shiny]{selectInput}} element.
#' The functions creates HTML tag definitions of its representation based on the parameters supplied.
#'
#' @param id The ID of the modules namespace.
#'
#' @return A list with HTML tags from \code{\link[shiny]{tag}}.
#'
#' @export
orTextualUI <- function(id){
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tagList(shinyjs::useShinyjs(), shiny::uiOutput(ns("label"))),
    shiny::uiOutput(ns("select")),
    shiny::uiOutput(ns("info"))
  )
}

#' orTextual module server logic
#'
#' Provides server logic for the orTextual module.
#'
#' @param input Shiny's input object.
#' @param output Shiny's output object.
#' @param session Shiny's session object.
#' @param choices A list or a character vector with the possible choices offered in the UI. See \code{\link[shiny]{selectInput}}.
#' @param selected The initially selected value. See \code{\link[shiny]{selectInput}}.
#' @param label A character vector of length one with the label for the \code{\link[shiny]{selectInput}}.
#' @param delimiter A single character indicating if and how items are delimited (default: \code{NULL} indicates no delimitation). Only if contains = FALSE.
#' @param multiple Whether or not selection of multiple items is allowed.
#' @param contains Logical variable. If TRUE shows module as a textsearch input.
#' @param reset A reactive which will trigger a module reset on change.
#'
#' @return Returns a reactive containing a named list with the label, the selected choices as a character vector (text) and a boolean vector of length \code{length(choices)} (bool), indicating whether a item has been chosen. If no item has been chosen, the return is \code{TRUE} for items.
#'
#' @export
orTextual <- function(input, output, session, choices, selected = NULL, label = "Column", delimiter = NULL, multiple = TRUE, contains = FALSE, reset = NULL){
  raw.choices <- choices

  #delimit choices
  if(!is.null(delimiter) & contains == FALSE){
    choices <- unlist(strsplit(choices, split = delimiter, fixed = TRUE))
  }

  output$label <- shiny::renderUI({
    shiny::tags$b(label)
  })

  output$select <- shiny::renderUI({
    if(contains){
      ui <- shiny::textInput(session$ns("column"), label = NULL)
    }else{
      ui <- shiny::selectizeInput(session$ns("column"), label = NULL, choices = NULL, multiple = multiple, selected = NULL)
      #only fetch needed data (calculation on server-side)
      shiny::updateSelectizeInput(session, "column", choices = unique(choices), selected = selected, server = TRUE)
    }

    return(ui)
  })

  output$info <- shiny::renderUI({
    shiny::tagList(
      #added css so that padding won't be added everytime (sums up) modal is shown
      shiny::tags$style(type="text/css", "body {padding-right: 0px !important;}"),
      shiny::actionLink(session$ns("infobutton"), label = NULL, icon = shiny::icon("question-circle"))
    )
  })

  if(shiny::is.reactive(reset)) {
    shiny::observeEvent(reset(), {
      if(is.null(selected)){
        shinyjs::reset("column")
      }else{
        shiny::updateSelectizeInput(session, "column", selected = selected)
      }
    })
  }

  #show right info
  shiny::observeEvent(input$infobutton, {
    if(contains){
      title <- "Textsearch"
      content <- shiny::HTML("Enter some text which will be used for textsearch.")
    }else{
      title <- "Text"
      content <- shiny::HTML("Select one or multiple values to filter.")
    }

    shiny::showModal(
      shiny::modalDialog(
        title = title,
        footer = shiny::modalButton("close"),
        easyClose = TRUE,
        content
      )
    )
  })

  selected.choices <- shiny::reactive({
    if(!is.null(input$column)){
      #escape all regex symbols
      esc.choices <- paste0("\\Q", input$column, "\\E")

      if(contains | !is.null(delimiter)){
        result <- grepl(pattern = paste0(esc.choices, paste0("($|", delimiter, ")"), collapse = "|"), raw.choices, perl = TRUE)
      }else{
        result <- grepl(pattern = paste0("^(", esc.choices, ")$", collapse = "|"), raw.choices, perl = TRUE)
      }

      #set all TRUE if nothing selected
      if(is.null(input$column) | input$column[1] == "" & !all(result)){
        result <- !result
      }

      return(result)
    }else{
      return(!logical(length = length(raw.choices)))
    }
  })

  return(shiny::reactive(list(label = label, bool = selected.choices(), text = input$column)))
}
