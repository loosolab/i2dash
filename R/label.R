#' label module UI representation
#'
#' @param id The ID of the modules namespace
#'
#' @return A list with HTML tags from \code{\link[shiny]{tag}}
#'
#' @export
labelUI <- function(id){
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::uiOutput(outputId = ns("label_container"))
  )
}

#' label module server logic
#'
#' @param input Shiny's input object.
#' @param output Shiny's output object.
#' @param session Shiny's session object.
#' @param data Data.table used for label creation. Column names will be used for selection. (supports reactive)
#' @param label Set label of selectizeInput.
#' @param multiple Allow multiple selection which will be merged with sep (default = TRUE).
#' @param sep Seperator used to collapse selection (default = ", ").
#' @param unique Make labels unique. Defaults to TRUE. See \code{\link[base]{make.unique}}.
#' @param unique_sep Seperator used for unique (default = "_"). Should differ from sep.
#' @param disable Reactive containing boolean. To disable/ enable module.
#'
#' @return Reactive containing list(label = vector of strings or NULL on empty selection, selected = user input).
#'
#' @export
label <- function(input, output, session, data, label = "Select label columns", multiple = TRUE, sep = ", ", unique = TRUE, unique_sep = "_", disable = NULL){
  # handle reactive data
  data.r <- shiny::reactive({
    if(shiny::is.reactive(data)) {
      data()
    } else {
      data
    }
  })

  output$label_container <- shiny::renderUI({
    # first choice = "" so no selection for multiple = F is possible
    shiny::selectizeInput(inputId = session$ns("label_creator"), label = label, choices = c("", names(data.r())), selected = "", multiple = multiple, options = list(placeholder = "None"))
  })

  # disable/ enable module
  if(!is.null(disable)) {
    shiny::observe({
      if(disable()) {
        shinyjs::disable("label_creator")
      } else {
        shinyjs::enable("label_creator")
      }
    })
  }

  shiny::reactive({
    if(!shiny::isTruthy(input$label_creator) || !is.null(disable) && disable()) return(NULL)

    # merge selected rows to vector of strings
    custom_label <- data.r()[, do.call(paste, c(... = .SD, sep = sep)), .SDcols = input$label_creator]

    # make unique labels
    if(unique) {
      custom_label <- make.unique(custom_label, sep = unique_sep)
    }

    list(label = custom_label, selected = input$label_creator)
  })
}
