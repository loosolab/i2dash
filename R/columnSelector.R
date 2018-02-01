#' columnSelector module UI representation
#'
#' @param id The ID of the modules namespace.
#' @param label Boolean value; if true include a text input field with the desired axis label (this should be preset with the headline of the column)
#' @param title String which is displayed as module title. (Default = NULL)
#'
#' @return A list from \code{\link[shiny]{tag}} with the UI elements.
#'
#' @export
columnSelectorUI <- function(id, label = F, title = NULL) {
  #create namespace
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$b(title),
    shinyjs::useShinyjs(),
    shiny::singleton(shiny::tags$head(shiny::tags$link(rel = "stylesheet", type = "text/css", href = "wilson_www/styles.css"))),
    shiny::uiOutput(ns("out")),
    {if(label) shiny::uiOutput(ns("showLabel"))}
  )
}

#' columnSelector module server logic
#'
#' @param input Shiny's input object
#' @param output Shiny's output object
#' @param session Shiny's session object
#' @param type.columns data.table: (Supports reactive)
#'                                column1 = columnnames (id)
#'                                column2 = type (datalevel)
#'                                column3 = label (optional, used instead of id)
#'                                column4 = sub_label (optional, added to id/ label)
#' @param type The type (contrast/group/sample of the type dropdown menu, selected in step 1 (upper dropdown). Defaults to unique(type.columns[,2]) (Supports reactive)
#' @param columnTypeLabel Changes the label of the first UI element
#' @param labelLabel Change label above label text input.
#' @param multiple Boolean value whether multiple values can be selected in second selector. (Default = TRUE)
#' @param none If TRUE adds "None to secondSelector and select is. (Default = FALSE)
#' @param sep Used to seperate labels on multi value selection.
#' @param suffix Added to label only on multiple = FALSE (supports reactive). Also uses sep as seperator.
#'
#' @return Returns the input. As named list: names("type", "selectedColumns", "label")
#'
#' @export
columnSelector <- function(input, output, session, type.columns, type = NULL, columnTypeLabel = "Type of Column", labelLabel = "Label", multiple = TRUE, none = FALSE, sep = ", ", suffix = NULL) {
  #handle reactive input
  type.columns.r <- shiny::reactive({
    if(shiny::is.reactive(type.columns)){
      type.columns()
    }else{
      type.columns
    }
  })
  type.r <- shiny::reactive({
    if(!is.null(type)){
      if(shiny::is.reactive(type)){
        type()
      }else{
        type
      }
    }else{
      unique(type.columns.r()[[2]])
    }
  })
  suffix.r <- shiny::reactive({
    if(shiny::is.reactive(suffix)) {
      suffix()
    } else {
      suffix
    }
  })

  output$out <- shiny::renderUI({
    if(none){
      choices <- c("None", type.columns.r()[type.columns.r()[[2]] %in% type.r()[1]][[1]])
    }else{
      choices <- type.columns.r()[type.columns.r()[[2]] %in% type.r()[1]][[1]]
    }
    columnSelectLabel = "Select individual column"
    if(multiple) {
      columnSelectLabel = paste0(columnSelectLabel, "(s)")
    }

    shiny::tagList(
      shiny::selectInput(session$ns("select.type"), label = columnTypeLabel, choices = type.r(), selected = type.r()[1], multiple = FALSE),
      shiny::div(shiny::selectizeInput(session$ns("select.column"), label = columnSelectLabel, choices = choices, multiple = multiple), class = "empty") # colored background if empty
    )
  })

  #show label textInput
  output$showLabel <- shiny::renderUI({
    shiny::textInput(session$ns("select.label"), label = labelLabel)
  })

  # make label
  create_label <- shiny::reactive({
    if(ncol(type.columns.r()) > 2) {
      label_id <- input$select.column
      label_label <- type.columns.r()[type.columns.r()[[1]] %in% input$select.column][[3]]

      # replace id with label
      label <- ifelse(label_label == "", label_id, label_label)

      if(ncol(type.columns.r()) > 3) {
        label <- paste(label, type.columns.r()[type.columns.r()[[1]] %in% input$select.column][[4]])
      }
    } else {
      label <- input$select.column
    }

    label <- paste(label, collapse = sep)

    return(label)
  })

  # update label
  shiny::observe({
    input$select.column
    suffix.r()

    shiny::isolate({
      if(!is.null(input$select.label)) {
        if(!multiple && !is.null(suffix.r())) {
          value <- paste(create_label(), suffix.r(), sep = sep)
        } else {
          value <- create_label()
        }
        shiny::updateTextInput(session = session, inputId = "select.label", value = value)
      }
    })
  })

  #show columns based on selected type
  shiny::observe({
    if(none){
      columns <- c("None", type.columns.r()[type.columns.r()[[2]] %in% input$select.type][[1]])
    }else{
      columns <- type.columns.r()[type.columns.r()[[2]] %in% input$select.type][[1]]
    }

    shiny::updateSelectizeInput(session = session, inputId = "select.column", choices = columns)
  })

  out.type <- shiny::reactive(input$select.type)
  out.selectedColumns <- shiny::reactive(if(shiny::isTruthy(input$select.column) && input$select.column != "None") input$select.column else "")
  out.label <- shiny::reactive({
    if(is.null(input$select.label)) {
      label <- create_label()
    } else {
      label <- input$select.label
    }

    if(multiple) {
      label <- unlist(strsplit(label, split = sep, fixed = TRUE))
    }

    return(label)
  })

  return(list(type = out.type, selectedColumns = out.selectedColumns, label = out.label))

}
