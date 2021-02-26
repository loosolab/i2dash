#' columnSelector module UI representation
#'
#' @param id The ID of the modules namespace.
#' @param label Boolean value; if true include a text input field with the desired axis label (this should be preset with the headline of the column)
#' @param title String which is displayed as module title. (Default = NULL)
#'
#' @return A list from \code{\link[shiny]{tag}} with the UI elements.
#'
#' @export
columnSelectorUI <- function(id, label = FALSE, title = NULL) {
  # create namespace
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$b(title),
    shinyjs::useShinyjs(),
    shiny::singleton(shiny::tags$head(shiny::tags$link(rel = "stylesheet", type = "text/css", href = "wilson_www/styles.css"))),
    shiny::uiOutput(ns("out")),
    if (label) shiny::uiOutput(ns("show_label"))
  )
}

#' columnSelector module server logic
#'
#' @param input Shiny's input object
#' @param output Shiny's output object
#' @param session Shiny's session object
#' @param type.columns data.table: (Supports reactive)
#'                                key = columnnames (id)
#'                                level = datalevel/ type of column
#'                                label = optional, used instead of id
#'                                sub_label = optional, added to id/ label
#' @param type The type (contrast/group/sample of the type dropdown menu, selected in step 1 (upper dropdown). Defaults to unique(type.columns[,2]) (Supports reactive)
#' @param column.type.label Changes the label of the first UI element
#' @param label.label Change label above label text input.
#' @param multiple Boolean value whether multiple values can be selected in second selector. (Default = TRUE)
#' @param none If TRUE adds "None to secondSelector and select is. (Default = FALSE)
#' @param sep Used to separate labels on multi value selection.
#' @param suffix Added to label only on multiple = FALSE (supports reactive). Also uses sep as separator.
#'
#' @return Returns the input. As named list: names("type", "selected_columns", "label")
#'
#' @export
columnSelector <- function(input, output, session, type.columns, type = NULL, column.type.label = "Type of Column", label.label = "Label", multiple = TRUE, none = FALSE, sep = ", ", suffix = NULL) {
  # handle reactive input
  type_columns_r <- shiny::reactive({
    if (shiny::is.reactive(type.columns)) {
      type.columns()
    } else {
      type.columns
    }
  })
  type_r <- shiny::reactive({
    if (!is.null(type)) {
      if (shiny::is.reactive(type)) {
        type()
      } else {
        type
      }
    } else {
      unique(type_columns_r()[[2]])
    }
  })
  suffix_r <- shiny::reactive({
    if (shiny::is.reactive(suffix)) {
      suffix()
    } else {
      suffix
    }
  })

  output$out <- shiny::renderUI({
    if (none) {
      choices <- c("None", type_columns_r()[type_columns_r()[["level"]] %in% type_r()[1]][["key"]])
    }else{
      choices <- type_columns_r()[type_columns_r()[["level"]] %in% type_r()[1]][["key"]]
    }
    column_select_label <- "Select individual column"
    if (multiple) {
      column_select_label <- paste0(column_select_label, "(s)")
    }

    shiny::tagList(
      shiny::selectInput(session$ns("select_type"), label = column.type.label, choices = type_r(), selected = type_r()[1], multiple = FALSE),
      shiny::div(shiny::selectizeInput(session$ns("select_column"), label = column_select_label, choices = choices, multiple = multiple), class = "empty") # colored background if empty
    )
  })

  # show label textInput
  output$show_label <- shiny::renderUI({
    shiny::textInput(session$ns("select_label"), label = label.label)
  })

  # make label
  create_label <- shiny::reactive({
    shiny::req(input$select_type)
    # empty label on 'None'
    if (none && input$select_column == "None") return("")

    if (is.element("label", names(type_columns_r()))) {
      label_id <- input$select_column
      label_label <- type_columns_r()[match(input$select_column, type_columns_r()[["key"]])][["label"]]

      # replace id with label
      label <- ifelse(label_label == "", label_id, label_label)

    } else {
      label <- input$select_column
    }

    # add sub_label
    if (is.element("sub_label", names(type_columns_r()))) {
      label <- paste(label, type_columns_r()[match(input$select_column, type_columns_r()[["key"]])][["sub_label"]])
    }

    label <- paste(label, collapse = sep)

    return(label)
  })

  # update label
  shiny::observe({
    input$select_column
    suffix_r()

    shiny::isolate({
      if (!is.null(input$select_label)) {
        if (!multiple && !is.null(suffix_r())) {
          value <- paste(create_label(), suffix_r(), sep = sep)
        } else {
          value <- create_label()
        }
        shiny::updateTextInput(session = session, inputId = "select_label", value = value)
      }
    })
  })

  # show columns based on selected type
  shiny::observe({
    if (none) {
      columns <- c("None", type_columns_r()[type_columns_r()[["level"]] %in% input$select_type][["key"]])
    } else {
      columns <- type_columns_r()[type_columns_r()[["level"]] %in% input$select_type][["key"]]
    }

    shiny::updateSelectizeInput(session = session, inputId = "select_column", choices = columns)
  })

  out_type <- shiny::reactive(input$select_type)
  out_selected_columns <- shiny::reactive(if (shiny::isTruthy(input$select_column) && input$select_column != "None") input$select_column else "")
  out_label <- shiny::reactive({
    if (is.null(input$select_label)) {
      label <- create_label()
    } else {
      label <- input$select_label
    }

    if (multiple) {
      label <- unlist(strsplit(label, split = sep, fixed = TRUE))
    }

    return(label)
  })

  return(list(type = out_type, selected_columns = out_selected_columns, label = out_label))
}
