#' featureSelector module UI representation
#'
#' @param id The ID of the modules namespace
#'
#' @return A list with HTML tags from \code{\link[shiny]{tag}}
#'
#' @export
featureSelectorUI <- function(id){
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidPage(
      rintrojs::introjsUI(),
      shiny::fluidRow(
        shinydashboard::box(width = 12, collapsible = TRUE,
          shiny::div(id = ns("guide_table"),
            shinycssloaders::withSpinner(DT::dataTableOutput(ns("table"))),
            shiny::br(),
            shiny::uiOutput(ns("row_select"))
          )
        )
      ),
      shiny::fluidRow(
        shinydashboard::box(width = 12,
          shiny::fluidRow(
                        shiny::column(width = 12,
                          shiny::div(id = ns("guide_buttons"),
                            shiny::actionButton(ns("select"), "Select", style = "color: #fff; background-color: #3c8dbc"),
                            shiny::actionButton(ns("reset"), "Reset", style = "color: #fff; background-color: #3c8dbc"),
                            shiny::actionButton(ns("guide"), "Launch guide", style = "color: #fff; background-color: #3c8dbc", icon = shiny::icon("question-circle")),
                            shiny::downloadButton(ns("download")),
                            shiny::br("The SELECT button only evaluates the filter(s) below. Sorting or sub-selections based on the table above will reset!")
                            )

                        )
          ),
          shiny::div(id = ns("guide_and"),
                     shiny::br(),
                     shiny::uiOutput(ns("and_container"))
                     )
        )
      )
    )
  )
}

#' featureSelector module server logic
#'
#' @param input Shiny's input object.
#' @param output Shiny's output object.
#' @param session Shiny's session object.
#' @param data data.table from which to select (Supports reactive).
#' @param features List of features (i.e. columnnames) the and module will show (Defaults to names(data))(Supports reactive).
#' @param feature.grouping Display features seperated in boxes. (Data.table: first column = columnnames, second column = groupnames) (Supports reactive)
#' @param delimiter A single character, or a vector indicating how column values are delimited. (Fills vector sequentially if needed)(Supports reactive)
#' @param multiple Whether or not textual ORs should allow multiple selections. (Fills vector sequentially if needed)(Supports reactive)
#' @param contains Whether or not textual ORs are initialized as textInput checking entries for given string. (Fills vector sequentially if needed)(Supports reactive)
#' @param ranged Whether or not numeric ORs are ranged. (Fills vector sequentially if needed)(Supports reactive)
#' @param step Set numeric ORs slider steps. (Fills vector sequentially if needed)(Supports reactive)
#' @param truncate Truncate datatable entries at x characters (Default = 30).
#' @param selection.default Decide whether everything or nothing is selected on default (no filters applied). Either "all" or "none" (Default = "all").
#'
#' @details Keep in mind that the order of features is the order in which delimiter, multiple, contains, ranged and step are evaluated.
#'
#' @return Reactive containing names list: Selected data as reactive containing data.table (data). Used filter to select data (filter).
#'
#' @export
featureSelector <- function(input, output, session, data, features = NULL, feature.grouping = NULL, delimiter = "|", multiple = TRUE, contains = FALSE, ranged = TRUE, step = 100, truncate = 30, selection.default = "all"){
  # handle reactive data
  data.r <- shiny::reactive({
    if(shiny::is.reactive(data)){
      data.table::copy(data())
    }else{
      data.table::copy(data)
    }
  })

  # handle reactive features
  features.r <- shiny::reactive({
    if(is.null(features)){
      names(data.r())
    }else{
      if(shiny::is.reactive(features)){
        if(is.null(features())){
          names(data.r())
        }else{
          features()
        }
      }else{
        features
      }
    }
  })

  # handle reactive grouping
  feature.grouping.r <- shiny::reactive({
    if(shiny::is.reactive(feature.grouping)){
      feature.grouping()
    }else{
      feature.grouping
    }
  })

  and_selected <- shiny::callModule(and, "and", data = data.r, show.elements = features.r, element.grouping = feature.grouping.r, delimiter = delimiter, multiple = multiple, contains = contains, ranged = ranged, step = step, reset = shiny::reactive(input$reset))
  row_selector <- shiny::callModule(orNumeric, "row_selector", choices = choices, value = value_wrapper, label = "Select n features from the top and/or bottom of the list", stepsize = 1)

  # row_selector choices
  choices <- shiny::reactive({
    if(nrow(data_output()$data) > 0) {
      c(1:nrow(data_output()$data))
    } else {
      c(0, 0)
    }
  })

  # row_selector value; saves last values
  value <- shiny::reactiveVal(value = NULL)

  # select all if no values stored
  value_wrapper <- shiny::reactive({
    if(is.null(value())) {
      value(c(min(choices()), max(choices())))
    }

    value()
  })

  # safe row_selector value
  shiny::observeEvent(input$select, {
    if(shiny::isTruthy(input$table_rows_selected)) {
      if(grepl("outer", row_selector()$text)) { # accomodate for outer selection
        diff <- setdiff(input$table_rows_all, input$table_rows_selected)
        value(c(min(diff), max(diff)))
      } else {
        value(c(min(input$table_rows_selected), max(input$table_rows_selected)))
      }
    } else {
      value(NULL)
    }
  })
  # reset row_selector value on data change
  shiny::observeEvent(data.r(), {
    value(NULL)
  })

  # reset row_selector
  shiny::observeEvent(input$reset, {
    log_message(message = "Filter reset", level = "INFO", token = session$token)

    value(NULL)
    row_selector <<- shiny::callModule(orNumeric, "row_selector", choices = choices, value = value_wrapper, label = "Select n features from the top and/or bottom of the list", stepsize = 1)
  })

  # Fetch reactive guide for this module
  guide <- featureSelectorGuide(session, !is.null(feature.grouping))
  shiny::observeEvent(input$guide, {
    rintrojs::introjs(session, options = list(steps = guide(), scrollToElement = FALSE))
  })

  output$and_container <- shiny::renderUI({
    andUI(session$ns("and"))
  })

  output$row_select <- shiny::renderUI({
    ui <- orNumericUI(session$ns("row_selector"))

    shiny::tagList(
      shiny::fluidRow(
        shiny::column(
          width = 4,
          shiny::column(
            width = 5,
            ui[1]
          ),
          shiny::column(
            width = 1,
            # added css so that padding won't be added everytime (sums up) modal is shown
            shiny::tags$style(type="text/css", "body {padding-right: 0px !important;}"),
            shiny::actionLink(session$ns("infobutton"), label = NULL, icon = shiny::icon("question-circle"))
          )
        ),
        shiny::column(
          width = 1,
          ui[2]
        ),
        shiny::column(
          width = 7,
          ui[3]
        )
      )
    )
  })

  # row selector info
  shiny::observeEvent(input$infobutton, {
    title <- "Select n features from the top and/or bottom of the list"
    content <- "Subset the TopX and/or BottomX features from the currently selected candidates."

    shiny::showModal(
      shiny::modalDialog(
        title = title,
        footer = shiny::modalButton("close"),
        easyClose = TRUE,
        size = "s",
        content
      )
    )
  })

  # access data table information
  proxy <- DT::dataTableProxy("table")

  # select rows via row_selector
  shiny::observe({
    shiny::req(row_selector()$bool, input$table_rows_all)

    row_order <- input$table_rows_all

    # don't select whole table
    if(any(row_selector()$bool == FALSE) & length(row_selector()$bool) == length(row_order)) {
      DT::selectRows(proxy, row_order[row_selector()$bool])
    } else {
      # delete selection
      DT::selectRows(proxy, list())
    }
  })

  output$table <- DT::renderDataTable(options = list(pageLength = 5, scrollX = TRUE, deferRender = TRUE, processing = FALSE, # deferRender = only render visible part of table
                                                        columnDefs = list(list(
                                                          targets = "_all",
                                                          render = DT::JS(
                                                            "function(data, type, row) {",
                                                            paste("var length =", truncate),
                                                            "return typeof data !== 'number' && data !== null && type === 'display' && data.length > length ?",
                                                            "'<span title=\"' + data + '\">' + data.substr(0, length) + '...</span>' : data;",
                                                            "}"
                                                          )
                                                        ))
                                                        ), {
      data_output()$data
  })

  # first filter (and) whole set in table
  select <- shiny::eventReactive(eventExpr = input$select, {
    log_message(message = "Filtering data", level = "INFO", token = session$token)

    data <- data.r()[and_selected()$bool]
  })

  # second filter (highlighted rows) selected via click and/ or 'select rows' ui
  result <- shiny::reactive({
    # create subset
    if(!is.null(input$table_rows_selected)) {
      data <- data_output()$data[input$table_rows_selected]
    } else if(!is.null(input$table_rows_all)) {
      data <- data_output()$data[input$table_rows_all]
    } else {
      data <- data_output()$data
    }

    # expand filter
    filter <- data_output()$filter

    # number of rows selected
    if(!is.null(input$table_rows_selected)) {
      filter <- append(filter, after = 1,
                       values = paste("Selected:", length(input$table_rows_selected))
                       )
    }

    # TODO add order information to filter

    # search text
    if(!is.null(input$table_search)) {
      if(nchar(input$table_search) > 0) {
        hits <- ifelse(is.null(input$table_rows_all), 0, length(input$table_rows_all))
        filter <- append(filter, after = 1,
                         values = paste("Search:", paste0("'", input$table_search, "'"), paste0("(Hits: ", hits, ")"))
                         )
      }
    }

    return(list(data = data, filter = filter))
  })

  # store change
  data_change <- shiny::reactiveVal(value = 0)

  # return on file change unprocessed table
  data_output <- shiny::reactive({
    if(data_change() == 0) {
      if(selection.default == "all") {
        data <- data.r()
      } else if(selection.default == "none") {
        data <- data.r()[FALSE]
      }

      # create filter text
      filter <- paste("Result:" , nrow(data), "hits")
    } else if(data_change() == 1) {
      data <- select()

      # create filter text
      filter <- c(paste("Result:", nrow(data), "hits"), "", shiny::isolate(and_selected()$text))
    }

    return(list(data = data , filter = filter))
  })

  # observe most recent change
  shiny::observe({
    data.r()
    data_change(0)
  })
  shiny::observe({
    select()
    data_change(1)
  })

  # download #####
  output$download <- shiny::downloadHandler(
    filename = "subset.tsv",
    content = function(file) {
      log_message("FeatureSelector: download", "INFO", token = session$token)

      data.table::fwrite(x = result()$data, file = file, sep = "\t")
    }
  )

  return(result)
}

#' featureSelector module guide
#'
#' @param session The shiny session
#' @param grouping Logical if Text for grouping should be displayed (Default = FALSE).
#'
#' @return A shiny reactive that contains the texts for the guide steps.
#'
featureSelectorGuide <- function(session, grouping = FALSE) {
  steps <- list(
    "guide_and" = paste0("<h4>Selectors</h4>
      The selectors are presented row-wise, so that each line represents a seperate selector.<br/>
      Each one operates on a single column of the dataset defined by the columnname on the left side.<br/>
      Basically there are two different types of selectors: one for numeric values and one for text.<br/>
      For further information on how to use any of those close this guide and click on one of the infobuttons ", shiny::icon("question-circle"), "."),
    "guide_and" = "<h4>Set filter</h4>
      As mentioned before each selector is connected to a specific column.<br/>
      So in order to apply a filter and create a specific subset adjust the selectors as needed.<br/>
      The sum of those adjustments will be the filter used in the next step.",
    "guide_buttons" = "<h4>Apply filter</h4>
      After the filter is set as intended, click on 'select' to filter the dataset, or click on 'reset' to delete the current filter.<br/>
      Download the current subset via the respecting 'Download' Button (includes reorder, text search & row selection).",
    "guide_table" = "<h4>Further limit dataset</h4>
      Once the filter is successfully applied the remaining data is shown in this table.<br/>
      <br/>
      The table along with the slider provides the following possibilities: <br/>
      <b>reorder</b>: Change the row order ascending/descending by clicking on the respective column name.<br/>
      <b>text search</b>: Use the field on the top right for text search.<br/>
      <b>select rows</b>: Either use the slider or directly click on rows to select only certain rows in the table."
  )

  if(grouping) {
    steps <- append(steps,
                    list("guide_and" = "<h4>Grouping</h4>
                          These boxes contain several selectors each.<br/>
                          Expand/ Collapse them with a click on the '+'/ '-' on the right side.<br/>
                          Please expand now one or more of those boxes."),
                    0)
  }

  shiny::reactive(data.frame(element = paste0("#", session$ns(names(steps))), intro = unlist(steps)))
}
