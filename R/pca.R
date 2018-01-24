#' pca module UI representation
#'
#' @param id The ID of the modules namespace.
#' @param show.label Set initial value of show label checkbox (Default = TRUE).
#'
#' @return A list with HTML tags from \code{\link[shiny]{tag}}.
#'
#' @export
pcaUI <- function(id, show.label = TRUE) {
  ns <- shiny::NS(id)

  shiny::tagList(shiny::fluidPage(
    rintrojs::introjsUI(),
    shinyjs::useShinyjs(),
    shiny::fluidRow(shinydashboard::box(
      width = 12,
      shiny::div(style = "overflow-y: scroll; overflow-x: scroll; height: 800px; text-align: center",
                 shiny::plotOutput(outputId = ns("pca"))
      )
    )),
    shiny::fluidRow(
      shinydashboard::box(
        width = 12,
        collapsible = TRUE,
        shiny::fluidRow(
          shiny::column(
            width = 4,
            shiny::div(id = ns("guide_selection"),
                       columnSelectorUI(ns("select")),
                       shiny::checkboxInput(ns("label"), label = "show label", value = show.label)
            )
          ),
          shiny::column(
            width = 4,
            shiny::div(id = ns("guide_dimensions"),
                       shiny::numericInput(ns("dimA"), label = "PCA dimension (x-axis)", min = 1, max = 6, step = 1, value = 1),
                       shiny::numericInput(ns("dimB"), label = "PCA dimension (y-axis)", min = 1, max = 6, step = 1, value = 2)
            )
          ),
          shiny::column(
            width = 4,
            shiny::div(id = ns("guide_pointsize"),
                       shiny::sliderInput(ns("pointsize"),label = "Point size", min = 0.1, max = 10, value = 2),
                       shiny::sliderInput(ns("labelsize"), label = "Label size", min = 1, max = 20, value = 5, round = TRUE)
            )
          )
        ),
        shiny::fluidRow(shiny::column(
          width = 12,
          shiny::div(id = ns("guide_buttons"),
                     shiny::actionButton(ns("plot"), "Plot", style = "color: #fff; background-color: #3c8dbc"),
                     shiny::actionButton(ns("reset"), "Reset", style = "color: #fff; background-color: #3c8dbc"),
                     shiny::actionButton(ns("guide"), "Launch guide", style = "color: #fff; background-color: #3c8dbc", icon = shiny::icon("question-circle")),
                     shiny::downloadButton(outputId = ns("download"), label = "Download")
          )

        ))

      )
    )
  ))
}

#' pca module server logic
#'
#' @param input Shiny's input object
#' @param output Shiny's output object
#' @param session Shiny's session object
#' @param data data.table data visualized in plot. (Supports Reactive)
#' @param types data.table: (Supports reactive)
#'                        column1: colnames of data
#'                        column2: corresponding column typ
#'                        column3: label (optional, used instead of id)
#'                        column4: sub_label (optional, added to id/ label)
#' @param levels Levels from which data is selected (Defaults to unique(metadata[["level"]])). (Supports Reactive)
#' @param entryLabel Define additional columns added to each entry (Default = NULL). Use a vector containing the desired columnnames e.g. c("column1", "column2").
#' @param width Width of the plot in cm. Defaults to 28 and supports reactive.
#' @param height Height of the plot in cm. Defaults to 28 and supports reactive.
#' @param ppi Pixel per inch. Defaults to 72 and supports reactive.
#' @param scale Scale plot size. Defaults to 1, supports reactive.
#'
#' @details Width/ height/ ppi less or equal to zero will use default value.
#'
#' @return A reactive containing list with dimensions.
#'
#' @import data.table
#'
#' @export
pca <- function(input, output, session, data, types, levels = NULL, entryLabel = NULL, width = 28, height = 28, ppi = 72, scale = 1) {
  #handle reactive data
  data.r <- shiny::reactive({
    if(shiny::is.reactive(data)){
      data <- data.table::copy(data())
    }else{
      data <- data.table::copy(data)
    }
    #merge columns for additional label info
    if(!is.null(entryLabel)){
      data[, 1 := apply(data[, c(names(data)[1], entryLabel), with = FALSE], 1, paste, collapse = ", ")]
      names(data)[1] <- paste(names(data)[1], paste(entryLabel, collapse = ", "), sep = ", ")
    }

    return(data)
  })
  types.r <- shiny::reactive({
    if(shiny::is.reactive(types)){
      types()
    }else{
      types
    }
  })
  levels.r <- shiny::reactive({
    if(is.null(levels)){
      metadata.r()[["level"]]
    }else{
      if(shiny::is.reactive(levels)){
        levels()
      }else{
        levels
      }
    }
  })
  # handle reactive sizes
  size <- shiny::reactive({
    width <- ifelse(shiny::is.reactive(width), width(), width)
    height <- ifelse(shiny::is.reactive(height), height(), height)
    ppi <- ifelse(shiny::is.reactive(ppi), ppi(), ppi)
    scale <- ifelse(shiny::is.reactive(scale), scale(), scale)

    if(!is.numeric(width) | width <= 0) {
      width <- 28
    }
    if(!is.numeric(height) | height <= 0) {
      height <- 28
    }
    if(!is.numeric(ppi) | ppi <= 0) {
      ppi <- 72
    }

    list(width = width,
         height = height,
         ppi = ppi,
         scale = scale)
  })


  guide <- pcaGuide(session)
  shiny::observeEvent(input$guide, {
    rintrojs::introjs(session, options = list(steps = guide()))
  })

  # clear plot
  clearPlot <- shiny::reactiveVal(value = FALSE)

  #reset ui
  shiny::observeEvent(input$reset, {
    log_message("PCA: reset", "INFO", token = session$token)

    shinyjs::reset("label")
    shinyjs::reset("dimA")
    shinyjs::reset("dimB")
    shinyjs::reset("pointsize")
    shinyjs::reset("labelsize")
    columnSelect <<- shiny::callModule(columnSelector, "select", type.columns = shiny::reactive(types.r()[level %in% levels.r(), c("key", "level"), with = FALSE]), columnTypeLabel = "Column types to choose from")
    clearPlot(TRUE)
  })

  columnSelect <- shiny::callModule(columnSelector, "select", type.columns = shiny::reactive(types.r()[level %in% levels.r(), c("key", "level"), with = FALSE]), columnTypeLabel = "Column types to choose from")

  output$datalevel <- shiny::renderUI({
    shiny::selectInput(session$ns("select"), label = "select data level", choices = unique(levels.r()))
  })

  #update dimension inputs
  shiny::observe({
    col.num <- length(columnSelect$selectedColumns())
    if(col.num < 3 | nrow(data.r()) < 3 | is.na(input$dimA) | is.na(input$dimB)){
      shinyjs::disable("plot")

      # show warning if not enough selected
      if(col.num > 0) {
        shiny::showNotification(
          ui = "Not enough columns selected! At least 3 needed for plotting.",
          id = "warning",
          type = "warning"
        )
      }else {
        shiny::removeNotification("warning")
      }

    }else{
      shiny::removeNotification("warning")
      shinyjs::enable("plot")

      if(col.num <= input$dimA){
        valueA <- col.num - 1
      }else{
        valueA <- input$dimA
      }
      if(col.num <= input$dimB){
        valueB <- col.num - 1
      }else{
        valueB <- input$dimB
      }
      shiny::updateNumericInput(inputId = "dimA", session = session, max = col.num - 1, value = valueA)
      shiny::updateNumericInput(inputId = "dimB", session = session, max = col.num - 1, value = valueB)
    }
  })

  # warning if plot size exceeds limits
  shiny::observe({
    if(computed.data()$exceed_size) {
      shiny::showNotification(
        ui = "Width and/ or height exceed limit. Using 500 cm instead.",
        id = "limit",
        type = "warning"
      )
    } else {
      shiny::removeNotification("limit")
    }
  })

  selected <- shiny::reactive({
    #new progress indicator
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(0.2, message = "Select data")

    selected <- data.r()[, c(names(data.r())[1], columnSelect$selectedColumns()), with = FALSE]

    progress$set(1)

    return(selected)
  })

  # disable downloadButton on init
  shinyjs::disable("download")

  computed.data <- shiny::eventReactive(input$plot, {
    log_message("PCA: computing plot...", "INFO", token = session$token)

    # enable downloadButton
    shinyjs::enable("download")
    clearPlot(FALSE)

    #new progress indicator
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(0.2, message = "Render plot")

    plot <- create_pca(
               data = selected(),
               dimensionA = input$dimA,
               dimensionB = input$dimB,
               dimensions = length(columnSelect$selectedColumns()) - 1,
               pointsize = input$pointsize,
               labelsize = input$labelsize,
               labels = input$label,
               custom.labels = columnSelect$label(),
               on.columns = TRUE,
               width = size()$width,
               height = size()$height,
               ppi = size()$ppi,
               scale = size()$scale
    )

    progress$set(1)

    log_message("PCA: done.", "INFO", token = session$token)

    # show plot
    shinyjs::show("pca")

    return(plot)
  })

  # get width in pixel
  plot_width <- shiny::reactive({
    width <- computed.data()$width * (computed.data()$ppi / 2.54)
    ifelse(width < 50, 50, width)
  })

  # get height in pixel
  plot_height <- shiny::reactive({
    height <- computed.data()$height * (computed.data()$ppi / 2.54)
    ifelse(height < 50, 50, height)
  })

  output$pca <- shiny::renderPlot(
    width = plot_width,
    height = plot_height,
    {
      if(clearPlot()){
        return()
      } else {
        log_message("PCA: render plot", "INFO", token = session$token)

        computed.data()$plot
      }
    })

  #group data by dimension
  reorganized.data <- shiny::reactive({
    sapply(colnames(computed.data()$data$var$coord), USE.NAMES = TRUE, simplify = FALSE, function(dim) {
      sapply(computed.data()$data$var, function(table) {
        table[, dim]
      })
    })
  })

  output$download <- shiny::downloadHandler(filename = "pca.zip",
                                            content = function(file) {
                                              log_message("PCA: download", "INFO", token = session$token)
                                              download(file = file, filename = "pca.zip", plot = computed.data()$plot, width = plot_width() / (computed.data()$ppi / 2.54), height = plot_height() / (computed.data()$ppi / 2.54), ppi = computed.data()$ppi, ui = user_input())
                                            })

  user_input <- shiny::reactive({
    # format selection
    selection <- list(
      data = list(type = columnSelect$type(), selectedColumns = columnSelect$selectedColumns()),
      dimensions = list(xaxis = input$dimA, yaxis = input$dimB)
    )

    # format options
    options <- list(
      show_label = input$label,
      pointsize = input$pointsize,
      labelsize = input$labelsize
    )

    # merge all
    all <- list(selection = selection, options = options)
  })

  return(reorganized.data)
}

#' pca module guide
#'
#' @param session The shiny session
#'
#' @return A shiny reactive that contains the texts for the Guide steps.
#'
pcaGuide <- function(session) {
  steps <- list(
    "guide_selection" = "<h4>Data selection</h4>
    Select a column type for visualization, then select individual columns based on the chosen type.<br/>
    At least three individual columns need to be selected for PCA.",
    "guide_dimensions" = "<h4>PCA dimensions</h4>
    Choose which PCA dimensions are shown on the x-axis and y-axis.<br/>
    The number of PCA dimensions available for visualization is one less than the number of selected columns in the previous step.",
    "guide_pointsize" = "<h4>Additional options</h4>
    You can increase or decrease the point size by dragging the slider to the right or to the left. The same goes for the label size and it's respecting slider.",
    "guide_buttons" = "<h4>Create the plot</h4>
    As a final step, a click on the 'Plot' button will render the plot, while a click on the 'Reset' button will reset the parameters to default."
  )

  shiny::reactive(data.frame(element = paste0("#", session$ns(names(steps))), intro = unlist(steps)))
}
