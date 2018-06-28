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
                       shiny::numericInput(ns("dim_a"), label = "PCA dimension (x-axis)", min = 1, max = 6, step = 1, value = 1),
                       shiny::numericInput(ns("dim_b"), label = "PCA dimension (y-axis)", min = 1, max = 6, step = 1, value = 2)
            ),
            shiny::div(id = ns("guide_grouping"),
                       labelUI(ns("group")),
                       labelUI(ns("group2"))
            )
          ),
          shiny::column(
            width = 4,
            shiny::div(id = ns("guide_pointsize"),
                       shiny::sliderInput(ns("pointsize"), label = "Point size", min = 0.1, max = 10, value = 2),
                       shiny::sliderInput(ns("labelsize"), label = "Label size", min = 1, max = 20, value = 5, round = TRUE)
            ),
            shiny::div(id = ns("guide_color"),
                       colorPickerUI(id = ns("colorPicker"), show.scaleoptions = FALSE, show.transparency = FALSE)
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
#' @param clarion A clarion object. See \code{\link[wilson]{Clarion}}. (Supports reactive)
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
pca <- function(input, output, session, clarion, width = 28, height = 28, ppi = 72, scale = 1) {
  # globals/ initialization #####
  # clear plot
  clear_plot <- shiny::reactiveVal(value = FALSE)
  # disable downloadButton on init
  shinyjs::disable("download")
  # disable plot button on init
  shinyjs::disable("plot")

  # input preparation #####
  object <- shiny::reactive({
    # support reactive
    if (shiny::is.reactive(clarion)) {
      if (!methods::is(clarion(), "Clarion")) shiny::stopApp("Object of class 'Clarion' needed!")

      clarion()$clone(deep = TRUE)
    } else {
      if (!methods::is(clarion, "Clarion")) shiny::stopApp("Object of class 'Clarion' needed!")

      clarion$clone(deep = TRUE)
    }
  })

  # handle reactive sizes
  size <- shiny::reactive({
    width <- ifelse(shiny::is.reactive(width), width(), width)
    height <- ifelse(shiny::is.reactive(height), height(), height)
    ppi <- ifelse(shiny::is.reactive(ppi), ppi(), ppi)
    scale <- ifelse(shiny::is.reactive(scale), scale(), scale)

    if (!is.numeric(width) | width <= 0) {
      width <- 28
    }
    if (!is.numeric(height) | height <= 0) {
      height <- 28
    }
    if (!is.numeric(ppi) | ppi <= 0) {
      ppi <- 72
    }

    list(width = width,
         height = height,
         ppi = ppi,
         scale = scale)
  })

  # modules/ ui #####
  columns <- shiny::callModule(columnSelector, "select", type.columns = shiny::reactive(object()$metadata[level != "feature", intersect(names(object()$metadata), c("key", "level", "label", "sub_label")), with = FALSE]), column.type.label = "Column types to choose from")
  factor_data <- shiny::callModule(label, "group", label = "Select color grouping factors", data = shiny::reactive(object()$get_factors()[key %in% columns$selected_columns(), !"key"]), unique = FALSE)
  factor_data2 <- shiny::callModule(label, "group2", label = "Select shape grouping factors", data = shiny::reactive(object()$get_factors()[key %in% columns$selected_columns(), !"key"]), unique = FALSE)
  color <- shiny::callModule(colorPicker, "colorPicker",  distribution = "categorical", selected = "Dark2")

  # update dimension inputs
  shiny::observe({
    col_num <- length(shiny::req(columns$selected_columns()))

    if (col_num >= 3) {
      value_a <- ifelse(col_num <= input$dim_a, col_num - 1, input$dim_a)
      value_b <- ifelse(col_num <= input$dim_b, col_num - 1, input$dim_b)

      shiny::updateNumericInput(session = session, inputId = "dim_a", max = col_num - 1, value = value_a)
      shiny::updateNumericInput(session = session, inputId = "dim_b", max = col_num - 1, value = value_b)
    }
  })

  # functionality/ plotting #####
  # reset ui
  shiny::observeEvent(input$reset, {
    log_message("PCA: reset", "INFO", token = session$token)

    shinyjs::reset("label")
    shinyjs::reset("dim_a")
    shinyjs::reset("dim_b")
    shinyjs::reset("pointsize")
    shinyjs::reset("labelsize")
    columns <<- shiny::callModule(columnSelector, "select", type.columns = shiny::reactive(object()$metadata[level != "feature", intersect(names(object()$metadata), c("key", "level", "label", "sub_label")), with = FALSE]), column.type.label = "Column types to choose from")
    factor_data <<- shiny::callModule(label, "group", label = "Select color grouping factors", data = shiny::reactive(object()$get_factors()[key %in% columns$selected_columns(), !"key"]), unique = FALSE)
    factor_data2 <<- shiny::callModule(label, "group2", label = "Select shape grouping factors", data = shiny::reactive(object()$get_factors()[key %in% columns$selected_columns(), !"key"]), unique = FALSE)
    color <<- shiny::callModule(colorPicker, "colorPicker",  distribution = "categorical", selected = "Dark2")
    clear_plot(TRUE)
  })

  result_data <- shiny::reactive({
    #new progress indicator
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(0.2, message = "Select data")

    selected <- object()$data[, c(object()$get_id(), columns$selected_columns()), with = FALSE]

    progress$set(1)

    return(selected)
  })

  plot <- shiny::eventReactive(input$plot, {
    log_message("PCA: computing plot...", "INFO", token = session$token)

    # enable downloadButton
    shinyjs::enable("download")
    clear_plot(FALSE)

    #new progress indicator
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(0.2, message = "Render plot")

    plot <- create_pca(
               data = result_data(),
               color.group = factor_data()$label,
               color.title = paste0(factor_data()$selected, collapse = ", "),
               palette = color()$palette,
               shape.group = factor_data2()$label,
               shape.title = paste0(factor_data2()$selected, collapse = ", "),
               dimension.a = input$dim_a,
               dimension.b = input$dim_b,
               dimensions = length(columns$selected_columns()) - 1,
               pointsize = input$pointsize,
               labelsize = input$labelsize,
               labels = input$label,
               custom.labels = columns$label(),
               on.columns = TRUE,
               width = size()$width,
               height = size()$height,
               ppi = size()$ppi,
               scale = size()$scale
    )

    progress$set(1)

    log_message("PCA: done.", "INFO", token = session$token)

    return(plot)
  })

  # render plot #####
  # get width in pixel
  plot_width <- shiny::reactive({
    width <- plot()$width * (plot()$ppi / 2.54)
    ifelse(width < 50, 50, width)
  })

  # get height in pixel
  plot_height <- shiny::reactive({
    height <- plot()$height * (plot()$ppi / 2.54)
    ifelse(height < 50, 50, height)
  })

  output$pca <- shiny::renderPlot(
    width = plot_width,
    height = plot_height,
    {
      if (clear_plot()) {
        return()
      } else {
        log_message("PCA: render plot", "INFO", token = session$token)

        plot()$plot
      }
    })

  # group data by dimension
  reorganized_data <- shiny::reactive({
    sapply(colnames(plot()$data$var$coord), USE.NAMES = TRUE, simplify = FALSE, function(dim) {
      sapply(plot()$data$var, function(table) {
        table[, dim]
      })
    })
  })

  # download #####
  output$download <- shiny::downloadHandler(filename = "pca.zip",
                                            content = function(file) {
                                              log_message("PCA: download", "INFO", token = session$token)
                                              download(file = file, filename = "pca.zip", plot = plot()$plot, width = plot_width() / (plot()$ppi / 2.54), height = plot_height() / (plot()$ppi / 2.54), ppi = plot()$ppi, ui = user_input())
                                            })

  user_input <- shiny::reactive({
    # format selection
    selection <- list(
      data = list(type = columns$type(), selectedColumns = columns$selected_columns()),
      dimensions = list(xaxis = input$dim_a, yaxis = input$dim_b),
      colorGrouping = factor_data()$selected,
      shapeGrouping = factor_data2()$selected
    )

    # format options
    options <- list(
      show_label = input$label,
      pointsize = input$pointsize,
      labelsize = input$labelsize,
      colorOptions = list(scheme = color()$name, reverse = color()$reverse)
    )

    # merge all
    list(selection = selection, options = options)
  })

  # notifications #####
  # invalid dimension/ insufficient data warnings
  # enable/ disable plot button
  shiny::observe({
    shinyjs::enable("plot")

    # no selection
    if (!shiny::isTruthy(columns$selected_columns())) {
      shinyjs::disable("plot")
    } else {
      col_num <- length(columns$selected_columns())
      # insufficient data
      if (col_num < 3 || nrow(shiny::isolate(object()$data)) < 3) {
        shinyjs::disable("plot")
        shiny::showNotification(
          ui = "Not enough columns/ rows selected! At least 3 of each needed for plotting.",
          id = session$ns("data"),
          type = "warning"
        )
      } else {
        shiny::removeNotification(session$ns("data"))
      }

      # invalid dimension
      if (col_num >= 3 && (is.na(input$dim_a) || is.na(input$dim_b) || input$dim_a <= 0 || input$dim_a >= col_num || input$dim_b <= 0 || input$dim_b >= col_num)) {
        shinyjs::disable("plot")
        shiny::showNotification(
          ui = "Invalid dimension(s)! Please select an integer value between 1 and number of selected columns - 1.",
          id = session$ns("dimension"),
          type = "warning"
        )
      } else {
        shiny::removeNotification(session$ns("dimension"))
      }
    }
  })

  # warning if plot size exceeds limits
  shiny::observe({
    if (plot()$exceed_size) {
      shiny::showNotification(
        ui = "Width and/ or height exceed limit. Using 500 cm instead.",
        id = session$ns("limit"),
        type = "warning"
      )
    } else {
      shiny::removeNotification(session$ns("limit"))
    }
  })

  guide <- pcaGuide(session)
  shiny::observeEvent(input$guide, {
    rintrojs::introjs(session, options = list(steps = guide()))
  })

  return(reorganized_data)
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
    "guide_grouping" = "<h4>Grouping</h4>
    Use the provided factor(s) to show groups based on colors and/ or shapes of the datapoints.<br/>
    Multi-factor selections will be merged and evaluated as a single factor.",
    "guide_pointsize" = "<h4>Additional options</h4>
    You can increase or decrease the point size by dragging the slider to the right or to the left. The same goes for the label size and it's respecting slider.",
    "guide_color" = "<h4>Color palettes</h4>
    Choose a color palette used for color grouping. The selected pallette can also be reversed.",
    "guide_buttons" = "<h4>Create the plot</h4>
    As a final step, a click on the 'Plot' button will render the plot, while a click on the 'Reset' button will reset the parameters to default."
  )

  shiny::reactive(data.frame(element = paste0("#", session$ns(names(steps))), intro = unlist(steps)))
}
