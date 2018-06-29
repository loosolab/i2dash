#' geneView's module UI representation
#'
#' @param id The ID of the modules namespace.
#' @param plot.columns Initial value of plot column slider. Integer value between 1 and 7 (Default = 3).
#'
#' @return A list with HTML tags from \code{\link[shiny]{tag}}.
#'
#' @export
geneViewUI <- function(id, plot.columns = 3){
  ns <- shiny::NS(id)

  shiny::tagList(
    rintrojs::introjsUI(),
    shinyjs::useShinyjs(),
    shiny::singleton(shiny::tags$head(shiny::tags$link(rel = "stylesheet", type = "text/css", href = "wilson_www/styles.css"))),
    shiny::fluidPage(
      shiny::fluidRow(
        shinydashboard::box(
          width = 12,
          shiny::div(style = "overflow-y: scroll; overflow-x: scroll; height: 800px; text-align: center",
            shiny::uiOutput(ns("geneView"))
          )
        )
      ),
      shiny::fluidRow(
        shinydashboard::box(
          width = 12,
          collapsible = TRUE,
          shiny::fluidRow(
            shiny::column(
              width = 3,

              shiny::div(id = ns("guide_geneSelection"),
                         shiny::uiOutput(ns("genes"))),
              shiny::div(id = ns("guide_genelabel"),
                         labelUI(ns("labeller")))
            ),
            shiny::column(
              width = 3,
              shiny::div(id = ns("guide_columnSelection"),
                         columnSelectorUI(ns("selector"), title = "Grouping:"),
                         labelUI(ns("group")),
                         shiny::selectInput(ns("groupby"), label = "by", choices = c("gene", "condition"))
              )
            ),
            shiny::column(
              width = 3,
              shiny::div(id = ns("guide_type"),
                         shiny::selectInput(ns("plot_type"), label = "Type of Plot", choices = c("box", "line", "violin", "bar"), selected = "line")),
              shiny::div(id = ns("guide_transformation"),
                         transformationUI(id = ns("transform"), choices = list(`None` = "raw", `log2` = "log2", `-log2` = "-log2", `log10` = "log10", `-log10` = "-log10", `Z score` = "zscore")),
                         shiny::textInput(ns("label"), label = "Y-Axis Label")),
              shiny::div(id = ns("guide_limit"),
                         limitUI(id = ns("limit"), label = "Y-Axis Limit"))
            ),
            shiny::column(
              width = 3,
              shiny::div(id = ns("guide_color"),
                         colorPickerUI(id = ns("color"), show.transparency = FALSE, show.scaleoptions = FALSE)),
              shiny::div(id = ns("guide_plotColumns"),
                         shiny::sliderInput(ns("plot_columns"), label = "Plot Columns", min = 1, max = 7, value = plot.columns, step = 1))
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width = 12,
              shiny::div(id = ns("guide_buttons"),
                         shiny::actionButton(ns("plot"), "Plot", style = "color: #fff; background-color: #3c8dbc"),
                         shiny::actionButton(ns("reset"), "Reset", style = "color: #fff; background-color: #3c8dbc"),
                         shiny::actionButton(ns("guide"), "Launch guide", style = "color: #fff; background-color: #3c8dbc", icon = shiny::icon("question-circle")),
                         shiny::downloadButton(outputId = ns("download"), label = "Download")
              )
            )
          )
        )
      )
    )
  )
}
#' geneView's module server logic
#'
#' Provides server logic for the geneView module.
#'
#' @param input Shiny's input object.
#' @param output Shiny's output object.
#' @param session Shiny's session object.
#' @param clarion A clarion object. See \code{\link[wilson]{Clarion}}. (Supports reactive)
#' @param plot.method Choose which method is used for plotting. Either "static" or "interactive" (Default = "static").
#' @param label.sep Separator used for label merging (Default = ", ").
#' @param width Width of the plot in cm. Defaults to minimal size for readable labels and supports reactive.
#' @param height Height of the plot in cm. Defaults to minimal size for readable labels and supports reactive.
#' @param ppi Pixel per inch. Defaults to 72 and supports reactive.
#' @param scale Scale plot size. Defaults to 1, supports reactive.
#'
#' @details Width/ height/ ppi less or equal to default will use default value.
#' @details Ppi less or equal to zero will use default.
#'
#' @return Reactive containing data.table used for plotting.
#'
#' @export
geneView <- function(input, output, session, clarion, plot.method = "static", label.sep = ", ", width = "auto", height = "auto", ppi = 72, scale = 1){
  # globals/ initialization #####
  clear_plot <- shiny::reactiveVal(FALSE)
  # disable downloadButton on init
  shinyjs::disable("download")

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

    if (!is.numeric(width) || width <= 0) {
      width <- "auto"
    }
    if (!is.numeric(height) || height <= 0) {
      height <- "auto"
    }
    if (!is.numeric(ppi) || ppi <= 0) {
      ppi <- 72
    }

    list(width = width,
         height = height,
         ppi = ppi,
         scale = scale)
  })

  # modules/ ui #####
  color <- shiny::callModule(colorPicker, "color", distribution = "all", selected = "Dark2")
  transform <- shiny::callModule(transformation, "transform", shiny::reactive(as.matrix(object()$data[which(object()$data[[object()$get_name()]] %in% input$genes), selector$selected_columns(), with = FALSE])))
  selector <- shiny::callModule(columnSelector, "selector", type.columns = shiny::reactive(object()$metadata[level != "feature", c("key", "level")]), column.type.label = "Select Columns")
  custom_label <- shiny::callModule(label, "labeller", data = shiny::reactive(object()$data[which(object()$data[[object()$get_name()]] %in% input$genes)]), sep = label.sep)
  factor_data <- shiny::callModule(label, "group", label = "Select grouping factors", data = shiny::reactive(object()$get_factors()[key %in% selector$selected_columns(), !"key"]), sep = label.sep, unique = FALSE)
  limiter <- shiny::callModule(limit, "limit", lower = shiny::reactive(get_limits()[1]), upper = shiny::reactive(get_limits()[2]))

  output$genes <- shiny::renderUI({
    output <- shiny::selectizeInput(session$ns("genes"), label = "Select Genes", choices = NULL, multiple = TRUE)
    # only fetch needed data (calculation on server-side)
    shiny::updateSelectizeInput(session, "genes", choices = unique(object()$data[[object()$get_name()]]), server = TRUE)

    # colored if not has item
    output <- shiny::div(class = "empty", output)

    return(output)
  })

  shiny::observe({
    shiny::updateTextInput(session = session, inputId = "label", value = transform$method())
  })

  output$geneView <- shiny::renderUI({
    if (plot.method == "interactive") {
      shinycssloaders::withSpinner(plotly::plotlyOutput(session$ns("interactive")), proxy.height = "800px")
    } else if (plot.method == "static") {
      shinycssloaders::withSpinner(shiny::plotOutput(session$ns("static")), proxy.height = "800px")
    }
  })

  # functionality/ plotting #####
  # reset
  shiny::observeEvent(input$reset, {
    log_message("GeneView: reset", "INFO", token = session$token)

    shinyjs::reset("genes")
    shinyjs::reset("plot_type")
    shinyjs::reset("groupby")
    shinyjs::reset("plot_columns")
    color <<- shiny::callModule(colorPicker, "color", distribution = "all", selected = "Dark2")
    transform <<- shiny::callModule(transformation, "transform", shiny::reactive(as.matrix(object()$data[which(object()$data[[object()$get_name()]] %in% input$genes), selector$selected_columns(), with = FALSE])))
    selector <<- shiny::callModule(columnSelector, "selector", type.columns = shiny::reactive(object()$metadata[level != "feature", c("key", "level")]), column.type.label = "Select Columns")
    custom_label <<- shiny::callModule(label, "labeller", data = shiny::reactive(object()$data[which(object()$data[[object()$get_name()]] %in% input$genes)]), sep = label.sep)
    factor_data <<- shiny::callModule(label, "group", label = "Select grouping factors", data = shiny::reactive(object()$get_factors()[key %in% selector$selected_columns(), !"key"]), sep = label.sep, unique = FALSE)
    limiter <<- shiny::callModule(limit, "limit", lower = shiny::reactive(get_limits()[1]), upper = shiny::reactive(get_limits()[2]))
    clear_plot(TRUE)
  })

  get_limits <- shiny::reactive({
    equalize(result_data()$data[, c(-1, -2)])
  })

  result_data <- shiny::eventReactive(input$plot, {
    columns <- switch((object()$get_id() == object()$get_name()) + 1,
                      c(object()$get_id(), object()$get_name()),
                      object()$get_id())

    result <- data.table::data.table(object()$data[which(object()$data[[object()$get_name()]] %in% input$genes), columns, with = FALSE], data.table::as.data.table(transform$data()))

    label <- custom_label()$label

    return(list(data = result, label = label))
  })

  plot <- shiny::eventReactive(input$plot, {
    log_message("GeneView: computing plot...", "INFO", token = session$token)

    # enable downloadButton
    shinyjs::enable("download")
    clear_plot(FALSE)

    # new progress indicator
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(0, message = "Computing data")

    progress$set(0.33, message = "Calculating plot")

    # generate groups from selection
    if (is.null(factor_data()$label)) {
      factor <- ""
    } else {
      factor <- factor_data()$label
    }
    grouping <- data.table::data.table(object()$metadata[key %in% selector$selected_columns(), key], factor)

    # plot
    plot <- create_geneview(
      data = if (object()$get_id() == object()$get_name()) result_data()$data else result_data()$data[, -2], # without name column
      grouping = grouping,
      plot.type = input$plot_type,
      facet.target = input$groupby,
      facet.cols = input$plot_columns,
      colors = color()$palette,
      ylabel = input$label,
      gene.label = result_data()$label,
      ylimits = unlist(unname(limiter())),
      plot.method = plot.method,
      width = size()$width,
      height = size()$height,
      ppi = size()$ppi,
      scale = size()$scale
    )

    log_message("GeneView: done.", "INFO", token = session$token)
    progress$set(1, detail = "Return plot")
    return(plot)
  })

  # render plot ######
  if (plot.method == "interactive") {
    output$interactive <- plotly::renderPlotly({
      if (clear_plot()) {
        return()
      } else {
        log_message("GeneView: render plot interactive", "INFO", token = session$token)

        # progress indicator
        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message = "Rendering plot", value = 0)

        plot <- plot()$plot

        progress$set(value = 1)
        return(plot)
      }
    })
  } else if (plot.method == "static") {
    output$static <- shiny::renderPlot(
      width = shiny::reactive(plot()$width * (plot()$ppi / 2.54)),
      height = shiny::reactive(plot()$height * (plot()$ppi / 2.54)),
      {
        if (clear_plot()) {
          return()
        } else {
          log_message("GeneView: render plot static", "INFO", token = session$token)

          # progress indicator
          progress <- shiny::Progress$new()
          on.exit(progress$close())
          progress$set(message = "Rendering plot", value = 0.3)

          plot <- plot()$plot

          progress$set(value = 1)
          return(plot)
        }
      })
  }

  # download #####
  output$download <- shiny::downloadHandler(filename = "geneView.zip",
                                            content = function(file) {
                                              log_message("GeneView: download", "INFO", token = session$token)

                                              download(file = file, filename = "geneView.zip", plot = plot()$plot, width = plot()$width, height = plot()$height, ppi = plot()$ppi, ui = user_input())
                                            })


  user_input <- shiny::reactive({
    # format data
    data <- list(
      genes = input$genes,
      columns = list(type = selector$type(), selectedColumns = selector$selected_columns()),
      group = factor_data()$selected,
      groupby = input$groupby
    )

    # format options
    label <- custom_label()$selected
    options <- list(
      plot_type = input$plot_type,
      transformation = transform$method(),
      yaxis_label = input$label,
      yaxis_limit = limiter(),
      plot_column = input$plot_columns,
      colors = list(scheme = color()$name, reverse = color()$reverse),
      custom_label = label
    )

    # merge all
    list(data = data, options = options)
  })

  # notifications #####
  # enable plot button only if plot possible
  shiny::observe({
    if (is.null(input$genes) || !shiny::isTruthy(selector$selected_columns())) {
      shiny::removeNotification(session$ns("violin"))
      shinyjs::disable("plot")
    } else if (input$plot_type == "violin") {
      factor_levels <- table(droplevels(as.factor(factor_data()$label), exclude = ""))

      if (input$groupby == "condition") {
        # every level >= 3 times
        factor_levels <- ifelse(length(factor_levels) > 0, factor_levels, FALSE)
        if (all(factor_levels >= 3)) {
          shiny::removeNotification(session$ns("violin"))
          shinyjs::enable("plot")
        } else {
          shiny::showNotification(
            paste("Violin plot not feasible. Insufficient data. Please try a boxplot instead."),
            duration = 5,
            type = "warning",
            id = session$ns("violin")
          )
          shinyjs::disable("plot")
        }
      } else if (input$groupby == "gene") {
        # at least one level >= 3 times
        if (any(factor_levels >= 3)) {
          shiny::removeNotification(session$ns("violin"))
          shinyjs::enable("plot")
        } else {
          shiny::showNotification(
            paste("Violin plot not feasible. Insufficient data. Please try a boxplot instead."),
            duration = 5,
            type = "warning",
            id = session$ns("violin")
          )
          shinyjs::disable("plot")
        }
      }
    } else {
      shiny::removeNotification(session$ns("violin"))
      shinyjs::enable("plot")
    }
  })

  # warning for heavy computation
  shiny::observe({
    shiny::req(input$genes)

    if (length(input$genes) > 50) {
      shiny::showNotification(
        paste("Caution! You selected", length(input$genes), "genes. This may take a while to compute."),
        duration = 5,
        type = "warning",
        id = session$ns("warning")
      )
    }else{
      shiny::removeNotification(session$ns("warning"))
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
  # Fetch the reactive guide for this module
  guide <- geneViewGuide(session)
  shiny::observeEvent(input$guide, {
    rintrojs::introjs(session, options = list(steps = guide()))
  })

  return(shiny::reactive(result_data()$data))
}

#' geneView module guide
#'
#' @param session The shiny session
#'
#' @return A shiny reactive that contains the texts for the Guide steps.
#'
geneViewGuide <- function(session) {
  steps <- list(
    "guide_geneSelection" = "<h4>Gene selection</h4>
      Select genes to be displayed.",
    "guide_genelabel" = "<h4>Custom label</h4>
      Select one or more columns to be used as a label instead of the names above.",
    "guide_columnSelection" = "<h4>Column selection</h4>
      First select a column type for visualization, then select individual columns from all columns of the chosen type.<br/>
      Second select grouping factor(s). Condense the above selected columns to factor(s). 'None' will result in no grouping, multiple selection in a single merged factor.<br/>
      After that choose a factor by which the given subset is grouped. E.g. 'condition' will generate a plot for each condition or uses the conditions as x-axis ticks, based on the choosen plot type.",
    "guide_type" = "<h4>Plot type</h4>
      Choose the preferred type of plot that will be rendered.",
    "guide_transformation" = "<h4>Data transformation</h4>
      Pick a transformation that you want to apply to your data or leave it as 'None' if no transformation is needed.<br/>
      The y-axis label will be changed according to transformation but can also be custom if wanted.",
    "guide_limit" = "<h4>Y-axis limit</h4>
      Use upper/ lower limit to customize the axis limits.",
    "guide_color" = "<h4>Color palettes</h4>
      Based on the selected data's distribution, one can choose between sequential, categorical or diverging color palettes.<br/>
      The selected palette can additionally be reversed.",
    "guide_plotColumns" = "<h4>Plots per column</h4>
      Select how many plots are displayed in each row or in other words how many columns are used.
      This slider doesn't affect line plots as they are consisting of only one plot.",
    "guide_buttons" = "<h4>Create the plot</h4>
      As a final step, a click on the 'Plot' button will render the plot, while a click on the 'Reset' button will reset the parameters to default."
  )

  shiny::reactive(data.frame(element = paste0("#", session$ns(names(steps))), intro = unlist(steps)))
}
