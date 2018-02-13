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
                         shiny::selectInput(ns("groupby"), label = "by", choices = c("gene", "condition"))
              )
            ),
            shiny::column(
              width = 3,
              shiny::div(id = ns("guide_type"),
                         shiny::selectInput(ns("plotType"), label = "Type of Plot", choices = c("box", "line", "violin", "bar"), selected = "line")),
              shiny::div(id = ns("guide_transformation"),
                         transformationUI(id = ns("transform"), choices = list(`None` = "raw", `log2` = "log2", `-log2` = "-log2", `log10` = "log10", `-log10` = "-log10", `Z score` = "zscore")),
                         shiny::textInput(ns("label"), label = "Y-Axis Label")),
              shiny::div(id = ns("guide_limit"),
                         limitUI(id = ns("limit"), label = "Y-Axis Limit"))
            ),
            shiny::column(
              width = 3,
              shiny::div(id = ns("guide_color"),
                         colorPicker2UI(id = ns("color"), show.transparency = FALSE, show.scaleoptions = FALSE)),
              shiny::div(id = ns("guide_plotColumns"),
                         shiny::sliderInput(ns("plotColumns"), label = "Plot Columns", min = 1, max = 7, value = plot.columns, step = 1))
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
#' @param data data.table:
#'                        column1   : ids
#'                        column2   : symbol (data used for selection)
#'                        column3-n : data
#' @param metadata data.table:
#'                            column1: ids
#'                            column2: factor1 (conditions)
#'                            column3: level (condition type)
#' @param level Vector containing data levels to select from (default: unique(metadata[["level"]])).
#' @param plot.method Choose which method is used for plotting. Either "static" or "interactive" (Default = "static").
#' @param custom.label Data.table used for creating custom labels (supports reactive).
#' @param label.sep Seperator used for label merging (Default = ", ").
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

geneView <- function(input, output, session, data, metadata, level = NULL, plot.method = "static", custom.label = NULL, label.sep = ", ", width = "auto", height = "auto", ppi = 72, scale = 1){
  #handle reactive data
  data.r <- shiny::reactive({
    if(shiny::is.reactive(data)){
      data.table::copy(data())
    }else{
      data.table::copy(data)
    }
  })
  metadata.r <- shiny::reactive({
    if(shiny::is.reactive(metadata)){
      metadata()
    }else{
      metadata
    }
  })
  level.r <- shiny::reactive({
    if(is.null(level)){
      metadata[[3]]
    }else if(shiny::is.reactive(level)){
      level()
    }else{
      level
    }
  })
  # handle reactive sizes
  size <- shiny::reactive({
    width <- ifelse(shiny::is.reactive(width), width(), width)
    height <- ifelse(shiny::is.reactive(height), height(), height)
    ppi <- ifelse(shiny::is.reactive(ppi), ppi(), ppi)
    scale <- ifelse(shiny::is.reactive(scale), scale(), scale)

    if(!is.numeric(width) || width <= 0) {
      width <- "auto"
    }
    if(!is.numeric(height) || height <= 0) {
      height <- "auto"
    }
    if(!is.numeric(ppi) || ppi <= 0) {
      ppi <- 72
    }

    list(width = width,
         height = height,
         ppi = ppi,
         scale = scale)
  })

  #Fetch the reactive guide for this module
  guide <- geneViewGuide(session, label = !is.null(custom.label))
  shiny::observeEvent(input$guide, {
    rintrojs::introjs(session, options = list(steps = guide()))
  })

  # clear plot
  clearPlot <- shiny::reactiveVal(FALSE)

  # reset
  shiny::observeEvent(input$reset, {
    log_message("GeneView: reset", "INFO", token = session$token)

    shinyjs::reset("genes")
    shinyjs::reset("plotType")
    shinyjs::reset("groupby")
    shinyjs::reset("plotColumns")
    colorPicker <<- shiny::callModule(colorPicker2, "color", distribution = "all", selected = "Dark2")
    transform <<- shiny::callModule(transformation, "transform", shiny::reactive(as.matrix(data.r()[, selector$selectedColumns(), with = FALSE])))
    selector <<- shiny::callModule(columnSelector, "selector", type.columns = shiny::reactive(metadata.r()[level %in% level.r(), c(1, 3)]), columnTypeLabel = "Select Columns")
    if(!is.null(custom.label)) {
      custom_label <<- shiny::callModule(label, "labeller", data = custom.label, sep = label.sep)
    }
    limiter <<- shiny::callModule(limit, "limit", lower = shiny::reactive(get_limits()[1]), upper = shiny::reactive(get_limits()[2]))
    clearPlot(TRUE)
  })

  get_limits <- shiny::reactive({
    equalize(result.data()$data[, c(-1, -2)])
  })

  colorPicker <- shiny::callModule(colorPicker2, "color", distribution = "all", selected = "Dark2")
  transform <- shiny::callModule(transformation, "transform", shiny::reactive(as.matrix(data.r()[, selector$selectedColumns(), with = FALSE])))
  selector <- shiny::callModule(columnSelector, "selector", type.columns = shiny::reactive(metadata.r()[level %in% level.r(), c(1, 3)]), columnTypeLabel = "Select Columns")
  if(!is.null(custom.label)) {
    custom_label <- shiny::callModule(label, "labeller", data = custom.label, sep = label.sep)
  }
  limiter <- shiny::callModule(limit, "limit", lower = shiny::reactive(get_limits()[1]), upper = shiny::reactive(get_limits()[2]))

  output$genes <- shiny::renderUI({
    output <- shiny::selectizeInput(session$ns("genes"), label = "Select Genes", choices = NULL, multiple = TRUE)
    #only fetch needed data (calculation on server-side)
    shiny::updateSelectizeInput(session, "genes", choices = unique(data.r()[[2]]), server = TRUE)

    # colored if not has item
    output <- shiny::div(class = "empty", output)

    return(output)
  })

  output$level <- shiny::renderUI({
    shiny::selectInput(session$ns("level"), label = "Data level", choices = unique(level.r()))
  })

  shiny::observe({
    shiny::updateTextInput(session = session, inputId = "label", value = transform$method())
  })

  #notification
  shiny::observe({
    shiny::req(input$genes)

    if(length(input$genes) > 50){
      shiny::showNotification(
        paste("Caution! You selected", length(input$genes), "genes. This may take a while to compute."),
        duration = 5,
        type = "warning",
        id = "warning",
        closeButton = FALSE
      )
    }else{
      shiny::removeNotification("warning")
    }
  })

  # warning if plot size exceeds limits
  shiny::observe({
    if(plot()$exceed_size) {
      shiny::showNotification(
        ui = "Width and/ or height exceed limit. Using 500 cm instead.",
        id = "limit",
        type = "warning"
      )
    } else {
      shiny::removeNotification("limit")
    }
  })

  result.data <- shiny::eventReactive(input$plot, {
    result <- data.table::data.table(data.r()[, c(1, 2)], data.table::as.data.table(transform$data()))

    # label selected?
    if(!is.null(custom.label)) {
      # drop not selected
      label <- custom_label()$label[which(result[[2]] %in% input$genes)]
    } else {
      label <- NULL
    }

    result <- result[result[[2]] %in% input$genes]

    return(list(data = result, label = label))
  })

  # disable downloadButton on init
  shinyjs::disable("download")

  plot <- shiny::eventReactive(input$plot, {
    log_message("GeneView: computing plot...", "INFO", token = session$token)

    # enable downloadButton
    shinyjs::enable("download")
    clearPlot(FALSE)

    #new progress indicator
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(0, message = "Computing data")

    processed.data <- data.table::copy(result.data()$data)

    #extract symbol
    processed.data <- processed.data[, -2]

    progress$set(0.33, message = "Calculating plot")

    #plot
    plot <- create_geneview(
      data = processed.data,
      grouping = metadata.r()[level == selector$type() & key %in% selector$selectedColumns(), c(1, 2)],
      plot.type = input$plotType,
      facet.target = input$groupby,
      facet.cols = input$plotColumns,
      colors = colorPicker()$palette,
      ylabel = input$label,
      gene.label = result.data()$label,
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

  #enable plot button only if plot possible
  shiny::observe({
    if(is.null(input$genes) | length(selector$selectedColumns()) < 1){
      shinyjs::disable("plot")
    }else if(input$plotType == "violin"){
      factor1.levels <- metadata.r()[level == selector$type() & key %in% selector$selectedColumns() & factor1 != ""][, .N, keyby = factor1][["N"]]

      if(input$groupby == "condition"){
        #every level >= 3 times
        factor1.levels <- ifelse(length(factor1.levels) > 0, factor1.levels, FALSE)
        if(all(factor1.levels >= 3)){
          shinyjs::enable("plot")
        }else{
          shinyjs::disable("plot")
        }
      }else if(input$groupby == "gene"){
        #at least one level >= 3 times
        if(any(factor1.levels >= 3)){
          shinyjs::enable("plot")
        }else{
          shinyjs::disable("plot")
        }
      }
    }else{
      shinyjs::enable("plot")
    }
  })

  output$geneView <- shiny::renderUI({
    if(plot.method == "interactive"){
      shinycssloaders::withSpinner(plotly::plotlyOutput(session$ns("interactive")), proxy.height = "800px")
    }else if (plot.method == "static"){
      shinycssloaders::withSpinner(shiny::plotOutput(session$ns("static")), proxy.height = "800px")
    }
  })

  if(plot.method == "interactive") {
    output$interactive <- plotly::renderPlotly({
      if(clearPlot()) {
        return()
      } else {
        log_message("GeneView: render plot interactive", "INFO", token = session$token)

        #progress indicator
        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message = "Rendering plot", value = 0)

        plot <- plot()$plot

        progress$set(value = 1)
        return(plot)
      }
    })
  } else if(plot.method == "static") {
    output$static <- shiny::renderPlot(
      width = shiny::reactive(plot()$width * (plot()$ppi / 2.54)),
      height = shiny::reactive(plot()$height * (plot()$ppi / 2.54)),
      {
        if(clearPlot()) {
          return()
        } else {
          log_message("GeneView: render plot static", "INFO", token = session$token)

          #progress indicator
          progress <- shiny::Progress$new()
          on.exit(progress$close())
          progress$set(message = "Rendering plot", value = 0.3)

          plot <- plot()$plot

          progress$set(value = 1)
          return(plot)
        }
      })
  }

  output$download <- shiny::downloadHandler(filename = "geneView.zip",
                                            content = function(file) {
                                              log_message("GeneView: download", "INFO", token = session$token)

                                              download(file = file, filename = "geneView.zip", plot = plot()$plot, width = plot()$width, height = plot()$height, ppi = plot()$ppi, ui = user_input())
                                            })


  user_input <- shiny::reactive({
    # format data
    data <- list(
      genes = input$genes,
      columns = list(type = selector$type(), selectedColumns = selector$selectedColumns()),
      groupby = input$groupby
    )

    # format options
    if(!is.null(custom.label)) {
      label <- custom_label()$selected
    } else {
      label <- NULL
    }
    options <- list(
      plot_type = input$plotType,
      transformation = transform$method(),
      yaxis_label = input$label,
      yaxis_limit = limiter(),
      plot_column = input$plotColumns,
      colors = list(scheme = colorPicker()$name, reverse = colorPicker()$reverse),
      custom_label = label
    )

    # merge all
    all <- list(data = data, options = options)
  })

  return(shiny::reactive(result.data()$data))
}

#' geneView module guide
#'
#' @param session The shiny session
#' @param label Boolean to show custom label step.
#'
#' @return A shiny reactive that contains the texts for the Guide steps.
#'
geneViewGuide <- function(session, label = FALSE) {
  steps <- list(
    "guide_geneSelection" = "<h4>Gene selection</h4>
      Select genes to be displayed.",
    "guide_columnSelection" = "<h4>Column selection</h4>
      First select a column type for visualization, then select individual columns from all columns of the chosen type.<br/>
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

  if(label) {
    steps <- append(steps, values = list("guide_genelabel" = "<h4>Custom label</h4>
      Select one or more columns to be used as a label instead of the names above."),
                    after = 1)
  }

  shiny::reactive(data.frame(element = paste0("#", session$ns(names(steps))), intro = unlist(steps)))
}
