#' colorPicker2 module UI representation
#'
#' The functions creates HTML tag definitions of its representation based on the parameters supplied.
#' Currently, two UI can be created for the user to choose either (a) colors from a given color scheme, or (b) choose one or more single colors.
#'
#' @param id The ID of the modules namespace.
#' @param label Either a character vector of length one with the label for the color scheme dropdown, or a character vector containing labels of the single colors.
#' @param custom Boolean if TRUE custom colors can be selected (Default = FALSE).
#' @param multiple Boolean value, if set to TRUE custom colorpalettes can be made. Only if custom = TRUE (Default = FALSE).
#' @param show.reverse Logical, whether or not to show the reverse switch (Default = TRUE).
#' @param show.scaleoptions Logical, whether or not to show color scaling option winorize (Default = TRUE).
#' @param show.transparency Logical, whether or not to show the transparency slider (Default = TRUE).
#'
#' @return A list with HTML tags from \code{\link[shiny]{tag}}.
#'
#' @export
colorPicker2UI <- function(id, label = "Color scheme", custom = FALSE, multiple = FALSE, show.reverse = TRUE, show.scaleoptions = TRUE, show.transparency = TRUE) {
  ns <- shiny::NS(id)

  if(custom){
    ret <- list(colourpicker::colourInput(ns("picker"), label = NULL, value = "red"))

    if(multiple){
      ret <- list(
        shinyjs::useShinyjs(),
        shinyBS::tipify(shiny::textInput(ns("palette"), label = NULL, value = "red,blue", placeholder = "e.g. black,#3c8dbc"), title = "Comma delimited colors (hex or name)", placement = "right"),
        ret,
        shiny::actionButton(ns("add"), "add", style = "color: #fff; background-color: #3c8dbc"),
        shiny::actionButton(ns("reset"), "reset", style = "color: #fff; background-color: #3c8dbc")
      )
    }

    ret <- list(shiny::tags$b(label), ret)
  }else{
    ret <- list(shiny::tags$b(label), shiny::uiOutput(ns("palette")))
  }

  if(!custom | custom & multiple){
    if(show.reverse) {
      ret <- c(ret, list(shiny::checkboxInput(ns("reverse"), label = "Reverse scheme")))
    }
    if(show.scaleoptions) {
      ret <- c(ret, limitUI(ns("winsorize"), label = "Winsorize to upper/lower"))
    }
    if(show.transparency) {
      ret <- c(ret, list(shiny::sliderInput(ns("transparency"), label = "Transparency", min = 0, max = 1, value = 1)))
    }
  }

  shiny::tagList(ret)
}

#' colorPicker2 module server logic
#'
#' Provides server logic for the colorPicker2 module.
#'
#' @param input Shiny's input object
#' @param output Shiny's output object
#' @param session Shiny's session object
#' @param num.colors Define length of colorpalette vector (Default = 256).
#' @param distribution Decide which palettes are selectable. One or more of list("sequential", "diverging", "categorical"). Defaults to "all" (Supports reactive).
#' @param winsorize Numeric vector of two. Dynamicly change lower and upper limit (supports reactive). Defaults to NULL.
#' @param selected Set the default selected palette.
#'
#' @details A custom colorpalette's return will be NULL if there is something wrong with it.
#' @details equalize will be returned as FALSE if not selected.
#'
#' @return Reactive containing list(palette = c(colors), name = palette_name, transparency = Integer, reverse = Boolean, winsorize = NULL or a two-component vector containing lower and upper limits).
#'
#' @export
colorPicker2 <- function(input, output, session, num.colors = 256, distribution = "all", winsorize = NULL, selected = NULL) {
  Sequential <- sequentialPalettes(num.colors)
  Diverging <- divergingPalettes(num.colors)
  Categorical <- categoricalPalettes(num.colors)

  shinyjs::reset("reverse")
  shinyjs::reset("transparency")

  #handle reactive distribution
  distribution.r <- shiny::reactive({
    if(shiny::is.reactive(distribution)){
      distribution()
    }else{
      distribution
    }
  })

  if(!is.null(winsorize)) {
    # handle reactive winsorize
    winsorize.r <- shiny::reactive({
      if(shiny::is.reactive(winsorize)) {
        winsorize()
      } else {
        winsorize
      }
    })
  }
  limits <- shiny::callModule(limit, "winsorize", lower = if(!is.null(winsorize)){shiny::reactive(winsorize.r()[1])}, upper = if(!is.null(winsorize)){shiny::reactive(winsorize.r()[2])})

  output$palette <- shiny::renderUI({
    choices <- list()

    if("sequential" %in% distribution.r()) choices <- append(choices, list(Sequential = names(Sequential)))
    if("diverging" %in% distribution.r()) choices <- append(choices, list(Diverging = names(Diverging)))
    if("categorical" %in% distribution.r()) choices <- append(choices, list(Categorical = names(Categorical)))
    if(length(distribution.r()) == 1 && distribution.r() == "all") {
      choices <- list(Sequential = names(Sequential), Diverging = names(Diverging), Categorical = names(Categorical))
    }

    shiny::selectInput(session$ns("palette"), label = NULL, choices = choices, selected = selected)
  })

  shiny::observeEvent(input$add, {
    pal <- ifelse(input$palette == "", input$picker, paste(input$palette, input$picker, sep = ","))

    shiny::updateTextInput(session, "palette", value = pal)
  })

  shiny::observeEvent(input$reset, {
    shiny::updateTextInput(session, "palette", value = "")
  })

  #create custom colorpalette
  custom <- shiny::reactive({
    #returns TRUE if String is a valid color
    isColor <- function(x){
      res <- try(grDevices::col2rgb(x),silent=TRUE)
      return(!"try-error"%in%class(res))
    }

    pal <- unlist(strsplit(input$palette, split = ",", fixed = TRUE))

    if(length(pal) != 0){
      valid <- unlist(lapply(pal, isColor))
      if(!all(valid)){
        shiny::showNotification(id = session$ns("notification"), shiny::HTML(paste("<b>ColorPicker</b><br/> Found invalid colors: ", paste(pal[!valid], collapse = ", "))), duration = NULL, type = "warning")
        pal <- NULL
      }else{
        shiny::removeNotification(id = session$ns("notification"))
        pal <- grDevices::colorRampPalette(pal)(num.colors)
      }
    }else{
      shiny::showNotification(id = session$ns("notification"), shiny::HTML("<b>ColorPicker</b><br/> Warning no colors selected!"), duration = NULL, type = "warning")
      pal <- NULL
    }

    return(pal)
  })

  output <- shiny::reactive({
    if(is.null(input$palette)){
      #custom single color
      pal <- input$picker
    }else{
      #predefined palettes
      if(is.null(shiny::isolate(input$picker))){
        #get palette
        if(input$palette %in% names(Sequential)){
          pal <- Sequential[[input$palette]]
        }else if(input$palette %in% names(Diverging)){
          pal <- Diverging[[input$palette]]
        }else {
          pal <- Categorical[[input$palette]]
        }
      }else{
        #custom palettes (multiple colors)
        pal <- custom()
      }
      #reverse palette
      if(!is.null(input$reverse)){
        if(input$reverse){
          pal <- rev(pal)
        }
      }
    }

    winsorize <- NULL
    if(!is.null(limits())) {
      winsorize <- c(limits()$lower, limits()$upper)
    }

    list(
      palette = pal,
      name = input$palette,
      transparency = input$transparency,
      reverse = input$reverse,
      winsorize = winsorize
    )
  })

  return(output)
}

#' Function to generate sequential (one-sided) color palettes (e.g. for expression, enrichment)
#'
#' @param n Number of colors to generate
#'
#' @return A data.table with (named) color palettes of length n
#'
sequentialPalettes <- function(n) {
  Heat <- grDevices::colorRampPalette(rev(RColorBrewer::brewer.pal(9, "YlOrRd")))(n)
  Viridis	<- viridis::viridis(n)
  Magma <- viridis::magma(n)
  Inferno	<- viridis::inferno(n)
  Plasma <- viridis::plasma(n)
  YlGnBu <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "YlGnBu"))(n)
  Blues <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"))(n)
  Reds <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "Reds"))(n)
  Cubehelix	<- rje::cubeHelix(n)

  BkOrYl <- grDevices::colorRampPalette(c("black", "orange", "yellow"))(n)						#one-sided (0 .. x): go enrichment
  GnBu <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "GnBu"))(n)							#one-sided (0 .. x): expression
  PuBuGn <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "PuBuGn"))(n)							#one-sided (0 .. x): expression
  BuGnYlRd <- grDevices::colorRampPalette(c("#000041", "#0000CB", "#0081FF", "#02DA81", "#80FE1A", "#FDEE02", "#FFAB00", "#FF3300"))(n)	#one-sided (0 .. x): expression, ~=spectral

  data.table::data.table(Heat, Viridis, Magma, Inferno, Plasma, YlGnBu, Blues, Reds, Cubehelix, BkOrYl, PuBuGn)
}

#' Function to generate diverging (two-sided) color palettes (e.g. for log2fc, zscore)
#'
#' @param n Number of colors to generate
#'
#' @return A data.table with (named) color palettes of length n
#'
divergingPalettes <- function(n) {
  BuWtRd <- grDevices::colorRampPalette(c("royalblue4", "steelblue4", "white", "indianred", "firebrick4"))(n)
  RdBkGr <- gplots::redgreen(n)
  RdYlGr <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(11, "RdYlGn"))(n)
  YlWtPu <- grDevices::colorRampPalette(c("gold", "white", "white", "mediumpurple4"))(n)					#two-sided (-1 .. +1): correlation
  Spectral <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"))(n)

  BuYlGn <- grDevices::colorRampPalette(c("dodgerblue4", "cadetblue1", "yellow", "darkolivegreen1", "darkgreen"))(n)	#two-sided (-x .. +x): fold-change
  TqWtRd <- grDevices::colorRampPalette(c("darkslategray", "darkturquoise", "cornsilk", "indianred3", "red3"))(n)		#two-sided (-x .. +x): fold-change
  YlGyRd <- grDevices::colorRampPalette(c("yellow", "grey25", "red"))(n)							#two-sided (-x .. +x): fold-change
  RdBu <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "RdBu"))(n)							#two-sided (-x .. +x): fold-change
  GnWtRd <- grDevices::colorRampPalette(c("chartreuse3", "white", "firebrick1"))(n)					#two-sided (-x .. +x): fold-change
  RdYlBu <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(11, "RdYlBu"))(n)							#two-sided (-x .. +x): fold-change
  RdGy <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(11, "RdGy"))(n)							#two-sided (-x .. +x): fold-change
  PuOr <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(11, "PuOr"))(n)							#two-sided (-x .. +x): fold-change

  data.table::data.table(BuWtRd, Spectral, RdBkGr, YlWtPu, BuYlGn, GnWtRd, RdGy, PuOr)
}

#' Function to generate categorical (qualitative) color palettes
#'
#' @param n Number of colors to generate
#'
#' @return A data.table with (named) color palettes of length n
#'
categoricalPalettes <- function(n) {
  Accent <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Accent"))(n)
  Dark2 <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(n)
  Paired <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(12, "Paired"))(n)
  Pastel1 <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "Pastel1"))(n)
  Pastel2 <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Pastel2"))(n)
  Set1 <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))(n)
  Set2 <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(n)
  Set3 <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))(n)

  data.table::data.table(Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3)
}

