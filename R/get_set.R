#' Get/set the interactivity of the i2dashboard.
#'
#' @param dashboard A \linkS4class{i2dash::i2dashboard}.
#' @param value The value of the desired property.
#'
#' @name i2dashboard-class
#' @rdname i2dashboard-class
setMethod("interactivity", "i2dashboard", function(dashboard) dashboard@interactive)

#' @name i2dashboard-class
#' @rdname i2dashboard-class
setMethod("interactivity<-", "i2dashboard", function(dashboard, value) {
  dashboard@interactive <- value
  dashboard
})

#' Get/set the title of the i2dashboard.
#'
#' @param dashboard A \linkS4class{i2dash::i2dashboard}.
#' @param value The value of the desired property.
#'
#' @name i2dashboard-class
#' @rdname i2dashboard-class
setMethod("title", "i2dashboard", function(dashboard) dashboard@title)

#' @name i2dashboard-class
#' @rdname i2dashboard-class
setMethod("title<-", "i2dashboard", function(dashboard, value) {
  dashboard@title <- value
  dashboard
})

#' Get/set the author of the i2dashboard.
#'
#' @param dashboard A \linkS4class{i2dash::i2dashboard}.
#' @param value The value of the desired property.
#'
#' @name i2dashboard-class
#' @rdname i2dashboard-class
setMethod("author", "i2dashboard", function(dashboard) dashboard@author)

#' @name i2dashboard-class
#' @rdname i2dashboard-class
setMethod("author<-", "i2dashboard", function(dashboard, value) {
  dashboard@author <- value
  dashboard
})

#' Get/set the theme of the i2dashboard.
#'
#' @param dashboard A \linkS4class{i2dash::i2dashboard}.
#' @param value The value of the desired property.
#'
#' @name i2dashboard-class
#' @rdname i2dashboard-class
setMethod("theme", "i2dashboard", function(dashboard) dashboard@theme)

#' @name i2dashboard-class
#' @rdname i2dashboard-class
setMethod("theme<-", "i2dashboard", function(dashboard, value) {
  dashboard@theme <- value
  dashboard
})

#' Get/set the datadir of the i2dashboard.
#'
#' @param dashboard A \linkS4class{i2dash::i2dashboard}.
#' @param value The value of the desired property.
#'
#' @name i2dashboard-class
#' @rdname i2dashboard-class
setMethod("datadir", "i2dashboard", function(dashboard) dashboard@datadir)

#' @name i2dashboard-class
#' @rdname i2dashboard-class
setMethod("datadir<-", "i2dashboard", function(dashboard, value) {
  dashboard@datadir <- value
  dashboard
})

#' Get/set the links to be shown for sharing on social media. Any of the following services are allowed: “facebook”, “twitter”, “google-plus”, “linkedin”, and “pinterest”. You can also specify “menu” to provide a generic sharing drop-down menu that includes all of the services.
#'
#' @param dashboard A \linkS4class{i2dash::i2dashboard}.
#' @param value The value of the desired property.
#'
#' @name i2dashboard-class
#' @rdname i2dashboard-class
setMethod("social_links", "i2dashboard", function(dashboard) dashboard@social_links)

#' @name i2dashboard-class
#' @rdname i2dashboard-class
setMethod("social_links<-", "i2dashboard", function(dashboard, value) {
  i <- intersect(tolower(value), c("facebook", "twitter", "google-plus", "linkedin", "pinterest", "menu"))
  if (length(i) > 0) {
    dashboard@social <- i
  }
  dashboard
})

#' Get/set the embedding of the source code of the i2dashboard. Can either be a URL pointing to where the source code can be found online or whether or not to embed the source code into the document.
#'
#' @param dashboard A \linkS4class{i2dash::i2dashboard}.
#' @param value The value of the desired property. A URL pointing to where the source code can be found online.
#'
#' @name i2dashboard-class
#' @rdname i2dashboard-class
setMethod("source", "i2dashboard", function(dashboard) dashboard@source)

#' @name i2dashboard-class
#' @rdname i2dashboard-class
setMethod("source<-", "i2dashboard", function(dashboard, value) {
  dashboard@source <- tolower(as.character(value))
  dashboard
})

#' @name i2dashboard-class
#' @rdname i2dashboard-class
setMethod("embed_source<-", "i2dashboard", function(dashboard, value) {
  if(value) {
    dashboard@source <- "embed"
  } else {
    dashboard@source <- ""
  }
  dashboard
})

#' Add a link to the navigation bar.
#'
#' @param dashboard A \linkS4class{i2dash::i2dashboard}.
#' @param href The target of the link.
#' @param title The link title.
#' @param icon An optional link icon (see https://rmarkdown.rstudio.com/flexdashboard/using.html#icon-sets)
#' @param align Optional argument that can be “left” or “right” (defaults = “right”) defining the alignment of the links in the navigation bar
#' @param target An optional target (e.g. "_blank")
#'
#' @rdname i2dashboard-methods
#' @export
setMethod("add_link", "i2dashboard", function(dashboard, href, title = NULL, icon = NULL, align = c("right","left"), target = NULL) {
  align <- match.arg(align)
  if(is.null(title) & is.null(icon)) {
    warning("Both, title and icon, cannot be NULL when adding a link.")
    return(dashboard)
  }

  # Workaround for NULL values
  if(is.null(icon)) {
    icon <- ""
  }
  if(is.null(title)) {
    title = ""
  }
  if(is.null(target)) {
    target = ""
  }

  dashboard@navbar <- append(dashboard@navbar, list(list("href" = href, "title" = title, "icon" = icon, "align" = align, "target" = target)))
  dashboard
})

