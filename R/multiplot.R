#' Renders a Sequence saturation plot
#'
#' @param plot_title The title of the Component
#' @param x A list with the x-axis values. If it is a nested list, a dropdown-field will be provided in the interactive mode.
#' @param y A list with the y-axis values. If it is a nested list, a dropdown-field will be provided in the interactive mode.
#' @param color_by A list with the color_by values. If it is a nested list, a dropdown-field will be provided in the interactive mode.
#'
#' @return A string containing markdown code for the rendered textbox
render_multiplot <- function(object, plot_title, x, y, color_by) {

  env_id <- .create_id()
  # validate input, create environment variables, save environment object
  .validate_input(object, env_id, x, y, color_by)
  timestamp <- Sys.time()
  expanded_component <- knitr::knit_expand(file = system.file("templates", "multiplot_template.Rmd", package = "i2dash"), plot_title = plot_title, env_id = env_id, date = timestamp)
  return(expanded_component)
}

.create_id <- function(n = 1) {
  a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
  paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
}

.validate_input <- function(object, env_id, x, y, color_by) {
  env <- new.env()
  env$x_selection <- FALSE
  env$y_selection <- FALSE
  env$color_selection <- FALSE

  # validate x and create environment variables
  if(is.list(x) & length(x) == 1) {
    env$x <- x
  } else if (is.list(x) & length(x) > 1) {
    env$x_selection <- TRUE
    env$x <- x
  } else if (!is.list(x) | (is.list(x) & length(x) == 0)){
    stop("x needs to be a named list with at least one element")
  }

  # validate y and create environment variables
  if(is.list(y) & length(y) == 1) {
    env$y <- y
  } else if (is.list(y) & length(y) > 1) {
    env$y_selection <- TRUE
    env$y <- y
  } else if (!is.list(y) | (is.list(y) & length(y) == 0)){
    stop("y needs to be a named list with at least one element")
  }

  # validate color_by and create environment variables
  if(is.list(color_by) & length(color_by) == 1) {
    env$color_by <- color_by
  } else if (is.list(color_by) & length(color_by) > 1) {
    env$color_selection <- TRUE
    env$color_by <- color_by
  } else if (!is.list(color_by) | (is.list(color_by) & length(color_by) == 0)){
    stop("color_by needs to be a named list with at least one element")
  }
  # for (i in color_by){
  #   if (!is.factor(i)){
  #     stop("color_by needs to be a list with factorial elements")
  #   }
  # }

  # save environment as rds-object
  saveRDS(env, file = file.path(object@workdir, "envs", sprintf("%s.rds", env_id)))
}