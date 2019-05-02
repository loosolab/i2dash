#' Renders a features by factor violin plot
#'
#' @param plot_title The title of the Component
#' @param x A list with the x-axis values. If it is a nested list, a dropdown-field will be provided in the interactive mode.
#' @param y A list with the y-axis values. If it is a nested list, a dropdown-field will be provided in the interactive mode.(Needs to be categorial. Horizontal violinplots are not possible.)
#'
#' @return A string containing markdown code for the rendered textbox
render_features_by_factors <- function(object, plot_title, x, y) {

  env_id <- .create_id()
  # validate input, create environment variables, save environment object
  .validate_input_ff(object, env_id, x, y)
  timestamp <- Sys.time()
  expanded_component <- knitr::knit_expand(file = system.file("templates", "features_by_factors_template.Rmd", package = "i2dash"), plot_title = plot_title, env_id = env_id, date = timestamp)
  return(expanded_component)
}

.create_id <- function(n = 1) {
  a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
  paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
}

.validate_input_ff <- function(object, env_id, x, y) {
  env <- new.env()
  env$x_selection <- FALSE
  env$y_selection <- FALSE

  # validate x and create environment variables
  if(is.list(x) & length(x) == 1) {
    if (is.factor(x[[1]])){
      env$x <- x
    } else {
      stop("x should contain factors")
    }

  } else if (is.list(x) & length(x) > 1) {
    for (i in length(x)){
      if (is.factor(x[[i]])){
        env$x_selection <- TRUE
        env$x <- x
      } else {
        stop("x should contain only factors")
      }
    }
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

  # save environment as rds-object
  saveRDS(env, file = file.path(object@workdir, "envs", sprintf("%s.rds", env_id)))
}