
wilson.globals <- new.env(parent = emptyenv())

#' set a log4r logger used within the package
#'
#' @param logger A logger object see \code{\link[log4r]{create.logger}}. NULL to disable logging.
#' @param token Set a unique identifier for this logger.
#'
#' @details This function will save each logger in the wilson.globals environment. Each logger is stored by the name 'logger'[token] (e.g. 'logger6b821824b0b53b1a3e8f531a34d0d6e6').
#' @details Use onSessionEnded to clean up after logging. See \code{\link[shiny]{onFlush}}.
#'
#' @export
set_logger <- function(logger, token = NULL) {
  if (is.null(logger) || methods::is(logger, "logger")) {
    assign(x = paste0("logger", token), value = logger, envir = wilson.globals)
  }
}

#' logger message convenience function
#'
#' @param message String of message to be written in log. See \code{\link[log4r]{levellog}}.
#' @param level Set priority level of the message (number or character). See \code{\link[log4r]{levellog}}.
#' @param token Use token bound to this identifier.
#'
#' @details Does nothing if logger doesn't exist.
#'
log_message <- function(message, level = c("DEBUG", "INFO", "WARN", "ERROR", "FATAL"), token = NULL) {
  if (exists(paste0("logger", token), envir = wilson.globals)) {
    logger <- get(paste0("logger", token), envir = wilson.globals)

    switch(level,
      DEBUG = log4r::debug(logger, message),
      INFO = log4r::info(logger, message),
      WARN = log4r::warn(logger, message),
      ERROR = log4r::error(logger, message),
      FATAL = log4r::fatal(logger, message)
    )
  }
}
