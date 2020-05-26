#' Download and install Wilson Apps
#'
#' @param location Where the app should be installed. Default is current location.
#' @param remove_data If TRUE demo data will be deleted.
#' @param start_after_install Start the app when done installing.
#' @param app_name Select app to install.
#' @param repository Link to the repository that holds the apps.
#'
#' @details  Will create a folder named after parameter app_name.
#'
#' @export
install_app <- function(location = ".", remove_data = FALSE, start_after_install = FALSE, app_name = "wilson-basic", repository = "https://github.molgen.mpg.de/loosolab/wilson-apps") {
  if (!requireNamespace("utils", quietly = TRUE)) {
    stop("Package utils required for this function. Please install it.")
  }

  former_wd <- getwd()
  on.exit(setwd(former_wd))
  # change to where the app should be installed
  setwd(location)

  # check if dir exists ONLY in interactive mode
  if (interactive() && dir.exists(file.path(getwd(), app_name))) {
    answer <- utils::menu(choices = c("Yes", "No"), title = "App folder already exists! Proceeding will delete all of its contents. Proceed anyway?")

    if (answer == 1) {
      # delete dir
      unlink(app_name, recursive = TRUE)
    } else {
      return(message("Installation aborted."))
    }
  }

  # create app dir
  dir.create(file.path(app_name, "tmp"), recursive = TRUE)
  setwd(app_name)
  # download from repo
  utils::download.file(url = paste0(repository, "/archive/master.zip"), destfile = file.path("tmp", "repository.zip"))
  # extract zip
  zip::unzip(zipfile = file.path("tmp", "repository.zip"), exdir = "tmp")

  # get specified app
  folder_name <- paste0(basename(repository), "-master")
  tmp_path <- file.path("tmp", folder_name, app_name)
  if (!dir.exists(tmp_path)) {
    stop("App with name ", app_name, " does not exist!")
  }

  app_files <- list.files(tmp_path)
  # copy app files
  file.copy(from = file.path(tmp_path, app_files), to = ".", recursive = TRUE)

  # clean
  unlink("tmp", recursive = TRUE)

  if (remove_data) {
    file.remove(list.files("data", full.names = TRUE))
  }

  message("App successfully installed.")
  message("App folder: ", file.path(getwd()))
  message("Use your data by adding it to ", file.path(app_name, "data"))
  message("\nStart app by running app.R e.g. shiny::runApp(\"", app_name, "\")")

  if (start_after_install) {
    shiny::runApp(".")
  }
}
