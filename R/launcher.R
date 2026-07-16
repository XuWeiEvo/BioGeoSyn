#' Create a Windows double-click launcher
#'
#' Writes a small `.bat` file that starts the installed Shiny interface without
#' requiring the user to open RStudio or type `launch_app()`. The launcher still
#' requires R, `BioGeoSyn`, Shiny, and BioGeoBEARS to be installed on the
#' user's machine for real analyses.
#'
#' @param path Output path for the launcher. When `NULL`, the launcher is
#'   written to the current user's Desktop on Windows, or to the working
#'   directory on other platforms.
#' @param overwrite Logical. If `FALSE`, stop when `path` already exists.
#' @return The normalized path to the launcher file.
#' @export
create_windows_launcher <- function(path = NULL, overwrite = FALSE) {
  if (is.null(path)) {
    path <- default_windows_launcher_path()
  }
  path <- path.expand(path)
  if (dir.exists(path)) {
    path <- file.path(path, "start-BioGeoSyn.bat")
  }
  if (!grepl("\\.(bat|cmd)$", path, ignore.case = TRUE)) {
    stop("Launcher path must end in .bat or .cmd.", call. = FALSE)
  }
  if (file.exists(path) && !isTRUE(overwrite)) {
    stop("Launcher already exists: ", path, call. = FALSE)
  }

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  template <- windows_launcher_template_path()
  ok <- file.copy(template, path, overwrite = TRUE)
  if (!isTRUE(ok)) {
    stop("Failed to write launcher: ", path, call. = FALSE)
  }
  as_path(path)
}

default_windows_launcher_path <- function() {
  user_profile <- Sys.getenv("USERPROFILE", unset = "")
  desktop <- if (nzchar(user_profile)) file.path(user_profile, "Desktop") else ""
  if (.Platform$OS.type == "windows" && nzchar(desktop) && dir.exists(desktop)) {
    return(file.path(desktop, "start-BioGeoSyn.bat"))
  }
  file.path(getwd(), "start-BioGeoSyn.bat")
}

windows_launcher_template_path <- function() {
  template <- system.file(
    "launcher",
    "start-BioGeoSyn.bat",
    package = "BioGeoSyn",
    mustWork = FALSE
  )
  if (!nzchar(template) || !file.exists(template)) {
    template <- file.path("inst", "launcher", "start-BioGeoSyn.bat")
  }
  if (!file.exists(template)) {
    stop("Windows launcher template was not found.", call. = FALSE)
  }
  template
}
