#' Create a reproducible iBiogeobears project directory
#'
#' @param output_dir Directory where workflow outputs should be written.
#' @param overwrite Logical. If `FALSE`, existing files are not removed or
#'   overwritten.
#' @return A list of project paths.
#' @export
create_project <- function(output_dir, overwrite = FALSE) {
  output_dir <- as_path(output_dir)
  dirs <- list(
    root = output_dir,
    inputs = file.path(output_dir, "inputs"),
    raw_biogeobears = file.path(output_dir, "raw_biogeobears"),
    tables = file.path(output_dir, "tables"),
    figures = file.path(output_dir, "figures"),
    reports = file.path(output_dir, "reports"),
    logs = file.path(output_dir, "logs")
  )

  if (dir.exists(output_dir) && !isTRUE(overwrite)) {
    existing <- list.files(output_dir, all.files = TRUE, no.. = TRUE)
    if (length(existing) > 0L) {
      message("Using existing project directory without deleting contents: ", output_dir)
    }
  }

  invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))
  dirs
}

#' Create a runnable example iBiogeobears project
#'
#' @param path Directory to create.
#' @param overwrite Logical. If `FALSE`, stop when `path` already contains
#'   files.
#' @return A list with paths to the example project files.
#' @export
create_example_project <- function(path, overwrite = FALSE) {
  root <- as_path(path)
  if (dir.exists(root)) {
    existing <- list.files(root, all.files = TRUE, no.. = TRUE)
    if (length(existing) > 0L && !isTRUE(overwrite)) {
      stop("Example project directory already contains files: ", root, call. = FALSE)
    }
  }

  data_dir <- file.path(root, "data")
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

  template <- system.file("templates", "analysis.yml", package = "iBiogeobears")
  example_dir <- system.file("example_data", package = "iBiogeobears")
  if (!file.exists(template) || !dir.exists(example_dir)) {
    stop("Installed example template or data files could not be found.", call. = FALSE)
  }

  tree_file <- file.path(data_dir, "tree.nwk")
  geography_file <- file.path(data_dir, "geography.csv")
  regions_file <- file.path(data_dir, "regions.csv")
  file.copy(file.path(example_dir, "tree.nwk"), tree_file, overwrite = TRUE)
  file.copy(file.path(example_dir, "geography.csv"), geography_file, overwrite = TRUE)
  file.copy(file.path(example_dir, "regions.csv"), regions_file, overwrite = TRUE)

  cfg <- yaml::read_yaml(template)
  cfg$project$name <- cfg$project$name %||% "example_clade"
  cfg$project$output_dir <- as_path(file.path(root, "results", cfg$project$name))
  cfg$inputs$tree_file <- "data/tree.nwk"
  cfg$inputs$geography_file <- "data/geography.csv"
  cfg$inputs$regions_file <- "data/regions.csv"

  config_file <- file.path(root, "analysis.yml")
  yaml::write_yaml(cfg, config_file)

  list(
    root = root,
    config = as_path(config_file),
    data = as_path(data_dir),
    tree_file = as_path(tree_file),
    geography_file = as_path(geography_file),
    regions_file = as_path(regions_file),
    output_dir = cfg$project$output_dir
  )
}
