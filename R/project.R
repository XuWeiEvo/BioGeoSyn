#' Create a reproducible BioGeoSyn project directory
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

#' Create a runnable example BioGeoSyn project
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

  template <- system.file("templates", "analysis.yml", package = "BioGeoSyn")
  example_dir <- system.file("example_data", package = "BioGeoSyn")
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

#' Create a BioGeoSyn analysis project from user input files
#'
#' Copies a tree, geography matrix, and region table into a portable project,
#' writes `analysis.yml`, and immediately validates the generated project.
#'
#' @param path Directory to create.
#' @param project_name Project name. It is converted to a portable directory
#'   name containing letters, numbers, underscores, and hyphens.
#' @param tree_file Path to a Newick tree file.
#' @param geography_file Path to a geography CSV file.
#' @param regions_file Path to a regions CSV file.
#' @param max_range_size Positive maximum range size.
#' @param models Character vector of BioGeoBEARS models to run.
#' @param overwrite Logical. If `FALSE`, stop when `path` already contains
#'   files.
#' @return A list with project paths, normalized project name, and input
#'   validation results.
#' @export
create_analysis_project <- function(
    path,
    project_name,
    tree_file,
    geography_file,
    regions_file,
    max_range_size = 3L,
    models = valid_models(),
    overwrite = FALSE) {
  input_files <- c(
    tree_file = tree_file,
    geography_file = geography_file,
    regions_file = regions_file
  )
  missing <- names(input_files)[!file.exists(input_files)]
  if (length(missing) > 0L) {
    stop("Input file(s) do not exist: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  parsed_max_range_size <- suppressWarnings(as.integer(max_range_size))
  if (is.na(parsed_max_range_size) || parsed_max_range_size < 1L) {
    stop("max_range_size must be a positive integer.", call. = FALSE)
  }

  models <- unique(as.character(models))
  unsupported <- setdiff(models, valid_models())
  if (length(models) == 0L || length(unsupported) > 0L) {
    stop(
      "Select at least one supported model. Unsupported model(s): ",
      paste(unsupported, collapse = ", "),
      call. = FALSE
    )
  }

  root <- as_path(path)
  if (dir.exists(root)) {
    existing <- list.files(root, all.files = TRUE, no.. = TRUE)
    if (length(existing) > 0L && !isTRUE(overwrite)) {
      stop("Analysis project directory already contains files: ", root, call. = FALSE)
    }
  }

  template <- system.file("templates", "analysis.yml", package = "BioGeoSyn")
  if (!file.exists(template)) {
    stop("Installed analysis template could not be found.", call. = FALSE)
  }

  normalized_name <- normalize_project_name(project_name)
  data_dir <- file.path(root, "data")
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

  tree_extension <- tools::file_ext(tree_file)
  tree_name <- if (nzchar(tree_extension)) paste0("tree.", tree_extension) else "tree.nwk"
  destinations <- c(
    tree_file = file.path(data_dir, tree_name),
    geography_file = file.path(data_dir, "geography.csv"),
    regions_file = file.path(data_dir, "regions.csv")
  )
  copied <- file.copy(input_files, destinations, overwrite = TRUE)
  if (!all(copied)) {
    stop(
      "Could not copy input file(s): ",
      paste(names(copied)[!copied], collapse = ", "),
      call. = FALSE
    )
  }

  cfg <- yaml::read_yaml(template)
  cfg$project$name <- normalized_name
  cfg$project$output_dir <- as_path(file.path(root, "results", normalized_name))
  cfg$inputs$tree_file <- file.path("data", tree_name)
  cfg$inputs$geography_file <- file.path("data", "geography.csv")
  cfg$inputs$regions_file <- file.path("data", "regions.csv")
  cfg$inputs$max_range_size <- parsed_max_range_size
  cfg$models$run <- models

  config_file <- file.path(root, "analysis.yml")
  yaml::write_yaml(cfg, config_file)
  normalized_config <- read_config(config_file)
  validation <- validate_inputs(normalized_config)

  list(
    root = root,
    config = as_path(config_file),
    data = as_path(data_dir),
    tree_file = as_path(destinations[["tree_file"]]),
    geography_file = as_path(destinations[["geography_file"]]),
    regions_file = as_path(destinations[["regions_file"]]),
    output_dir = normalized_config$project$output_dir,
    project_name = normalized_name,
    validation = validation
  )
}

normalize_project_name <- function(project_name) {
  value <- trimws(as.character(project_name %||% ""))
  value <- gsub("[^\\p{L}\\p{N}_-]+", "_", value, perl = TRUE)
  value <- gsub("^_+|_+$", "", value)
  if (!nzchar(value)) {
    stop("Project name must contain at least one letter or number.", call. = FALSE)
  }
  value
}
