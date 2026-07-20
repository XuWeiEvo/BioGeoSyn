#' Create a workflow output manifest
#'
#' @param result_or_path A workflow result returned by [run_workflow()] or a
#'   path to a workflow output directory.
#' @param write Logical. If `TRUE`, write `tables/workflow_manifest.csv`.
#' @return A data frame listing files in the workflow output directory.
#' @export
create_workflow_manifest <- function(result_or_path, write = TRUE) {
  root <- workflow_root_path(result_or_path)
  if (!dir.exists(root)) {
    stop("Workflow output directory does not exist: ", root, call. = FALSE)
  }

  manifest <- collect_workflow_manifest(root)
  if (isTRUE(write)) {
    tables_dir <- file.path(root, "tables")
    dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
    manifest_path <- file.path(tables_dir, "workflow_manifest.csv")
    write_csv_base(manifest, manifest_path)
    manifest <- collect_workflow_manifest(root)
    write_csv_base(manifest, manifest_path)
  }
  manifest
}

#' Bundle workflow results into a zip archive
#'
#' @param result_or_path A workflow result returned by [run_workflow()] or a
#'   path to a workflow output directory.
#' @param bundle_file Optional output `.zip` path. Defaults to a zip file next
#'   to the workflow output directory.
#' @param include_raw Logical. If `FALSE`, omit `raw_biogeobears/` files.
#' @param overwrite Logical. If `FALSE`, stop when `bundle_file` exists.
#' @param refresh_manifest Logical. If `TRUE`, rewrite
#'   `tables/workflow_manifest.csv` before bundling.
#' @return Path to the created zip archive.
#' @export
bundle_results <- function(result_or_path, bundle_file = NULL, include_raw = TRUE, overwrite = FALSE, refresh_manifest = TRUE) {
  root <- workflow_root_path(result_or_path)
  if (!dir.exists(root)) {
    stop("Workflow output directory does not exist: ", root, call. = FALSE)
  }

  if (is.null(bundle_file)) {
    bundle_file <- file.path(dirname(root), paste0(basename(root), "_results.zip"))
  }
  bundle_file <- as_path(bundle_file)
  if (file.exists(bundle_file) && !isTRUE(overwrite)) {
    stop("Bundle file already exists: ", bundle_file, call. = FALSE)
  }

  manifest <- create_workflow_manifest(root, write = isTRUE(refresh_manifest))
  if (!isTRUE(include_raw)) {
    manifest <- manifest[manifest$category != "raw_biogeobears", , drop = FALSE]
  }
  if (nrow(manifest) == 0L) {
    stop("No workflow output files are available to bundle.", call. = FALSE)
  }

  files <- manifest$relative_path
  zip_relative_files(root, bundle_file, files)
}

#' Bundle workflow diagnostics into a lightweight zip archive
#'
#' @param result_or_path A workflow result returned by [run_workflow()] or a
#'   path to a workflow output directory.
#' @param bundle_file Optional output `.zip` path. Defaults to a diagnostics zip
#'   file next to the workflow output directory.
#' @param overwrite Logical. If `FALSE`, stop when `bundle_file` exists.
#' @param refresh_manifest Logical. If `TRUE`, rewrite
#'   `tables/workflow_manifest.csv` before bundling.
#' @return Path to the created diagnostics zip archive.
#' @export
bundle_diagnostics <- function(result_or_path, bundle_file = NULL, overwrite = FALSE, refresh_manifest = TRUE) {
  root <- workflow_root_path(result_or_path)
  if (!dir.exists(root)) {
    stop("Workflow output directory does not exist: ", root, call. = FALSE)
  }

  if (is.null(bundle_file)) {
    bundle_file <- file.path(dirname(root), paste0(basename(root), "_diagnostics.zip"))
  }
  bundle_file <- as_path(bundle_file)
  if (file.exists(bundle_file) && !isTRUE(overwrite)) {
    stop("Diagnostics bundle file already exists: ", bundle_file, call. = FALSE)
  }

  create_workflow_manifest(root, write = isTRUE(refresh_manifest))
  files <- diagnostic_bundle_files(root)
  if (length(files) == 0L) {
    stop("No diagnostic workflow files are available to bundle.", call. = FALSE)
  }

  zip_relative_files(root, bundle_file, files)
}

diagnostic_bundle_files <- function(root) {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  fixed <- c(
    "config_used.yml",
    "tables/input_validation.csv",
    "tables/model_run_plan.csv",
    "tables/model_run_status.csv",
    "tables/workflow_manifest.csv",
    "logs/session_info.txt",
    "logs/biogeobears_citation.txt"
  )
  files <- fixed[file.exists(file.path(root, fixed))]

  status_path <- file.path(root, "tables", "model_run_status.csv")
  if (file.exists(status_path)) {
    status <- utils::read.csv(status_path, check.names = FALSE, stringsAsFactors = FALSE)
    if ("log_file" %in% names(status)) {
      files <- c(files, diagnostic_relative_paths(root, status$log_file))
    }
  }

  discovered_logs <- list.files(root, pattern = "[.]log$", recursive = TRUE, full.names = TRUE)
  files <- c(files, diagnostic_relative_paths(root, discovered_logs))
  files <- unique(files[file.exists(file.path(root, files))])
  sort(files)
}

diagnostic_relative_paths <- function(root, paths) {
  paths <- as.character(paths %||% character())
  paths <- paths[!is.na(paths) & nzchar(paths)]
  if (length(paths) == 0L) {
    return(character())
  }

  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  absolute <- ifelse(grepl("^([A-Za-z]:)?[/\\\\]", paths), paths, file.path(root, paths))
  absolute <- normalizePath(absolute, winslash = "/", mustWork = FALSE)
  inside <- startsWith(absolute, paste0(root, "/")) | absolute == root
  absolute <- absolute[inside]
  if (length(absolute) == 0L) {
    return(character())
  }
  substring(absolute, nchar(root) + 2L)
}

zip_relative_files <- function(root, bundle_file, files) {
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(root)

  zip_error <- NULL
  status <- NULL
  invisible(utils::capture.output({
    if (requireNamespace("zip", quietly = TRUE)) {
      status <- tryCatch(
        {
          zip::zip(zipfile = bundle_file, files = files, include_directories = FALSE)
          0L
        },
        error = function(e) {
          zip_error <<- e
          NA_integer_
        }
      )
    } else {
      status <- tryCatch(
        utils::zip(zipfile = bundle_file, files = files, flags = "-qr9X"),
        error = function(e) {
          zip_error <<- e
          NA_integer_
        }
      )
    }
  }))
  if (!is.null(zip_error) || (!is.null(status) && !identical(status, 0L))) {
    stop(
      "Unable to create zip archive. Install the R package 'zip' or ensure a system zip utility is available to R.",
      call. = FALSE
    )
  }

  as_path(bundle_file)
}

collect_workflow_manifest <- function(root) {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  files <- list.files(root, recursive = TRUE, all.files = FALSE, full.names = TRUE, no.. = TRUE)
  if (length(files) == 0L) {
    return(empty_workflow_manifest())
  }

  info <- file.info(files)
  files <- files[!is.na(info$isdir) & !info$isdir]
  if (length(files) == 0L) {
    return(empty_workflow_manifest())
  }

  files <- normalizePath(files, winslash = "/", mustWork = FALSE)
  info <- file.info(files)
  relative_path <- substring(files, nchar(root) + 2L)
  category <- workflow_manifest_category(relative_path)
  out <- data.frame(
    category = category,
    relative_path = relative_path,
    file_name = basename(files),
    extension = tolower(tools::file_ext(files)),
    size_bytes = as.numeric(info$size),
    modified_time = format(info$mtime, "%Y-%m-%d %H:%M:%S %z"),
    stringsAsFactors = FALSE
  )
  out <- out[order(out$category, out$relative_path), , drop = FALSE]
  row.names(out) <- NULL
  out
}

workflow_manifest_category <- function(relative_path) {
  first <- ifelse(grepl("/", relative_path, fixed = TRUE), sub("/.*$", "", relative_path), relative_path)
  known <- c("inputs", "raw_biogeobears", "tables", "figures", "reports", "logs")
  ifelse(
    first %in% known,
    first,
    ifelse(
      relative_path == "config_used.yml",
      "config",
      ifelse(relative_path == "reproduce.R", "script", "root")
    )
  )
}

workflow_root_path <- function(result_or_path) {
  if (is.list(result_or_path) && !is.null(result_or_path$project_paths$root)) {
    return(as_path(result_or_path$project_paths$root))
  }
  if (is.character(result_or_path) && length(result_or_path) == 1L) {
    return(as_path(result_or_path))
  }
  stop("Expected a workflow result or a workflow output directory path.", call. = FALSE)
}

empty_workflow_manifest <- function() {
  data.frame(
    category = character(),
    relative_path = character(),
    file_name = character(),
    extension = character(),
    size_bytes = numeric(),
    modified_time = character(),
    stringsAsFactors = FALSE
  )
}
