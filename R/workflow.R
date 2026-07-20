#' Run a BioGeoSyn workflow
#'
#' @param config Path to YAML configuration file.
#' @param output_dir Optional output directory overriding the config.
#' @param dry_run Logical. If `TRUE`, validate and plan the workflow without
#'   executing BioGeoBEARS.
#' @param require_biogeobears Logical. If `TRUE`, stop when BioGeoBEARS is not
#'   installed. In dry runs this can be `FALSE`.
#' @param force Logical. If `TRUE`, execute even when input validation checks
#'   fail. Use only after reviewing `tables/input_validation.csv`.
#' @param resume_completed_models Optional logical override. Reuse completed
#'   model results only when their saved run signature matches current inputs
#'   and settings.
#' @param retry_failed_only Optional logical override. Execute only models
#'   marked failed in the previous model status table while reusing valid
#'   completed results.
#' @return An object of class `bgs_workflow_result`.
#' @export
run_workflow <- function(
    config,
    output_dir = NULL,
    dry_run = TRUE,
    require_biogeobears = !dry_run,
    force = FALSE,
    resume_completed_models = NULL,
    retry_failed_only = NULL) {
  cfg <- read_config(config)
  if (!is.null(output_dir)) {
    cfg$project$output_dir <- output_dir
  }
  if (!is.null(resume_completed_models)) {
    cfg$analysis$resume_completed_models <- isTRUE(resume_completed_models)
  }
  if (!is.null(retry_failed_only)) {
    cfg$analysis$retry_failed_only <- isTRUE(retry_failed_only)
  }
  if (isTRUE(cfg$analysis$retry_failed_only)) {
    cfg$analysis$resume_completed_models <- TRUE
  }

  project_paths <- create_project(cfg$project$output_dir)
  validation <- validate_inputs(cfg)
  utils::write.csv(validation, file.path(project_paths$tables, "input_validation.csv"), row.names = FALSE)
  yaml::write_yaml(cfg, file.path(project_paths$root, "config_used.yml"))
  writeLines(utils::capture.output(utils::sessionInfo()), file.path(project_paths$logs, "session_info.txt"))

  if (!isTRUE(dry_run) && !isTRUE(force) && any(!validation$ok)) {
    stop(format_validation_failure_message(validation, project_paths), call. = FALSE)
  }

  bgb_check <- check_biogeobears(required = require_biogeobears)
  model_result <- run_models(cfg, project_paths, execute = !dry_run)
  model_run_status <- attr(model_result, "run_status") %||% model_result
  model_sensitivity <- attr(model_result, "sensitivity")
  model_sensitivity_table <- attr(model_result, "sensitivity_table")
  node_state_sensitivity <- attr(model_result, "node_state_sensitivity")
  best_fit_events <- attr(model_result, "best_fit_events")
  bsm_tables <- attr(model_result, "bsm_tables")
  standardized_tables <- attr(model_result, "standardized_tables")
  model_comparison <- if (isTRUE(dry_run)) NULL else model_result
  figure_manifest <- if (!isTRUE(dry_run) && !is.null(model_comparison)) {
    generate_figures(
      model_comparison = model_comparison,
      standardized_tables = standardized_tables %||% list(),
      project_paths = project_paths,
      formats = cfg$figures$output_formats
    )
  } else {
    NULL
  }

  utils::write.csv(model_run_status, file.path(project_paths$tables, "model_run_plan.csv"), row.names = FALSE)
  writeLines(bgb_check$citation %||% "", file.path(project_paths$logs, "biogeobears_citation.txt"))
  # Written before the manifest is built so that the script is inventoried and
  # travels inside the result bundle.
  write_reproducibility_script(cfg, project_paths, dry_run = dry_run)
  workflow_manifest <- create_workflow_manifest(project_paths$root, write = TRUE)

  result <- list(
    config = cfg,
    project_paths = project_paths,
    validation = validation,
    biogeobears = bgb_check,
    model_plan = model_run_status,
    model_run_status = model_run_status,
    model_comparison = model_comparison,
    model_sensitivity = model_sensitivity,
    model_sensitivity_table = model_sensitivity_table,
    node_state_sensitivity = node_state_sensitivity,
    best_fit_events = best_fit_events,
    bsm_tables = bsm_tables,
    standardized_tables = standardized_tables,
    figure_manifest = figure_manifest,
    workflow_manifest = workflow_manifest,
    dry_run = dry_run,
    force = force,
    resume_completed_models = isTRUE(cfg$analysis$resume_completed_models),
    retry_failed_only = isTRUE(cfg$analysis$retry_failed_only),
    validation_failed = any(!validation$ok)
  )
  class(result) <- c("bgs_workflow_result", "list")
  result
}

#' Write an executable script that reproduces a workflow run
#'
#' Writes `reproduce.R` into the workflow output directory, so that it travels
#' with the result bundle. The script is self-contained: it reads the
#' configuration saved beside it, repoints the input paths at the copies stored
#' in `inputs/`, re-fits the same models and regenerates every standardized
#' table, figure and report. A recipient of a bundle can therefore reproduce its
#' outputs from the command line, without using the graphical interface.
#'
#' @param cfg Configuration list, as returned by [read_config()].
#' @param project_paths Project paths, as returned by [create_project()].
#' @param dry_run Logical. Recorded in the script header so a reader can tell
#'   whether models were actually fitted in the run that produced the bundle.
#' @return The path to the written script, invisibly.
#' @export
write_reproducibility_script <- function(cfg, project_paths, dry_run = FALSE) {
  bundled <- function(path) {
    if (is.null(path) || !nzchar(path)) NULL else paste0("inputs/", basename(path))
  }
  quoted <- function(path) paste0('"', path, '"')
  version <- tryCatch(
    as.character(utils::packageVersion("BioGeoSyn")),
    error = function(e) "unknown"
  )

  repoint <- character()
  tree <- bundled(cfg$inputs$tree_file)
  geography <- bundled(cfg$inputs$geography_file)
  regions <- bundled(cfg$inputs$regions_file)
  if (!is.null(tree)) {
    repoint <- c(repoint, paste0("config$inputs$tree_file <- ", quoted(tree)))
  }
  if (!is.null(geography)) {
    repoint <- c(repoint, paste0("config$inputs$geography_file <- ", quoted(geography)))
  }
  if (!is.null(regions)) {
    repoint <- c(repoint, paste0("config$inputs$regions_file <- ", quoted(regions)))
  }

  lines <- c(
    "#!/usr/bin/env Rscript",
    "# ---------------------------------------------------------------------",
    "# BioGeoSyn reproducibility script",
    "#",
    paste0("# Project   : ", cfg$project$name %||% "unnamed"),
    paste0("# Generated : ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
           " by BioGeoSyn ", version),
    paste0("# Run type  : ", if (isTRUE(dry_run)) {
      "dry run (inputs validated and models planned, nothing fitted)"
    } else {
      "full analysis"
    }),
    "#",
    "# Sourcing this file re-fits the same models on the same inputs and",
    "# regenerates every standardized table, figure and report. Run it from the",
    "# directory that holds this script, config_used.yml and inputs/.",
    "#",
    "# Requires R (>= 4.1), the BioGeoSyn package, and BioGeoBEARS, which is",
    "# installed separately; see the BioGeoSyn README.",
    "# ---------------------------------------------------------------------",
    "",
    "library(BioGeoSyn)",
    "",
    "config <- read_config(\"config_used.yml\")",
    "",
    "# config_used.yml records the absolute paths of the machine that produced",
    "# this bundle. Point the run at the input copies shipped in inputs/, and",
    "# write the new results to a local directory.",
    repoint,
    "config$project$output_dir <- \"reproduced_run\"",
    "",
    "result <- run_workflow(config, dry_run = FALSE)",
    "",
    "# run_workflow() already writes the tables, figures and manifest. Uncomment",
    "# to render the Quarto report and to repackage the results as a bundle.",
    "# render_report(result)",
    "# bundle_results(result, overwrite = TRUE)",
    ""
  )

  script_path <- file.path(project_paths$root, "reproduce.R")
  writeLines(lines, script_path)
  invisible(script_path)
}

format_validation_failure_message <- function(validation, project_paths) {
  failed <- validation[!validation$ok, , drop = FALSE]
  failed_checks <- paste(failed$label %||% failed$check, collapse = ", ")
  paste(
    "Input validation failed; refusing to execute BioGeoBEARS.",
    "Failed checks:",
    failed_checks,
    "Review:",
    file.path(project_paths$tables, "input_validation.csv"),
    "Set force = TRUE only if you intentionally want to run despite these failures."
  )
}
