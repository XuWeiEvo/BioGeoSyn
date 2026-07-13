#' Run an iBiogeobears workflow
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
#' @return An object of class `iBGB_workflow_result`.
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
  class(result) <- c("iBGB_workflow_result", "list")
  result
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
