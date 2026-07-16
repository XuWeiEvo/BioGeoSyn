#' Run an end-to-end BioGeoSyn acceptance check
#'
#' Creates a fresh bundled example project and records each user-facing
#' workflow stage in a machine-readable acceptance table. Quick mode validates
#' the installed package without executing BioGeoBEARS. Full mode requires the
#' graphical and report environment, executes all six supported models, and
#' verifies that a second run reuses the completed results.
#'
#' @param path Empty directory in which to create acceptance artifacts.
#' @param mode Either `"quick"` for a dependency-light dry run or `"full"` for
#'   real six-model execution.
#' @param render_html Logical. Render and require an HTML report. Defaults to
#'   `TRUE` in full mode and `FALSE` in quick mode.
#' @return An object of class `bgs_acceptance_result` containing the check
#'   table, artifact paths, workflow result, and overall pass status.
#' @export
run_acceptance_check <- function(
    path = tempfile("BioGeoSyn-acceptance-"),
    mode = c("quick", "full"),
    render_html = identical(match.arg(mode), "full")) {
  mode <- match.arg(mode)
  root <- as_path(path)
  if (dir.exists(root) && length(list.files(root, all.files = TRUE, no.. = TRUE)) > 0L) {
    stop("Acceptance directory already contains files: ", root, call. = FALSE)
  }
  dir.create(root, recursive = TRUE, showWarnings = FALSE)

  state <- new.env(parent = emptyenv())
  rows <- list()
  add_step <- function(check, required, action, next_step) {
    step <- acceptance_run_step(check, required, action, next_step)
    rows[[length(rows) + 1L]] <<- step$row
    step$value
  }
  add_skipped <- function(check, detail) {
    rows[[length(rows) + 1L]] <<- acceptance_check_row(
      check = check,
      required = FALSE,
      status = "Skipped",
      detail = detail,
      next_step = "No action required for this acceptance mode.",
      elapsed_seconds = 0
    )
  }

  installation <- check_installation(include_pdf = FALSE)
  state$core_installation <- add_step(
    "Core installation",
    TRUE,
    function() {
      acceptance_require_components(installation, c("R", "Core R packages"))
      installation
    },
    "Run check_installation() and install the missing required components."
  )

  if (identical(mode, "full")) {
    state$shiny_installation <- add_step(
      "Shiny installation",
      TRUE,
      function() {
        acceptance_require_components(installation, "Shiny")
        installation[installation$component == "Shiny", , drop = FALSE]
      },
      "Install Shiny with install.packages('shiny')."
    )
    state$biogeobears_installation <- add_step(
      "BioGeoBEARS installation",
      TRUE,
      function() {
        acceptance_require_components(installation, "BioGeoBEARS")
        installation[installation$component == "BioGeoBEARS", , drop = FALSE]
      },
      paste(
        "Inspect biogeobears_install_plan(), then run",
        "install_biogeobears(execute = TRUE) if needed."
      )
    )
  } else {
    add_skipped("Shiny installation", "Quick mode tests the package backend without launching Shiny.")
    add_skipped("BioGeoBEARS installation", "Quick mode plans models without executing BioGeoBEARS.")
  }

  state$project <- add_step(
    "Create example project",
    TRUE,
    function() create_example_project(file.path(root, "example_project")),
    "Confirm that the installed package contains inst/templates and inst/example_data."
  )

  state$validation <- add_step(
    "Validate bundled inputs",
    TRUE,
    function() {
      acceptance_assert(!is.null(state$project), "The example project was not created.")
      validation <- validate_inputs(read_config(state$project$config))
      if (!all(validation$ok)) {
        failed <- validation[!validation$ok, , drop = FALSE]
        acceptance_assert(
          FALSE,
          paste(paste0(failed$label, ": ", failed$detail), collapse = "; ")
        )
      }
      validation
    },
    "Review the generated analysis.yml and input validation details."
  )

  state$workflow <- add_step(
    if (identical(mode, "full")) "Execute six-model workflow" else "Plan six-model workflow",
    TRUE,
    function() {
      acceptance_assert(!is.null(state$project), "The example project was not created.")
      run_workflow(
        state$project$config,
        dry_run = !identical(mode, "full"),
        require_biogeobears = identical(mode, "full")
      )
    },
    "Review the workflow logs and tables/model_run_status.csv."
  )

  state$outputs <- add_step(
    "Verify workflow outputs",
    TRUE,
    function() {
      acceptance_assert(!is.null(state$workflow), "The workflow did not return a result.")
      expected <- c(
        "config_used.yml",
        "tables/input_validation.csv",
        "tables/model_run_plan.csv",
        "tables/workflow_manifest.csv",
        "logs/session_info.txt",
        "logs/biogeobears_citation.txt"
      )
      if (identical(mode, "full")) {
        expected <- c(
          expected,
          "tables/model_run_status.csv",
          "tables/model_comparison.csv",
          "tables/model_sensitivity.csv"
        )
      }
      paths <- file.path(state$workflow$project_paths$root, expected)
      acceptance_assert(
        all(file.exists(paths)),
        paste("Missing output file(s):", paste(expected[!file.exists(paths)], collapse = ", "))
      )
      expected
    },
    "Review the workflow output directory and workflow manifest."
  )

  if (identical(mode, "full")) {
    state$model_results <- add_step(
      "Verify six model results",
      TRUE,
      function() {
        status <- state$workflow$model_run_status
        acceptance_assert(
          !is.null(status) && nrow(status) == length(valid_models()),
          "The model status table does not contain all six supported models."
        )
        acceptance_assert(
          all(status$status == "completed"),
          paste("Incomplete model(s):", paste(status$model[status$status != "completed"], collapse = ", "))
        )
        required_paths <- c(status$result_file, status$log_file)
        acceptance_assert(
          all(file.exists(required_paths)),
          paste(
            "Missing raw result or log file(s):",
            paste(required_paths[!file.exists(required_paths)], collapse = ", ")
          )
        )
        status
      },
      "Open the failed model logs listed in tables/model_run_status.csv."
    )
    state$resumed_workflow <- add_step(
      "Reuse completed models",
      TRUE,
      function() {
        resumed <- run_workflow(
          state$project$config,
          dry_run = FALSE,
          require_biogeobears = TRUE
        )
        status <- resumed$model_run_status
        acceptance_assert(
          nrow(status) == length(valid_models()) && all(status$run_action == "reused"),
          "The second run did not reuse every completed model."
        )
        resumed
      },
      "Compare saved model run signatures and inspect model metadata files."
    )
    state$workflow <- state$resumed_workflow %||% state$workflow
  } else {
    add_skipped("Verify six model results", "Quick mode does not execute BioGeoBEARS.")
    add_skipped("Reuse completed models", "Completed model reuse is verified only in full mode.")
  }

  state$source_report <- add_step(
    "Create report source",
    TRUE,
    function() {
      acceptance_assert(!is.null(state$workflow), "The workflow did not return a result.")
      report <- render_report(state$workflow, format = "source")
      acceptance_assert(file.exists(report), "The report source was not created.")
      report
    },
    "Review report_template.qmd and the workflow reports directory."
  )

  if (isTRUE(render_html)) {
    state$html_report <- add_step(
      "Render HTML report",
      TRUE,
      function() {
        report <- render_report(state$workflow, format = "html")
        acceptance_assert(
          file.exists(report) && identical(tolower(tools::file_ext(report)), "html"),
          "Quarto did not create an HTML report."
        )
        report
      },
      "Run check_report_environment('html') and install the missing Quarto component."
    )
  } else {
    add_skipped("Render HTML report", "HTML rendering was not requested.")
  }

  state$result_bundle <- add_step(
    "Create result bundle",
    TRUE,
    function() {
      acceptance_assert(!is.null(state$workflow), "The workflow did not return a result.")
      include_raw <- identical(mode, "full")
      bundle <- bundle_results(state$workflow, include_raw = include_raw, overwrite = TRUE)
      acceptance_assert(file.exists(bundle) && file.info(bundle)$size > 0, "The result bundle is empty.")
      if (include_raw) {
        contents <- utils::unzip(bundle, list = TRUE)
        acceptance_assert(
          any(startsWith(contents$Name, "raw_biogeobears/")),
          "The full result bundle does not contain raw BioGeoBEARS outputs."
        )
      }
      bundle
    },
    "Ensure that the R package 'zip' is installed and the destination is writable."
  )

  state$diagnostic_bundle <- add_step(
    "Create diagnostic bundle",
    TRUE,
    function() {
      acceptance_assert(!is.null(state$workflow), "The workflow did not return a result.")
      bundle <- bundle_diagnostics(state$workflow, overwrite = TRUE)
      acceptance_assert(file.exists(bundle) && file.info(bundle)$size > 0, "The diagnostic bundle is empty.")
      bundle
    },
    "Ensure that logs, status tables, the R package 'zip', and a writable destination are available."
  )

  checks <- do.call(rbind, rows)
  checks <- acceptance_add_environment(checks, mode)
  results_file <- file.path(root, "acceptance_results.csv")
  session_file <- file.path(root, "acceptance_session_info.txt")
  write_csv_base(checks, results_file)
  writeLines(utils::capture.output(utils::sessionInfo()), session_file)

  result <- list(
    mode = mode,
    passed = !any(checks$required == "yes" & checks$status != "Passed"),
    checks = checks,
    root = root,
    results_file = as_path(results_file),
    session_file = as_path(session_file),
    project = state$project %||% NULL,
    workflow = state$workflow %||% NULL,
    source_report = state$source_report %||% NULL,
    html_report = state$html_report %||% NULL,
    result_bundle = state$result_bundle %||% NULL,
    diagnostic_bundle = state$diagnostic_bundle %||% NULL
  )
  class(result) <- c("bgs_acceptance_result", "list")
  result
}

acceptance_run_step <- function(check, required, action, next_step) {
  started <- proc.time()[["elapsed"]]
  value <- NULL
  error_message <- NULL
  value <- tryCatch(
    action(),
    error = function(e) {
      error_message <<- conditionMessage(e)
      NULL
    }
  )
  elapsed <- proc.time()[["elapsed"]] - started
  ok <- is.null(error_message)
  list(
    value = value,
    row = acceptance_check_row(
      check = check,
      required = required,
      status = if (ok) "Passed" else "Failed",
      detail = if (ok) "Completed successfully." else error_message,
      next_step = if (ok) "No action needed." else next_step,
      elapsed_seconds = elapsed
    )
  )
}

acceptance_check_row <- function(check, required, status, detail, next_step, elapsed_seconds) {
  data.frame(
    check = as.character(check),
    required = if (isTRUE(required)) "yes" else "no",
    status = as.character(status),
    elapsed_seconds = round(as.numeric(elapsed_seconds), 3),
    detail = as.character(detail),
    next_step = as.character(next_step),
    stringsAsFactors = FALSE
  )
}

acceptance_require_components <- function(installation, components) {
  selected <- installation[installation$component %in% components, , drop = FALSE]
  missing <- setdiff(components, selected$component)
  acceptance_assert(length(missing) == 0L, paste("Missing installation check(s):", paste(missing, collapse = ", ")))
  action_needed <- selected[selected$status != "Ready", , drop = FALSE]
  action_detail <- paste0(
    action_needed$component,
    " (",
    action_needed$next_step,
    ")"
  )
  acceptance_assert(
    nrow(action_needed) == 0L,
    paste("Components requiring action:", paste(action_detail, collapse = "; "))
  )
  invisible(TRUE)
}

acceptance_assert <- function(ok, message) {
  if (!isTRUE(ok)) {
    stop(message, call. = FALSE)
  }
  invisible(TRUE)
}

acceptance_add_environment <- function(checks, mode) {
  package_version <- tryCatch(
    as.character(utils::packageVersion("BioGeoSyn")),
    error = function(e) NA_character_
  )
  data.frame(
    timestamp_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    mode = mode,
    platform = R.version$platform,
    os = paste(Sys.info()[c("sysname", "release")], collapse = " "),
    r_version = as.character(getRversion()),
    package_version = package_version,
    checks,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

#' @export
print.bgs_acceptance_result <- function(x, ...) {
  print(x$checks[, c("check", "required", "status", "detail"), drop = FALSE], row.names = FALSE)
  cat(
    "\nOverall:", if (isTRUE(x$passed)) "PASSED" else "FAILED",
    "\nResults:", x$results_file, "\n"
  )
  invisible(x)
}
