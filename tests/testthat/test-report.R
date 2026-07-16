test_that("render_report writes report source when quarto is unavailable", {
  out <- tempfile("bgs-report-source-")
  paths <- create_project(out)
  result <- list(project_paths = paths)

  report <- render_report(result, format = "source")

  expect_true(file.exists(report))
  expect_match(basename(report), "summary_report[.]qmd")
  report_source <- paste(readLines(report, warn = FALSE), collapse = "\n")
  expect_match(report_source, "Fitted models", fixed = TRUE)
  expect_match(report_source, "Best statistical model", fixed = TRUE)
  expect_match(report_source, "+J caution triggered", fixed = TRUE)
  expect_match(report_source, "### Fit Metrics", fixed = TRUE)
  expect_match(report_source, "Output Manifest", fixed = TRUE)
  expect_match(report_source, "workflow_manifest.csv", fixed = TRUE)
  expect_match(report_source, "### Interpretation Notes", fixed = TRUE)
  expect_match(report_source, "Model Sensitivity Summary", fixed = TRUE)
  expect_match(report_source, "Event Summary", fixed = TRUE)
  expect_match(report_source, "event_summary.csv", fixed = TRUE)
  expect_match(report_source, "figure_path(\"event_summary\")", fixed = TRUE)
  expect_match(report_source, "Biogeographical Stochastic Mapping (BSM)", fixed = TRUE)
  expect_match(report_source, "bsm_event_summary.csv", fixed = TRUE)
  expect_match(report_source, "bsm_event_times.csv", fixed = TRUE)
  expect_match(report_source, "figure_path(\"bsm_event_summary\")", fixed = TRUE)
  expect_match(report_source, "Node State Summary", fixed = TRUE)
  expect_match(report_source, "node_state_summary.csv", fixed = TRUE)
  expect_match(report_source, "Node State Sensitivity", fixed = TRUE)
  expect_match(report_source, "node_state_sensitivity.csv", fixed = TRUE)
  expect_match(report_source, "figure_path(\"node_state_sensitivity\")", fixed = TRUE)
  expect_match(report_source, "node_state_summary_best_model", fixed = TRUE)
  expect_match(report_source, "node_state_summary_best_non_j", fixed = TRUE)
  expect_match(report_source, "node_state_summary_best_plus_j", fixed = TRUE)
  expect_match(report_source, "display_label", fixed = TRUE)
  expect_match(report_source, "model_sensitivity.csv", fixed = TRUE)
})

test_that("check_report_environment reports source, html, and pdf readiness", {
  env <- check_report_environment(c("source", "html", "pdf"))

  expect_equal(env$format, c("source", "html", "pdf"))
  expect_true(env$available[env$format == "source"])
  expect_true(all(c(
    "quarto_package", "quarto_cli", "latex_available", "next_step"
  ) %in% names(env)))
  expect_true(all(nzchar(env$next_step)))
})

test_that("check_report_environment rejects unsupported formats", {
  expect_error(check_report_environment("docx"), "Unsupported report format")
})
