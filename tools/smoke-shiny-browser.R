args <- commandArgs(trailingOnly = TRUE)
repo <- normalizePath(if (length(args) >= 1L) args[[1L]] else ".", winslash = "/", mustWork = TRUE)

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("The pkgload package is required for this smoke test.", call. = FALSE)
}
if (!requireNamespace("shinytest2", quietly = TRUE)) {
  stop("The shinytest2 package is required. Install it with install.packages('shinytest2').", call. = FALSE)
}

Sys.setenv(SHINYTEST2_APP_DRIVER_TEST_ON_CRAN = "1")
pkgload::load_all(repo, quiet = TRUE)

app_dir <- tempfile("ibgb-shiny-browser-app-")
dir.create(app_dir, recursive = TRUE)
repo_literal <- encodeString(repo, quote = "'")
writeLines(
  c(
    paste0("pkgload::load_all(", repo_literal, ", quiet = TRUE)"),
    "example <- create_example_project(file.path(tempdir(), 'ibgb-shiny-browser-example'))",
    "existing <- create_project(file.path(tempdir(), 'ibgb-shiny-browser-existing-results'))",
    "utils::write.csv(data.frame(check = 'tree_file', ok = TRUE), file.path(existing$tables, 'input_validation.csv'), row.names = FALSE)",
    "utils::write.csv(data.frame(model = 'DEC', status = 'completed', warning_count = 0L), file.path(existing$tables, 'model_run_status.csv'), row.names = FALSE)",
    "utils::write.csv(data.frame(model = 'DEC', AICc = 10, delta_aicc = 0), file.path(existing$tables, 'model_comparison.csv'), row.names = FALSE)",
    "writeLines('<html></html>', file.path(existing$reports, 'summary_report.html'))",
    "grDevices::png(file.path(existing$figures, 'model_comparison.png'), width = 400, height = 300)",
    "plot.new(); text(0.5, 0.5, 'model comparison')",
    "grDevices::dev.off()",
    "utils::write.csv(data.frame(figure = 'model_comparison', format = 'png', path = file.path(existing$figures, 'model_comparison.png'), status = 'created'), file.path(existing$figures, 'figure_manifest.csv'), row.names = FALSE)",
    "create_iBGB_shiny_app(config = example$config, output_dir = existing$root)"
  ),
  file.path(app_dir, "app.R")
)

app <- shinytest2::AppDriver$new(
  app_dir = app_dir,
  name = "ibgb-shiny-browser-smoke",
  load_timeout = 30000,
  timeout = 30000,
  height = 900,
  width = 1400
)
on.exit(app$stop(), add = TRUE)

body_text <- app$get_text("body")
required_initial_text <- c(
  "Project", "Run options", "Workflow", "Report and export",
  "Run Summary",
  "Model Comparison", "+J Sensitivity", "Warnings",
  "Node States", "Node Sensitivity", "Figure Dashboard",
  "Load existing results"
)
missing_initial <- required_initial_text[!vapply(required_initial_text, grepl, logical(1), x = body_text, fixed = TRUE)]
if (length(missing_initial) > 0L) {
  stop("Initial Shiny UI is missing expected text: ", paste(missing_initial, collapse = ", "), call. = FALSE)
}

app$click("load_results")
app$wait_for_idle(timeout = 60000)
loaded_text <- app$get_text("body")
if (!grepl("Loaded existing results", loaded_text, fixed = TRUE)) {
  stop("Load existing results action did not report success.", call. = FALSE)
}

app$click("validate")
app$wait_for_idle()
if (!grepl("Validation passed", app$get_text("body"), fixed = TRUE)) {
  stop("Validation action did not report success.", call. = FALSE)
}

app$click("run")
app$wait_for_idle(timeout = 60000)
if (!grepl("Dry run completed", app$get_text("body"), fixed = TRUE)) {
  stop("Dry-run workflow action did not report success.", call. = FALSE)
}

app$click("render_report")
app$wait_for_idle(timeout = 60000)
if (!grepl("Report:", app$get_text("body"), fixed = TRUE)) {
  stop("Report action did not report a rendered report path.", call. = FALSE)
}

app$click("bundle")
app$wait_for_idle(timeout = 60000)
if (!grepl("Bundle:", app$get_text("body"), fixed = TRUE)) {
  stop("Bundle action did not report a bundle path.", call. = FALSE)
}

cat("Shiny browser smoke test passed\n")
