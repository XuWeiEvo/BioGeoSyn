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
    "create_iBGB_shiny_app(config = example$config, output_dir = example$output_dir)"
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
  "Model Comparison", "+J Sensitivity", "Warnings",
  "Node States", "Node Sensitivity", "Figure Dashboard",
  "Load existing results"
)
missing_initial <- required_initial_text[!vapply(required_initial_text, grepl, logical(1), x = body_text, fixed = TRUE)]
if (length(missing_initial) > 0L) {
  stop("Initial Shiny UI is missing expected text: ", paste(missing_initial, collapse = ", "), call. = FALSE)
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
