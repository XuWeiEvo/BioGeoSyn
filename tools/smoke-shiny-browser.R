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

# A minimal app that loads the bundled example project. The data step's hidden
# config handle defaults to this config, so validation and the (dry) run work
# without simulating file uploads.
app_dir <- tempfile("ibgb-shiny-browser-app-")
dir.create(app_dir, recursive = TRUE)
repo_literal <- encodeString(repo, quote = "'")
writeLines(
  c(
    paste0("pkgload::load_all(", repo_literal, ", quiet = TRUE)"),
    "example <- create_example_project(file.path(tempdir(), 'ibgb-shiny-browser-example'))",
    "out <- file.path(tempdir(), 'ibgb-shiny-browser-out')",
    "create_iBGB_shiny_app(config = example$config, output_dir = out)"
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
app$wait_for_idle(timeout = 30000)

assert <- function(ok, message) {
  if (!isTRUE(ok)) {
    stop(message, call. = FALSE)
  }
}

exists_id <- function(id) {
  isTRUE(app$get_js(sprintf("document.getElementById('%s') !== null", id)))
}

nth_tab <- function(n) sprintf("#wizard_nav li:nth-child(%d) a", n)

body_has <- function(text) grepl(text, app$get_text("body"), fixed = TRUE)

# 1. Five-step wizard: data / analysis / single-clade results / cross-clade / about.
nav_html <- app$get_html("#wizard_nav")
n_tabs <- length(gregexpr("data-value=", nav_html)[[1L]])
assert(n_tabs == 5L, paste0("Expected 5 wizard tabs, found ", n_tabs, "."))

# 2. Data step + top-level environment section are present on load.
for (id in c(
  "wizard_tree", "wizard_geography", "wizard_regions", "wizard_max_range_size",
  "wizard_models", "output_dir", "config_path", "validate",
  "refresh_setup", "installation_table", "biogeobears_install_plan_table",
  "wizard_constraint_times_file", "download_tree_template"
)) {
  assert(exists_id(id), paste("Missing element on load:", id))
}

# 3. Validate the bundled example inputs.
app$click("validate")
app$wait_for_idle(timeout = 60000)
assert(body_has("Validation passed"), "Validation action did not report success.")

# 4. Analysis step: a bare dry-run toggle plus the BSM fold; run in dry-run mode.
app$click(selector = nth_tab(2))
app$wait_for_idle(timeout = 30000)
assert(exists_id("dry_run"), "Analysis step is missing the dry-run toggle.")
assert(exists_id("run_stochastic_mapping"), "Analysis step is missing the BSM toggle.")
assert(exists_id("stochastic_mapping_replicates"), "Analysis step is missing the BSM replicate input.")
app$click("run")
app$wait_for_idle(timeout = 120000)
assert(body_has("Dry run completed"), "Dry-run workflow action did not report success.")

# 5. Single-clade results step: two downloads and the output-file legend.
app$click(selector = nth_tab(3))
app$wait_for_idle(timeout = 30000)
for (id in c("render_report", "download_bundle", "download_report", "output_file_legend_table")) {
  assert(exists_id(id), paste("Missing results-step element:", id))
}

# 6. Cross-clade step: overall and per-region uploads plus their CSV exports.
app$click(selector = nth_tab(4))
app$wait_for_idle(timeout = 30000)
for (id in c(
  "cross_clade_files", "cross_clade_region_files",
  "download_cross_clade", "download_cross_clade_region"
)) {
  assert(exists_id(id), paste("Missing cross-clade element:", id))
}

# 7. About step: software status, report environment, and BioGeoBEARS citation.
app$click(selector = nth_tab(5))
app$wait_for_idle(timeout = 30000)
for (id in c("about_table", "report_environment_table", "citation_text")) {
  assert(exists_id(id), paste("Missing about-step element:", id))
}

cat("Shiny browser smoke test passed\n")
