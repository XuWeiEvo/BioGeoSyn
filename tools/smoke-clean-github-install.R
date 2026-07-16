args <- commandArgs(trailingOnly = TRUE)
repo <- if (length(args) >= 1L && nzchar(args[[1L]])) args[[1L]] else "XuWeiEvo/BioGeoSyn"

assert <- function(ok, message) {
  if (!isTRUE(ok)) {
    stop(message, call. = FALSE)
  }
}

assert_file <- function(path, label) {
  assert(file.exists(path), paste(label, "was not created:", path))
}

lib <- tempfile("bgs-clean-github-lib-")
dir.create(lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(lib, .libPaths()))

if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes", lib = lib, repos = "https://cloud.r-project.org")
}

remotes::install_github(
  repo,
  lib = lib,
  dependencies = c("Depends", "Imports"),
  upgrade = "never",
  build_vignettes = FALSE
)

library(BioGeoSyn, lib.loc = lib)

installed_path <- normalizePath(find.package("BioGeoSyn", lib.loc = lib), winslash = "/", mustWork = TRUE)
assert(startsWith(installed_path, normalizePath(lib, winslash = "/", mustWork = TRUE)), "Package was not loaded from the clean temporary library.")

guide <- open_user_guide(browse = FALSE)
assert_file(guide, "ordinary-user guide")

checks <- check_installation(include_pdf = FALSE)
assert(all(c("component", "status", "next_step") %in% names(checks)), "check_installation() returned an unexpected table.")
required <- checks$component[checks$required == "yes" & checks$component != "BioGeoBEARS"]
required_ready <- checks$status[match(required, checks$component)] == "Ready"
assert(all(required_ready), paste("Required non-BioGeoBEARS checks are not ready:", paste(required[!required_ready], collapse = ", ")))

example_root <- tempfile("bgs-clean-github-example-")
project <- create_example_project(example_root)
assert_file(project$config, "example analysis.yml")

dry <- run_workflow(project$config, dry_run = TRUE, require_biogeobears = FALSE)
assert(inherits(dry, "bgs_workflow_result"), "run_workflow() did not return an bgs_workflow_result.")
assert(isTRUE(dry$dry_run), "The clean-install workflow should run in dry-run mode.")
assert(all(dry$validation$ok), "The clean-install example did not validate.")
assert_file(file.path(dry$project_paths$tables, "input_validation.csv"), "input validation table")
assert_file(file.path(dry$project_paths$tables, "model_run_plan.csv"), "model run plan")

report_source <- render_report(dry, format = "source")
assert_file(report_source, "source report")

bundle <- bundle_results(dry, include_raw = FALSE, overwrite = TRUE)
diagnostics <- bundle_diagnostics(dry, overwrite = TRUE)
assert_file(bundle, "result bundle")
assert_file(diagnostics, "diagnostic bundle")

if (requireNamespace("shiny", quietly = TRUE)) {
  app <- getFromNamespace("create_bgs_shiny_app", "BioGeoSyn")()
  assert(inherits(app, "shiny.appobj"), "Installed Shiny app did not build.")
} else {
  message("Shiny is not installed; launch_app() was not built in this smoke test.")
}

cat("Clean GitHub install smoke test passed\n")
cat("Repository: ", repo, "\n", sep = "")
cat("Temporary library: ", lib, "\n", sep = "")
cat("Installed package: ", installed_path, "\n", sep = "")
cat("Example project: ", project$root, "\n", sep = "")
cat("Workflow output: ", dry$project_paths$root, "\n", sep = "")
cat("User guide: ", guide, "\n", sep = "")
cat("Report source: ", report_source, "\n", sep = "")
cat("Result bundle: ", bundle, "\n", sep = "")
cat("Diagnostic bundle: ", diagnostics, "\n", sep = "")
