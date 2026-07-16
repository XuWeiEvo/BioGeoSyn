args <- commandArgs(trailingOnly = TRUE)
repo <- normalizePath(if (length(args) >= 1L) args[[1L]] else ".", winslash = "/", mustWork = TRUE)

required <- c("yaml")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0L) {
  stop("Missing required smoke-test package(s): ", paste(missing, collapse = ", "), call. = FALSE)
}
lib <- tempfile("bgs-user-workflow-lib-")
dir.create(lib, recursive = TRUE, showWarnings = FALSE)
install.packages(repo, lib = lib, repos = NULL, type = "source")
.libPaths(c(lib, .libPaths()))

library(BioGeoSyn)

assert <- function(ok, message) {
  if (!isTRUE(ok)) {
    stop(message, call. = FALSE)
  }
}

assert_file <- function(path, label) {
  assert(file.exists(path), paste(label, "was not created:", path))
}

read_csv <- function(path) {
  utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
}

apply_wizard <- getFromNamespace("apply_shiny_wizard_overrides", "BioGeoSyn")
apply_overrides <- getFromNamespace("apply_shiny_config_overrides", "BioGeoSyn")
write_gui_config <- getFromNamespace("write_shiny_workflow_config", "BioGeoSyn")
constraint_template <- getFromNamespace("constraint_template_path", "BioGeoSyn")

root <- tempfile("bgs-user-workflow-smoke-")
example <- create_example_project(root)

base_cfg <- read_config(example$config)
edited_output <- file.path(example$root, "results", "gui_edited_clade")

upload <- function(path) {
  data.frame(name = basename(path), datapath = path, stringsAsFactors = FALSE)
}
times_template <- constraint_template("times_file")

# The wizard data step drives current_config() through these upload inputs.
wizard_input <- list(
  wizard_project_name = "gui_edited_clade",
  wizard_tree = upload(example$tree_file),
  wizard_geography = upload(example$geography_file),
  wizard_regions = upload(example$regions_file),
  wizard_max_range_size = 2L,
  wizard_models = c("DEC", "DEC+J"),
  wizard_constraint_times_file = upload(times_template)
)
edited_cfg <- apply_overrides(
  apply_wizard(base_cfg, wizard_input),
  input = wizard_input,
  output_dir = edited_output
)
edited_config <- write_gui_config(edited_cfg, source_config = example$config)
roundtrip <- read_config(edited_config)

assert(identical(roundtrip$project$name, "gui_edited_clade"), "Wizard override did not update project name.")
assert(identical(roundtrip$models$run, c("DEC", "DEC+J")), "Wizard override did not update selected models.")
assert(file.exists(roundtrip$advanced$constraints$times_file), "times_file constraint was not resolved.")

result <- run_workflow(
  edited_config,
  dry_run = TRUE,
  require_biogeobears = FALSE
)

assert(inherits(result, "bgs_workflow_result"), "run_workflow() did not return an bgs_workflow_result.")
assert(isTRUE(result$dry_run), "Smoke workflow should run in dry-run mode.")
assert(all(result$validation$ok), "Input validation failed in the smoke workflow.")
assert(identical(result$config$models$run, c("DEC", "DEC+J")), "Workflow did not use edited model selection.")
assert(file.exists(result$config$advanced$constraints$times_file), "Workflow config lost times_file constraint.")

tables <- result$project_paths$tables
logs <- result$project_paths$logs
reports <- result$project_paths$reports

expected_tables <- c(
  "input_validation.csv",
  "model_run_plan.csv",
  "workflow_manifest.csv"
)
for (file in expected_tables) {
  assert_file(file.path(tables, file), paste("Expected table", file))
}
assert_file(file.path(result$project_paths$root, "config_used.yml"), "config_used.yml")
assert_file(file.path(logs, "session_info.txt"), "session_info.txt")
assert_file(file.path(logs, "biogeobears_citation.txt"), "biogeobears_citation.txt")

plan <- read_csv(file.path(tables, "model_run_plan.csv"))
assert(identical(plan$model, c("DEC", "DEC+J")), "Model run plan does not match GUI-edited model selection.")
assert(all(plan$status == "planned"), "Dry-run model plan should contain planned statuses.")

report_source <- render_report(result, format = "source")
assert_file(report_source, "source report")
report_source_dir <- normalizePath(dirname(report_source), winslash = "/", mustWork = TRUE)
reports_dir <- normalizePath(reports, winslash = "/", mustWork = TRUE)
assert(identical(report_source_dir, reports_dir), "Report source path is outside reports directory.")
report_text <- paste(readLines(report_source, warn = FALSE), collapse = "\n")
assert(grepl("Model Sensitivity Summary", report_text, fixed = TRUE), "Report template is missing model sensitivity section.")
assert(grepl("Output Manifest", report_text, fixed = TRUE), "Report template is missing output manifest section.")

report_env <- check_report_environment(c("source", "html", "pdf"))
assert(isTRUE(report_env$available[report_env$format == "source"]), "Source report environment should always be available.")

manifest <- create_workflow_manifest(result, write = TRUE)
assert(any(manifest$relative_path == "reports/summary_report.qmd"), "Manifest does not include source report.")
assert(any(manifest$relative_path == "tables/model_run_plan.csv"), "Manifest does not include model run plan.")

bundle <- bundle_results(result, include_raw = FALSE, overwrite = TRUE)
diagnostics <- bundle_diagnostics(result, overwrite = TRUE)
assert_file(bundle, "result bundle")
assert_file(diagnostics, "diagnostic bundle")

assert(utils::file_test("-f", bundle), "Result bundle path is not a file.")
assert(utils::file_test("-f", diagnostics), "Diagnostic bundle path is not a file.")

cat("User workflow smoke test passed\n")
cat("Example project: ", example$root, "\n", sep = "")
cat("Workflow output: ", result$project_paths$root, "\n", sep = "")
cat("Report source: ", report_source, "\n", sep = "")
cat("Result bundle: ", bundle, "\n", sep = "")
cat("Diagnostic bundle: ", diagnostics, "\n", sep = "")
