args <- commandArgs(trailingOnly = TRUE)
repo <- normalizePath(if (length(args) >= 1L) args[[1L]] else ".", winslash = "/", mustWork = TRUE)

required <- c("yaml")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0L) {
  stop("Missing required smoke-test package(s): ", paste(missing, collapse = ", "), call. = FALSE)
}
lib <- tempfile("ibgb-user-workflow-lib-")
dir.create(lib, recursive = TRUE, showWarnings = FALSE)
install.packages(repo, lib = lib, repos = NULL, type = "source")
.libPaths(c(lib, .libPaths()))

library(iBiogeobears)

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

apply_overrides <- getFromNamespace("apply_shiny_config_overrides", "iBiogeobears")
write_gui_config <- getFromNamespace("write_shiny_workflow_config", "iBiogeobears")

root <- tempfile("ibgb-user-workflow-smoke-")
example <- create_example_project(root)

times_file <- file.path(example$data, "times.txt")
dists_file <- file.path(example$data, "dists.txt")
writeLines(c("0 1", "1 2"), times_file)
writeLines(c("1 1", "1 1"), dists_file)

base_cfg <- read_config(example$config)
edited_output <- file.path(example$root, "results", "gui_edited_clade")
edited_cfg <- apply_overrides(
  base_cfg,
  input = list(
    use_config_editor = TRUE,
    project_name = "gui_edited_clade",
    tree_file = "data/tree.nwk",
    geography_file = "data/geography.csv",
    regions_file = "data/regions.csv",
    max_range_size = "2",
    models_run = c("DEC", "DEC+J"),
    constraint_times_file = "data/times.txt",
    constraint_dists_file = "data/dists.txt"
  ),
  output_dir = edited_output
)
edited_config <- write_gui_config(edited_cfg, source_config = example$config)
roundtrip <- read_config(edited_config)

assert(identical(roundtrip$project$name, "gui_edited_clade"), "GUI override did not update project name.")
assert(identical(roundtrip$models$run, c("DEC", "DEC+J")), "GUI override did not update selected models.")
assert(file.exists(roundtrip$advanced$constraints$times_file), "times_file constraint was not resolved.")
assert(file.exists(roundtrip$advanced$constraints$dists_file), "dists_file constraint was not resolved.")

result <- run_workflow(
  edited_config,
  dry_run = TRUE,
  require_biogeobears = FALSE
)

assert(inherits(result, "iBGB_workflow_result"), "run_workflow() did not return an iBGB_workflow_result.")
assert(isTRUE(result$dry_run), "Smoke workflow should run in dry-run mode.")
assert(all(result$validation$ok), "Input validation failed in the smoke workflow.")
assert(identical(result$config$models$run, c("DEC", "DEC+J")), "Workflow did not use edited model selection.")
assert(file.exists(result$config$advanced$constraints$times_file), "Workflow config lost times_file constraint.")
assert(file.exists(result$config$advanced$constraints$dists_file), "Workflow config lost dists_file constraint.")

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
assert(identical(dirname(report_source), normalizePath(reports, winslash = "/", mustWork = TRUE)), "Report source path is outside reports directory.")
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
