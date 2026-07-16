args <- commandArgs(trailingOnly = TRUE)
pkg_dir <- normalizePath(if (length(args) > 0L) args[[1L]] else getwd(), mustWork = TRUE)
lib <- tempfile("bgs-installed-lib-")
dir.create(lib, recursive = TRUE, showWarnings = FALSE)

install.packages(pkg_dir, lib = lib, repos = NULL, type = "source")
.libPaths(c(lib, .libPaths()))

library(BioGeoSyn)

template <- system.file("templates", "analysis.yml", package = "BioGeoSyn")
tree <- system.file("example_data", "tree.nwk", package = "BioGeoSyn")
geography <- system.file("example_data", "geography.csv", package = "BioGeoSyn")
regions <- system.file("example_data", "regions.csv", package = "BioGeoSyn")

stopifnot(file.exists(template))
stopifnot(file.exists(tree))
stopifnot(file.exists(geography))
stopifnot(file.exists(regions))

example <- tempfile("bgs-installed-example-")
project <- create_example_project(example)
result <- run_workflow(project$config, dry_run = TRUE, require_biogeobears = FALSE)

stopifnot(inherits(result, "bgs_workflow_result"))
stopifnot(file.exists(file.path(result$project_paths$tables, "model_run_plan.csv")))
stopifnot(file.exists(file.path(result$project_paths$tables, "input_validation.csv")))
stopifnot(all(result$validation$ok))

user_project <- create_analysis_project(
  tempfile("bgs-installed-user-project-"),
  "installed smoke project",
  tree,
  geography,
  regions,
  max_range_size = 2L,
  models = c("DEC", "DEC+J")
)
stopifnot(file.exists(user_project$config))
stopifnot(identical(user_project$project_name, "installed_smoke_project"))
stopifnot(all(user_project$validation$ok))

install_plan <- biogeobears_install_plan()
stopifnot(all(c("package", "source", "status", "next_step") %in% names(install_plan)))
stopifnot(identical(install_biogeobears(), install_plan))

cat("Installed package smoke test passed\n")
