test_that("run_workflow returns structured dry-run outputs", {
  config <- system.file("templates", "analysis.yml", package = "iBiogeobears")
  out <- tempfile("ibgb-workflow-dry-")

  result <- run_workflow(config, output_dir = out, dry_run = TRUE, require_biogeobears = FALSE)

  expect_s3_class(result, "iBGB_workflow_result")
  expect_true(all(c("model_plan", "model_run_status") %in% names(result)))
  expect_true(is.null(result$model_comparison))
  expect_equal(result$model_plan$status[1], "planned")
  expect_true(file.exists(file.path(out, "tables", "model_run_plan.csv")))
})

test_that("run_workflow blocks execution when validation fails", {
  config <- system.file("templates", "analysis.yml", package = "iBiogeobears")
  cfg <- read_config(config)
  temp_dir <- tempfile("ibgb-workflow-invalid-")
  dir.create(temp_dir)
  out <- file.path(temp_dir, "results")

  bad_geography <- file.path(temp_dir, "bad_geography.csv")
  writeLines(c(
    "species,A,B,C",
    "sp1,1,0,0",
    "sp2,1,1,0",
    "spX,0,1,0",
    "sp4,0,1,1",
    "sp5,0,0,1"
  ), bad_geography)

  cfg$project$output_dir <- out
  cfg$inputs$tree_file <- system.file("example_data", "tree.nwk", package = "iBiogeobears")
  cfg$inputs$geography_file <- bad_geography
  cfg$inputs$regions_file <- system.file("example_data", "regions.csv", package = "iBiogeobears")
  cfg$.config_file <- NULL

  invalid_config <- file.path(temp_dir, "analysis.yml")
  yaml::write_yaml(cfg, invalid_config)

  expect_error(
    run_workflow(invalid_config, dry_run = FALSE, require_biogeobears = FALSE),
    "Input validation failed"
  )
  validation_path <- file.path(out, "tables", "input_validation.csv")
  expect_true(file.exists(validation_path))
  validation <- utils::read.csv(validation_path)
  expect_false(validation$ok[match("tree_geography_species_match", validation$check)])
})

test_that("run_workflow exposes model comparison when BioGeoBEARS is available", {
  testthat::skip_if_not_installed("BioGeoBEARS")
  testthat::skip_if_not_installed("ape")

  config <- system.file("templates", "analysis.yml", package = "iBiogeobears")
  out <- tempfile("ibgb-workflow-dec-")
  cfg <- read_config(config)
  cfg$models$run <- "DEC"
  cfg$project$output_dir <- out
  cfg$inputs$tree_file <- system.file("example_data", "tree.nwk", package = "iBiogeobears")
  cfg$inputs$geography_file <- system.file("example_data", "geography.csv", package = "iBiogeobears")
  cfg$inputs$regions_file <- system.file("example_data", "regions.csv", package = "iBiogeobears")
  cfg$.config_file <- NULL
  dec_config <- tempfile(fileext = ".yml")
  yaml::write_yaml(cfg, dec_config)

  result <- suppressWarnings(run_workflow(dec_config, dry_run = FALSE))

  expect_equal(result$model_comparison$model, "DEC")
  expect_equal(result$model_run_status$status, "completed")
  expect_false(is.null(result$model_sensitivity))
  expect_false(is.null(result$model_sensitivity_table))
  expect_false(is.null(result$figure_manifest))
  expect_true(any(result$figure_manifest$figure == "model_comparison" & result$figure_manifest$status == "created"))
  expect_true(any(result$figure_manifest$figure == "node_state_summary_best_model" & result$figure_manifest$status == "created"))
  expect_true(all(c(
    "geographic_states",
    "tree_nodes",
    "parameter_table",
    "ancestral_state_probabilities",
    "root_state_probabilities",
    "node_state_summary"
  ) %in% names(result$standardized_tables)))
  expect_true(file.exists(file.path(out, "tables", "model_run_status.csv")))
  expect_true(file.exists(file.path(out, "tables", "model_comparison.csv")))
  expect_true(file.exists(file.path(out, "tables", "model_sensitivity.csv")))
  expect_true(file.exists(file.path(out, "tables", "model_parameters.csv")))
  expect_true(file.exists(file.path(out, "figures", "model_comparison.png")))

  sensitivity <- utils::read.csv(file.path(out, "tables", "model_sensitivity.csv"), check.names = FALSE)
  expect_true("best_overall_is_plus_j" %in% sensitivity$summary_item)
  expect_true("auto_declare_best_model" %in% sensitivity$summary_item)
})
