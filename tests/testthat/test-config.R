test_that("read_config fills defaults", {
  config <- system.file("templates", "analysis.yml", package = "iBiogeobears")
  cfg <- read_config(config)
  expect_equal(cfg$project$name, "example_clade")
  expect_true("DEC+J" %in% cfg$models$run)
  expect_false(cfg$methodology$auto_declare_best_model)
  expect_false(cfg$analysis$run_stochastic_mapping)
  expect_equal(cfg$analysis$stochastic_mapping_model, "best")
  expect_equal(cfg$analysis$stochastic_mapping_replicates, 100L)
  expect_true(all(c(
    "times_file",
    "dists_file",
    "dispersal_multipliers_file",
    "areas_allowed_file",
    "areas_adjacency_file",
    "area_of_areas_file"
  ) %in% names(cfg$advanced$constraints)))
})

test_that("validate_inputs returns checks", {
  config <- system.file("templates", "analysis.yml", package = "iBiogeobears")
  cfg <- read_config(config)
  checks <- validate_inputs(cfg)
  expect_true(all(c("check", "ok", "detail") %in% names(checks)))
  expect_true(all(c(
    "tree_geography_species_match",
    "geography_species_unique",
    "max_range_size_within_area_count",
    "models_not_duplicated",
    "output_parent_writable"
  ) %in% checks$check))
  expect_true(all(checks$ok))
  expect_true(all(c("label", "status", "next_step") %in% names(checks)))
  expect_true(all(checks$status == "Passed"))
  expect_true(all(checks$next_step == "No action needed."))
})

test_that("configuration defaults enable safe model resume", {
  cfg <- fill_config_defaults(list())

  expect_true(cfg$analysis$resume_completed_models)
  expect_false(cfg$analysis$retry_failed_only)
})

test_that("format_validation_results explains how to repair failures", {
  validation <- data.frame(
    check = c("tree_geography_species_match", "custom_future_check"),
    ok = c(FALSE, FALSE),
    detail = c("missing_from_geography: taxon_b", "custom detail"),
    stringsAsFactors = FALSE
  )

  formatted <- format_validation_results(validation)

  expect_equal(formatted$status, c("Needs attention", "Needs attention"))
  expect_equal(formatted$label[[1L]], "Tree and geography taxon names match")
  expect_match(formatted$next_step[[1L]], "identical", fixed = TRUE)
  expect_equal(formatted$label[[2L]], "Custom future check")
  expect_match(formatted$next_step[[2L]], "technical detail", fixed = TRUE)
  expect_equal(formatted$check, validation$check)
  expect_equal(formatted$detail, validation$detail)
})

test_that("validate_inputs catches common configuration errors", {
  config <- system.file("templates", "analysis.yml", package = "iBiogeobears")
  cfg <- read_config(config)
  temp_dir <- tempfile("ibgb-validation-")
  dir.create(temp_dir)

  bad_geography <- file.path(temp_dir, "bad_geography.csv")
  writeLines(c(
    "species,A,B,C",
    "sp1,1,0,0",
    "sp2,1,1,0",
    "spX,0,1,0",
    "sp4,0,1,1",
    "sp5,0,0,1"
  ), bad_geography)

  cfg$inputs$tree_file <- system.file("example_data", "tree.nwk", package = "iBiogeobears")
  cfg$inputs$geography_file <- bad_geography
  cfg$inputs$regions_file <- system.file("example_data", "regions.csv", package = "iBiogeobears")
  cfg$models$run <- c("DEC", "DEC")
  cfg$advanced$constraints <- list(dists_file = file.path(temp_dir, "missing_distances.txt"))

  checks <- validate_inputs(cfg)
  lookup <- stats::setNames(checks$ok, checks$check)

  expect_false(lookup[["tree_geography_species_match"]])
  expect_false(lookup[["models_not_duplicated"]])
  expect_false(lookup[["advanced_constraint_files_exist"]])
})
