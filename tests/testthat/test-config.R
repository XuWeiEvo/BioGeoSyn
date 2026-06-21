test_that("read_config fills defaults", {
  config <- system.file("templates", "analysis.yml", package = "iBiogeobears")
  cfg <- read_config(config)
  expect_equal(cfg$project$name, "example_clade")
  expect_true("DEC+J" %in% cfg$models$run)
  expect_false(cfg$methodology$auto_declare_best_model)
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
