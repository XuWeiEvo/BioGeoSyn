test_that("create_project creates standard directories", {
  out <- tempfile("ibgb-project-")
  paths <- create_project(out)
  expect_true(dir.exists(paths$raw_biogeobears))
  expect_true(dir.exists(paths$tables))
  expect_true(dir.exists(paths$reports))
})

test_that("create_example_project creates a runnable example", {
  out <- tempfile("ibgb-example-project-")
  paths <- create_example_project(out)

  expect_true(file.exists(paths$config))
  expect_true(file.exists(paths$tree_file))
  expect_true(file.exists(paths$geography_file))
  expect_true(file.exists(paths$regions_file))

  cfg <- read_config(paths$config)
  expect_equal(cfg$inputs$tree_file, "data/tree.nwk")
  expect_equal(cfg$inputs$geography_file, "data/geography.csv")
  expect_true(grepl("results/example_clade$", cfg$project$output_dir))

  checks <- validate_inputs(cfg)
  expect_true(all(checks$ok))
})

test_that("create_example_project protects non-empty directories", {
  out <- tempfile("ibgb-example-project-")
  dir.create(out)
  writeLines("keep", file.path(out, "existing.txt"))

  expect_error(create_example_project(out), "already contains files")
})
