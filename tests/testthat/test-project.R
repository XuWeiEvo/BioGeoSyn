test_that("create_project creates standard directories", {
  out <- tempfile("bgs-project-")
  paths <- create_project(out)
  expect_true(dir.exists(paths$raw_biogeobears))
  expect_true(dir.exists(paths$tables))
  expect_true(dir.exists(paths$reports))
})

test_that("create_example_project creates a runnable example", {
  out <- tempfile("bgs-example-project-")
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
  out <- tempfile("bgs-example-project-")
  dir.create(out)
  writeLines("keep", file.path(out, "existing.txt"))

  expect_error(create_example_project(out), "already contains files")
})

test_that("create_analysis_project copies inputs and writes valid config", {
  source <- create_example_project(tempfile("bgs-analysis-source-"))
  target <- tempfile("bgs-analysis-project-")

  project <- create_analysis_project(
    path = target,
    project_name = "My clade study",
    tree_file = source$tree_file,
    geography_file = source$geography_file,
    regions_file = source$regions_file,
    max_range_size = 2,
    models = c("DEC", "DEC+J")
  )
  cfg <- read_config(project$config)

  expect_equal(project$project_name, "My_clade_study")
  expect_equal(cfg$project$name, "My_clade_study")
  expect_equal(cfg$inputs$max_range_size, 2L)
  expect_equal(cfg$models$run, c("DEC", "DEC+J"))
  expect_true(all(file.exists(c(
    project$config, project$tree_file, project$geography_file, project$regions_file
  ))))
  expect_true(all(project$validation$ok))
  expect_equal(cfg$project$output_dir, project$output_dir)
})

test_that("create_analysis_project rejects unsafe or incomplete requests", {
  source <- create_example_project(tempfile("bgs-analysis-invalid-source-"))

  expect_error(
    create_analysis_project(
      tempfile("bgs-analysis-invalid-"),
      "***",
      source$tree_file,
      source$geography_file,
      source$regions_file
    ),
    "Project name"
  )
  expect_error(
    create_analysis_project(
      tempfile("bgs-analysis-invalid-"),
      "study",
      "missing-tree.nwk",
      source$geography_file,
      source$regions_file
    ),
    "tree_file"
  )
  expect_error(
    create_analysis_project(
      tempfile("bgs-analysis-invalid-"),
      "study",
      source$tree_file,
      source$geography_file,
      source$regions_file,
      models = "UNKNOWN"
    ),
    "Unsupported"
  )
})

test_that("installed package exposes templates, example data, and public API", {
  expect_true(file.exists(system.file("templates", "analysis.yml", package = "BioGeoSyn")))
  expect_true(file.exists(system.file("example_data", "tree.nwk", package = "BioGeoSyn")))
  expect_true(file.exists(system.file("example_data", "geography.csv", package = "BioGeoSyn")))
  expect_true(file.exists(system.file("example_data", "regions.csv", package = "BioGeoSyn")))

  exported <- getNamespaceExports("BioGeoSyn")
  expect_true(all(c(
    "bundle_diagnostics",
    "bundle_results",
    "biogeobears_install_plan",
    "create_workflow_manifest",
    "create_analysis_project",
    "create_example_project",
    "create_windows_launcher",
    "install_biogeobears",
    "open_user_guide",
    "run_acceptance_check",
    "run_workflow",
    "render_report",
    "run_models"
  ) %in% exported))

  out <- tempfile("bgs-installed-assets-")
  project <- create_example_project(out)
  result <- run_workflow(project$config, dry_run = TRUE, require_biogeobears = FALSE)

  expect_s3_class(result, "bgs_workflow_result")
  expect_true(file.exists(file.path(result$project_paths$tables, "model_run_plan.csv")))
  expect_true(all(result$validation$ok))
})
