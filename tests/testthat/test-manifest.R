test_that("create_workflow_manifest lists workflow output files", {
  out <- tempfile("ibgb-manifest-")
  paths <- create_project(out)
  writeLines("a,b\n1,2", file.path(paths$tables, "example.csv"))
  writeLines("log", file.path(paths$logs, "example.log"))

  manifest <- create_workflow_manifest(out)

  expect_true(file.exists(file.path(paths$tables, "workflow_manifest.csv")))
  expect_true(all(c(
    "category", "relative_path", "file_name", "extension",
    "size_bytes", "modified_time"
  ) %in% names(manifest)))
  expect_true("tables/example.csv" %in% manifest$relative_path)
  expect_true("logs/example.log" %in% manifest$relative_path)
  expect_true("tables/workflow_manifest.csv" %in% manifest$relative_path)
})

test_that("bundle_results creates a zip archive", {
  out <- tempfile("ibgb-bundle-")
  paths <- create_project(out)
  writeLines("a,b\n1,2", file.path(paths$tables, "example.csv"))
  writeLines("raw", file.path(paths$raw_biogeobears, "raw.txt"))
  bundle_file <- tempfile(fileext = ".zip")

  bundle <- bundle_results(out, bundle_file = bundle_file)

  expect_true(file.exists(bundle))
  expect_gt(file.info(bundle)$size, 0)
  manifest <- utils::read.csv(file.path(paths$tables, "workflow_manifest.csv"), check.names = FALSE)
  expect_true("raw_biogeobears/raw.txt" %in% manifest$relative_path)
})

test_that("bundle_results can omit raw BioGeoBEARS files", {
  out <- tempfile("ibgb-bundle-no-raw-")
  paths <- create_project(out)
  writeLines("a,b\n1,2", file.path(paths$tables, "example.csv"))
  writeLines("raw", file.path(paths$raw_biogeobears, "raw.txt"))
  bundle_file <- tempfile(fileext = ".zip")

  bundle <- bundle_results(out, bundle_file = bundle_file, include_raw = FALSE)
  contents <- utils::unzip(bundle, list = TRUE)

  expect_true("tables/example.csv" %in% contents$Name)
  expect_false("raw_biogeobears/raw.txt" %in% contents$Name)
})

test_that("bundle_diagnostics creates a lightweight diagnostic archive", {
  out <- tempfile("ibgb-diagnostics-")
  paths <- create_project(out)
  model_log <- file.path(paths$raw_biogeobears, "DEC", "DEC.log")
  dir.create(dirname(model_log), recursive = TRUE, showWarnings = FALSE)
  writeLines("project: diagnostic", file.path(paths$root, "config_used.yml"))
  writeLines("session", file.path(paths$logs, "session_info.txt"))
  writeLines("citation", file.path(paths$logs, "biogeobears_citation.txt"))
  writeLines("optimizer log", model_log)
  writeLines("raw result", file.path(paths$raw_biogeobears, "DEC", "DEC.rds"))
  utils::write.csv(
    data.frame(
      model = "DEC",
      status = "failed",
      error_message = "optimizer failed",
      log_file = model_log
    ),
    file.path(paths$tables, "model_run_status.csv"),
    row.names = FALSE
  )
  bundle_file <- tempfile(fileext = ".zip")

  bundle <- bundle_diagnostics(out, bundle_file = bundle_file)
  contents <- utils::unzip(bundle, list = TRUE)

  expect_true(file.exists(bundle))
  expect_true("config_used.yml" %in% contents$Name)
  expect_true("tables/model_run_status.csv" %in% contents$Name)
  expect_true("tables/workflow_manifest.csv" %in% contents$Name)
  expect_true("logs/session_info.txt" %in% contents$Name)
  expect_true("logs/biogeobears_citation.txt" %in% contents$Name)
  expect_true("raw_biogeobears/DEC/DEC.log" %in% contents$Name)
  expect_false("raw_biogeobears/DEC/DEC.rds" %in% contents$Name)
})
