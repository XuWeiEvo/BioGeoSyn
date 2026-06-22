test_that("planned_model_table summarizes configured models", {
  config <- list(models = list(run = c("DEC", "DEC+J", "BAYAREALIKE")))

  plan <- planned_model_table(config)

  expect_equal(plan$model, c("DEC", "DEC+J", "BAYAREALIKE"))
  expect_equal(plan$model_family, c("DEC", "DEC", "BAYAREALIKE"))
  expect_equal(plan$has_j, c(FALSE, TRUE, FALSE))
  expect_equal(plan$status, rep("planned", 3L))
})

test_that("create_iBGB_shiny_app builds a Shiny app when shiny is installed", {
  testthat::skip_if_not_installed("shiny")

  app <- create_iBGB_shiny_app()

  expect_s3_class(app, "shiny.appobj")
})

test_that("run_app_action captures errors in state messages", {
  state <- new.env(parent = emptyenv())
  state$message <- "Ready."
  state$messages <- "Ready."
  state$status_type <- "info"

  value <- run_app_action(state, {
    stop("broken", call. = FALSE)
  })

  expect_null(value)
  expect_equal(state$status_type, "error")
  expect_match(state$message, "Error: broken", fixed = TRUE)
  expect_true(any(grepl("Error: broken", state$messages, fixed = TRUE)))
})

test_that("download file helpers resolve and copy report files", {
  out <- tempfile("ibgb-shiny-download-")
  paths <- create_project(out)
  report <- file.path(paths$reports, "summary_report.html")
  writeLines("<html></html>", report)
  state <- new.env(parent = emptyenv())
  state$report <- NULL
  state$result <- list(project_paths = paths)

  resolved <- resolve_report_file(state)
  copied <- tempfile(fileext = ".html")
  copy_download_file(resolved, copied)

  expect_equal(basename(resolved), "summary_report.html")
  expect_true(file.exists(copied))
})

test_that("download file helper reports missing files clearly", {
  expect_error(
    require_existing_file(NULL, "missing file"),
    "missing file"
  )
})

test_that("Shiny server validates and dry-runs a workflow", {
  testthat::skip_if_not_installed("shiny")

  project <- create_example_project(tempfile("ibgb-shiny-server-"))

  shiny::testServer(iBGB_shiny_server, {
    session$setInputs(
      config_path = project$config,
      output_dir = project$output_dir,
      dry_run = TRUE,
      require_biogeobears = FALSE,
      force = FALSE,
      report_format = "source"
    )

    session$setInputs(validate = 1)
    state <- session$userData$state
    expect_true(all(state$validation$ok))
    expect_true(nrow(state$model_table) > 0L)
    expect_match(state$message, "Validation passed", fixed = TRUE)

    session$setInputs(run = 1)
    expect_s3_class(state$result, "iBGB_workflow_result")
    expect_true(isTRUE(state$result$dry_run))
    expect_true(file.exists(file.path(state$result$project_paths$tables, "workflow_manifest.csv")))
    expect_match(state$message, "Dry run completed", fixed = TRUE)
  })
})

test_that("Shiny server renders reports and bundles dry-run results", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if(Sys.which("zip") == "", "zip utility is not available")

  project <- create_example_project(tempfile("ibgb-shiny-render-"))

  shiny::testServer(iBGB_shiny_server, {
    session$setInputs(
      config_path = project$config,
      output_dir = project$output_dir,
      dry_run = TRUE,
      require_biogeobears = FALSE,
      force = FALSE,
      report_format = "source"
    )

    session$setInputs(run = 1)
    state <- session$userData$state
    expect_s3_class(state$result, "iBGB_workflow_result")

    session$setInputs(render_report = 1)
    expect_true(file.exists(state$report))
    expect_match(state$message, "Report:", fixed = TRUE)

    session$setInputs(bundle = 1)
    expect_true(file.exists(state$bundle))
    expect_match(state$message, "Bundle:", fixed = TRUE)
  })
})
