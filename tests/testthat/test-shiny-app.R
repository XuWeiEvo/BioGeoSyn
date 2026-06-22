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
