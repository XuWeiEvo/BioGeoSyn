test_that("render_report writes report source when quarto is unavailable", {
  out <- tempfile("ibgb-report-source-")
  paths <- create_project(out)
  result <- list(project_paths = paths)

  report <- render_report(result, format = "source")

  expect_true(file.exists(report))
  expect_match(basename(report), "summary_report[.]qmd")
})
