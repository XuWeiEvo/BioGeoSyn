test_that("ordinary-user guide is installed and reachable", {
  guide <- open_user_guide(browse = FALSE)
  expect_true(file.exists(guide))

  contents <- readLines(guide, warn = FALSE)
  required_text <- c(
    "check_installation()",
    "install_biogeobears(execute = TRUE)",
    "create_example_project",
    "run_workflow",
    "launch_app()",
    "create_windows_launcher()",
    "bundle_diagnostics",
    "run_acceptance_check"
  )
  expect_true(all(vapply(required_text, function(x) any(grepl(x, contents, fixed = TRUE)), logical(1))))
})

test_that("the guide documents the steps the current app actually shows", {
  contents <- paste(readLines(open_user_guide(browse = FALSE), warn = FALSE), collapse = "\n")
  # Wizard steps, named as the interface names them.
  for (step in c("1 · Data", "2 · Analysis", "3. Single clade", "4. Multi-clade synthesis")) {
    expect_match(contents, step, fixed = TRUE)
  }
  # The controls a first run depends on, and the headline feature.
  expect_match(contents, "Start the analysis", fixed = TRUE)
  expect_match(contents, "Run BSM stochastic mapping", fixed = TRUE)
  expect_match(contents, "Download result bundle", fixed = TRUE)
  expect_match(contents, "CPU cores", fixed = TRUE)
})

test_that("the guide and the interface are English only", {
  guide <- open_user_guide(browse = FALSE)
  contents <- readLines(guide, warn = FALSE, encoding = "UTF-8")
  cjk <- grepl("[\u4e00-\u9fff]", contents)
  expect_false(any(cjk), info = paste(contents[cjk], collapse = " | "))

  # The Chinese guide was retired when the interface went English-only, so
  # open_user_guide() no longer takes a language.
  expect_false("language" %in% names(formals(open_user_guide)))
  expect_length(list.files(system.file("docs", package = "BioGeoSyn"), pattern = "zh"), 0L)
})
