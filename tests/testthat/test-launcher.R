test_that("Windows launcher template is packaged and self-contained", {
  template <- windows_launcher_template_path()

  expect_true(file.exists(template))
  contents <- readLines(template, warn = FALSE)
  expect_true(any(grepl("Rscript.exe", contents, fixed = TRUE)))
  expect_true(any(grepl("BioGeoSyn::launch_app", contents, fixed = TRUE)))
  expect_true(any(grepl("requireNamespace('shiny'", contents, fixed = TRUE)))
})

test_that("create_windows_launcher writes a double-click launcher", {
  out <- tempfile("bgs-launcher-")
  dir.create(out)
  path <- file.path(out, "start-BioGeoSyn.bat")

  launcher <- create_windows_launcher(path)

  expect_equal(launcher, as_path(path))
  expect_true(file.exists(launcher))
  expect_error(create_windows_launcher(path), "already exists", fixed = TRUE)

  overwritten <- create_windows_launcher(path, overwrite = TRUE)
  expect_equal(overwritten, as_path(path))
})

test_that("create_windows_launcher accepts a target directory", {
  out <- tempfile("bgs-launcher-dir-")
  dir.create(out)

  launcher <- create_windows_launcher(out)

  expect_equal(basename(launcher), "start-BioGeoSyn.bat")
  expect_true(file.exists(launcher))
})
