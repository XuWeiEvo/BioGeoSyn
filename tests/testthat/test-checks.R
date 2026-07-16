test_that("check_installation returns actionable workflow readiness", {
  checks <- check_installation()

  expect_s3_class(checks, "data.frame")
  expect_true(all(c(
    "component", "required_for", "required", "status", "version", "next_step"
  ) %in% names(checks)))
  expect_true(all(c(
    "R", "Core R packages", "Shiny", "BioGeoBEARS", "Quarto HTML", "Quarto PDF"
  ) %in% checks$component))
  expect_true(all(checks$status %in% c("Ready", "Action needed")))
  expect_true(all(nzchar(checks$next_step)))
  expect_equal(checks$status[checks$component == "R"], "Ready")

  core <- checks[checks$component == "Core R packages", , drop = FALSE]
  if (identical(core$status[[1L]], "Ready")) {
    expect_equal(core$next_step[[1L]], "Ready.")
  } else {
    expect_match(core$next_step[[1L]], "Install missing packages", fixed = TRUE)
  }
})

test_that("check_installation can omit optional PDF readiness", {
  checks <- check_installation(include_pdf = FALSE)

  expect_false("Quarto PDF" %in% checks$component)
  expect_true("Quarto HTML" %in% checks$component)
})

test_that("core package guidance distinguishes missing and unloadable packages", {
  status <- data.frame(
    package = c("missingPkg", "igraph"),
    installed = c(FALSE, TRUE),
    available = c(FALSE, FALSE),
    version = NA_character_,
    error_message = c(NA_character_, "libglpk.so.40: cannot open shared object file"),
    stringsAsFactors = FALSE
  )

  guidance <- BioGeoSyn:::core_package_next_step(status)

  expect_match(guidance, "install.packages\\(c\\('missingPkg'\\)\\)")
  expect_match(guidance, "Installed package\\(s\\) could not be loaded: igraph")
  expect_match(guidance, "libglpk-dev and libxml2-dev", fixed = TRUE)
  expect_match(guidance, "libglpk.so.40", fixed = TRUE)
})
