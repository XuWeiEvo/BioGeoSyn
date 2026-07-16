test_that("biogeobears_install_plan reports complete package readiness", {
  plan <- biogeobears_install_plan()

  expect_s3_class(plan, "data.frame")
  expect_true(all(c(
    "package", "source", "status", "version", "next_step"
  ) %in% names(plan)))
  expect_true(all(c(
    "rexpokit", "cladoRcpp", "devtools", "BioGeoBEARS"
  ) %in% plan$package))
  expect_equal(plan$source[plan$package == "MultinomialCI"], "CRAN Archive")
  expect_equal(plan$source[plan$package == "BioGeoBEARS"], "GitHub")
  expect_true(all(plan$status %in% c("Ready", "Action needed")))
  expect_true(all(nzchar(plan$next_step)))
})

test_that("install_biogeobears is non-mutating by default", {
  expect_equal(install_biogeobears(), biogeobears_install_plan())
})

test_that("installation path and CRAN repository helpers are safe", {
  lib <- tempfile("bgs-user-library-")

  expect_equal(resolve_install_library(lib), as_path(lib))
  expect_true(dir.exists(lib))
  expect_equal(
    unname(resolve_cran_repositories("@CRAN@")),
    "https://cloud.r-project.org"
  )
})
