test_that("quick acceptance check covers the installed user workflow", {
  out <- tempfile("ibgb-acceptance-")
  result <- run_acceptance_check(out, mode = "quick")
  failed <- result$checks[result$checks$status == "Failed", , drop = FALSE]
  failure_info <- if (nrow(failed) == 0L) {
    "No failed acceptance row was recorded."
  } else {
    paste(
      paste0(
        failed$check,
        ": ",
        failed$detail,
        " Next step: ",
        failed$next_step
      ),
      collapse = "\n"
    )
  }

  expect_s3_class(result, "iBGB_acceptance_result")
  expect_true(result$passed, info = failure_info)
  expect_true(file.exists(result$results_file))
  expect_true(file.exists(result$session_file))
  expect_true(file.exists(result$source_report))
  expect_true(file.exists(result$result_bundle))
  expect_true(file.exists(result$diagnostic_bundle))
  expect_true(all(c(
    "timestamp_utc", "mode", "platform", "os", "r_version",
    "package_version", "check", "required", "status", "elapsed_seconds",
    "detail", "next_step"
  ) %in% names(result$checks)))
  expect_true(all(result$checks$status %in% c("Passed", "Failed", "Skipped")))
  expect_equal(
    result$checks$status[result$checks$check == "Plan six-model workflow"],
    "Passed"
  )
  expect_equal(
    result$checks$status[result$checks$check == "BioGeoBEARS installation"],
    "Skipped"
  )
  expect_equal(
    result$checks$status[result$checks$check == "Render HTML report"],
    "Skipped"
  )
})

test_that("acceptance check protects existing directories", {
  out <- tempfile("ibgb-acceptance-existing-")
  dir.create(out)
  writeLines("keep", file.path(out, "existing.txt"))

  expect_error(
    run_acceptance_check(out, mode = "quick"),
    "already contains files"
  )
  expect_true(file.exists(file.path(out, "existing.txt")))
})
