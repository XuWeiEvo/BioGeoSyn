test_that("record_environment lists R and loaded packages with provenance", {
  env <- record_environment()
  expect_true(all(c("package", "version", "source", "remote_repo", "remote_sha") %in% names(env)))
  expect_equal(env$package[[1]], "R")
  expect_true("ape" %in% env$package)
  f <- tempfile(fileext = ".csv")
  record_environment(f)
  expect_true(file.exists(f))
})

test_that("check_environment reports matches and gives install commands for differences", {
  f <- tempfile(fileext = ".csv")
  env <- record_environment(f)
  ok <- check_environment(f, packages = c("R", "ape"), quiet = TRUE)
  expect_true(all(ok$status == "match"))
  env$version[env$package == "ape"] <- "0.0.1"
  env <- rbind(env, data.frame(package = "BioGeoBEARSfake", version = "1.1.3", source = "GitHub",
                               remote_repo = "nmatzke/BioGeoBEARSfake", remote_sha = "abc123"))
  utils::write.csv(env, f, row.names = FALSE)
  chk <- check_environment(f, packages = c("ape", "BioGeoBEARSfake"), quiet = TRUE)
  expect_equal(chk$status[chk$package == "ape"], "differs")
  expect_match(chk$install_command[chk$package == "ape"], 'install_version\\("ape", "0.0.1"\\)')
  expect_equal(chk$status[chk$package == "BioGeoBEARSfake"], "missing")
  expect_match(chk$install_command[chk$package == "BioGeoBEARSfake"], "nmatzke/BioGeoBEARSfake@abc123")
  expect_message(check_environment(f, packages = c("ape")), "differ")
  expect_error(check_environment(tempfile()), "No recorded environment")
})
