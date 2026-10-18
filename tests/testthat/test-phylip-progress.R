test_that("PHYLIP geography files are read like the CSV", {
  f <- tempfile(fileext = ".data")
  writeLines(c("3\t3 (A B C)", "sp1\t100", "sp2 011", "", "sp3\t111"), f)
  expect_true(is_phylip_geography(f))
  g <- read_phylip_geography(f)
  expect_equal(g$species, c("sp1", "sp2", "sp3"))
  expect_equal(names(g), c("species", "A", "B", "C"))
  expect_equal(unname(as.matrix(g[, -1])), rbind(c(1, 0, 0), c(0, 1, 1), c(1, 1, 1)))
  m <- read_range_matrix(f)
  expect_equal(colnames(m$matrix), c("A", "B", "C"))
  v <- read_geography_for_validation(f)
  expect_equal(v$taxa, c("sp1", "sp2", "sp3"))
  csv <- system.file("example_data", "geography.csv", package = "BioGeoSyn")
  if (!nzchar(csv)) csv <- testthat::test_path("..", "..", "inst", "example_data", "geography.csv")
  expect_false(is_phylip_geography(csv))
})

test_that("malformed PHYLIP geography is rejected with a clear message", {
  f <- tempfile(fileext = ".data")
  writeLines(c("2\t3 (A B C)", "sp1\t10", "sp2\t011"), f)
  expect_error(read_phylip_geography(f), "one 0/1 per area")
  writeLines(c("1\t2 (A B C)", "sp1\t101"), f)
  expect_error(read_phylip_geography(f), "declares 2 areas")
})

test_that("a PHYLIP geography validates and prepares like the CSV", {
  ex <- system.file("example_data", package = "BioGeoSyn")
  if (!nzchar(ex)) ex <- testthat::test_path("..", "..", "inst", "example_data")
  root <- tempfile("bgs-phylip-")
  dir.create(root)
  file.copy(file.path(ex, c("tree.nwk", "regions.csv", "geography.csv")), root)
  g <- utils::read.csv(file.path(root, "geography.csv"), check.names = FALSE)
  ph <- c(sprintf("%d\t%d (%s)", nrow(g), ncol(g) - 1L, paste(names(g)[-1], collapse = " ")),
          paste(g[[1]], apply(g[, -1], 1, paste0, collapse = ""), sep = "\t"))
  writeLines(ph, file.path(root, "geography.data"))
  cfg <- list(project = list(name = "phy", output_dir = file.path(root, "out")),
              inputs = list(tree_file = "tree.nwk", geography_file = "geography.data", regions_file = "regions.csv", max_range_size = 2L),
              models = list(run = "DEC"))
  yaml::write_yaml(cfg, file.path(root, "analysis.yml"))
  val <- validate_inputs(read_config(file.path(root, "analysis.yml")))
  csv_cfg <- cfg
  csv_cfg$inputs$geography_file <- "geography.csv"
  yaml::write_yaml(csv_cfg, file.path(root, "analysis_csv.yml"))
  val_csv <- validate_inputs(read_config(file.path(root, "analysis_csv.yml")))
  expect_equal(val$ok, val_csv$ok)
})

test_that("progress is reported to the console and to a callback", {
  seen <- character()
  old <- options(BioGeoSyn.progress_callback = function(msg, value) seen <<- c(seen, msg))
  on.exit(options(old))
  expect_message(NULL, NA)
  out <- capture.output(bgs_progress("Model 1/2: fitting DEC ...", value = 0.1), type = "message")
  expect_match(out, "\\[BioGeoSyn [0-9:]+\\] Model 1/2: fitting DEC")
  expect_equal(seen, "Model 1/2: fitting DEC ...")
  options(BioGeoSyn.progress = FALSE)
  expect_length(capture.output(bgs_progress("quiet"), type = "message"), 0L)
  options(BioGeoSyn.progress = NULL)
})
