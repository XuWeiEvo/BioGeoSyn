constraint_template <- function(name) {
  system.file("example_data", "constraints", name, package = "BioGeoSyn")
}

example_input <- function(name) {
  system.file("example_data", name, package = "BioGeoSyn")
}

constraint_config <- function(constraints, output_dir, models = list("DEC")) {
  list(
    project = list(name = "constraint_test", output_dir = output_dir),
    inputs = list(
      tree_file = example_input("tree.nwk"),
      geography_file = example_input("geography.csv"),
      regions_file = example_input("regions.csv"),
      max_range_size = 3L
    ),
    models = list(run = models),
    analysis = list(run_stochastic_mapping = FALSE, resume_completed_models = FALSE),
    figures = list(output_formats = list("png")),
    report = list(formats = list("html")),
    advanced = list(constraints = constraints)
  )
}

# The matrix-style constraint files BioGeoBEARS reads expect a header of area
# names followed by rows of plain numbers -- NO row labels. Row labels make
# read_distances_fn() and friends abort ("you have N area names, but 1
# numbers"), which silently failed every constrained run before this guard.
test_that("shipped matrix constraint templates use the BioGeoBEARS layout", {
  matrix_templates <- c(
    "distances.txt", "areas_allowed.txt",
    "dispersal_multipliers.txt", "areas_adjacency.txt"
  )
  for (name in matrix_templates) {
    path <- constraint_template(name)
    expect_true(file.exists(path), info = name)
    lines <- readLines(path, warn = FALSE)
    lines <- trimws(lines)
    lines <- lines[nzchar(lines) & lines != "END"]
    header <- strsplit(lines[[1L]], "[[:space:]]+")[[1L]]
    n_areas <- length(header)
    expect_gt(n_areas, 1L)
    for (line in lines) {
      fields <- strsplit(line, "[[:space:]]+")[[1L]]
      expect_equal(length(fields), n_areas, info = paste(name, "line:", line))
      is_header <- identical(fields, header)
      if (!is_header) {
        # Data rows must be numbers, not "A 1 1 1".
        expect_false(
          any(is.na(suppressWarnings(as.numeric(fields)))),
          info = paste(name, "non-numeric data row:", line)
        )
      }
    }
  }
})

test_that("area_of_areas and times templates keep their own layouts", {
  aoa <- readLines(constraint_template("area_of_areas.txt"), warn = FALSE)
  aoa <- trimws(aoa)
  aoa <- aoa[nzchar(aoa) & aoa != "END"]
  # header + a single row of per-area values per stratum
  expect_equal(length(strsplit(aoa[[2L]], "[[:space:]]+")[[1L]]), 3L)
  expect_false(any(is.na(suppressWarnings(as.numeric(strsplit(aoa[[2L]], "[[:space:]]+")[[1L]])))))

  times <- readLines(constraint_template("times.txt"), warn = FALSE)
  times <- trimws(times)
  times <- times[nzchar(times)]
  expect_false(any(is.na(suppressWarnings(as.numeric(times)))))
})

test_that("copy_constraint_files preserves each constraint in the project inputs", {
  input_dir <- tempfile("inputs-")
  dir.create(input_dir, recursive = TRUE)
  cfg <- constraint_config(
    list(
      times_file = constraint_template("times.txt"),
      dists_file = constraint_template("distances.txt")
    ),
    output_dir = tempfile("proj-")
  )
  preserved <- copy_constraint_files(cfg, input_dir)

  expect_setequal(names(preserved), c("times_file", "dists_file"))
  # Copies live in the project, named after the config field, and match the source.
  expect_equal(basename(preserved$times_file), "times_file.txt")
  expect_equal(basename(preserved$dists_file), "dists_file.txt")
  expect_true(all(file.exists(unlist(preserved))))
  expect_equal(
    readLines(preserved$dists_file, warn = FALSE),
    readLines(constraint_template("distances.txt"), warn = FALSE)
  )
})

test_that("copy_constraint_files skips missing files and returns NULL when unused", {
  input_dir <- tempfile("inputs-")
  dir.create(input_dir, recursive = TRUE)

  expect_null(copy_constraint_files(constraint_config(list(), tempfile()), input_dir))
  expect_null(copy_constraint_files(constraint_config(NULL, tempfile()), input_dir))

  partial <- copy_constraint_files(
    constraint_config(
      list(times_file = constraint_template("times.txt"), dists_file = "does-not-exist.txt"),
      tempfile()
    ),
    input_dir
  )
  expect_equal(names(partial), "times_file")
})

# Shiny hands every upload a datapath like <tmp>/0.txt, so copying by basename
# would let one constraint overwrite another. Names come from the config field.
test_that("uploads sharing a basename do not overwrite each other", {
  input_dir <- tempfile("inputs-")
  dir.create(input_dir, recursive = TRUE)
  upload_a <- file.path(tempfile("up-a-"), "0.txt")
  upload_b <- file.path(tempfile("up-b-"), "0.txt")
  dir.create(dirname(upload_a), recursive = TRUE)
  dir.create(dirname(upload_b), recursive = TRUE)
  writeLines("times-content", upload_a)
  writeLines("areas-content", upload_b)

  preserved <- copy_constraint_files(
    constraint_config(list(times_file = upload_a, areas_allowed_file = upload_b), tempfile()),
    input_dir
  )
  expect_equal(readLines(preserved$times_file, warn = FALSE), "times-content")
  expect_equal(readLines(preserved$areas_allowed_file, warn = FALSE), "areas-content")
})

test_that("apply_constraint_files maps every config field to its run-object slot", {
  run_object <- list()
  constraints <- list(
    times_file = "t.txt", dists_file = "d.txt", dispersal_multipliers_file = "dm.txt",
    areas_allowed_file = "aa.txt", areas_adjacency_file = "adj.txt", area_of_areas_file = "aoa.txt"
  )
  out <- apply_constraint_files(run_object, constraints, base_dir = ".")
  expect_equal(basename(out$timesfn), "t.txt")
  expect_equal(basename(out$distsfn), "d.txt")
  expect_equal(basename(out$dispersal_multipliers_fn), "dm.txt")
  expect_equal(basename(out$areas_allowed_fn), "aa.txt")
  expect_equal(basename(out$areas_adjacency_fn), "adj.txt")
  expect_equal(basename(out$area_of_areas_fn), "aoa.txt")

  expect_equal(apply_constraint_files(list(), NULL, "."), list())
})

# --- validation of constraint-set consistency -------------------------------
# Each rule below mirrors a failure mode confirmed against BioGeoBEARS itself.

validate_constraints <- function(constraints) {
  cfg <- constraint_config(constraints, tempfile("proj-"))
  cfg$.config_file <- file.path(tempdir(), "config.yml")
  v <- validate_inputs(cfg)
  v[grepl("^advanced_constraint", v$check), , drop = FALSE]
}

write_blocks <- function(n_blocks, rows, areas = c("A", "B", "C")) {
  path <- tempfile(fileext = ".txt")
  block <- c(paste(areas, collapse = "\t"), rows)
  writeLines(c(rep(c(block, ""), n_blocks), "END"), path)
  path
}

square_rows <- function() c("1\t1\t1", "1\t1\t1", "1\t1\t1")

test_that("valid constraints pass every consistency check", {
  v <- validate_constraints(list(dists_file = constraint_template("distances.txt")))
  expect_true(all(v$ok))
})

test_that("row labels in a constraint file are rejected", {
  bad <- write_blocks(1L, c("A\t1\t1\t1", "B\t1\t1\t1", "C\t1\t1\t1"))
  v <- validate_constraints(list(dispersal_multipliers_file = bad))
  expect_false(v$ok[v$check == "advanced_constraint_layout"])
  expect_match(v$detail[v$check == "advanced_constraint_layout"], "row labels", fixed = TRUE)
})

test_that("constraint areas must match the geography areas", {
  wrong <- write_blocks(1L, square_rows(), areas = c("X", "Y", "Z"))
  v <- validate_constraints(list(areas_allowed_file = wrong))
  expect_false(v$ok[v$check == "advanced_constraint_areas_match_geography"])
})

test_that("fewer constraint blocks than time bins is rejected", {
  # BioGeoBEARS aborts when a stratified file has no block for a time bin;
  # extra blocks are tolerated, so only under-provisioning may fail.
  times <- tempfile(fileext = ".txt")
  writeLines(c("1.5", "3", "5"), times)

  too_few <- write_blocks(2L, square_rows())
  v <- validate_constraints(list(times_file = times, dispersal_multipliers_file = too_few))
  expect_false(v$ok[v$check == "advanced_constraint_strata_cover_times"])

  enough <- write_blocks(3L, square_rows())
  v_ok <- validate_constraints(list(times_file = times, dispersal_multipliers_file = enough))
  expect_true(v_ok$ok[v_ok$check == "advanced_constraint_strata_cover_times"])

  extra <- write_blocks(4L, square_rows())
  v_extra <- validate_constraints(list(times_file = times, dispersal_multipliers_file = extra))
  expect_true(v_extra$ok[v_extra$check == "advanced_constraint_strata_cover_times"])
})

test_that("the oldest time bin must be older than the tree root", {
  # Example tree root age is 2.
  too_young <- tempfile(fileext = ".txt")
  writeLines(c("0.75", "1.5"), too_young)
  v <- validate_constraints(list(times_file = too_young))
  expect_false(v$ok[v$check == "advanced_constraint_times_cover_root"])

  covers <- tempfile(fileext = ".txt")
  writeLines(c("1.5", "5"), covers)
  v_ok <- validate_constraints(list(times_file = covers))
  expect_true(v_ok$ok[v_ok$check == "advanced_constraint_times_cover_root"])
})

test_that("a time bin landing on a tree node is rejected", {
  # section_the_tree() aborts with "your tree has 2 nodes with date 1".
  on_node <- tempfile(fileext = ".txt")
  writeLines(c("1", "5"), on_node)
  v <- validate_constraints(list(times_file = on_node))
  expect_false(v$ok[v$check == "advanced_constraint_times_avoid_nodes"])

  off_node <- tempfile(fileext = ".txt")
  writeLines(c("1.5", "5"), off_node)
  v_ok <- validate_constraints(list(times_file = off_node))
  expect_true(v_ok$ok[v_ok$check == "advanced_constraint_times_avoid_nodes"])
})

test_that("a constrained run completes, preserves its constraint, and frees x", {
  testthat::skip_if_not_installed("BioGeoBEARS")

  out_dir <- tempfile("constrained-run-")
  cfg <- constraint_config(list(dists_file = constraint_template("distances.txt")), out_dir)
  cfg_path <- tempfile(fileext = ".yml")
  yaml::write_yaml(cfg, cfg_path)

  result <- run_workflow(cfg_path, dry_run = FALSE, require_biogeobears = TRUE, force = TRUE)
  expect_equal(result$model_run_status$status[[1L]], "completed")

  # The distance file is preserved inside the project, so the saved run (and its
  # bundle) can be reproduced after the caller's copy is gone.
  preserved <- file.path(result$project_paths$inputs, "dists_file.txt")
  expect_true(file.exists(preserved))
  expect_equal(
    readLines(preserved, warn = FALSE),
    readLines(constraint_template("distances.txt"), warn = FALSE)
  )

  # Supplying distances frees the distance-decay exponent x (fixed at 0 otherwise).
  params <- utils::read.csv(
    file.path(result$project_paths$tables, "model_parameters.csv"),
    stringsAsFactors = FALSE
  )
  x_row <- params[params$parameter == "x", , drop = FALSE]
  expect_equal(nrow(x_row), 1L)
  expect_true(x_row$is_free[[1L]])
  expect_equal(x_row$type[[1L]], "free")

  # Preserved constraints reach the result bundle via the workflow manifest.
  manifest <- create_workflow_manifest(result$project_paths$root, write = FALSE)
  expect_true(any(grepl("dists_file.txt", manifest$relative_path, fixed = TRUE)))
})

test_that("a time-stratified run completes", {
  testthat::skip_if_not_installed("BioGeoBEARS")

  # A times file makes BioGeoBEARS demand a sectioned tree; without
  # section_the_tree() every stratified run aborted with "You have time slices,
  # but you do not have 'inputs$tree_sections_list'". Bins avoid the example
  # tree's node dates (0.5, 1, 2) and the oldest bin clears its root age (2).
  times <- tempfile(fileext = ".txt")
  writeLines(c("1.5", "5"), times)
  multipliers <- write_blocks(2L, square_rows())

  out_dir <- tempfile("stratified-run-")
  cfg <- constraint_config(
    list(times_file = times, dispersal_multipliers_file = multipliers),
    out_dir
  )
  cfg_path <- tempfile(fileext = ".yml")
  yaml::write_yaml(cfg, cfg_path)

  result <- run_workflow(cfg_path, dry_run = FALSE, require_biogeobears = TRUE, force = TRUE)
  expect_equal(result$model_run_status$status[[1L]], "completed")
  expect_true(file.exists(file.path(result$project_paths$inputs, "times_file.txt")))
})
