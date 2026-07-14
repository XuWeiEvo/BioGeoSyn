make_input_summary_fixture <- function(dir) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  tree_file <- file.path(dir, "tree.nwk")
  geography_file <- file.path(dir, "geography.csv")
  regions_file <- file.path(dir, "regions.csv")

  writeLines("((sp1:1,sp2:1):1,(sp3:1,(sp4:1,sp5:1):1):1);", tree_file)
  geography <- data.frame(
    species = c("sp1", "sp2", "sp3", "sp4", "sp5"),
    A = c(1, 1, 0, 0, 0),
    B = c(0, 1, 1, 1, 0),
    C = c(0, 0, 0, 1, 1),
    stringsAsFactors = FALSE
  )
  utils::write.csv(geography, geography_file, row.names = FALSE)
  regions <- data.frame(
    region = c("A", "B", "C"),
    label = c("Region A", "Region B", "Region C"),
    color = c("#1b9e77", "#d95f02", "#7570b3"),
    stringsAsFactors = FALSE
  )
  utils::write.csv(regions, regions_file, row.names = FALSE)

  list(
    inputs = list(
      tree_file = tree_file,
      geography_file = geography_file,
      regions_file = regions_file,
      max_range_size = 3L
    )
  )
}

test_that("summarize_input_data reports tree, geography, and region occupancy", {
  dir <- tempfile("ibgb-input-")
  config <- make_input_summary_fixture(dir)

  summary <- summarize_input_data(config, base_dir = dir)

  expect_s3_class(summary, "iBGB_input_summary")
  expect_equal(summary$tree$n_tips, 5L)
  expect_true(summary$tree$has_branch_lengths)

  expect_equal(summary$geography$n_species, 5L)
  expect_equal(summary$geography$n_areas, 3L)
  expect_equal(summary$geography$max_range_size_setting, 3L)
  # sp2 (A,B) and sp4 (B,C) span two areas; the rest are single-area.
  expect_equal(summary$geography$widespread_species, 2L)
  expect_equal(summary$geography$single_area_species, 3L)
  expect_equal(summary$geography$max_range_size_observed, 2L)

  occ <- summary$region_occupancy
  expect_equal(nrow(occ), 3L)
  # Region B holds sp2, sp3, sp4.
  expect_equal(occ$n_species[occ$region == "B"], 3L)
  expect_equal(occ$n_species[occ$region == "A"], 2L)
  expect_equal(occ$n_species[occ$region == "C"], 2L)
  # Endemics (single-area species): A has sp1, C has sp5, B has sp3.
  expect_equal(occ$n_endemic[occ$region == "A"], 1L)
  expect_equal(occ$n_endemic[occ$region == "B"], 1L)
  expect_equal(occ$n_endemic[occ$region == "C"], 1L)
  # Region metadata labels are joined onto the area ids.
  expect_equal(occ$label[occ$region == "B"], "Region B")
  # Ordered by descending occupancy: B first.
  expect_equal(occ$region[[1]], "B")
})

test_that("summarize_input_data range-size distribution sums to species count", {
  dir <- tempfile("ibgb-input-")
  config <- make_input_summary_fixture(dir)

  summary <- summarize_input_data(config, base_dir = dir)
  dist <- summary$range_size_distribution

  expect_equal(sum(dist$n_species), summary$geography$n_species)
  expect_equal(dist$n_species[dist$range_size == 1L], 3L)
  expect_equal(dist$n_species[dist$range_size == 2L], 2L)
  expect_equal(sum(dist$proportion), 1)
})

test_that("summarize_input_data flags tree/geography name mismatches", {
  dir <- tempfile("ibgb-input-")
  config <- make_input_summary_fixture(dir)

  summary <- summarize_input_data(config, base_dir = dir)
  expect_true(summary$taxon_match$all_match)
  expect_equal(summary$taxon_match$n_shared, 5L)

  # Drop a tip from the tree so it no longer matches the geography.
  writeLines("((sp1:1,sp2:1):1,sp3:1);", file.path(dir, "tree.nwk"))
  summary2 <- summarize_input_data(config, base_dir = dir)
  expect_false(summary2$taxon_match$all_match)
  expect_setequal(summary2$taxon_match$missing_from_tree, c("sp4", "sp5"))
})

test_that("summarize_input_data degrades gracefully when files are missing", {
  summary <- summarize_input_data(list(inputs = list()))
  expect_s3_class(summary, "iBGB_input_summary")
  expect_null(summary$tree)
  expect_null(summary$geography)
  expect_null(summary$region_occupancy)
  expect_null(summary$taxon_match)
  expect_equal(nrow(summary$overview), 0L)
})

test_that("print.iBGB_input_summary shows the overview and region table", {
  dir <- tempfile("ibgb-input-")
  config <- make_input_summary_fixture(dir)
  summary <- summarize_input_data(config, base_dir = dir)

  out <- utils::capture.output(print(summary))
  expect_true(any(grepl("input data overview", out)))
  expect_true(any(grepl("Tree tips", out)))
  expect_true(any(grepl("Species per region", out)))
})

test_that("shiny overview display helpers turn a summary into 2-column tables", {
  dir <- tempfile("ibgb-input-")
  config <- make_input_summary_fixture(dir)
  summary <- summarize_input_data(config, base_dir = dir)

  overview <- shiny_data_overview_table(summary)
  expect_equal(ncol(overview), 2L)
  expect_true(nrow(overview) >= 5L)

  occ <- shiny_region_occupancy_table(summary)
  expect_equal(nrow(occ), 3L)
  expect_equal(ncol(occ), 5L)

  rng <- shiny_range_size_table(summary)
  expect_equal(nrow(rng), 2L)
  expect_equal(ncol(rng), 3L)
})

test_that("shiny overview display helpers return empty frames for NULL summaries", {
  expect_equal(nrow(shiny_data_overview_table(NULL)), 0L)
  expect_equal(nrow(shiny_region_occupancy_table(NULL)), 0L)
  expect_equal(nrow(shiny_range_size_table(NULL)), 0L)
})
