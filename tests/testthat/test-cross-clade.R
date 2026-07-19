write_clade_rates <- function(dir, clade, mean_counts) {
  tables <- file.path(dir, clade, "tables")
  dir.create(tables, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(tables, "process_rates_through_time.csv")
  df <- data.frame(
    model = "DEC+J",
    process_key = "range_expansion",
    process_label = "Range expansion",
    process_group = "anagenetic",
    time_bin = seq_along(mean_counts),
    bin_start = seq_along(mean_counts) - 1,
    bin_end = seq_along(mean_counts),
    bin_midpoint = seq_along(mean_counts) - 0.5,
    mean_count = mean_counts,
    sd_count = 0.1,
    ci_lower = pmax(0, mean_counts - 0.2),
    ci_upper = mean_counts + 0.2,
    rate = mean_counts,
    interpretation_note = "test",
    stringsAsFactors = FALSE
  )
  utils::write.csv(df, path, row.names = FALSE)
  path
}

write_clade_region_rates <- function(dir, clade, regions, mean_counts) {
  tables <- file.path(dir, clade, "tables")
  dir.create(tables, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(tables, "region_process_rates_through_time.csv")
  df <- do.call(rbind, lapply(regions, function(region) {
    data.frame(
      model = "DEC+J",
      process_key = "range_expansion",
      process_label = "Range expansion",
      process_group = "anagenetic",
      region = region,
      time_bin = seq_along(mean_counts),
      bin_start = seq_along(mean_counts) - 1,
      bin_end = seq_along(mean_counts),
      bin_midpoint = seq_along(mean_counts) - 0.5,
      mean_count = mean_counts,
      sd_count = 0.1,
      ci_lower = pmax(0, mean_counts - 0.2),
      ci_upper = mean_counts + 0.2,
      rate = mean_counts,
      interpretation_note = "test",
      stringsAsFactors = FALSE
    )
  }))
  utils::write.csv(df, path, row.names = FALSE)
  path
}

test_that("combine_process_rates_across_clades tags and merges clades", {
  root <- tempfile("bgs-crossclade-")
  f1 <- write_clade_rates(root, "Anolis", c(1, 2, 3))
  f2 <- write_clade_rates(root, "Phelsuma", c(3, 2, 1))

  out <- combine_process_rates_across_clades(c(f1, f2))
  expect_true("clade" %in% names(out))
  expect_setequal(unique(out$clade), c("Anolis", "Phelsuma"))
  expect_equal(nrow(out), 6L)
  expect_equal(out$mean_count[out$clade == "Anolis"], c(1, 2, 3))

  # Explicit clade names override the derived names.
  named <- combine_process_rates_across_clades(c(f1, f2), clade_names = c("A", "B"))
  expect_setequal(unique(named$clade), c("A", "B"))
})

test_that("combine_process_rates_across_clades returns an empty table for no valid files", {
  expect_equal(nrow(combine_process_rates_across_clades(character())), 0L)
  expect_true("clade" %in% names(combine_process_rates_across_clades(character())))
  expect_equal(nrow(combine_process_rates_across_clades("does-not-exist.csv")), 0L)
})

test_that("duplicate clade labels are disambiguated", {
  root <- tempfile("bgs-crossclade-dup-")
  # Same clade folder name reused would collide; force identical labels.
  f1 <- write_clade_rates(root, "same", c(1, 2))
  f2 <- write_clade_rates(tempfile("other-"), "same", c(3, 4))
  out <- combine_process_rates_across_clades(c(f1, f2), clade_names = c("Clade", "Clade"))
  expect_equal(length(unique(out$clade)), 2L)
})

test_that("plot_process_rates_across_clades returns a ggplot and carries the CI", {
  root <- tempfile("bgs-crossclade-plot-")
  f1 <- write_clade_rates(root, "Anolis", c(1, 2, 3))
  f2 <- write_clade_rates(root, "Phelsuma", c(3, 2, 1))
  combined <- combine_process_rates_across_clades(c(f1, f2))

  # The 95% CI columns flow through the combine step.
  expect_true(all(c("ci_lower", "ci_upper") %in% names(combined)))
  expect_s3_class(plot_process_rates_across_clades(combined), "ggplot")
  expect_error(
    plot_process_rates_across_clades(data.frame(clade = "A")),
    "missing required columns"
  )
})

test_that("combine_region_process_rates_across_clades tags clades and keeps regions", {
  root <- tempfile("bgs-crossclade-region-")
  f1 <- write_clade_region_rates(root, "Anolis", c("A", "B"), c(1, 2, 3))
  f2 <- write_clade_region_rates(root, "Phelsuma", c("A", "B"), c(3, 2, 1))

  out <- combine_region_process_rates_across_clades(c(f1, f2))
  expect_true(all(c("clade", "region") %in% names(out)))
  expect_setequal(unique(out$clade), c("Anolis", "Phelsuma"))
  expect_setequal(unique(out$region), c("A", "B"))
  # 2 clades x 2 regions x 3 bins.
  expect_equal(nrow(out), 12L)

  expect_equal(nrow(combine_region_process_rates_across_clades(character())), 0L)
  # A file that lacks the region column is rejected.
  plain <- write_clade_rates(tempfile("plain-"), "NoRegion", c(1, 2))
  expect_equal(nrow(combine_region_process_rates_across_clades(plain)), 0L)
})

test_that("plot_region_process_rates_across_clades returns a ggplot", {
  root <- tempfile("bgs-crossclade-region-plot-")
  f1 <- write_clade_region_rates(root, "Anolis", c("A", "B"), c(1, 2, 3))
  f2 <- write_clade_region_rates(root, "Phelsuma", c("A", "B"), c(3, 2, 1))
  combined <- combine_region_process_rates_across_clades(c(f1, f2))

  expect_s3_class(plot_region_process_rates_across_clades(combined), "ggplot")
  expect_error(
    plot_region_process_rates_across_clades(data.frame(clade = "A")),
    "missing required columns"
  )
})

test_that("write_cross_clade_bundle zips the combined table with its figure", {
  root <- tempfile("bgs-crossclade-bundle-")
  f1 <- write_clade_rates(root, "Anolis", c(1, 2, 3))
  f2 <- write_clade_rates(root, "Phelsuma", c(3, 2, 1))
  combined <- combine_process_rates_across_clades(c(f1, f2))
  plot <- plot_process_rates_across_clades(combined)

  zipfile <- tempfile(fileext = ".zip")
  write_cross_clade_bundle(
    zipfile, combined, plot,
    stem = "cross_clade_process_rates", width = 8, height = 5
  )

  expect_true(file.exists(zipfile))
  entries <- utils::unzip(zipfile, list = TRUE)$Name
  expect_true("cross_clade_process_rates.csv" %in% entries)
  expect_true("cross_clade_process_rates.png" %in% entries)
})

test_that("render_cross_clade_report writes a self-contained HTML with figures", {
  root <- tempfile("bgs-xclade-report-")
  f1 <- write_clade_rates(root, "Anolis", c(1, 2, 3))
  f2 <- write_clade_rates(root, "Phelsuma", c(3, 2, 1))
  overall <- combine_process_rates_across_clades(c(f1, f2))
  r1 <- write_clade_region_rates(root, "Anolis", c("A", "B"), c(1, 2, 3))
  r2 <- write_clade_region_rates(root, "Phelsuma", c("A", "B"), c(3, 2, 1))
  region <- combine_region_process_rates_across_clades(c(r1, r2))

  html <- render_cross_clade_report(list(rates = overall, region_rates = region))
  expect_true(file.exists(html))
  txt <- paste(readLines(html, warn = FALSE), collapse = "\n")
  expect_match(txt, "Cross-clade synthesis", fixed = TRUE)
  expect_match(txt, "Process rates through time (overall)", fixed = TRUE)
  expect_match(txt, "Process rates through time (by region)", fixed = TRUE)
  expect_match(txt, "data:image/png;base64", fixed = TRUE)

  # Both the full per-clade long table and the clade-pooled summary appear.
  expect_match(txt, "Combined rates per clade", fixed = TRUE)
  expect_match(txt, "all clades pooled", fixed = TRUE)
  expect_match(txt, "Summed mean events", fixed = TRUE)
  expect_error(render_cross_clade_report(list()), "No cross-clade results")
})

write_bundle_zip <- function(clade, tables) {
  root <- tempfile(paste0("bgs-bundle-", clade, "-"))
  tdir <- file.path(root, "tables")
  dir.create(tdir, recursive = TRUE, showWarnings = FALSE)
  for (name in names(tables)) {
    utils::write.csv(tables[[name]], file.path(tdir, name), row.names = FALSE)
  }
  zipfile <- file.path(tempdir(), paste0(clade, ".zip"))
  unlink(zipfile)
  zip_relative_files(root, zipfile, file.path("tables", names(tables)))
  zipfile
}

test_that("bundle_has_cross_clade_data detects BSM-derived tables", {
  # A bundle from a run WITH BSM carries the cross-clade tables.
  with_bsm <- write_bundle_zip("WithBSM", list(
    "biogeographic_process_summary.csv" = data.frame(process = "vicariance", mean = 1),
    "model_comparison.csv" = data.frame(model = "DEC", AICc = 10)
  ))
  # A bundle from a run WITHOUT BSM has only the non-BSM tables.
  no_bsm <- write_bundle_zip("NoBSM", list(
    "model_comparison.csv" = data.frame(model = "DEC", AICc = 10),
    "node_state_summary.csv" = data.frame(node_index = 1, best_state = "A")
  ))

  expect_true(bundle_has_cross_clade_data(with_bsm))
  expect_false(bundle_has_cross_clade_data(no_bsm))
  expect_false(bundle_has_cross_clade_data("does-not-exist.zip"))
})

test_that("bundles_missing_cross_clade_data names only the BSM-free bundles", {
  with_bsm <- write_bundle_zip("Amolops", list(
    "bsm_event_summary.csv" = data.frame(event_type = "d", mean_count = 2)
  ))
  no_bsm <- write_bundle_zip("Rana", list(
    "model_comparison.csv" = data.frame(model = "DEC", AICc = 10)
  ))

  missing <- bundles_missing_cross_clade_data(c(with_bsm, no_bsm), c("Amolops", "Rana"))
  expect_equal(missing, "Rana")

  # No uploads -> nothing missing.
  expect_length(bundles_missing_cross_clade_data(character()), 0L)
})

test_that("render_cross_clade_report explains the BSM requirement when empty", {
  # The message must point at BSM, since that is the real cause.
  expect_error(render_cross_clade_report(list()), "BSM stochastic")
})

test_that("rebin_rates_to_common_grid conserves totals across a shared grid", {
  # Two clades with DIFFERENT native bins (the exact case that used to spike):
  # clade A in [0,4],[4,8]; clade B in [0,3],[3,6],[6,9].
  data <- rbind(
    data.frame(region = "R", process_label = "Immigration",
               bin_start = c(0, 4), bin_end = c(4, 8), bin_midpoint = c(2, 6), mean_count = c(2, 4)),
    data.frame(region = "R", process_label = "Immigration",
               bin_start = c(0, 3, 6), bin_end = c(3, 6, 9), bin_midpoint = c(1.5, 4.5, 7.5), mean_count = c(3, 3, 3))
  )
  pooled <- rebin_rates_to_common_grid(data, bin_width = 5)

  expect_true(all(c("region", "process_label", "grid_bin", "mean_count", "bin_midpoint") %in% names(pooled)))
  # Proportional splitting conserves the grand total (6 + 9 = 15).
  expect_equal(sum(pooled$mean_count), 15)
  # Bins are the common 5-Ma grid, not the clades' native edges.
  expect_setequal(round(pooled$bin_midpoint, 2), c(2.5, 7.5))
})

test_that("plot_region_process_rates_across_clades facets by region and filters", {
  data <- rbind(
    data.frame(clade = "A", region = "Southern Asia", process_label = "In-situ speciation",
               bin_start = c(0, 5), bin_end = c(5, 10), bin_midpoint = c(2.5, 7.5), mean_count = c(3, 1)),
    data.frame(clade = "B", region = "Africa", process_label = "Immigration",
               bin_start = c(0, 5), bin_end = c(5, 10), bin_midpoint = c(2.5, 7.5), mean_count = c(2, 1))
  )
  p <- plot_region_process_rates_across_clades(data, bin_width = 5)
  expect_s3_class(p, "ggplot")
  # Colour now maps to process, and regions face out into panels.
  expect_identical(rlang::quo_get_expr(p$mapping$colour), quote(process_label))

  # Region filter keeps only requested regions.
  filtered <- plot_region_process_rates_across_clades(data, regions = "Southern Asia", bin_width = 5)
  expect_true(all(filtered$data$region == "Southern Asia"))
})

test_that("dispersal_routes_from_event_times slices by period and partitions the total", {
  # Two dispersal events at 40 Ma (Paleogene) and 10 Ma (Neogene), plus a
  # non-dispersal (empty target) that must be ignored.
  et <- data.frame(
    clade = "A", replicate = 1L,
    source_region = c("S", "S", "H"),
    target_region = c("H", "E", ""),
    event_time_before_present = c(40, 10, 5),
    stringsAsFactors = FALSE
  )
  total <- dispersal_routes_from_event_times(et, Inf, -Inf)
  expect_equal(sum(total$mean_count), 2)                       # two dispersals, 1 replicate
  expect_false(any(total$source_region == total$target_region))

  paleo <- bgs_period_window("Paleogene")
  neo <- bgs_period_window("Neogene")
  quat <- bgs_period_window("Quaternary")
  sp <- function(w) { r <- dispersal_routes_from_event_times(et, w$from, w$to); if (is.null(r)) 0 else sum(r$mean_count) }
  # The three periods partition the total exactly (no double-counting).
  expect_equal(sp(paleo) + sp(neo) + sp(quat), sum(total$mean_count))
  expect_equal(sp(paleo), 1)  # the 40-Ma event
  expect_equal(sp(neo), 1)    # the 10-Ma event
})

test_that("bgs_period_window maps names to half-open windows", {
  expect_equal(bgs_period_window("Total"), list(from = Inf, to = -Inf))
  expect_equal(bgs_period_window("Neogene"), list(from = 23.03, to = 2.58))
  expect_equal(bgs_period_window("nonsense"), list(from = Inf, to = -Inf))
})

test_that("keep_first_model drops extra models so cross-clade curves are single", {
  root <- tempfile("bgs-multimodel-")
  tables <- file.path(root, "Anolis", "tables")
  dir.create(tables, recursive = TRUE, showWarnings = FALSE)
  two_model <- rbind(
    data.frame(model = "DEC", process_key = "range_expansion", process_label = "Range expansion",
      process_group = "anagenetic", region = "A", time_bin = 1:3, bin_midpoint = (1:3) - 0.5,
      mean_count = c(1, 2, 3), stringsAsFactors = FALSE),
    data.frame(model = "DEC+J", process_key = "range_expansion", process_label = "Range expansion",
      process_group = "anagenetic", region = "A", time_bin = 1:3, bin_midpoint = (1:3) - 0.5,
      mean_count = c(9, 9, 9), stringsAsFactors = FALSE)
  )
  utils::write.csv(two_model, file.path(tables, "region_process_rates_through_time.csv"), row.names = FALSE)
  combined <- combine_region_process_rates_across_clades(
    file.path(tables, "region_process_rates_through_time.csv")
  )
  # Only the first model's rows survive: one row per (region, process, bin).
  expect_equal(unique(combined$model), "DEC")
  expect_equal(nrow(combined), 3L)
})
