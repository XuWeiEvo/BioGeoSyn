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
    rate = mean_counts,
    interpretation_note = "test",
    stringsAsFactors = FALSE
  )
  utils::write.csv(df, path, row.names = FALSE)
  path
}

test_that("combine_process_rates_across_clades tags and merges clades", {
  root <- tempfile("ibgb-crossclade-")
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
  root <- tempfile("ibgb-crossclade-dup-")
  # Same clade folder name reused would collide; force identical labels.
  f1 <- write_clade_rates(root, "same", c(1, 2))
  f2 <- write_clade_rates(tempfile("other-"), "same", c(3, 4))
  out <- combine_process_rates_across_clades(c(f1, f2), clade_names = c("Clade", "Clade"))
  expect_equal(length(unique(out$clade)), 2L)
})

test_that("plot_process_rates_across_clades returns a ggplot", {
  root <- tempfile("ibgb-crossclade-plot-")
  f1 <- write_clade_rates(root, "Anolis", c(1, 2, 3))
  f2 <- write_clade_rates(root, "Phelsuma", c(3, 2, 1))
  combined <- combine_process_rates_across_clades(c(f1, f2))

  expect_s3_class(plot_process_rates_across_clades(combined), "ggplot")
  expect_error(
    plot_process_rates_across_clades(data.frame(clade = "A")),
    "missing required columns"
  )
})
