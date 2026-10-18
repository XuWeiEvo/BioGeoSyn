pool_bundle_zip <- function(clade, tables) {
  root <- tempfile(paste0("bgs-pool-", clade, "-"))
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

# Simulate one clade's per-event BSM table: in every stochastic map, the number
# of range-expansion events in each 1-Ma bin is Poisson(lambda[bin]).
sim_clade_events <- function(lambda, n_maps, seed, process = "Range expansion") {
  set.seed(seed)
  rows <- list()
  for (map in seq_len(n_maps)) {
    for (b in seq_along(lambda)) {
      k <- stats::rpois(1, lambda[[b]])
      if (k > 0) {
        rows[[length(rows) + 1L]] <- data.frame(
          replicate = map,
          process_label = process,
          event_time_before_present = stats::runif(k, b - 1, b),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  list(events = do.call(rbind, rows), n_maps = n_maps)
}

test_that("pooled mean equals the sum of the clades' per-map means", {
  a <- sim_clade_events(c(2, 4, 1), n_maps = 50, seed = 1)
  b <- sim_clade_events(c(3, 0.5, 5), n_maps = 80, seed = 2)
  out <- pool_rates_by_map_resampling(list(A = a, B = b), bin_width = 1, n_draws = 500)
  per_clade_mean <- function(x, bin) sum(floor(x$events$event_time_before_present) + 1 == bin) / x$n_maps
  for (bin in 1:3) {
    expect_equal(out$mean_count[out$grid_bin == bin], per_clade_mean(a, bin) + per_clade_mean(b, bin))
  }
  expect_true(all(out$ci_method == "map_resampling"))
  expect_equal(unique(out$n_clades), 2L)
})

test_that("maps without events count as zeros", {
  # Two maps with one event each in bin 1, eight maps with none.
  x <- list(
    events = data.frame(replicate = c(1, 2), process_label = "Range expansion",
                        event_time_before_present = c(0.5, 0.5)),
    n_maps = 10
  )
  out <- pool_rates_by_map_resampling(list(A = x), bin_width = 1, n_draws = 2000)
  expect_equal(out$mean_count, 0.2)
  expect_equal(out$ci_lower, 0)
  expect_equal(out$ci_upper, 1)
})

test_that("identical maps give a zero-width interval at the mean", {
  one_map <- data.frame(replicate = 1, process_label = "Vicariance",
                        event_time_before_present = c(0.2, 1.4, 1.6))
  x <- list(events = do.call(rbind, lapply(1:20, function(r) transform(one_map, replicate = r))), n_maps = 20)
  out <- pool_rates_by_map_resampling(list(A = x, B = x), bin_width = 1, n_draws = 200)
  expect_equal(out$mean_count, c(2, 4))
  expect_equal(out$ci_lower, out$mean_count)
  expect_equal(out$ci_upper, out$mean_count)
})

test_that("resampled interval matches the known distribution of a sum and is narrower than summed limits", {
  # 21 clades, each Poisson(5) per map in a single bin. The pooled count is
  # Poisson(105), whose central 95% interval is [85, 126].
  clades <- lapply(1:21, function(i) sim_clade_events(5, n_maps = 400, seed = 100 + i))
  names(clades) <- paste0("clade", 1:21)
  out <- pool_rates_by_map_resampling(clades, bin_width = 1, n_draws = 5000, seed = 42)
  expect_equal(out$mean_count, 105, tolerance = 0.02)
  expect_equal(out$ci_lower, stats::qpois(0.025, 105), tolerance = 0.04)
  expect_equal(out$ci_upper, stats::qpois(0.975, 105), tolerance = 0.04)

  summed_upper <- sum(vapply(clades, function(x) {
    counts <- tabulate(x$events$replicate, nbins = x$n_maps)
    stats::quantile(counts, 0.975, names = FALSE)
  }, numeric(1)))
  expect_gt(summed_upper, out$ci_upper + 50)
})

test_that("an independent synchronous pulse is recovered on the shared grid", {
  # Clades of different crown ages share a pulse at 4-6 Ma; each also has a
  # private pulse at a different time.
  young <- sim_clade_events(c(0.2, 0.2, 0.2, 0.2, 3, 3, 0.2, 0.2), n_maps = 100, seed = 7)
  old <- sim_clade_events(c(0.2, 0.2, 0.2, 0.2, 3, 3, 0.2, 0.2, 0.2, 0.2, 0.2, 3, 0.2), n_maps = 100, seed = 8)
  out <- pool_rates_by_map_resampling(list(young = young, old = old), bin_width = 2, n_draws = 1000)
  peak <- out$bin_midpoint[which.max(out$mean_count)]
  expect_equal(peak, 5)
  expect_true(all(out$ci_lower <= out$mean_count & out$mean_count <= out$ci_upper))
})

test_that("results are reproducible with a seed and do not disturb the caller's RNG", {
  a <- sim_clade_events(c(2, 4), n_maps = 30, seed = 1)
  b <- sim_clade_events(c(1, 1), n_maps = 30, seed = 2)
  set.seed(99)
  before <- stats::runif(1)
  set.seed(99)
  r1 <- pool_rates_by_map_resampling(list(A = a, B = b), bin_width = 1, n_draws = 300, seed = 5)
  after <- stats::runif(1)
  r2 <- pool_rates_by_map_resampling(list(A = a, B = b), bin_width = 1, n_draws = 300, seed = 5)
  expect_identical(r1, r2)
  expect_identical(before, after)
})

test_that("region pooling keeps regions separate", {
  x <- list(
    events = data.frame(replicate = c(1, 1, 2), process_label = "Immigration",
                        region = c("A", "B", "A"), event_time_before_present = c(0.5, 0.5, 1.5)),
    n_maps = 2
  )
  out <- pool_rates_by_map_resampling(list(X = x), bin_width = 1, n_draws = 100, by_region = TRUE)
  expect_true(all(c("region", "process_label") %in% names(out)))
  expect_equal(out$mean_count[out$region == "A"], c(0.5, 0.5))
  expect_equal(out$mean_count[out$region == "B"], c(0.5, 0))
})

test_that("bundles are pooled from their per-event BSM tables", {
  mk_events <- function(times, reps, type = "d") {
    data.frame(
      event_index = seq_along(times), model = "DEC", replicate = reps,
      event_class = "anagenetic", event_type = type, event_label = "Range expansion",
      event_time_before_present = times,
      source_region_code = "A", target_region_code = "B",
      source_region = "Region A", target_region = "Region B",
      stringsAsFactors = FALSE
    )
  }
  esum <- function(n) data.frame(model = "DEC", event_type = "d", replicate_count = n, mean_count = 1)
  z1 <- pool_bundle_zip("PoolA", list(
    "bsm_events.csv" = mk_events(c(1, 2, 6), c(1, 2, 2)),
    "bsm_event_summary.csv" = esum(4)
  ))
  z2 <- pool_bundle_zip("PoolB", list(
    "bsm_events.csv" = mk_events(c(1.5, 7), c(1, 3)),
    "bsm_event_summary.csv" = esum(5)
  ))
  out <- pool_process_rates_from_bundles(c(z1, z2), bin_width = 5, n_draws = 400)
  expect_equal(out$mean_count[out$grid_bin == 1], 2 / 4 + 1 / 5)
  expect_equal(out$mean_count[out$grid_bin == 2], 1 / 4 + 1 / 5)
  reg <- pool_process_rates_from_bundles(c(z1, z2), bin_width = 5, n_draws = 400, by_region = TRUE)
  expect_setequal(unique(reg$region), c("Region A", "Region B"))
  expect_setequal(unique(reg$process_label), c("Immigration", "Emigration"))

  combined <- combine_process_rates_from_bundles(c(z1, z2))
  expect_null(combined)
  expect_null(pool_process_rates_from_bundles(character()))
})

test_that("pooled plots use the resampled interval and say so", {
  a <- sim_clade_events(c(2, 4, 1), n_maps = 50, seed = 1)
  b <- sim_clade_events(c(3, 0.5, 5), n_maps = 50, seed = 2)
  pooled <- pool_rates_by_map_resampling(list(A = a, B = b), bin_width = 1, n_draws = 200)
  combined <- data.frame(clade = c("A", "B"), process_label = "Range expansion",
                         bin_midpoint = 0.5, mean_count = 1, bin_start = 0, bin_end = 1,
                         ci_lower = 0, ci_upper = 2)
  p <- plot_process_rates_across_clades(combined, pooled = TRUE, pooled_data = pooled)
  expect_match(p$labels$subtitle, "resampled stochastic maps", fixed = TRUE)
  p_old <- plot_process_rates_across_clades(combined, pooled = TRUE)
  expect_match(p_old$labels$subtitle, "sum of per-clade 95% limits", fixed = TRUE)
})
