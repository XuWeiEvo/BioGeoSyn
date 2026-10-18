test_that("expected_event_counts integrates a known rate over branch time", {
  tn <- make_node_lookup({f <- tempfile(); ape::write.tree(ape::read.tree(text = "((a:2,b:2):3,c:5);"), f); f})
  rf <- function(t) ifelse(t < 2, 0.5, 0.1)
  e <- expected_event_counts(tn, rf, n_bins = 3, bin_width = 2)
  expect_equal(e$lineage_myr, c(6, 4, 2))
  expect_equal(e$expected_count, c(6 * 0.5, 4 * 0.1, 2 * 0.1), tolerance = 1e-8)
  expect_equal(e$true_rate, c(0.5, 0.1, 0.1), tolerance = 1e-8)
})

test_that("simulate_event_histories reproduces the expected counts on average", {
  set.seed(4)
  tr <- ape::rcoal(40)
  tr$edge.length <- tr$edge.length * 20 / max(ape::node.depth.edgelength(tr))
  rf <- function(t) ifelse(t >= 4 & t < 6, 1, 0.1)
  x <- simulate_event_histories(tr, rf, n_maps = 400, seed = 9)
  expect_equal(x$n_maps, 400L)
  e <- expected_event_counts(x$tree_nodes, rf, n_bins = 10, bin_width = 2)
  pooled <- pool_rates_by_map_resampling(list(A = x), bin_width = 2, n_draws = 200)
  got <- pooled$mean_count[match(e$grid_bin, pooled$grid_bin)]
  got[is.na(got)] <- 0
  ok <- e$expected_count > 2
  expect_true(all(abs(got[ok] - e$expected_count[ok]) / e$expected_count[ok] < 0.1))
  expect_true(all(x$events$event_time_before_present >= 0 & x$events$event_time_before_present <= 20 + 1e-8))
  # Reproducible with a seed.
  expect_identical(simulate_event_histories(tr, rf, n_maps = 5, seed = 1)$events,
                   simulate_event_histories(tr, rf, n_maps = 5, seed = 1)$events)
})

test_that("bin_width_sensitivity locates a synchronous pulse at every width", {
  set.seed(7)
  rf <- function(t) ifelse(t >= 10 & t < 12, 0.5, 0.05)
  cd <- lapply(1:4, function(i) {
    tr <- ape::rcoal(60)
    tr$edge.length <- tr$edge.length * 30 / max(ape::node.depth.edgelength(tr))
    simulate_event_histories(tr, rf, n_maps = 30, seed = i)
  })
  names(cd) <- paste0("c", 1:4)
  s <- bin_width_sensitivity(cd, bin_widths = c(2, 5), n_draws = 100, seed = 1, min_lineage_myr = 2)
  expect_setequal(s$peaks$bin_width, c(2, 5))
  expect_true(all(s$peaks$peak_start < 12 & s$peaks$peak_end > 10))
  # Wider bins dilute the pulse.
  expect_gt(s$peaks$peak_rate[s$peaks$bin_width == 2], s$peaks$peak_rate[s$peaks$bin_width == 5])
  expect_s3_class(plot_bin_width_sensitivity(s), "ggplot")
})
