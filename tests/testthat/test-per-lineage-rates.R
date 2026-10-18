tree_nodes_from_phylo <- function(tree) {
  f <- tempfile(fileext = ".tre")
  ape::write.tree(tree, f)
  make_node_lookup(f)
}

# Place events on the branches of `tree_nodes` as a Poisson process with rate
# `lambda` per lineage-Myr, independently for each of `n_maps` maps.
sim_branch_events <- function(tree_nodes, lambda, n_maps, seed, process = "Range expansion") {
  set.seed(seed)
  br <- tree_branches(tree_nodes)
  rows <- list()
  for (m in seq_len(n_maps)) {
    k <- stats::rpois(nrow(br), lambda * (br$old - br$young))
    if (sum(k) == 0) next
    idx <- rep(seq_len(nrow(br)), k)
    rows[[length(rows) + 1L]] <- data.frame(
      replicate = m, process_label = process,
      event_time_before_present = stats::runif(length(idx), br$young[idx], br$old[idx])
    )
  }
  list(events = do.call(rbind, rows), n_maps = n_maps, replicates = seq_len(n_maps), tree_nodes = tree_nodes)
}

test_that("tree_lineage_myr sums branch time per bin", {
  # ((a:2,b:2):3,c:5); root at 5, internal node at 2.
  tn <- tree_nodes_from_phylo(ape::read.tree(text = "((a:2,b:2):3,c:5);"))
  expect_equal(tree_lineage_myr(tn, n_bins = 3, bin_width = 2), c(2 + 2 + 2, 2 + 2, 1 + 1))
  expect_equal(sum(tree_lineage_myr(tn, 3, 2)), 2 + 2 + 3 + 5)
})

test_that("a known per-lineage rate is recovered while counts rise with lineage number", {
  set.seed(11)
  tree <- ape::rcoal(80)
  tree$edge.length <- tree$edge.length * 30 / max(ape::node.depth.edgelength(tree))
  tn <- tree_nodes_from_phylo(tree)
  x <- sim_branch_events(tn, lambda = 0.2, n_maps = 200, seed = 3)
  out <- pool_per_lineage_rates(list(A = x), bin_width = 5, n_draws = 1000, seed = 1)
  ok <- !is.na(out$rate)
  expect_true(sum(ok) >= 4)
  expect_true(all(abs(out$rate[ok] - 0.2) < 0.03))
  expect_true(all(out$rate_ci_lower[ok] <= 0.2 & 0.2 <= out$rate_ci_upper[ok]))
  # Counts rise towards the present only because lineages accumulate.
  expect_gt(out$mean_count[out$grid_bin == 1], 3 * out$mean_count[out$grid_bin == max(out$grid_bin[ok])])
})

test_that("lineage weighting follows the large clade and clade weighting treats clades equally", {
  # Pure-birth trees: a species-rich clade and a small one.
  set.seed(21)
  big <- ape::rlineage(0.3, 0, Tmax = 20)
  set.seed(22)
  small <- ape::rlineage(0.1, 0, Tmax = 20)
  xb <- sim_branch_events(tree_nodes_from_phylo(big), lambda = 0.1, n_maps = 200, seed = 1)
  xs <- sim_branch_events(tree_nodes_from_phylo(small), lambda = 0.5, n_maps = 200, seed = 2)
  out <- pool_per_lineage_rates(list(big = xb, small = xs), bin_width = 5, n_draws = 1000, seed = 1)
  b1 <- out[out$grid_bin == 1, ]
  lm_big <- tree_lineage_myr(xb$tree_nodes, 4, 5)[1]
  lm_small <- tree_lineage_myr(xs$tree_nodes, 4, 5)[1]
  expected_lineage <- (0.1 * lm_big + 0.5 * lm_small) / (lm_big + lm_small)
  expect_equal(b1$n_clades_rate, 2L)
  expect_gt(lm_big, 3 * lm_small)
  expect_lt(abs(b1$rate - expected_lineage), 0.03)
  expect_lt(b1$rate, 0.2)
  expect_lt(abs(b1$clade_rate - 0.3), 0.05)
  expect_true(b1$clade_rate_ci_lower <= 0.3 && 0.3 <= b1$clade_rate_ci_upper)
})

test_that("bins with too little lineage time report no rate", {
  tn <- tree_nodes_from_phylo(ape::read.tree(text = "((a:2,b:2):3,c:5);"))
  x <- list(events = data.frame(replicate = 1, process_label = "Vicariance", event_time_before_present = 4.5),
            n_maps = 1, replicates = 1, tree_nodes = tn)
  out <- pool_per_lineage_rates(list(A = x), bin_width = 1, n_draws = 10, min_lineage_myr = 3)
  expect_true(is.na(out$rate[out$grid_bin == 5]))   # 2 lineage-Myr in [4, 5]
  expect_false(is.na(out$rate[out$grid_bin == 1]))  # 3 lineage-Myr in [0, 1]
})

test_that("region occupancy follows the sampled ranges on every branch", {
  # (t1:10,t2:10); root range AB splits by vicariance into A and B; t1 expands
  # into C at 4 Ma. t2 has no anagenetic event, so it takes the remaining
  # daughter range B.
  tn <- tree_nodes_from_phylo(ape::read.tree(text = "(t1:10,t2:10);"))
  ev <- data.frame(
    replicate = 1,
    event_class = c("cladogenetic", "anagenetic"),
    event_type = c("vicariance", "d"),
    node_index = c(3L, 1L),
    parent_state = c("AB", "A"),
    child_state = c("A,B", "AC"),
    event_time_before_present = c(10, 4),
    source_region_code = c(NA, "A"), target_region_code = c(NA, "C"),
    source_region = c(NA, "Area A"), target_region = c(NA, "Area C"),
    stringsAsFactors = FALSE
  )
  occ <- map_region_occupancy(ev, tn, replicates = 1, n_bins = 2, bin_width = 5)
  expect_equal(dimnames(occ)[[2L]], c("Area A", "B", "Area C"))
  expect_equal(occ[1, "Area A", ], c(5, 5))
  expect_equal(occ[1, "B", ], c(5, 5))
  expect_equal(occ[1, "Area C", ], c(4, 0))
})

test_that("region rates divide by time in or outside the region", {
  tn <- tree_nodes_from_phylo(ape::read.tree(text = "(t1:10,t2:10);"))
  occ <- array(c(5, 5, 5, 5), dim = c(1, 1, 2), dimnames = list(NULL, "R", NULL))
  occ[1, "R", ] <- c(4, 6)
  x <- list(
    events = data.frame(replicate = 1, process_label = c("Emigration", "Immigration"), region = "R",
                        event_time_before_present = c(1, 2)),
    n_maps = 1, replicates = 1, tree_nodes = tn, occupancy = occ
  )
  out <- pool_per_lineage_rates(list(A = x), bin_width = 5, n_draws = 5, by_region = TRUE, min_lineage_myr = 1)
  em <- out[out$process_label == "Emigration" & out$grid_bin == 1, ]
  im <- out[out$process_label == "Immigration" & out$grid_bin == 1, ]
  expect_equal(em$lineage_myr, 4)
  expect_equal(em$rate, 1 / 4)
  expect_equal(im$lineage_myr, 10 - 4)
  expect_equal(im$rate, 1 / 6)
  expect_equal(im$denominator, "lineage-Myr outside the region")
})

test_that("per-lineage plots draw both weightings", {
  set.seed(5)
  tree <- ape::rcoal(30)
  tn <- tree_nodes_from_phylo(tree)
  x <- sim_branch_events(tn, lambda = 1, n_maps = 50, seed = 1)
  out <- pool_per_lineage_rates(list(A = x), bin_width = 0.2, n_draws = 100, min_lineage_myr = 0.1)
  expect_s3_class(plot_per_lineage_rates_across_clades(out, "lineage"), "ggplot")
  expect_s3_class(plot_per_lineage_rates_across_clades(out, "clade"), "ggplot")
})
