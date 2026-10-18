one_tree_events <- function(shift, n_maps, seed) {
  # One bin of width 1; per-map count Poisson(5 + shift).
  set.seed(seed)
  k <- stats::rpois(n_maps, 5 + shift)
  list(
    events = data.frame(replicate = rep(seq_len(n_maps), k), process_label = "Range expansion",
                        event_time_before_present = 0.5),
    n_maps = n_maps, replicates = seq_len(n_maps)
  )
}

test_that("clade_group_name strips tree-replicate suffixes", {
  expect_equal(
    clade_group_name(c("Clade1__tree001", "Clade1_tree2_results", "Clade 2-tree-10", "Clade3", "treeshrews")),
    c("Clade1", "Clade1", "Clade 2", "Clade3", "treeshrews")
  )
})

test_that("trees of one clade are averaged, not added, and widen the interval", {
  # Three trees whose expected counts differ (topology / age uncertainty).
  trees <- list(
    A__tree1 = one_tree_events(-3, 200, 1),
    A__tree2 = one_tree_events(0, 200, 2),
    A__tree3 = one_tree_events(3, 200, 3)
  )
  multi <- pool_rates_by_map_resampling(trees, bin_width = 1, n_draws = 4000, seed = 1)
  single <- pool_rates_by_map_resampling(trees["A__tree2"], bin_width = 1, n_draws = 4000, seed = 1)
  expect_equal(multi$n_clades, 1L)
  expect_equal(multi$n_trees, 3L)
  expect_equal(multi$ci_method, "tree_and_map_resampling")
  per_tree_means <- vapply(trees, function(x) nrow(x$events) / x$n_maps, numeric(1))
  expect_equal(multi$mean_count, mean(per_tree_means))
  expect_gt(multi$ci_upper - multi$ci_lower, single$ci_upper - single$ci_lower)
})

test_that("trees get equal weight whatever their number of maps", {
  # Tree 1: 10 maps, one event in each. Tree 2: 100 maps, one event in total.
  both <- pool_rates_by_map_resampling(
    list(
      A__tree1 = list(events = data.frame(replicate = 1:10, process_label = "Vicariance", event_time_before_present = 0.5),
                      n_maps = 10, replicates = 1:10),
      A__tree2 = list(events = data.frame(replicate = 1L, process_label = "Vicariance", event_time_before_present = 0.5),
                      n_maps = 100, replicates = 1:100)
    ),
    bin_width = 1, n_draws = 20000, seed = 2)
  expect_equal(both$mean_count, (1 + 1 / 100) / 2)
  expect_equal(both$ci_lower, 0)
  expect_equal(both$ci_upper, 1)
})

test_that("per-lineage pooling uses each tree's own denominator", {
  tn1 <- make_node_lookup({f <- tempfile(); ape::write.tree(ape::read.tree(text = "(a:2,b:2);"), f); f})
  tn2 <- make_node_lookup({f <- tempfile(); ape::write.tree(ape::read.tree(text = "(a:1,b:1);"), f); f})
  ev <- data.frame(replicate = 1L, process_label = "Range expansion", event_time_before_present = 0.5)
  x1 <- list(events = ev, n_maps = 1, replicates = 1L, tree_nodes = tn1)
  x2 <- list(events = ev, n_maps = 1, replicates = 1L, tree_nodes = tn2)
  out <- pool_per_lineage_rates(list(A__tree1 = x1, A__tree2 = x2), bin_width = 1, n_draws = 500,
                                min_lineage_myr = 0.5, seed = 1)
  b1 <- out[out$grid_bin == 1, ]
  # Both trees have 2 lineage-Myr in [0, 1] and one event: rate 0.5 either way.
  expect_equal(b1$rate, 0.5)
  expect_equal(b1$n_trees, 2L)
  b2 <- out[out$grid_bin == 2, ]
  # [1, 2] exists only on tree 1 (2 lineage-Myr); the averaged denominator is 1.
  expect_equal(b2$lineage_myr, 1)
})

test_that("bundle summaries average a clade's trees before summing clades", {
  mk <- function(name, mean_count) {
    root <- tempfile(paste0("bgs-trees-", name, "-"))
    dir.create(file.path(root, "tables"), recursive = TRUE)
    utils::write.csv(data.frame(model = "DEC", event_type = "d", event_label = "Range expansion", mean_count = mean_count),
                     file.path(root, "tables", "bsm_event_summary.csv"), row.names = FALSE)
    z <- file.path(tempdir(), paste0(name, ".zip"))
    unlink(z)
    zip_relative_files(root, z, "tables/bsm_event_summary.csv")
  }
  z <- c(mk("X__tree1", 2), mk("X__tree2", 4), mk("Y", 10))
  out <- combine_event_summary_across_clades(z, tools::file_path_sans_ext(basename(z)))
  expect_equal(out$mean_count, (2 + 4) / 2 + 10)
})

test_that("run_workflow_over_trees writes one configuration per tree", {
  skip_if_not_installed("yaml")
  root <- tempfile("bgs-trees-wf-")
  dir.create(root)
  ex <- system.file("example_data", package = "BioGeoSyn")
  if (!nzchar(ex)) ex <- testthat::test_path("..", "..", "inst", "example_data")
  file.copy(list.files(ex, full.names = TRUE), root)
  tree <- ape::read.tree(file.path(root, "tree.nwk"))
  trees <- c(tree, tree, tree)
  for (i in seq_along(trees)) trees[[i]]$edge.length <- tree$edge.length * i
  class(trees) <- "multiPhylo"
  ape::write.tree(trees, file.path(root, "posterior.trees"))
  cfg <- list(project = list(name = "Toy", output_dir = "results/toy"),
              inputs = list(tree_file = "tree.nwk", geography_file = "geography.csv",
                            regions_file = "regions.csv", max_range_size = 2L),
              models = list(run = "DEC"))
  yaml::write_yaml(cfg, file.path(root, "analysis.yml"))
  runs <- run_workflow_over_trees(file.path(root, "analysis.yml"), file.path(root, "posterior.trees"),
                                  n_trees = 2, dry_run = TRUE, progress = FALSE)
  expect_equal(nrow(runs), 2L)
  expect_true(all(runs$status == "planned"))
  expect_true(all(grepl("^Toy__tree00[1-3]$", basename(runs$output_dir))))
  expect_true(file.exists(file.path(root, "results", "toy_trees", "tree_runs.csv")))
  used <- yaml::read_yaml(file.path(root, "results", "toy_trees", "configs", paste0(basename(runs$output_dir[[1]]), ".yml")))
  expect_true(file.exists(used$inputs$geography_file))
  expect_equal(clade_group_name(basename(runs$output_dir)), c("Toy", "Toy"))
})
