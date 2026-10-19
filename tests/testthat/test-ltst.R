tn_ltst <- function(txt) {
  f <- tempfile(fileext = ".tre"); ape::write.tree(ape::read.tree(text = txt), f); make_node_lookup(f)
}

test_that("lineages through space and time count lineages per region and bin", {
  # (t1:10,t2:10); root AB splits into A and B; t1 expands into C at 4 Ma.
  tn <- tn_ltst("(t1:10,t2:10);")
  ev <- data.frame(
    replicate = 1, event_class = c("cladogenetic", "anagenetic"), event_type = c("vicariance", "d"),
    node_index = c(3L, 1L), parent_state = c("AB", "A"), child_state = c("A,B", "AC"),
    event_time_before_present = c(10, 4), source_region_code = c(NA, "A"), target_region_code = c(NA, "C"),
    source_region = c(NA, "Area A"), target_region = c(NA, "Area C"), stringsAsFactors = FALSE
  )
  x <- list(tree_nodes = tn, all_events = ev, replicates = 1)
  out <- lineages_through_space_time(list(A = x), bin_width = 5, n_draws = 10)
  get <- function(r, b) out$mean_lineages[out$region == r & out$bin_midpoint == b]
  expect_equal(get("Area A", 2.5), 1)
  expect_equal(get("B", 7.5), 1)
  expect_equal(get("Area C", 2.5), 4 / 5)   # present for 4 of the 5 Myr
  expect_equal(get("Area C", 7.5), 0)
  # Two identical clades add up, and a single map gives a zero-width interval.
  out2 <- lineages_through_space_time(list(A = x, B = x), bin_width = 5, n_draws = 10)
  expect_equal(out2$mean_lineages, 2 * out$mean_lineages)
  expect_equal(out2$ci_lower, out2$ci_upper)
  expect_equal(out2$n_clades[out2$region == "Area A" & out2$bin_midpoint == 2.5], 2L)
  expect_s3_class(plot_lineages_through_space_time(out2), "ggplot")
})

test_that("map-to-map variation gives an interval and tree replicates are grouped", {
  tn <- tn_ltst("(t1:10,t2:10);")
  mk <- function(rep, t) data.frame(
    replicate = rep, event_class = c("cladogenetic", "anagenetic"), event_type = c("vicariance", "d"),
    node_index = c(3L, 1L), parent_state = c("AB", "A"), child_state = c("A,B", "AC"),
    event_time_before_present = c(10, t), source_region_code = c(NA, "A"), target_region_code = c(NA, "C"),
    source_region = NA, target_region = NA, stringsAsFactors = FALSE)
  x <- list(tree_nodes = tn, all_events = rbind(mk(1, 2), mk(2, 8)), replicates = 1:2)
  out <- lineages_through_space_time(list(K__tree001 = x, K__tree002 = x), bin_width = 10, n_draws = 200)
  c10 <- out[out$region == "C" & out$bin_midpoint == 5, ]
  expect_equal(c10$mean_lineages, 0.5)      # 2 or 8 Myr of 10, averaged
  expect_equal(c10$n_clades, 1L)            # two trees of one clade
  expect_true(c10$ci_lower < c10$ci_upper)
})
