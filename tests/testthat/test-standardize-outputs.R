test_that("compare_node_state_sensitivity compares best non-J and +J nodes", {
  node_state_summary <- data.frame(
    model = rep(c("DEC", "DEC+J"), each = 2),
    location = "branch_top_at_node",
    node_index = rep(c(1L, 2L), times = 2),
    node_type = "tip",
    node_label = rep(c("sp1", "sp2"), times = 2),
    best_state = c("A", "B", "A", "AB"),
    best_probability = c(0.9, 0.7, 0.8, 0.6),
    stringsAsFactors = FALSE
  )
  comparison <- data.frame(
    model = c("DEC", "DEC+J"),
    has_j = c(FALSE, TRUE),
    AICc = c(10, 11)
  )

  out <- compare_node_state_sensitivity(node_state_summary, comparison)

  expect_equal(nrow(out), 2L)
  expect_true(all(c(
    "non_j_state",
    "plus_j_state",
    "state_differs",
    "probability_difference_abs"
  ) %in% names(out)))
  expect_false(out$state_differs[out$node_index == 1L])
  expect_true(out$state_differs[out$node_index == 2L])
})

test_that("compare_node_state_sensitivity returns stable empty table without paired models", {
  out <- compare_node_state_sensitivity(
    node_state_summary = data.frame(),
    comparison = data.frame(model = "DEC", has_j = FALSE, AICc = 1)
  )

  expect_equal(nrow(out), 0L)
  expect_true(all(c("non_j_model", "plus_j_model", "state_differs") %in% names(out)))
})

test_that("range-change event summaries classify ancestral state changes", {
  node_state_summary <- data.frame(
    model = "DEC",
    location = "branch_top_at_node",
    node_index = c(1L, 2L, 3L, 4L),
    node_type = c("tip", "tip", "internal", "internal"),
    node_label = c("sp1", "sp2", "node_3", "node_4"),
    best_state = c("A", "AB", "A", "null"),
    best_probability = c(0.9, 0.8, 0.7, 0.6),
    stringsAsFactors = FALSE
  )
  tree_nodes <- data.frame(
    node_index = c(1L, 2L, 3L, 4L),
    node_type = c("tip", "tip", "internal", "internal"),
    node_label = c("sp1", "sp2", "node_3", "node_4"),
    parent_node_index = c(3L, 3L, 4L, NA_integer_),
    edge_length = c(1, 1, 1, NA_real_),
    distance_from_root = c(2, 2, 1, 0),
    time_before_present = c(0, 0, 1, 2),
    tree_height = c(2, 2, 2, 2),
    stringsAsFactors = FALSE
  )
  geographic_states <- data.frame(
    state = c("null", "A", "B", "AB"),
    areas = c(NA, "A", "B", "A;B"),
    stringsAsFactors = FALSE
  )

  regions <- data.frame(region = c("A", "B"), label = c("Area A", "Area B"))
  events <- summarize_range_change_events(node_state_summary, tree_nodes, geographic_states, regions)
  summary <- summarize_range_change_event_counts(events)
  best_events <- summarize_best_fit_events(
    events,
    data.frame(model = "DEC", AICc = 1, delta_aicc = 0)
  )

  expect_true(all(c(
    "event_type",
    "gained_areas",
    "lost_areas",
    "event_time_midpoint",
    "direction",
    "direction_label"
  ) %in% names(events)))
  expect_true("range_expansion" %in% events$event_type)
  expect_true("range_origin" %in% events$event_type)
  expect_true(any(grepl(" -> ", events$direction, fixed = TRUE)))
  expect_true(any(grepl("Area", events$direction_label, fixed = TRUE)))
  expect_equal(summary$event_count[match("Range expansion", summary$event_label)], 1L)
  expect_true(nrow(best_events) > 0L)
  expect_true(all(c("event_time_midpoint", "direction_label") %in% names(best_events)))
  expect_true(any(grepl("stochastic mapping", summary$interpretation_note, fixed = TRUE)))
})

test_that("BSM standardization exposes counts, routes, and event times", {
  counts <- list(
    summary_counts_BSMs = data.frame(
      founder = c(1, 0.2, 2),
      a = c(0, 0, 0),
      d = c(2, 0.5, 4),
      e = c(1, 0.1, 2),
      subset = c(1, 0, 2),
      vicariance = c(0, 0, 0),
      sympatry = c(1, 0, 2),
      ALL_disp = c(3, 0.4, 6),
      ana_disp = c(2, 0.3, 4),
      all_ana = c(3, 0.4, 6),
      all_clado = c(3, 0.4, 6),
      total_events = c(6, 0.8, 12),
      row.names = c("means", "stdevs", "sums")
    ),
    founder_totals_list = c(1, 1),
    a_totals_list = c(0, 0),
    d_totals_list = c(2, 2),
    e_totals_list = c(1, 1),
    subsetSymp_totals_list = c(1, 1),
    vicariance_totals_list = c(0, 0),
    sympatry_totals_list = c(1, 1),
    all_dispersals_totals_list = c(3, 3),
    anagenetic_dispersals_totals_list = c(2, 2),
    ana_totals_list = c(3, 3),
    clado_totals_list = c(3, 3),
    all_totals_list = c(6, 6),
    all_dispersals_counts_fromto_means = matrix(c(0, 1, 0, 0), nrow = 2, dimnames = list(c("Area A", "Area B"), c("Area A", "Area B"))),
    all_dispersals_counts_fromto_sds = matrix(0, nrow = 2, ncol = 2, dimnames = list(c("Area A", "Area B"), c("Area A", "Area B"))),
    ana_dispersals_counts_fromto_means = matrix(c(0, 1, 0, 0), nrow = 2, dimnames = list(c("Area A", "Area B"), c("Area A", "Area B"))),
    ana_dispersals_counts_fromto_sds = matrix(0, nrow = 2, ncol = 2, dimnames = list(c("Area A", "Area B"), c("Area A", "Area B"))),
    founder_counts_fromto_means = matrix(0, nrow = 2, ncol = 2, dimnames = list(c("Area A", "Area B"), c("Area A", "Area B"))),
    founder_counts_fromto_sds = matrix(0, nrow = 2, ncol = 2, dimnames = list(c("Area A", "Area B"), c("Area A", "Area B"))),
    a_counts_fromto_means = matrix(0, nrow = 2, ncol = 2, dimnames = list(c("Area A", "Area B"), c("Area A", "Area B"))),
    a_counts_fromto_sds = matrix(0, nrow = 2, ncol = 2, dimnames = list(c("Area A", "Area B"), c("Area A", "Area B"))),
    d_counts_fromto_means = matrix(c(0, 1, 0, 0), nrow = 2, dimnames = list(c("Area A", "Area B"), c("Area A", "Area B"))),
    d_counts_fromto_sds = matrix(0, nrow = 2, ncol = 2, dimnames = list(c("Area A", "Area B"), c("Area A", "Area B")))
  )
  raw_result <- list(
    bsm_output = list(RES_clado_events_tables = list(data.frame(node = 1L))),
    source_area_output = list(
      clado_events_tables = list(data.frame(
        node = 3L,
        label = "node_3",
        time_bp = 1.5,
        clado_event_type = "founder (j)",
        clado_event_txt = "A->A,B",
        clado_dispersal_from = "A",
        clado_dispersal_to = "B",
        stringsAsFactors = FALSE
      )),
      ana_events_tables = list(data.frame(
        node = 1L,
        label = "sp1",
        event_type = "d",
        event_txt = "A->AB",
        abs_event_time = 0.7,
        event_time = 0.3,
        ana_dispersal_from = "A",
        dispersal_to = "B",
        extirpation_from = "-",
        stringsAsFactors = FALSE
      ))
    ),
    counts = counts
  )
  prepared <- list(
    areas = c("A", "B"),
    region_metadata = data.frame(region = c("A", "B"), label = c("Area A", "Area B"))
  )

  out <- standardize_bsm_result("DEC", raw_result, prepared)

  expect_true(all(c("bsm_event_summary", "bsm_dispersal_routes", "bsm_event_times") %in% names(out)))
  expect_true("Founder-event jump dispersal" %in% out$bsm_event_summary$event_label)
  expect_true(any(out$bsm_dispersal_routes$direction_label == "Area B -> Area A"))
  expect_true(any(out$bsm_events$direction_label == "Area A -> Area B"))
  expect_true(any(out$bsm_event_times$event_time_before_present == 0.7))
})
