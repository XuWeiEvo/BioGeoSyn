test_that("plot_node_state_summary returns a ggplot", {
  tree_nodes <- data.frame(
    node_index = c(1L, 2L, 3L),
    node_type = c("tip", "tip", "internal"),
    node_label = c("sp1", "sp2", "node_3"),
    parent_node_index = c(3L, 3L, NA_integer_),
    edge_length = c(1, 1, NA_real_),
    is_root = c(FALSE, FALSE, TRUE)
  )
  node_state_summary <- data.frame(
    model = "DEC",
    location = "branch_top_at_node",
    node_index = c(1L, 2L, 3L),
    best_state = c("A", "B", "AB"),
    best_probability = c(0.9, 0.8, 0.6)
  )

  plot <- plot_node_state_summary(tree_nodes, node_state_summary, label_internal_nodes = TRUE)

  expect_s3_class(plot, "ggplot")
})

test_that("plot_node_state_summary draws node pies from the state distribution", {
  tree_nodes <- data.frame(
    node_index = c(1L, 2L, 3L),
    node_type = c("tip", "tip", "internal"),
    node_label = c("sp1", "sp2", "node_3"),
    parent_node_index = c(3L, 3L, NA_integer_),
    edge_length = c(1, 1, NA_real_),
    is_root = c(FALSE, FALSE, TRUE)
  )
  node_state_summary <- data.frame(
    model = "DEC", location = "branch_top_at_node",
    node_index = c(1L, 2L, 3L),
    best_state = c("A", "B", "AB"),
    best_probability = c(1, 1, 0.6)
  )
  ancestral <- data.frame(
    model = "DEC", location = "branch_top_at_node",
    node_index = c(1L, 2L, 3L, 3L),
    state = c("A", "B", "AB", "A"),
    probability = c(1, 1, 0.6, 0.4),
    stringsAsFactors = FALSE
  )

  # Each node's pie wedges close a full circle.
  wedges <- node_state_pie_wedges(
    ancestral, layout_tree_nodes(tree_nodes), "DEC", "branch_top_at_node", 0.16
  )
  expect_true(all(c("x0", "y0", "state", "start", "end") %in% names(wedges)))
  expect_true(any(abs(wedges$end - 2 * pi) < 1e-8))

  plot <- plot_node_state_summary(tree_nodes, node_state_summary, ancestral)
  expect_s3_class(plot, "ggplot")
})

test_that("plot_node_state_sensitivity returns a ggplot", {
  node_state_sensitivity <- data.frame(
    location = rep("branch_top_at_node", 3L),
    node_index = c(1L, 2L, 3L),
    node_type = c("tip", "tip", "internal"),
    node_label = c("sp1", "sp2", "node_3"),
    non_j_model = rep("DEC", 3L),
    non_j_state = c("A", "B", "AB"),
    non_j_probability = c(0.7, 0.8, 0.6),
    plus_j_model = rep("DEC+J", 3L),
    plus_j_state = c("A", "AB", "B"),
    plus_j_probability = c(0.65, 0.7, 0.55),
    state_differs = c(FALSE, TRUE, TRUE),
    probability_difference = c(-0.05, -0.1, -0.05),
    probability_difference_abs = c(0.05, 0.1, 0.05)
  )

  plot <- plot_node_state_sensitivity(node_state_sensitivity, top_n = 2L)

  expect_s3_class(plot, "ggplot")
})

test_that("plot_event_summary returns a ggplot", {
  event_summary <- data.frame(
    model = c("DEC", "DEC"),
    location = c("branch_top_at_node", "branch_top_at_node"),
    event_label = c("Range expansion", "Local extinction"),
    event_count = c(3L, 1L),
    stringsAsFactors = FALSE
  )

  plot <- plot_event_summary(event_summary)

  expect_s3_class(plot, "ggplot")
})

test_that("BSM plot helpers return ggplots", {
  bsm_event_summary <- data.frame(
    model = "DEC",
    event_type = c("d", "e"),
    event_label = c("Range-expansion dispersal", "Local extinction"),
    mean_count = c(2, 1),
    sd_count = c(0.5, 0.2),
    stringsAsFactors = FALSE
  )
  bsm_events <- data.frame(
    model = "DEC",
    event_time_before_present = c(1.2, 0.4),
    event_label = c("Range-expansion dispersal", "Local extinction"),
    stringsAsFactors = FALSE
  )
  bsm_routes <- data.frame(
    model = "DEC",
    route_type = "all_dispersal",
    source_region = c("Area A", "Area B"),
    target_region = c("Area B", "Area C"),
    mean_count = c(2, 1),
    stringsAsFactors = FALSE
  )

  expect_s3_class(plot_bsm_event_summary(bsm_event_summary), "ggplot")
  expect_s3_class(plot_bsm_event_times(bsm_events), "ggplot")
  expect_s3_class(plot_bsm_dispersal_routes(bsm_routes), "ggplot")
})

test_that("generate_figures writes node-state sensitivity figures", {
  out <- tempfile("bgs-figures-")
  paths <- create_project(out)
  comparison <- data.frame(
    model = c("DEC", "DEC+J"),
    model_family = c("DEC", "DEC"),
    has_j = c(FALSE, TRUE),
    logLik = c(-10, -9),
    num_params = c(2L, 3L),
    AIC = c(24, 24),
    AICc = c(30, 36),
    delta_aicc = c(0, 6),
    aicc_weight = c(0.95, 0.05),
    caution_flag = c("none", "none"),
    interpretation_note = c("", "")
  )
  standardized_tables <- list(
    node_state_sensitivity = data.frame(
      location = rep("branch_top_at_node", 2L),
      node_index = c(1L, 2L),
      node_type = c("tip", "internal"),
      node_label = c("sp1", "node_2"),
      non_j_model = rep("DEC", 2L),
      non_j_state = c("A", "AB"),
      non_j_probability = c(0.7, 0.6),
      plus_j_model = rep("DEC+J", 2L),
      plus_j_state = c("A", "B"),
      plus_j_probability = c(0.65, 0.55),
      state_differs = c(FALSE, TRUE),
      probability_difference = c(-0.05, -0.05),
      probability_difference_abs = c(0.05, 0.05)
    ),
    event_summary = data.frame(
      model = "DEC",
      location = "branch_top_at_node",
      event_label = "Range expansion",
      event_count = 2L,
      changed_edges = 2L,
      interpretation_note = "derived",
      stringsAsFactors = FALSE
    ),
    bsm_event_summary = data.frame(
      model = "DEC",
      event_type = "d",
      event_label = "Range-expansion dispersal",
      mean_count = 2,
      sd_count = 0.5,
      stringsAsFactors = FALSE
    ),
    bsm_events = data.frame(
      model = "DEC",
      event_time_before_present = 0.7,
      event_label = "Range-expansion dispersal",
      stringsAsFactors = FALSE
    ),
    bsm_dispersal_routes = data.frame(
      model = "DEC",
      route_type = "all_dispersal",
      source_region = "Area A",
      target_region = "Area B",
      mean_count = 2,
      stringsAsFactors = FALSE
    )
  )

  manifest <- generate_figures(comparison, standardized_tables, paths, formats = "png")

  expect_true(any(manifest$figure == "node_state_sensitivity" & manifest$status == "created"))
  expect_true(any(manifest$figure == "event_summary" & manifest$status == "created"))
  expect_true(any(manifest$figure == "bsm_event_summary" & manifest$status == "created"))
  expect_true(any(manifest$figure == "bsm_event_times" & manifest$status == "created"))
  expect_true(any(manifest$figure == "bsm_dispersal_routes" & manifest$status == "created"))
  expect_true(file.exists(file.path(paths$figures, "node_state_sensitivity.png")))
  expect_true(file.exists(file.path(paths$figures, "event_summary.png")))
  expect_true(file.exists(file.path(paths$figures, "bsm_event_summary.png")))
})

test_that("layout_tree_nodes adds plotting coordinates", {
  tree_nodes <- data.frame(
    node_index = c(1L, 2L, 3L),
    node_type = c("tip", "tip", "internal"),
    node_label = c("sp1", "sp2", "node_3"),
    parent_node_index = c(3L, 3L, NA_integer_),
    edge_length = c(1, 2, NA_real_),
    is_root = c(FALSE, FALSE, TRUE)
  )

  layout <- layout_tree_nodes(tree_nodes)

  expect_true(all(c("x", "y", "parent_x", "parent_y") %in% names(layout)))
  expect_equal(layout$x[layout$is_root], 0)
  expect_true(all(!is.na(layout$y)))
})

test_that("tree_edge_segments creates rectangular phylogram segments", {
  edges <- data.frame(
    parent_x = c(0, 0),
    parent_y = c(1.5, 1.5),
    x = c(1, 2),
    y = c(1, 2)
  )

  segments <- tree_edge_segments(edges)

  expect_equal(nrow(segments), 4L)
  expect_true(all(c("x", "y", "xend", "yend") %in% names(segments)))
  expect_true(any(segments$y == segments$yend))
  expect_true(any(segments$x == segments$xend))
})

test_that("select_node_state_plot_models returns non-duplicated model roles", {
  comparison <- data.frame(
    model = c("DEC", "DEC+J", "DIVALIKE"),
    has_j = c(FALSE, TRUE, FALSE),
    AICc = c(10, 12, 11)
  )

  selected <- select_node_state_plot_models(comparison)

  expect_equal(selected$figure, c("node_state_summary_best_model", "node_state_summary_best_plus_j"))
  expect_equal(selected$model, c("DEC", "DEC+J"))
})

test_that("plot_bsm_dispersal_network returns a ggraph/ggplot", {
  routes <- data.frame(
    model = "DEC+J", route_type = "all_dispersal",
    source_region = c("A", "B", "C", "A"),
    target_region = c("B", "C", "A", "C"),
    mean_count = c(2, 1.5, 0.5, 1),
    stringsAsFactors = FALSE
  )
  plot <- plot_bsm_dispersal_network(routes)
  expect_s3_class(plot, "ggplot")
  expect_error(
    plot_bsm_dispersal_network(data.frame(route_type = "all_dispersal")),
    "missing required columns"
  )
})
