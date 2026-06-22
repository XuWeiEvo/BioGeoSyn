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

test_that("generate_figures writes node-state sensitivity figures", {
  out <- tempfile("ibgb-figures-")
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
    )
  )

  manifest <- generate_figures(comparison, standardized_tables, paths, formats = "png")

  expect_true(any(manifest$figure == "node_state_sensitivity" & manifest$status == "created"))
  expect_true(file.exists(file.path(paths$figures, "node_state_sensitivity.png")))
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
