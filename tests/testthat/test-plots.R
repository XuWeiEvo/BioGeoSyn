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
