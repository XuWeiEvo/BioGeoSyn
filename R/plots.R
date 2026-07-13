#' Plot events through time
#'
#' @param event_table Data frame containing `event_time` and `event_type`.
#' @return A ggplot object.
#' @export
plot_event_through_time <- function(event_table) {
  required <- c("event_time", "event_type")
  missing <- setdiff(required, names(event_table))
  if (length(missing) > 0L) {
    stop("event_table is missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  ggplot2::ggplot(event_table, ggplot2::aes(x = event_time, colour = event_type)) +
    ggplot2::stat_ecdf(linewidth = 0.9) +
    ggplot2::scale_x_reverse() +
    ggplot2::labs(x = "Time before present", y = "Cumulative proportion of events", colour = "Event type") +
    ggplot2::theme_minimal(base_size = 11)
}

#' Plot a region-to-region dispersal network
#'
#' @param event_table Data frame containing `source_region`, `target_region`,
#'   and optionally `frequency`.
#' @return A ggraph object.
#' @export
plot_dispersal_network <- function(event_table) {
  required <- c("source_region", "target_region")
  missing <- setdiff(required, names(event_table))
  if (length(missing) > 0L) {
    stop("event_table is missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  edge_cols <- if ("frequency" %in% names(event_table)) c(required, "frequency") else required
  edges <- event_table[
    !is.na(event_table$source_region) & !is.na(event_table$target_region),
    edge_cols,
    drop = FALSE
  ]
  if ("frequency" %in% names(event_table)) {
    edges$frequency <- as.numeric(edges$frequency)
  } else {
    edges$frequency <- 1
  }
  if (nrow(edges) == 0L) {
    stop("No complete source_region -> target_region events are available to plot.", call. = FALSE)
  }
  edges <- stats::aggregate(frequency ~ source_region + target_region, data = edges, FUN = sum)
  graph <- igraph::graph_from_data_frame(edges, directed = TRUE)

  ggraph::ggraph(graph, layout = "circle") +
    ggraph::geom_edge_link(ggplot2::aes(width = frequency), alpha = 0.55, arrow = grid::arrow(length = grid::unit(3, "mm"))) +
    ggraph::geom_node_point(size = 4) +
    ggraph::geom_node_text(ggplot2::aes(label = name), repel = TRUE) +
    ggraph::scale_edge_width(range = c(0.3, 2.5)) +
    ggplot2::theme_void()
}

plot_event_summary <- function(event_summary) {
  required <- c("model", "event_label", "event_count")
  missing <- setdiff(required, names(event_summary))
  if (length(missing) > 0L) {
    stop("event_summary is missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (nrow(event_summary) == 0L) {
    stop("event_summary must contain at least one row.", call. = FALSE)
  }

  plot_data <- event_summary[!is.na(event_summary$event_count), , drop = FALSE]
  if ("location" %in% names(plot_data) && "branch_top_at_node" %in% plot_data$location) {
    plot_data <- plot_data[plot_data$location == "branch_top_at_node", , drop = FALSE]
  }
  if (nrow(plot_data) == 0L) {
    stop("No event summary rows are available to plot.", call. = FALSE)
  }
  plot_data$event_count <- as.numeric(plot_data$event_count)
  plot_data$event_label <- stats::reorder(plot_data$event_label, plot_data$event_count)

  ggplot2::ggplot(plot_data, ggplot2::aes(x = event_label, y = event_count, fill = event_label)) +
    ggplot2::geom_col(width = 0.72, colour = "grey25", linewidth = 0.25, show.legend = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(stats::as.formula("~ model")) +
    ggplot2::labs(
      x = NULL,
      y = "Branch count",
      title = "Range-change event summary",
      subtitle = "Derived from highest-probability ancestral states; not stochastic mapping counts"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}

plot_bsm_event_summary <- function(bsm_event_summary) {
  required <- c("model", "event_label", "mean_count")
  missing <- setdiff(required, names(bsm_event_summary))
  if (length(missing) > 0L) {
    stop("bsm_event_summary is missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (nrow(bsm_event_summary) == 0L) {
    stop("bsm_event_summary must contain at least one row.", call. = FALSE)
  }

  plot_data <- bsm_event_summary[!is.na(bsm_event_summary$mean_count), , drop = FALSE]
  plot_data <- plot_data[plot_data$event_type %in% c(
    "founder", "a", "d", "e", "subset", "vicariance", "sympatry"
  ), , drop = FALSE]
  if (nrow(plot_data) == 0L) {
    stop("No BSM event summary rows are available to plot.", call. = FALSE)
  }
  plot_data$event_label <- stats::reorder(plot_data$event_label, plot_data$mean_count)
  plot_data$sd_count <- suppressWarnings(as.numeric(plot_data$sd_count %||% NA_real_))

  ggplot2::ggplot(plot_data, ggplot2::aes(x = event_label, y = mean_count, fill = event_label)) +
    ggplot2::geom_col(width = 0.72, colour = "grey25", linewidth = 0.25, show.legend = FALSE) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = pmax(0, mean_count - sd_count), ymax = mean_count + sd_count),
      width = 0.18,
      na.rm = TRUE
    ) +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(stats::as.formula("~ model")) +
    ggplot2::labs(
      x = NULL,
      y = "Mean count per stochastic map",
      title = "BSM event-count summary",
      subtitle = "Formal BioGeoBEARS stochastic mapping counts"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}

plot_bsm_event_times <- function(bsm_events) {
  required <- c("event_time_before_present", "event_label")
  missing <- setdiff(required, names(bsm_events))
  if (length(missing) > 0L) {
    stop("bsm_events is missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  plot_data <- bsm_events[!is.na(bsm_events$event_time_before_present), , drop = FALSE]
  if (nrow(plot_data) == 0L) {
    stop("No BSM events with event_time_before_present are available to plot.", call. = FALSE)
  }

  ggplot2::ggplot(plot_data, ggplot2::aes(x = event_time_before_present, colour = event_label)) +
    ggplot2::stat_ecdf(linewidth = 0.9) +
    ggplot2::scale_x_reverse() +
    ggplot2::facet_wrap(stats::as.formula("~ model")) +
    ggplot2::labs(
      x = "Time before present",
      y = "Cumulative proportion of sampled events",
      colour = "Event",
      title = "BSM event timing"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "bottom")
}

plot_bsm_dispersal_routes <- function(bsm_dispersal_routes, route_type = "all_dispersal") {
  required <- c("route_type", "source_region", "target_region", "mean_count")
  missing <- setdiff(required, names(bsm_dispersal_routes))
  if (length(missing) > 0L) {
    stop("bsm_dispersal_routes is missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  plot_data <- bsm_dispersal_routes[bsm_dispersal_routes$route_type == route_type, , drop = FALSE]
  plot_data <- plot_data[!is.na(plot_data$mean_count) & plot_data$mean_count > 0, , drop = FALSE]
  if (nrow(plot_data) == 0L) {
    stop("No positive BSM dispersal routes are available to plot.", call. = FALSE)
  }

  ggplot2::ggplot(plot_data, ggplot2::aes(x = target_region, y = source_region, fill = mean_count)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.35) +
    ggplot2::geom_text(ggplot2::aes(label = round(mean_count, 2)), size = 3) +
    ggplot2::facet_wrap(stats::as.formula("~ model")) +
    ggplot2::scale_fill_gradient(low = "#f7fbff", high = "#08519c") +
    ggplot2::labs(
      x = "Target region",
      y = "Source region",
      fill = "Mean count",
      title = "BSM dispersal directions",
      subtitle = route_type
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid = ggplot2::element_blank())
}

#' Plot model comparison results
#'
#' @param comparison Model comparison table returned by [compare_models()].
#' @return A ggplot object.
#' @export
plot_model_comparison <- function(comparison) {
  required <- c("model", "delta_aicc", "has_j")
  missing <- setdiff(required, names(comparison))
  if (length(missing) > 0L) {
    stop("comparison is missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  comparison$plus_j <- ifelse(comparison$has_j, "+J", "no +J")
  ggplot2::ggplot(
    comparison,
    ggplot2::aes(x = stats::reorder(model, delta_aicc), y = delta_aicc, fill = plus_j)
  ) +
    ggplot2::geom_col(width = 0.72, colour = "grey25", linewidth = 0.25) +
    ggplot2::geom_hline(yintercept = 2, linetype = "dashed", colour = "grey45", linewidth = 0.4) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = c("+J" = "#d95f02", "no +J" = "#1b9e77")) +
    ggplot2::labs(x = NULL, y = expression(Delta * "AICc"), fill = "Model type") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      legend.position = "bottom"
    )
}

#' Plot root state probabilities
#'
#' @param root_state_probabilities Table returned in
#'   `standardized_tables$root_state_probabilities`.
#' @param top_n Number of highest-probability states to show per model.
#' @return A ggplot object.
#' @export
plot_root_state_probabilities <- function(root_state_probabilities, top_n = 8L) {
  required <- c("model", "state", "probability")
  missing <- setdiff(required, names(root_state_probabilities))
  if (length(missing) > 0L) {
    stop("root_state_probabilities is missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  plot_data <- root_state_probabilities[!is.na(root_state_probabilities$probability), , drop = FALSE]
  plot_data <- do.call(rbind, lapply(split(plot_data, plot_data$model), function(x) {
    x <- x[order(-x$probability), , drop = FALSE]
    utils::head(x, top_n)
  }))
  row.names(plot_data) <- NULL
  plot_data$state <- stats::reorder(plot_data$state, plot_data$probability)

  ggplot2::ggplot(plot_data, ggplot2::aes(x = state, y = probability, fill = model)) +
    ggplot2::geom_col(width = 0.72, colour = "grey25", linewidth = 0.2, show.legend = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(stats::as.formula("~ model"), scales = "free_y") +
    ggplot2::scale_y_continuous(limits = c(0, 1), expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(x = "Root range state", y = "Probability") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}

#' Plot best ancestral range state by tree node
#'
#' @param tree_nodes Tree node metadata table from
#'   `standardized_tables$tree_nodes`.
#' @param node_state_summary Node-state summary table from
#'   `standardized_tables$node_state_summary`.
#' @param model Optional model name to plot. Defaults to the first model in
#'   `node_state_summary`.
#' @param location BioGeoBEARS probability location to plot. Defaults to
#'   `"branch_top_at_node"` when available.
#' @param label_tips Logical. If `TRUE`, label tip nodes.
#' @param label_internal_nodes Logical. If `TRUE`, label internal nodes by
#'   best state and probability.
#' @return A ggplot object.
#' @export
plot_node_state_summary <- function(tree_nodes, node_state_summary, model = NULL, location = "branch_top_at_node", label_tips = TRUE, label_internal_nodes = TRUE) {
  node_required <- c("node_index", "node_type", "node_label", "parent_node_index", "edge_length")
  summary_required <- c("model", "location", "node_index", "best_state", "best_probability")
  missing_nodes <- setdiff(node_required, names(tree_nodes))
  missing_summary <- setdiff(summary_required, names(node_state_summary))
  if (length(missing_nodes) > 0L) {
    stop("tree_nodes is missing required columns: ", paste(missing_nodes, collapse = ", "), call. = FALSE)
  }
  if (length(missing_summary) > 0L) {
    stop("node_state_summary is missing required columns: ", paste(missing_summary, collapse = ", "), call. = FALSE)
  }
  if (nrow(tree_nodes) == 0L || nrow(node_state_summary) == 0L) {
    stop("tree_nodes and node_state_summary must contain at least one row.", call. = FALSE)
  }

  if (is.null(model)) {
    model <- unique(node_state_summary$model)[1L]
  }
  available_locations <- unique(node_state_summary$location[node_state_summary$model == model])
  if (!location %in% available_locations) {
    location <- available_locations[1L]
  }

  layout <- layout_tree_nodes(tree_nodes)
  summary_rows <- node_state_summary[
    node_state_summary$model == model & node_state_summary$location == location,
    c("node_index", "best_state", "best_probability"),
    drop = FALSE
  ]
  plot_data <- merge(layout, summary_rows, by = "node_index", all.x = TRUE, sort = FALSE)
  plot_data$plot_probability <- ifelse(is.na(plot_data$best_probability), 0, plot_data$best_probability)
  plot_data$best_state_label <- ifelse(is.na(plot_data$best_state), "not estimated", plot_data$best_state)
  plot_data$probability_label <- ifelse(
    is.na(plot_data$best_probability),
    "NA",
    format(round(plot_data$best_probability, 2L), nsmall = 2L, trim = TRUE)
  )

  edges <- plot_data[!is.na(plot_data$parent_node_index), , drop = FALSE]
  edges <- edges[!is.na(edges$parent_x) & !is.na(edges$parent_y), , drop = FALSE]
  edge_segments <- tree_edge_segments(edges)
  tip_labels <- if (isTRUE(label_tips)) plot_data[plot_data$node_type == "tip", , drop = FALSE] else plot_data[0L, , drop = FALSE]
  tip_labels$tip_display <- if (nrow(tip_labels) > 0L) {
    paste0(tip_labels$node_label, " [", tip_labels$best_state_label, "]")
  } else {
    character(0)
  }
  internal_labels <- if (isTRUE(label_internal_nodes)) plot_data[plot_data$node_type == "internal", , drop = FALSE] else plot_data[0L, , drop = FALSE]
  internal_labels$internal_node_label <- if (nrow(internal_labels) > 0L) {
    paste0(internal_labels$best_state_label, "\n", internal_labels$probability_label)
  } else {
    character(0)
  }

  ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = edge_segments,
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
      linewidth = 0.45,
      colour = "#64748b"
    ) +
    ggplot2::geom_point(
      data = plot_data,
      ggplot2::aes(x = x, y = y, fill = best_state_label, size = plot_probability),
      shape = 21,
      colour = "#1f2937",
      stroke = 0.35,
      alpha = 0.95
    ) +
    ggplot2::geom_text(
      data = tip_labels,
      ggplot2::aes(x = x, y = y, label = tip_display),
      hjust = -0.05,
      size = 3.1
    ) +
    ggplot2::geom_label(
      data = internal_labels,
      ggplot2::aes(x = x, y = y, label = internal_node_label),
      nudge_y = 0.22,
      size = 2.8,
      colour = "#111827",
      fill = "white",
      linewidth = 0.15,
      alpha = 0.9
    ) +
    ggplot2::scale_size_continuous(limits = c(0, 1), range = c(2.4, 7.2), name = "Best-state probability") +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.03, 0.2))) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(
      title = paste("Best ancestral ranges:", model),
      subtitle = location,
      x = "Distance from root",
      y = NULL,
      fill = "Best state"
    ) +
    ggplot2::theme_classic(base_size = 11) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      legend.position = "bottom",
      plot.margin = ggplot2::margin(8, 44, 8, 8)
    )
}

#' Plot node-state sensitivity between non-+J and +J models
#'
#' @param node_state_sensitivity Table from `node_state_sensitivity.csv`.
#' @param top_n Number of highest-sensitivity nodes to show.
#' @param location Optional BioGeoBEARS probability location. Defaults to
#'   `"branch_top_at_node"` when available.
#' @return A ggplot object.
#' @export
plot_node_state_sensitivity <- function(node_state_sensitivity, top_n = 20L, location = NULL) {
  required <- c(
    "location", "node_index", "node_label",
    "non_j_model", "non_j_state", "non_j_probability",
    "plus_j_model", "plus_j_state", "plus_j_probability",
    "state_differs", "probability_difference_abs"
  )
  missing <- setdiff(required, names(node_state_sensitivity))
  if (length(missing) > 0L) {
    stop("node_state_sensitivity is missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (nrow(node_state_sensitivity) == 0L) {
    stop("node_state_sensitivity must contain at least one row.", call. = FALSE)
  }

  available_locations <- unique(node_state_sensitivity$location)
  if (is.null(location)) {
    location <- if ("branch_top_at_node" %in% available_locations) "branch_top_at_node" else available_locations[1L]
  }
  plot_data <- node_state_sensitivity[node_state_sensitivity$location == location, , drop = FALSE]
  if (nrow(plot_data) == 0L) {
    stop("No node_state_sensitivity rows are available for location: ", location, call. = FALSE)
  }

  plot_data$probability_difference_abs <- as.numeric(plot_data$probability_difference_abs)
  plot_data$state_differs <- as.logical(plot_data$state_differs)
  plot_data <- plot_data[order(-plot_data$state_differs, -plot_data$probability_difference_abs), , drop = FALSE]
  plot_data <- utils::head(plot_data, as.integer(top_n))
  plot_data$node_display <- paste0(plot_data$node_label, " (node ", plot_data$node_index, ")")
  plot_data$node_display <- stats::reorder(plot_data$node_display, plot_data$probability_difference_abs)
  plot_data$state_change <- ifelse(plot_data$state_differs, "Best state changed", "Same best state")
  plot_data$comparison <- paste(plot_data$non_j_state, plot_data$plus_j_state, sep = " -> ")
  title <- paste0(unique(plot_data$non_j_model)[1L], " vs ", unique(plot_data$plus_j_model)[1L])

  ggplot2::ggplot(plot_data, ggplot2::aes(x = node_display, y = probability_difference_abs, fill = state_change)) +
    ggplot2::geom_col(width = 0.72, colour = "grey25", linewidth = 0.25) +
    ggplot2::geom_text(ggplot2::aes(label = comparison), hjust = -0.08, size = 3) +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::scale_fill_manual(values = c("Best state changed" = "#d95f02", "Same best state" = "#4c78a8")) +
    ggplot2::scale_y_continuous(limits = c(0, NA), expand = ggplot2::expansion(mult = c(0, 0.22))) +
    ggplot2::labs(
      title = paste("Node-state sensitivity:", title),
      subtitle = location,
      x = NULL,
      y = "Absolute probability difference",
      fill = "Node sensitivity"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      legend.position = "bottom",
      plot.margin = ggplot2::margin(5.5, 35, 5.5, 5.5)
    )
}

#' Generate workflow figures
#'
#' Saves model comparison, root-state probability, and node-state summary
#' figures for a completed workflow run.
#'
#' @param model_comparison Model comparison table returned by [compare_models()].
#' @param standardized_tables List of standardized output tables from
#'   BioGeoBEARS results.
#' @param project_paths Paths returned by [create_project()].
#' @param formats Character vector of graphics formats to write.
#' @return A data frame manifest describing the generated figure files.
#' @export
generate_figures <- function(model_comparison, standardized_tables, project_paths, formats = c("pdf", "png", "svg")) {
  if (is.null(model_comparison) || nrow(model_comparison) == 0L) {
    return(data.frame())
  }

  dir.create(project_paths$figures, recursive = TRUE, showWarnings = FALSE)
  plots <- list(model_comparison = plot_model_comparison(model_comparison))

  root_table <- standardized_tables$root_state_probabilities %||% data.frame()
  if (nrow(root_table) > 0L) {
    plots$root_state_probabilities <- plot_root_state_probabilities(root_table)
  }

  node_table <- standardized_tables$node_state_summary %||% data.frame()
  tree_nodes <- standardized_tables$tree_nodes %||% data.frame()
  if (nrow(node_table) > 0L && nrow(tree_nodes) > 0L) {
    node_plot_models <- select_node_state_plot_models(model_comparison)
    for (i in seq_len(nrow(node_plot_models))) {
      plots[[node_plot_models$figure[[i]]]] <- plot_node_state_summary(
        tree_nodes = tree_nodes,
        node_state_summary = node_table,
        model = node_plot_models$model[[i]]
      )
    }
  }

  node_sensitivity <- standardized_tables$node_state_sensitivity %||% data.frame()
  if (nrow(node_sensitivity) > 0L) {
    plots$node_state_sensitivity <- plot_node_state_sensitivity(node_sensitivity)
  }

  event_summary <- standardized_tables$event_summary %||% data.frame()
  if (nrow(event_summary) > 0L) {
    plots$event_summary <- plot_event_summary(event_summary)
  }

  bsm_event_summary <- standardized_tables$bsm_event_summary %||% data.frame()
  if (nrow(bsm_event_summary) > 0L) {
    plots$bsm_event_summary <- plot_bsm_event_summary(bsm_event_summary)
  }

  bsm_events <- standardized_tables$bsm_events %||% data.frame()
  if (nrow(bsm_events) > 0L) {
    plots$bsm_event_times <- plot_bsm_event_times(bsm_events)
  }

  bsm_routes <- standardized_tables$bsm_dispersal_routes %||% data.frame()
  if (nrow(bsm_routes) > 0L && any(bsm_routes$route_type == "all_dispersal" & bsm_routes$mean_count > 0, na.rm = TRUE)) {
    plots$bsm_dispersal_routes <- plot_bsm_dispersal_routes(bsm_routes)
  }

  manifest <- do.call(rbind, lapply(names(plots), function(name) {
    save_plot_outputs(
      plot = plots[[name]],
      name = name,
      figures_dir = project_paths$figures,
      formats = formats
    )
  }))
  row.names(manifest) <- NULL
  write_csv_base(manifest, file.path(project_paths$figures, "figure_manifest.csv"))
  manifest
}

save_plot_outputs <- function(plot, name, figures_dir, formats) {
  formats <- unique(as.character(formats %||% "png"))
  dimensions <- plot_output_dimensions(name)
  do.call(rbind, lapply(formats, function(format) {
    path <- file.path(figures_dir, paste0(name, ".", format))
    status <- "created"
    error_message <- NA_character_
    tryCatch(
      ggplot2::ggsave(
        filename = path,
        plot = plot,
        width = dimensions$width,
        height = dimensions$height,
        units = "in",
        dpi = 300
      ),
      error = function(e) {
        status <<- "failed"
        error_message <<- conditionMessage(e)
      }
    )
    data.frame(
      figure = name,
      format = format,
      path = as_path(path),
      status = status,
      error_message = error_message,
      stringsAsFactors = FALSE
    )
  }))
}

plot_output_dimensions <- function(name) {
  if (grepl("^node_state_summary", name)) {
    return(list(width = 9.5, height = 6.2))
  }
  if (identical(name, "event_summary")) {
    return(list(width = 7.5, height = 4.8))
  }
  if (identical(name, "bsm_event_summary")) {
    return(list(width = 7.8, height = 5.1))
  }
  if (identical(name, "bsm_event_times")) {
    return(list(width = 7.8, height = 4.8))
  }
  if (identical(name, "bsm_dispersal_routes")) {
    return(list(width = 7.2, height = 5.6))
  }
  list(width = 7, height = 4.5)
}

layout_tree_nodes <- function(tree_nodes) {
  layout <- tree_nodes[order(tree_nodes$node_index), , drop = FALSE]
  layout$node_index <- as.integer(layout$node_index)
  layout$parent_node_index <- as.integer(layout$parent_node_index)
  layout$edge_length <- suppressWarnings(as.numeric(layout$edge_length))

  node_ids <- layout$node_index
  children <- split(
    layout$node_index[!is.na(layout$parent_node_index)],
    layout$parent_node_index[!is.na(layout$parent_node_index)]
  )

  y <- rep(NA_real_, nrow(layout))
  tip_rows <- which(layout$node_type == "tip")
  y[tip_rows] <- seq_along(tip_rows)

  unresolved <- which(is.na(y))
  while (length(unresolved) > 0L) {
    changed <- FALSE
    for (row in unresolved) {
      child_ids <- children[[as.character(layout$node_index[row])]]
      child_rows <- match(child_ids, node_ids)
      child_y <- y[child_rows]
      if (length(child_y) > 0L && all(!is.na(child_y))) {
        y[row] <- mean(child_y)
        changed <- TRUE
      }
    }
    if (!changed) {
      y[unresolved] <- seq_along(unresolved) + max(y, na.rm = TRUE)
      break
    }
    unresolved <- which(is.na(y))
  }

  x <- rep(NA_real_, nrow(layout))
  root_rows <- which(is.na(layout$parent_node_index))
  x[root_rows] <- 0
  unresolved <- which(is.na(x))
  while (length(unresolved) > 0L) {
    changed <- FALSE
    for (row in unresolved) {
      parent_row <- match(layout$parent_node_index[row], node_ids)
      if (!is.na(parent_row) && !is.na(x[parent_row])) {
        branch_length <- layout$edge_length[row]
        if (is.na(branch_length) || !is.finite(branch_length)) {
          branch_length <- 1
        }
        x[row] <- x[parent_row] + branch_length
        changed <- TRUE
      }
    }
    if (!changed) {
      x[unresolved] <- 0
      break
    }
    unresolved <- which(is.na(x))
  }

  layout$x <- x
  layout$y <- y
  parent_rows <- match(layout$parent_node_index, node_ids)
  layout$parent_x <- x[parent_rows]
  layout$parent_y <- y[parent_rows]
  layout
}

tree_edge_segments <- function(edges) {
  if (is.null(edges) || nrow(edges) == 0L) {
    return(data.frame(x = numeric(), y = numeric(), xend = numeric(), yend = numeric()))
  }
  horizontal <- data.frame(
    x = edges$parent_x,
    y = edges$parent_y,
    xend = edges$x,
    yend = edges$parent_y
  )
  vertical <- data.frame(
    x = edges$x,
    y = edges$parent_y,
    xend = edges$x,
    yend = edges$y
  )
  rbind(horizontal, vertical)
}

select_node_state_plot_models <- function(model_comparison) {
  if (is.null(model_comparison) || nrow(model_comparison) == 0L) {
    return(data.frame())
  }
  if (!"has_j" %in% names(model_comparison)) {
    model_comparison$has_j <- is_j_model(model_comparison$model)
  }
  if (!"AICc" %in% names(model_comparison)) {
    model_comparison$AICc <- seq_len(nrow(model_comparison))
  }

  rows <- list(data.frame(
    figure = "node_state_summary_best_model",
    role = "best_overall",
    model = model_comparison$model[which.min(model_comparison$AICc)],
    stringsAsFactors = FALSE
  ))

  non_j <- model_comparison[!model_comparison$has_j, , drop = FALSE]
  if (nrow(non_j) > 0L) {
    rows[[length(rows) + 1L]] <- data.frame(
      figure = "node_state_summary_best_non_j",
      role = "best_non_j",
      model = non_j$model[which.min(non_j$AICc)],
      stringsAsFactors = FALSE
    )
  }

  plus_j <- model_comparison[model_comparison$has_j, , drop = FALSE]
  if (nrow(plus_j) > 0L) {
    rows[[length(rows) + 1L]] <- data.frame(
      figure = "node_state_summary_best_plus_j",
      role = "best_plus_j",
      model = plus_j$model[which.min(plus_j$AICc)],
      stringsAsFactors = FALSE
    )
  }

  out <- do.call(rbind, rows)
  out <- out[!duplicated(out$model), , drop = FALSE]
  row.names(out) <- NULL
  out
}
