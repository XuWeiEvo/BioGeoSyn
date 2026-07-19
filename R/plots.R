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
    scale_colour_bgs() +
    ggplot2::labs(x = "Time before present", y = "Cumulative proportion of events", colour = "Event type") +
    theme_bgs()
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
    ggplot2::geom_col(width = 0.72, colour = bgs_palette()$outline, linewidth = 0.25, show.legend = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(stats::as.formula("~ model")) +
    scale_fill_bgs() +
    ggplot2::labs(
      x = NULL,
      y = "Branch count",
      title = "Range-change event summary",
      subtitle = "Derived from highest-probability ancestral states; not stochastic mapping counts"
    ) +
    theme_bgs() +
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
    ggplot2::geom_col(width = 0.72, colour = bgs_palette()$outline, linewidth = 0.25, show.legend = FALSE) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = pmax(0, mean_count - sd_count), ymax = mean_count + sd_count),
      width = 0.18,
      colour = bgs_palette()$ink,
      na.rm = TRUE
    ) +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(stats::as.formula("~ model")) +
    scale_fill_bgs() +
    ggplot2::labs(
      x = NULL,
      y = "Mean count per stochastic map",
      title = "BSM event-count summary",
      subtitle = "Formal BioGeoBEARS stochastic mapping counts"
    ) +
    theme_bgs() +
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
    scale_colour_bgs() +
    ggplot2::labs(
      x = "Time before present",
      y = "Cumulative proportion of sampled events",
      colour = "Event",
      title = "BSM event timing"
    ) +
    theme_bgs()
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
    ggplot2::geom_tile(colour = "white", linewidth = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = round(mean_count, 2)), size = 3, colour = bgs_palette()$ink) +
    ggplot2::facet_wrap(stats::as.formula("~ model")) +
    scale_fill_bgs_seq(name = "Mean count") +
    ggplot2::labs(
      x = "Target region",
      y = "Source region",
      title = "BSM dispersal directions",
      subtitle = route_type
    ) +
    theme_bgs() +
    ggplot2::theme(panel.grid = ggplot2::element_blank())
}

#' Plot BSM dispersal as a directed arrow network
#'
#' Draw region-to-region BSM dispersal as a directed graph: nodes are regions
#' and directed arrows run source to target with arrow width proportional to the
#' mean dispersal count per stochastic map.
#'
#' @param bsm_dispersal_routes Standardized BSM dispersal routes table.
#' @param route_type Route type to plot; defaults to `"all_dispersal"`.
#' @param model Optional model name; defaults to the first model present.
#' @param subtitle Optional subtitle; defaults to `route_type`. Used to label a
#'   geological period when the network is sliced by time.
#' @return A ggraph/ggplot object.
#' @export
plot_bsm_dispersal_network <- function(bsm_dispersal_routes, route_type = "all_dispersal", model = NULL, subtitle = NULL) {
  required <- c("route_type", "source_region", "target_region", "mean_count")
  missing <- setdiff(required, names(bsm_dispersal_routes))
  if (length(missing) > 0L) {
    stop("bsm_dispersal_routes is missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  data <- bsm_dispersal_routes[bsm_dispersal_routes$route_type == route_type, , drop = FALSE]
  if (!is.null(model) && "model" %in% names(data)) {
    data <- data[data$model == model, , drop = FALSE]
  } else if ("model" %in% names(data) && length(unique(data$model)) > 1L) {
    data <- data[data$model == unique(data$model)[[1L]], , drop = FALSE]
  }
  data <- data[!is.na(data$mean_count) & data$mean_count > 0 &
    !is.na(data$source_region) & !is.na(data$target_region), , drop = FALSE]
  data <- data[data$source_region != data$target_region, , drop = FALSE]
  if (nrow(data) == 0L) {
    stop("No positive between-region BSM dispersal routes are available to plot.", call. = FALSE)
  }

  edges <- stats::aggregate(mean_count ~ source_region + target_region, data = data, FUN = sum)
  nodes <- data.frame(name = sort(unique(c(edges$source_region, edges$target_region))), stringsAsFactors = FALSE)
  graph <- igraph::graph_from_data_frame(edges, directed = TRUE, vertices = nodes)

  ggraph::ggraph(graph, layout = "circle") +
    ggraph::geom_edge_arc(
      ggplot2::aes(width = mean_count),
      strength = 0.12, alpha = 0.6, colour = "#D55E00",
      arrow = grid::arrow(length = grid::unit(3.2, "mm"), type = "closed"),
      start_cap = ggraph::circle(6, "mm"), end_cap = ggraph::circle(6, "mm")
    ) +
    ggraph::geom_node_point(ggplot2::aes(colour = name), size = 9, show.legend = FALSE) +
    ggraph::geom_node_text(ggplot2::aes(label = name), repel = TRUE, size = 3.3, colour = bgs_palette()$ink) +
    ggraph::scale_edge_width(range = c(0.4, 3.2), name = "Mean count") +
    scale_colour_bgs() +
    ggplot2::labs(title = "BSM dispersal network", subtitle = subtitle %||% route_type) +
    ggplot2::coord_fixed(clip = "off") +
    ggplot2::theme_void(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1.15), colour = bgs_palette()$ink),
      plot.subtitle = ggplot2::element_text(size = ggplot2::rel(0.9), colour = bgs_palette()$muted, margin = ggplot2::margin(b = 8)),
      legend.position = "bottom",
      plot.margin = ggplot2::margin(10, 18, 10, 18)
    )
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
    ggplot2::geom_col(width = 0.72, colour = bgs_palette()$outline, linewidth = 0.25) +
    ggplot2::geom_hline(yintercept = 2, linetype = "dashed", colour = bgs_palette()$muted, linewidth = 0.4) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = c("+J" = "#D55E00", "no +J" = "#0072B2")) +
    ggplot2::labs(x = NULL, y = expression(Delta * "AICc"), fill = "Model type") +
    theme_bgs() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
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
    ggplot2::geom_col(width = 0.72, colour = bgs_palette()$outline, linewidth = 0.2, show.legend = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(stats::as.formula("~ model"), scales = "free_y") +
    scale_fill_bgs() +
    ggplot2::scale_y_continuous(limits = c(0, 1), expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(x = "Root range state", y = "Probability") +
    theme_bgs() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}

#' Plot ancestral range states on the tree
#'
#' Draw a rectangular phylogram with a pie chart at every node showing the
#' probability of each ancestral range state, so the reconstruction reads like a
#' standard BioGeoBEARS ancestral-state figure. When the full state distribution
#' is not supplied, each node falls back to a single point coloured by its best
#' state.
#'
#' @param tree_nodes Tree node metadata table from
#'   `standardized_tables$tree_nodes`.
#' @param node_state_summary Node-state summary table from
#'   `standardized_tables$node_state_summary`.
#' @param ancestral_state_probabilities Optional long-format per-node per-state
#'   probability table from `standardized_tables$ancestral_state_probabilities`;
#'   when supplied, node pies show the full distribution.
#' @param model Optional model name to plot. Defaults to the first model in
#'   `node_state_summary`.
#' @param location BioGeoBEARS probability location to plot. Defaults to
#'   `"branch_top_at_node"` when available.
#' @param label_tips Logical. If `TRUE`, label tip nodes with taxon and best
#'   state.
#' @param label_internal_nodes Deprecated; kept for backward compatibility and
#'   ignored (internal nodes are shown as pies).
#' @param node_radius Numeric radius of the node pies (and fallback points) in
#'   tip-spacing units. Larger values make the pie wedges easier to see, which
#'   helps when most nodes are reconstructed with high confidence (near-solid
#'   pies) and only a few carry visible uncertainty. Defaults to `0.28`
#'   (`"pie"`) or `0.42` (`"single"`, to leave room for the area code).
#' @param style Marker style at each node. `"pie"` (default) draws a pie of the
#'   full state probability distribution. `"single"` draws one solid circle per
#'   node, coloured by the single most likely range and labelled with that
#'   range's area code in the centre - a cleaner read for large trees where the
#'   pies become hard to tell apart.
#' @return A ggplot object.
#' @export
plot_node_state_summary <- function(tree_nodes, node_state_summary, ancestral_state_probabilities = NULL, model = NULL, location = "branch_top_at_node", label_tips = TRUE, label_internal_nodes = TRUE, node_radius = 0.28, style = c("pie", "single")) {
  style <- match.arg(style)
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
  n_tips <- sum(tree_nodes$node_type == "tip", na.rm = TRUE)
  y_span <- max(1, n_tips - 1L)
  x_max <- suppressWarnings(max(layout$x, na.rm = TRUE))
  if (!is.finite(x_max) || x_max <= 0) {
    x_max <- 1
  }

  # Figure height grows with tip count so node pies keep a legible, roughly
  # constant absolute size instead of shrinking as species accumulate. The
  # distance-from-root axis (x) is rescaled to the figure aspect so that
  # coord_fixed() fills the canvas (keeping the pies circular and correctly
  # sized) rather than squeezing the whole tree into a square. The axis still
  # reports true distances via relabelled breaks below.
  fig_width <- 9.5
  fig_height <- min(30, max(6.2, n_tips * 0.4))
  attr_dims <- list(width = fig_width, height = fig_height)
  aspect <- fig_width / fig_height
  x_scale <- (y_span * aspect) / x_max
  layout$xp <- layout$x * x_scale
  layout$parent_xp <- layout$parent_x * x_scale

  best <- node_state_summary[
    node_state_summary$model == model & node_state_summary$location == location,
    c("node_index", "best_state", "best_probability"),
    drop = FALSE
  ]
  layout <- merge(layout, best, by = "node_index", all.x = TRUE, sort = FALSE)
  layout$best_state_label <- ifelse(is.na(layout$best_state), "not estimated", layout$best_state)

  ed <- layout[
    !is.na(layout$parent_node_index) & !is.na(layout$parent_xp) & !is.na(layout$parent_y),
    ,
    drop = FALSE
  ]
  edge_segments <- rbind(
    data.frame(x = ed$parent_xp, y = ed$y, xend = ed$xp, yend = ed$y, stringsAsFactors = FALSE),
    data.frame(x = ed$parent_xp, y = ed$parent_y, xend = ed$parent_xp, yend = ed$y, stringsAsFactors = FALSE)
  )

  default_radius <- if (identical(style, "single")) 0.42 else 0.28
  radius <- if (missing(node_radius)) default_radius else suppressWarnings(as.numeric(node_radius))
  if (length(radius) != 1L || !is.finite(radius) || radius <= 0) {
    radius <- default_radius
  }

  single_style <- identical(style, "single")
  pie_df <- if (single_style) NULL else node_state_pie_wedges(ancestral_state_probabilities, layout, model, location, radius)
  marker_df <- if (single_style) node_state_single_markers(layout, radius) else NULL

  tip_rows <- layout[layout$node_type == "tip", , drop = FALSE]
  # Tips show the observed range after the name in both styles: pies label tips
  # this way, and the single style draws discs only at internal nodes.
  tip_rows$tip_display <- paste0(tip_rows$node_label, "  [", tip_rows$best_state_label, "]")

  # Choose which ranges appear in the legend. With many areas the number of
  # possible ranges explodes (e.g. 11 areas, max range 3 = 231 states); listing
  # every one buries the tree. Keep a range if it reaches a meaningful share of
  # at least one node's reconstruction (>= legend_threshold there), so a locally
  # dominant range is kept even when it occurs at a single node (e.g. 100% at
  # one node), while ranges that are only ever minor slivers are dropped. Every
  # wedge is still coloured; this only trims the legend.
  legend_threshold <- 0.2
  max_legend <- 24L
  ap_cols <- c("model", "location", "state", "probability")
  if (single_style) {
    # The legend lists the ranges actually used as best states, most common
    # first; the on-node letters already say which is which, so this is only a
    # colour key.
    counts <- sort(table(marker_df$state), decreasing = TRUE)
    fill_levels <- names(counts)
    total_ranges <- length(fill_levels)
  } else if (!is.null(pie_df) && nrow(pie_df) > 0L && !is.null(ancestral_state_probabilities) &&
      all(ap_cols %in% names(ancestral_state_probabilities))) {
    apl <- ancestral_state_probabilities
    apl <- apl[apl$model == model & apl$location == location &
                 !is.na(apl$probability) & apl$probability > 0, , drop = FALSE]
    total_ranges <- length(unique(apl$state))
    maxp <- stats::aggregate(list(p = apl$probability), by = list(state = apl$state), FUN = max)
    maxp <- maxp[order(-maxp$p), , drop = FALSE]
    fill_levels <- maxp$state[maxp$p >= legend_threshold]
    if (length(fill_levels) == 0L) {
      fill_levels <- utils::head(maxp$state, 1L)
    }
  } else {
    counts <- sort(table(layout$best_state_label[layout$node_type != "tip"]), decreasing = TRUE)
    fill_levels <- names(counts)
    total_ranges <- length(fill_levels)
  }
  fill_levels <- fill_levels[!is.na(fill_levels)]
  if (length(fill_levels) > max_legend) {
    fill_levels <- fill_levels[seq_len(max_legend)]
  }
  legend_truncated <- length(fill_levels) < total_ranges
  legend_breaks <- if (legend_truncated) fill_levels else ggplot2::waiver()
  legend_subtitle <- if (single_style) {
    if (legend_truncated) {
      sprintf("Internal nodes show the single most likely range as an area code; tips show observed ranges. %d ranges used, legend shows the %d most common.",
              total_ranges, length(fill_levels))
    } else {
      "Internal nodes show the single most likely range as an area code; tips show observed ranges in brackets."
    }
  } else if (legend_truncated) {
    sprintf("%d ranges reconstructed; legend shows the %d reaching >=%d%% at some node - tip ranges are labelled at the tips.",
            total_ranges, length(fill_levels), round(legend_threshold * 100))
  } else {
    NULL
  }
  plot_title <- if (single_style) {
    paste("Most likely ancestral range:", model)
  } else {
    paste("Ancestral ranges:", model)
  }

  plot <- ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = edge_segments,
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
      linewidth = 0.55,
      colour = bgs_palette()$ink,
      lineend = "round"
    )

  if (single_style) {
    plot <- plot +
      ggforce::geom_circle(
        data = marker_df,
        ggplot2::aes(x0 = x0, y0 = y0, r = r, fill = state),
        colour = bgs_palette()$ink, linewidth = 0.2
      ) +
      ggplot2::geom_text(
        data = marker_df,
        ggplot2::aes(x = x0, y = y0, label = state),
        colour = marker_df$text_colour, size = marker_df$text_size,
        fontface = "bold"
      )
  } else if (!is.null(pie_df)) {
    plot <- plot +
      ggforce::geom_arc_bar(
        data = pie_df,
        ggplot2::aes(x0 = x0, y0 = y0, r0 = 0, r = radius, start = start, end = end, fill = state),
        colour = "white", linewidth = 0.18
      )
  } else {
    plot <- plot +
      ggplot2::geom_point(
        data = layout,
        ggplot2::aes(x = xp, y = y, fill = best_state_label),
        shape = 21, size = max(2, 18 * radius), colour = bgs_palette()$ink, stroke = 0.4
      )
  }

  if (isTRUE(label_tips) && nrow(tip_rows) > 0L) {
    plot <- plot +
      ggplot2::geom_text(
        data = tip_rows,
        ggplot2::aes(x = xp, y = y, label = tip_display),
        hjust = 0, nudge_x = radius + 0.08, size = 3.2,
        colour = bgs_palette()$ink
      )
  }

  # Label the axis as time before present: the tips (right) are the present
  # (age 0) and the root (left) is the oldest. Breaks are chosen in age units
  # and mapped back to plot positions (distance-from-root = x_max - age).
  age_breaks <- pretty(c(0, x_max))
  age_breaks <- age_breaks[age_breaks >= 0 & age_breaks <= x_max + 1e-9]

  final_plot <- plot +
    scale_fill_bgs(breaks = legend_breaks) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2L)) +
    ggplot2::scale_x_continuous(
      breaks = (x_max - age_breaks) * x_scale,
      labels = age_breaks,
      expand = ggplot2::expansion(mult = c(0.03, 0.38))
    ) +
    ggplot2::coord_fixed(clip = "off") +
    ggplot2::labs(
      title = plot_title,
      subtitle = legend_subtitle,
      x = "Time before present",
      y = NULL,
      fill = "Range"
    ) +
    theme_bgs() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_line(colour = bgs_palette()$ink, linewidth = 0.5),
      axis.ticks.x = ggplot2::element_line(colour = bgs_palette()$ink, linewidth = 0.4),
      plot.margin = ggplot2::margin(8, 60, 8, 8)
    )
  # Preferred save dimensions (height scales with tip count); honoured by
  # save_plot_outputs() so the pies stay legible for large trees.
  attr(final_plot, "bgs_dims") <- attr_dims
  final_plot
}

node_state_pie_wedges <- function(ancestral_state_probabilities, layout, model, location, radius) {
  ap <- ancestral_state_probabilities
  needed <- c("model", "location", "node_index", "state", "probability")
  if (is.null(ap) || nrow(ap) == 0L || !all(needed %in% names(ap))) {
    return(NULL)
  }
  ap <- ap[ap$model == model & ap$location == location & !is.na(ap$probability) & ap$probability > 0, , drop = FALSE]
  if (nrow(ap) == 0L) {
    return(NULL)
  }
  x_positions <- if ("xp" %in% names(layout)) layout$xp else layout$x
  pieces <- lapply(split(seq_len(nrow(ap)), ap$node_index), function(idx) {
    d <- ap[idx, , drop = FALSE]
    d <- d[order(d$state), , drop = FALSE]
    node_index <- d$node_index[[1L]]
    x0 <- x_positions[match(node_index, layout$node_index)]
    y0 <- layout$y[match(node_index, layout$node_index)]
    if (length(x0) == 0L || is.na(x0) || is.na(y0)) {
      return(NULL)
    }
    end <- cumsum(d$probability / sum(d$probability)) * (2 * pi)
    start <- c(0, utils::head(end, -1L))
    data.frame(
      x0 = x0, y0 = y0, state = d$state,
      start = start, end = end,
      stringsAsFactors = FALSE
    )
  })
  pieces <- pieces[!vapply(pieces, is.null, logical(1))]
  if (length(pieces) == 0L) {
    return(NULL)
  }
  do.call(rbind, pieces)
}

# One labelled circle per internal node for the "single" style: the single most
# likely range as a solid disc, with that range's area code sitting in the
# centre. Tips carry observed (not reconstructed) ranges, so they are left to
# the tip labels; this also stops big discs from colliding with their parent
# node on short terminal branches.
node_state_single_markers <- function(layout, radius) {
  keep <- layout[!is.na(layout$best_state) & layout$node_type != "tip", , drop = FALSE]
  if (nrow(keep) == 0L) {
    return(NULL)
  }
  x0 <- if ("xp" %in% names(keep)) keep$xp else keep$x
  df <- data.frame(
    x0 = x0,
    y0 = keep$y,
    r = radius,
    state = as.character(keep$best_state),
    stringsAsFactors = FALSE
  )
  df <- df[!is.na(df$x0) & !is.na(df$y0), , drop = FALSE]
  if (nrow(df) == 0L) {
    return(NULL)
  }

  # Match the fill scale_fill_bgs() will apply: discrete_scale colours the
  # sorted levels in order, so we colour the same way to pick a legible text
  # colour for each disc.
  levels_used <- sort(unique(df$state))
  fills <- bgs_qual_colors(length(levels_used))
  names(fills) <- levels_used
  df$text_colour <- high_contrast_text(fills[df$state])

  # Shrink the code so multi-area ranges still fit inside the disc.
  code_len <- nchar(df$state)
  base_size <- 3.4 * (radius / 0.42)
  df$text_size <- pmax(1.5, base_size * pmin(1, 1.7 / pmax(1, code_len)))
  df
}

# Pick black or white text for each fill colour by whichever gives the higher
# WCAG contrast ratio, so area codes stay readable on every palette hue.
high_contrast_text <- function(fill_hex, dark = bgs_palette()$ink) {
  lum <- relative_luminance(fill_hex)
  dark_lum <- relative_luminance(dark)
  contrast <- function(a, b) (pmax(a, b) + 0.05) / (pmin(a, b) + 0.05)
  ifelse(contrast(lum, 1) >= contrast(lum, dark_lum), "#ffffff", dark)
}

relative_luminance <- function(hex) {
  rgb <- grDevices::col2rgb(hex) / 255
  linearise <- function(ch) ifelse(ch <= 0.03928, ch / 12.92, ((ch + 0.055) / 1.055)^2.4)
  lin <- apply(rgb, 2L, linearise)
  if (is.null(dim(lin))) {
    lin <- matrix(lin, nrow = 3L)
  }
  as.numeric(0.2126 * lin[1L, ] + 0.7152 * lin[2L, ] + 0.0722 * lin[3L, ])
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
    ggplot2::geom_col(width = 0.72, colour = bgs_palette()$outline, linewidth = 0.25) +
    ggplot2::geom_text(ggplot2::aes(label = comparison), hjust = -0.08, size = 3, colour = bgs_palette()$ink) +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::scale_fill_manual(values = c("Best state changed" = "#D55E00", "Same best state" = "#0072B2")) +
    ggplot2::scale_y_continuous(limits = c(0, NA), expand = ggplot2::expansion(mult = c(0, 0.22))) +
    ggplot2::labs(
      title = paste("Node-state sensitivity:", title),
      subtitle = location,
      x = NULL,
      y = "Absolute probability difference",
      fill = "Node sensitivity"
    ) +
    theme_bgs() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
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
  ancestral_probs <- standardized_tables$ancestral_state_probabilities %||% data.frame()
  if (nrow(node_table) > 0L && nrow(tree_nodes) > 0L) {
    node_plot_models <- select_node_state_plot_models(model_comparison)
    for (i in seq_len(nrow(node_plot_models))) {
      figure <- node_plot_models$figure[[i]]
      model_name <- node_plot_models$model[[i]]
      plots[[figure]] <- plot_node_state_summary(
        tree_nodes = tree_nodes,
        node_state_summary = node_table,
        ancestral_state_probabilities = ancestral_probs,
        model = model_name
      )
      # Companion "single most likely range" view of the same tree: one solid,
      # area-code-labelled disc per node instead of a probability pie.
      plots[[paste0(figure, "_single")]] <- plot_node_state_summary(
        tree_nodes = tree_nodes,
        node_state_summary = node_table,
        ancestral_state_probabilities = ancestral_probs,
        model = model_name,
        style = "single"
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
    network <- tryCatch(plot_bsm_dispersal_network(bsm_routes), error = function(e) NULL)
    if (!is.null(network)) {
      plots$bsm_dispersal_network <- network
    }
  }

  process_summary <- standardized_tables$biogeographic_process_summary %||% data.frame()
  if (nrow(process_summary) > 0L) {
    plots$biogeographic_process_synthesis <- plot_biogeographic_process_synthesis(process_summary)
  }

  region_budgets <- standardized_tables$region_process_budgets %||% data.frame()
  if (nrow(region_budgets) > 0L) {
    plots$region_process_budget <- plot_region_process_budget(region_budgets)
  }

  process_rates <- standardized_tables$process_rates_through_time %||% data.frame()
  if (nrow(process_rates) > 0L) {
    plots$process_rates_through_time <- plot_process_rates_through_time(process_rates)
  }

  region_process_rates <- standardized_tables$region_process_rates_through_time %||% data.frame()
  if (nrow(region_process_rates) > 0L) {
    plots$region_process_rates_through_time <- plot_region_process_rates_through_time(region_process_rates)
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
  dimensions <- attr(plot, "bgs_dims") %||% plot_output_dimensions(name)
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
  if (identical(name, "bsm_dispersal_network")) {
    return(list(width = 7.0, height = 6.4))
  }
  if (identical(name, "biogeographic_process_synthesis")) {
    return(list(width = 8.2, height = 5.2))
  }
  if (identical(name, "region_process_budget")) {
    return(list(width = 7.8, height = 5.0))
  }
  if (identical(name, "process_rates_through_time")) {
    return(list(width = 8.0, height = 5.6))
  }
  if (identical(name, "region_process_rates_through_time")) {
    return(list(width = 8.6, height = 6.2))
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
