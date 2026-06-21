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

#' Generate workflow figures
#'
#' Saves model comparison and root-state probability figures for a completed
#' workflow run.
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
  do.call(rbind, lapply(formats, function(format) {
    path <- file.path(figures_dir, paste0(name, ".", format))
    status <- "created"
    error_message <- NA_character_
    tryCatch(
      ggplot2::ggsave(
        filename = path,
        plot = plot,
        width = 7,
        height = 4.5,
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
