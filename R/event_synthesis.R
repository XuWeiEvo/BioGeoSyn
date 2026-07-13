#' Biogeographic process taxonomy
#'
#' Canonical mapping from BioGeoBEARS event codes to interpretable
#' biogeographic process categories used throughout the iBiogeobears event
#' statistics, synthesis tables, and figures. Cladogenetic processes are
#' speciation modes realized at nodes; anagenetic processes are range changes
#' realized along branches. This mapping is the reference used to translate raw
#' BioGeoBEARS stochastic mapping event codes (`y`, `s`, `v`, `j`, `d`, `e`,
#' `a`) into named biogeographic processes.
#'
#' @return A data frame with one row per process and the columns
#'   `process_key`, `process_label`, `process_group` (`cladogenetic` or
#'   `anagenetic`), `biogeobears_code`, `bsm_event_type` (the matching
#'   `event_type` value in `bsm_event_summary`), and `definition`.
#' @examples
#' biogeographic_process_taxonomy()
#' @export
biogeographic_process_taxonomy <- function() {
  data.frame(
    process_key = c(
      "in_situ_speciation",
      "subset_sympatry",
      "vicariance",
      "founder_event_speciation",
      "range_expansion",
      "local_extinction",
      "range_switching"
    ),
    process_label = c(
      "In-situ (sympatric) speciation",
      "Subset sympatry",
      "Vicariance",
      "Founder-event (jump) speciation",
      "Range expansion",
      "Local extinction",
      "Range switching"
    ),
    process_group = c(
      "cladogenetic",
      "cladogenetic",
      "cladogenetic",
      "cladogenetic",
      "anagenetic",
      "anagenetic",
      "anagenetic"
    ),
    biogeobears_code = c("y", "s", "v", "j", "d", "e", "a"),
    bsm_event_type = c(
      "sympatry",
      "subset",
      "vicariance",
      "founder",
      "d",
      "e",
      "a"
    ),
    definition = c(
      "Both daughter lineages inherit the same single ancestral area (within-area speciation).",
      "One daughter inherits the full ancestral range; the other inherits a single-area subset.",
      "The ancestral range is divided between the two daughter lineages.",
      "One daughter lineage jumps to a new area outside the ancestral range.",
      "A lineage adds a new area to its range along a branch (range-expansion dispersal).",
      "A lineage loses an area from its range along a branch (range contraction).",
      "A lineage replaces one occupied area with another along a branch."
    ),
    stringsAsFactors = FALSE
  )
}

#' Summarize BioGeoBEARS stochastic mapping into biogeographic processes
#'
#' Aggregate the formal BioGeoBEARS biogeographical stochastic mapping (BSM)
#' event-count summary into named biogeographic processes, split into
#' cladogenetic speciation modes and anagenetic range changes. Proportions are
#' computed both within each process group and across all processes so that,
#' for example, the relative importance of founder-event speciation among
#' cladogenetic events and among all events can both be read directly.
#'
#' @param bsm_tables A list of standardized BSM tables containing at least
#'   `bsm_event_summary`, as returned in a workflow result's `bsm_tables`.
#' @return A data frame with one row per model and process, including
#'   `process_group`, `process_label`, `mean_count`, `sd_count`, `sum_count`,
#'   `proportion_within_group`, and `proportion_overall`. Returns an empty
#'   table with the same columns when no BSM event summary is available.
#' @export
summarize_biogeographic_processes <- function(bsm_tables) {
  empty <- empty_process_summary_table()
  summary <- (bsm_tables %||% list())$bsm_event_summary %||% NULL
  if (is.null(summary) || nrow(summary) == 0L) {
    return(empty)
  }
  required <- c("model", "event_type", "mean_count")
  if (!all(required %in% names(summary))) {
    return(empty)
  }

  taxonomy <- biogeographic_process_taxonomy()
  summary <- summary[summary$event_type %in% taxonomy$bsm_event_type, , drop = FALSE]
  if (nrow(summary) == 0L) {
    return(empty)
  }

  idx <- match(summary$event_type, taxonomy$bsm_event_type)
  out <- data.frame(
    model = as.character(summary$model),
    process_group = taxonomy$process_group[idx],
    process_key = taxonomy$process_key[idx],
    process_label = taxonomy$process_label[idx],
    biogeobears_code = taxonomy$biogeobears_code[idx],
    mean_count = suppressWarnings(as.numeric(summary$mean_count)),
    sd_count = suppressWarnings(as.numeric(summary$sd_count %||% rep(NA_real_, nrow(summary)))),
    sum_count = suppressWarnings(as.numeric(summary$sum_count %||% rep(NA_real_, nrow(summary)))),
    replicate_count = suppressWarnings(as.integer(summary$replicate_count %||% rep(NA_integer_, nrow(summary)))),
    definition = taxonomy$definition[idx],
    stringsAsFactors = FALSE
  )

  out <- do.call(rbind, lapply(split(out, out$model), function(model_rows) {
    overall_total <- sum(model_rows$mean_count, na.rm = TRUE)
    model_rows$proportion_overall <- if (overall_total > 0) {
      model_rows$mean_count / overall_total
    } else {
      rep(NA_real_, nrow(model_rows))
    }
    do.call(rbind, lapply(split(model_rows, model_rows$process_group), function(group_rows) {
      group_total <- sum(group_rows$mean_count, na.rm = TRUE)
      group_rows$proportion_within_group <- if (group_total > 0) {
        group_rows$mean_count / group_total
      } else {
        rep(NA_real_, nrow(group_rows))
      }
      group_rows
    }))
  }))

  out$interpretation_note <-
    "Mean BioGeoBEARS stochastic mapping count per biogeographic process, aggregated across completed maps."
  group_order <- factor(out$process_group, levels = c("cladogenetic", "anagenetic"))
  out <- out[order(out$model, group_order, -out$mean_count), , drop = FALSE]
  row.names(out) <- NULL
  out[, names(empty), drop = FALSE]
}

#' Plot the biogeographic process synthesis
#'
#' Draw the centerpiece synthesis figure: mean stochastic mapping event counts
#' per biogeographic process, coloured by process group (cladogenetic
#' speciation modes versus anagenetic range changes), with one panel per model.
#'
#' @param process_summary A data frame from
#'   [summarize_biogeographic_processes()].
#' @return A ggplot object.
#' @export
plot_biogeographic_process_synthesis <- function(process_summary) {
  required <- c("model", "process_group", "process_label", "mean_count")
  missing <- setdiff(required, names(process_summary))
  if (length(missing) > 0L) {
    stop("process_summary is missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  plot_data <- process_summary[!is.na(process_summary$mean_count), , drop = FALSE]
  if (nrow(plot_data) == 0L) {
    stop("process_summary has no rows with a mean_count to plot.", call. = FALSE)
  }

  plot_data$group_label <- ifelse(
    plot_data$process_group == "cladogenetic",
    "Cladogenetic (speciation mode)",
    "Anagenetic (range evolution)"
  )
  plot_data$sd_count <- suppressWarnings(as.numeric(plot_data$sd_count %||% rep(NA_real_, nrow(plot_data))))

  ordering <- order(
    factor(plot_data$process_group, levels = c("anagenetic", "cladogenetic")),
    plot_data$mean_count
  )
  level_order <- unique(plot_data$process_label[ordering])
  plot_data$process_label <- factor(plot_data$process_label, levels = level_order)

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = process_label, y = mean_count, fill = group_label)
  ) +
    ggplot2::geom_col(width = 0.72, colour = "grey25", linewidth = 0.25) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = pmax(0, mean_count - sd_count), ymax = mean_count + sd_count),
      width = 0.18,
      na.rm = TRUE
    ) +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(stats::as.formula("~ model")) +
    ggplot2::scale_fill_manual(values = ibgb_palette()$process) +
    ggplot2::labs(
      x = NULL,
      y = "Mean count per stochastic map",
      fill = NULL,
      title = "Biogeographic process synthesis",
      subtitle = "Formal BioGeoBEARS stochastic mapping, grouped by process type"
    ) +
    theme_ibgb() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}

empty_process_summary_table <- function() {
  data.frame(
    model = character(),
    process_group = character(),
    process_key = character(),
    process_label = character(),
    biogeobears_code = character(),
    mean_count = numeric(),
    sd_count = numeric(),
    sum_count = numeric(),
    replicate_count = integer(),
    proportion_within_group = numeric(),
    proportion_overall = numeric(),
    definition = character(),
    interpretation_note = character(),
    stringsAsFactors = FALSE
  )
}
