#' Summarize biogeographic process rates through time
#'
#' Bin the sampled BioGeoBEARS stochastic mapping (BSM) events by time before
#' present and summarize, per biogeographic process and time bin, the mean and
#' standard deviation of the event count per stochastic map, and the mean rate
#' (events per unit time). This gives a standardized view of when each
#' biogeographic process occurred that BioGeoBEARS does not produce directly.
#'
#' @param bsm_tables A list of standardized BSM tables containing at least
#'   `bsm_events`, and optionally `bsm_event_summary`, as returned in a workflow
#'   result's `bsm_tables`.
#' @param n_bins Number of equal-width time bins between the present and the
#'   oldest sampled event.
#' @return A data frame with one row per model, process, and time bin, including
#'   `bin_start`, `bin_end`, `bin_midpoint`, `mean_count`, `sd_count`,
#'   `ci_lower` and `ci_upper` (the 2.5% and 97.5% percentiles of the per-map
#'   count across stochastic maps), and `rate`. Returns an empty table with the
#'   same columns when no timed BSM events are available.
#' @export
summarize_process_rates_through_time <- function(bsm_tables, n_bins = 10L) {
  empty <- empty_process_rates_table()
  bsm_tables <- bsm_tables %||% list()
  events <- bsm_tables$bsm_events %||% NULL
  event_summary <- bsm_tables$bsm_event_summary %||% NULL
  if (is.null(events) || nrow(events) == 0L) {
    return(empty)
  }
  needed <- c("model", "replicate", "event_type", "event_time_before_present")
  if (!all(needed %in% names(events))) {
    return(empty)
  }
  n_bins <- max(1L, as.integer(n_bins))

  taxonomy <- biogeographic_process_taxonomy()
  ev <- events[
    events$event_type %in% taxonomy$bsm_event_type &
      !is.na(events$event_time_before_present),
    ,
    drop = FALSE
  ]
  if (nrow(ev) == 0L) {
    return(empty)
  }
  idx <- match(ev$event_type, taxonomy$bsm_event_type)
  ev$process_key <- taxonomy$process_key[idx]
  ev$process_label <- taxonomy$process_label[idx]
  ev$process_group <- taxonomy$process_group[idx]

  out <- do.call(rbind, lapply(split(ev, ev$model), function(model_ev) {
    process_rates_for_model(model_ev, event_summary, events, n_bins)
  }))
  if (is.null(out) || nrow(out) == 0L) {
    return(empty)
  }
  out <- out[order(out$model, out$process_group, out$process_label, out$time_bin), , drop = FALSE]
  row.names(out) <- NULL
  out[, names(empty), drop = FALSE]
}

process_rates_for_model <- function(model_ev, event_summary, events, n_bins) {
  model <- as.character(model_ev$model[[1L]])
  max_time <- max(model_ev$event_time_before_present, na.rm = TRUE)
  if (!is.finite(max_time) || max_time <= 0) {
    return(NULL)
  }
  breaks <- seq(0, max_time, length.out = n_bins + 1L)
  bin_width <- max_time / n_bins
  model_ev$bin <- findInterval(
    model_ev$event_time_before_present, breaks,
    rightmost.closed = TRUE, all.inside = TRUE
  )

  n_maps <- region_n_maps(event_summary, events, model)
  replicates <- sort(unique(model_ev$replicate))
  if (is.na(n_maps) || n_maps < length(replicates)) {
    n_maps <- length(replicates)
  }

  processes <- unique(model_ev[, c("process_key", "process_label", "process_group")])
  bins <- seq_len(n_bins)

  do.call(rbind, lapply(seq_len(nrow(processes)), function(i) {
    proc_rows <- model_ev[model_ev$process_key == processes$process_key[[i]], , drop = FALSE]
    do.call(rbind, lapply(bins, function(b) {
      in_bin <- proc_rows[proc_rows$bin == b, , drop = FALSE]
      per_map <- rep(0, n_maps)
      if (nrow(in_bin) > 0L) {
        map_counts <- table(factor(in_bin$replicate, levels = replicates))
        length(map_counts) <- n_maps
        map_counts[is.na(map_counts)] <- 0
        per_map <- as.numeric(map_counts)
      }
      mean_count <- mean(per_map)
      sd_count <- if (length(per_map) > 1L) stats::sd(per_map) else 0
      ci <- rate_count_ci(per_map)
      data.frame(
        model = model,
        process_key = processes$process_key[[i]],
        process_label = processes$process_label[[i]],
        process_group = processes$process_group[[i]],
        time_bin = b,
        bin_start = breaks[[b]],
        bin_end = breaks[[b + 1L]],
        bin_midpoint = (breaks[[b]] + breaks[[b + 1L]]) / 2,
        mean_count = mean_count,
        sd_count = sd_count,
        ci_lower = ci[[1L]],
        ci_upper = ci[[2L]],
        rate = if (bin_width > 0) mean_count / bin_width else NA_real_,
        interpretation_note = "Mean BioGeoBEARS stochastic mapping event count per map in the time bin.",
        stringsAsFactors = FALSE
      )
    }))
  }))
}

#' Plot biogeographic process rates through time
#'
#' Draw the mean event count per stochastic map through time for each
#' biogeographic process, with a standard-deviation ribbon, split into
#' cladogenetic and anagenetic process groups.
#'
#' @param process_rates A data frame from
#'   [summarize_process_rates_through_time()].
#' @return A ggplot object.
#' @export
plot_process_rates_through_time <- function(process_rates) {
  required <- c("model", "process_label", "process_group", "bin_midpoint", "mean_count")
  missing <- setdiff(required, names(process_rates))
  if (length(missing) > 0L) {
    stop("process_rates is missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (nrow(process_rates) == 0L) {
    stop("process_rates must contain at least one row.", call. = FALSE)
  }
  process_rates$sd_count <- suppressWarnings(as.numeric(process_rates$sd_count %||% rep(NA_real_, nrow(process_rates))))
  group_labels <- c(cladogenetic = "Cladogenetic (speciation mode)", anagenetic = "Anagenetic (range evolution)")
  process_rates$group_label <- group_labels[process_rates$process_group]
  process_rates$group_label[is.na(process_rates$group_label)] <- process_rates$process_group[is.na(process_rates$group_label)]

  facets <- if (length(unique(process_rates$model)) > 1L) {
    ggplot2::facet_grid(stats::as.formula("group_label ~ model"))
  } else {
    ggplot2::facet_wrap(stats::as.formula("~ group_label"), ncol = 1L)
  }

  ggplot2::ggplot(process_rates, ggplot2::aes(x = bin_midpoint, y = mean_count, colour = process_label)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = pmax(0, mean_count - sd_count), ymax = mean_count + sd_count, fill = process_label),
      alpha = 0.12, colour = NA
    ) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 1.4) +
    ggplot2::scale_x_reverse() +
    scale_colour_bgs() +
    scale_fill_bgs() +
    facets +
    ggplot2::labs(
      x = "Time before present",
      y = "Mean events per stochastic map",
      colour = NULL, fill = NULL,
      title = "Biogeographic process rates through time",
      subtitle = "Mean BSM event counts per time bin, with standard-deviation ribbons"
    ) +
    theme_bgs() +
    ggplot2::guides(fill = "none")
}

#' Summarize region-resolved biogeographic process rates through time
#'
#' Like [summarize_process_rates_through_time()], but additionally split by the
#' region each event is attributed to, so the timing of a process can be compared
#' across regions. Each event is attributed to the region it gained (dispersal
#' target), lost (local extinction), or colonized (founder-event target). Events
#' whose sampled record has no such region (narrow sympatry, subset sympatry, and
#' vicariance, which are not tied to a single dispersal/extinction region) are
#' omitted.
#'
#' @param bsm_tables A list of standardized BSM tables containing at least
#'   `bsm_events`, and optionally `bsm_event_summary`.
#' @param n_bins Number of equal-width time bins between the present and the
#'   oldest sampled event.
#' @return A data frame with one row per model, process, region, and time bin.
#'   Returns an empty table with the same columns when no region-attributable
#'   timed events are available.
#' @export
summarize_region_process_rates_through_time <- function(bsm_tables, n_bins = 10L) {
  empty <- empty_region_process_rates_table()
  bsm_tables <- bsm_tables %||% list()
  events <- bsm_tables$bsm_events %||% NULL
  event_summary <- bsm_tables$bsm_event_summary %||% NULL
  if (is.null(events) || nrow(events) == 0L) {
    return(empty)
  }
  needed <- c("model", "replicate", "event_type", "event_time_before_present")
  if (!all(needed %in% names(events))) {
    return(empty)
  }
  n_bins <- max(1L, as.integer(n_bins))

  ev <- attribute_region_events(events)
  ev <- ev[
    !is.na(ev$event_time_before_present) &
      !is.na(ev$event_region) & nzchar(ev$event_region),
    ,
    drop = FALSE
  ]
  if (is.null(ev) || nrow(ev) == 0L) {
    return(empty)
  }

  out <- do.call(rbind, lapply(split(ev, ev$model), function(model_ev) {
    region_process_rates_for_model(model_ev, event_summary, events, n_bins)
  }))
  if (is.null(out) || nrow(out) == 0L) {
    return(empty)
  }
  out <- out[order(out$model, out$process_group, out$process_label, out$region, out$time_bin), , drop = FALSE]
  row.names(out) <- NULL
  out[, names(empty), drop = FALSE]
}

rate_count_ci <- function(per_map) {
  if (length(per_map) == 0L) {
    return(c(0, 0))
  }
  as.numeric(stats::quantile(per_map, c(0.025, 0.975), names = FALSE, type = 7))
}

coalesce_region <- function(...) {
  cols <- list(...)
  out <- rep(NA_character_, length(cols[[1L]]))
  for (col in cols) {
    col <- as.character(col)
    take <- (is.na(out) | !nzchar(out)) & !is.na(col) & nzchar(col)
    out[take] <- col[take]
  }
  out
}

# Attribute BSM events to regions for the per-region rates. Range-expansion
# dispersal (`d`) counts as immigration for its target region and emigration for
# its source region; in-situ (sympatric) speciation (`sympatry`/`y`) counts for
# the single ancestral area. These are the three region-level event categories.
attribute_region_events <- function(events) {
  build <- function(sub, region, process_key, process_label, process_group) {
    region <- as.character(region)
    keep <- !is.na(region) & nzchar(region)
    if (!any(keep)) {
      return(NULL)
    }
    data.frame(
      model = as.character(sub$model)[keep],
      replicate = sub$replicate[keep],
      process_key = process_key,
      process_label = process_label,
      process_group = process_group,
      event_region = region[keep],
      event_time_before_present = sub$event_time_before_present[keep],
      stringsAsFactors = FALSE
    )
  }

  pieces <- list()
  disp <- events[!is.na(events$event_type) & events$event_type == "d", , drop = FALSE]
  if (nrow(disp) > 0L) {
    pieces$immigration <- build(
      disp, disp$target_region %||% rep(NA_character_, nrow(disp)),
      "immigration", "Immigration", "anagenetic"
    )
    pieces$emigration <- build(
      disp, disp$source_region %||% rep(NA_character_, nrow(disp)),
      "emigration", "Emigration", "anagenetic"
    )
  }
  insitu <- events[!is.na(events$event_type) & events$event_type == "sympatry", , drop = FALSE]
  if (nrow(insitu) > 0L) {
    code_to_name <- region_code_name_map(events)
    area_code <- as.character(insitu$parent_state %||% rep(NA_character_, nrow(insitu)))
    region <- unname(code_to_name[area_code])
    region[is.na(region)] <- area_code[is.na(region)]
    pieces$in_situ <- build(insitu, region, "in_situ_speciation", "In-situ speciation", "cladogenetic")
  }
  pieces <- pieces[!vapply(pieces, is.null, logical(1))]
  if (length(pieces) == 0L) {
    return(NULL)
  }
  do.call(rbind, pieces)
}

# Map single-area region codes (e.g. "B") to region names (e.g. "Region B")
# using the source/target region pairs recorded on anagenetic dispersal events.
region_code_name_map <- function(events) {
  pair <- function(code_col, name_col) {
    if (!all(c(code_col, name_col) %in% names(events))) {
      return(character())
    }
    code <- as.character(events[[code_col]])
    name <- as.character(events[[name_col]])
    ok <- !is.na(code) & nzchar(code) & !is.na(name) & nzchar(name)
    stats::setNames(name[ok], code[ok])
  }
  m <- c(pair("source_region_code", "source_region"), pair("target_region_code", "target_region"))
  m[!duplicated(names(m))]
}

region_process_rates_for_model <- function(model_ev, event_summary, events, n_bins) {
  model <- as.character(model_ev$model[[1L]])
  max_time <- max(model_ev$event_time_before_present, na.rm = TRUE)
  if (!is.finite(max_time) || max_time <= 0) {
    return(NULL)
  }
  breaks <- seq(0, max_time, length.out = n_bins + 1L)
  bin_width <- max_time / n_bins
  model_ev$bin <- findInterval(
    model_ev$event_time_before_present, breaks,
    rightmost.closed = TRUE, all.inside = TRUE
  )

  n_maps <- region_n_maps(event_summary, events, model)
  replicates <- sort(unique(model_ev$replicate))
  if (is.na(n_maps) || n_maps < length(replicates)) {
    n_maps <- length(replicates)
  }

  combos <- unique(model_ev[, c("process_key", "process_label", "process_group", "event_region")])
  bins <- seq_len(n_bins)

  do.call(rbind, lapply(seq_len(nrow(combos)), function(i) {
    sub <- model_ev[
      model_ev$process_key == combos$process_key[[i]] &
        model_ev$event_region == combos$event_region[[i]],
      ,
      drop = FALSE
    ]
    do.call(rbind, lapply(bins, function(b) {
      in_bin <- sub[sub$bin == b, , drop = FALSE]
      per_map <- rep(0, n_maps)
      if (nrow(in_bin) > 0L) {
        map_counts <- table(factor(in_bin$replicate, levels = replicates))
        length(map_counts) <- n_maps
        map_counts[is.na(map_counts)] <- 0
        per_map <- as.numeric(map_counts)
      }
      mean_count <- mean(per_map)
      ci <- rate_count_ci(per_map)
      data.frame(
        model = model,
        process_key = combos$process_key[[i]],
        process_label = combos$process_label[[i]],
        process_group = combos$process_group[[i]],
        region = combos$event_region[[i]],
        time_bin = b,
        bin_start = breaks[[b]],
        bin_end = breaks[[b + 1L]],
        bin_midpoint = (breaks[[b]] + breaks[[b + 1L]]) / 2,
        mean_count = mean_count,
        sd_count = if (length(per_map) > 1L) stats::sd(per_map) else 0,
        ci_lower = ci[[1L]],
        ci_upper = ci[[2L]],
        rate = if (bin_width > 0) mean_count / bin_width else NA_real_,
        interpretation_note = "Mean BSM event count per map in the time bin, attributed to the region gained, lost, or colonized.",
        stringsAsFactors = FALSE
      )
    }))
  }))
}

#' Plot region-resolved process rates through time
#'
#' Draw the mean event count per stochastic map through time for each region,
#' one panel per biogeographic process, so the regional timing of each process
#' is read directly.
#'
#' @param region_process_rates A data frame from
#'   [summarize_region_process_rates_through_time()].
#' @param process Optional process key(s) or label(s) to restrict the plot to.
#' @return A ggplot object.
#' @export
plot_region_process_rates_through_time <- function(region_process_rates, process = NULL) {
  required <- c("model", "process_label", "region", "bin_midpoint", "mean_count")
  missing <- setdiff(required, names(region_process_rates))
  if (length(missing) > 0L) {
    stop("region_process_rates is missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  data <- region_process_rates
  if (!is.null(process)) {
    data <- data[data$process_label %in% process | data$process_key %in% process, , drop = FALSE]
  }
  if (nrow(data) == 0L) {
    stop("region_process_rates has no rows to plot.", call. = FALSE)
  }

  facets <- if (length(unique(data$model)) > 1L) {
    ggplot2::facet_grid(stats::as.formula("process_label ~ model"), scales = "free_y")
  } else {
    ggplot2::facet_wrap(stats::as.formula("~ process_label"), scales = "free_y")
  }

  ggplot2::ggplot(data, ggplot2::aes(x = bin_midpoint, y = mean_count, colour = region)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 1.4) +
    ggplot2::scale_x_reverse() +
    scale_colour_bgs() +
    facets +
    ggplot2::labs(
      x = "Time before present",
      y = "Mean events per stochastic map",
      colour = "Region",
      title = "Region-resolved process rates through time",
      subtitle = "One panel per process; each region shows the timing of events it gained, lost, or was colonized by"
    ) +
    theme_bgs()
}

empty_region_process_rates_table <- function() {
  data.frame(
    model = character(),
    process_key = character(),
    process_label = character(),
    process_group = character(),
    region = character(),
    time_bin = integer(),
    bin_start = numeric(),
    bin_end = numeric(),
    bin_midpoint = numeric(),
    mean_count = numeric(),
    sd_count = numeric(),
    ci_lower = numeric(),
    ci_upper = numeric(),
    rate = numeric(),
    interpretation_note = character(),
    stringsAsFactors = FALSE
  )
}

empty_process_rates_table <- function() {
  data.frame(
    model = character(),
    process_key = character(),
    process_label = character(),
    process_group = character(),
    time_bin = integer(),
    bin_start = numeric(),
    bin_end = numeric(),
    bin_midpoint = numeric(),
    mean_count = numeric(),
    sd_count = numeric(),
    ci_lower = numeric(),
    ci_upper = numeric(),
    rate = numeric(),
    interpretation_note = character(),
    stringsAsFactors = FALSE
  )
}
