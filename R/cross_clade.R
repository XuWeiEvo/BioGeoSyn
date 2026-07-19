#' Combine process rates through time across clades
#'
#' Read several single-clade `process_rates_through_time.csv` files (one per
#' analysed clade) and combine them into one tidy table with a `clade` column,
#' so biogeographic process rates through time can be compared across clades.
#'
#' Upload or supply the `process_rates_through_time.csv` file written to each
#' clade's `tables/` directory. Times are interpreted on a shared
#' before-present axis, so the clades must use comparable time units.
#'
#' @param files Character vector of paths to `process_rates_through_time.csv`
#'   files from different clade analyses.
#' @param clade_names Optional character vector of clade labels, one per file.
#'   When omitted, each label is taken from the file name (without extension),
#'   falling back to the analysis directory name.
#' @return A data frame combining the inputs with an added `clade` column.
#'   Returns an empty table when no readable input files are supplied.
#' @export
combine_process_rates_across_clades <- function(files, clade_names = NULL) {
  empty <- empty_combined_rates_table()
  files <- as.character(files %||% character())
  if (length(files) == 0L) {
    return(empty)
  }
  if (!is.null(clade_names)) {
    clade_names <- as.character(clade_names)
  }

  pieces <- lapply(seq_along(files), function(i) {
    file <- files[[i]]
    if (is.na(file) || !nzchar(file) || !file.exists(file)) {
      return(NULL)
    }
    df <- tryCatch(utils::read.csv(file, stringsAsFactors = FALSE), error = function(e) NULL)
    if (is.null(df) || nrow(df) == 0L) {
      return(NULL)
    }
    required <- c("process_label", "bin_midpoint", "mean_count")
    if (!all(required %in% names(df))) {
      return(NULL)
    }
    df <- keep_first_model(df)
    clade <- if (!is.null(clade_names) && length(clade_names) >= i && !is.na(clade_names[[i]]) && nzchar(clade_names[[i]])) {
      clade_names[[i]]
    } else {
      derive_clade_name(file)
    }
    df$clade <- clade
    df
  })
  pieces <- pieces[!vapply(pieces, is.null, logical(1))]
  if (length(pieces) == 0L) {
    return(empty)
  }

  # Disambiguate duplicate clade labels (e.g. unrenamed files with the same name).
  labels <- vapply(pieces, function(d) d$clade[[1L]], character(1))
  labels <- make.unique(labels, sep = " ")
  pieces <- Map(function(d, lab) {
    d$clade <- lab
    d
  }, pieces, labels)

  common <- Reduce(intersect, lapply(pieces, names))
  common <- c("clade", setdiff(common, "clade"))
  out <- do.call(rbind, lapply(pieces, function(d) d[, common, drop = FALSE]))
  sort_cols <- intersect(c("clade", "process_group", "process_label", "time_bin"), names(out))
  out <- out[do.call(order, out[sort_cols]), , drop = FALSE]
  row.names(out) <- NULL
  out
}

derive_clade_name <- function(file) {
  base <- tools::file_path_sans_ext(basename(file))
  generic <- c("process_rates_through_time", "region_process_rates_through_time")
  if (!nzchar(base) || base %in% generic) {
    # <root>/tables/process_rates_through_time.csv -> use the <root> directory.
    root <- basename(dirname(dirname(file)))
    if (nzchar(root) && root != "." && root != "tables") {
      return(root)
    }
  }
  if (!nzchar(base)) "clade" else base
}

# Cross-clade comparison uses a single model per clade. If an uploaded rate file
# contains several models (e.g. BSM was run on all models), keep only the first
# model so each clade shows one curve instead of overlapping models.
keep_first_model <- function(df) {
  if ("model" %in% names(df) && length(unique(df$model)) > 1L) {
    df <- df[df$model == df$model[[1L]], , drop = FALSE]
  }
  df
}

# ---- Cross-clade integration from per-clade result-bundle zip archives ----

# Read one table (tables/<name>.csv) out of a result-bundle zip archive.
read_bundle_table <- function(zip_path, table) {
  if (is.null(zip_path) || !nzchar(zip_path) || !file.exists(zip_path)) {
    return(NULL)
  }
  entries <- tryCatch(utils::unzip(zip_path, list = TRUE)$Name, error = function(e) character())
  if (length(entries) == 0L) {
    return(NULL)
  }
  want <- paste0(table, ".csv")
  cand <- entries[basename(entries) == want]
  if (length(cand) == 0L) {
    return(NULL)
  }
  pref <- cand[grepl("tables/", cand, fixed = TRUE)]
  target <- if (length(pref) > 0L) pref[[1L]] else cand[[1L]]
  tmp <- tempfile("bgs-bundle-")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  ok <- tryCatch({
    utils::unzip(zip_path, files = target, exdir = tmp)
    TRUE
  }, error = function(e) FALSE)
  if (!isTRUE(ok)) {
    return(NULL)
  }
  f <- file.path(tmp, target)
  if (!file.exists(f)) {
    return(NULL)
  }
  tryCatch(utils::read.csv(f, stringsAsFactors = FALSE), error = function(e) NULL)
}

# The cross-clade integration is built entirely from BSM stochastic-mapping
# tables. A bundle from a run without BSM has none of them, so report which
# uploaded bundles are missing them - that is the usual reason integration is
# empty, and the raw "nothing to report" error does not make it clear.
bundle_has_cross_clade_data <- function(zip_path) {
  if (is.null(zip_path) || !nzchar(zip_path) || !file.exists(zip_path)) {
    return(FALSE)
  }
  entries <- tryCatch(basename(utils::unzip(zip_path, list = TRUE)$Name),
                      error = function(e) character())
  markers <- c("biogeographic_process_summary.csv", "process_rates_through_time.csv",
               "bsm_event_summary.csv", "bsm_dispersal_routes.csv")
  any(markers %in% entries)
}

# Names of the uploaded bundles that carry no cross-clade (BSM) tables.
bundles_missing_cross_clade_data <- function(zip_paths, clade_names = NULL) {
  zip_paths <- as.character(zip_paths %||% character())
  if (length(zip_paths) == 0L) {
    return(character())
  }
  clade_names <- as.character(clade_names %||% rep(NA_character_, length(zip_paths)))
  missing <- !vapply(zip_paths, bundle_has_cross_clade_data, logical(1))
  labels <- vapply(seq_along(zip_paths), function(i) {
    if (length(clade_names) >= i && !is.na(clade_names[[i]]) && nzchar(clade_names[[i]])) {
      clade_names[[i]]
    } else {
      derive_clade_name(zip_paths[[i]])
    }
  }, character(1))
  labels[missing]
}

# Read one table from several bundles, keep a single model per clade, tag clade.
read_clade_bundle_tables <- function(zip_paths, clade_names, table) {
  zip_paths <- as.character(zip_paths %||% character())
  if (length(zip_paths) == 0L) {
    return(NULL)
  }
  clade_names <- as.character(clade_names %||% rep(NA_character_, length(zip_paths)))
  pieces <- lapply(seq_along(zip_paths), function(i) {
    df <- read_bundle_table(zip_paths[[i]], table)
    if (is.null(df) || nrow(df) == 0L) {
      return(NULL)
    }
    df <- keep_first_model(df)
    lab <- if (length(clade_names) >= i && !is.na(clade_names[[i]]) && nzchar(clade_names[[i]])) {
      clade_names[[i]]
    } else {
      derive_clade_name(zip_paths[[i]])
    }
    df$clade <- lab
    df
  })
  pieces <- pieces[!vapply(pieces, is.null, logical(1))]
  if (length(pieces) == 0L) {
    return(NULL)
  }
  labels <- make.unique(vapply(pieces, function(d) d$clade[[1L]], character(1)), sep = " ")
  pieces <- Map(function(d, l) {
    d$clade <- l
    d
  }, pieces, labels)
  common <- Reduce(intersect, lapply(pieces, names))
  common <- c("clade", setdiff(common, "clade"))
  do.call(rbind, lapply(pieces, function(d) d[, common, drop = FALSE]))
}

# Sum a per-clade combined table across clades over grouping columns, giving one
# integrated table tagged model = "All clades" (so single-clade plots pool it).
aggregate_across_clades <- function(df, group_cols, sum_cols, label = "All clades") {
  if (is.null(df) || nrow(df) == 0L) {
    return(NULL)
  }
  group_cols <- intersect(group_cols, names(df))
  sum_cols <- intersect(sum_cols, names(df))
  if (length(group_cols) == 0L || length(sum_cols) == 0L) {
    return(NULL)
  }
  values <- df[, sum_cols, drop = FALSE]
  values[] <- lapply(values, function(x) suppressWarnings(as.numeric(x)))
  agg <- stats::aggregate(values, by = as.list(df[, group_cols, drop = FALSE]), FUN = function(x) sum(x, na.rm = TRUE))
  names(agg)[seq_along(group_cols)] <- group_cols
  agg$model <- label
  agg
}

combine_process_rates_from_bundles <- function(zip_paths, clade_names = NULL) {
  read_clade_bundle_tables(zip_paths, clade_names, "process_rates_through_time")
}

combine_region_rates_from_bundles <- function(zip_paths, clade_names = NULL) {
  read_clade_bundle_tables(zip_paths, clade_names, "region_process_rates_through_time")
}

combine_process_synthesis_across_clades <- function(zip_paths, clade_names = NULL) {
  df <- read_clade_bundle_tables(zip_paths, clade_names, "biogeographic_process_summary")
  agg <- aggregate_across_clades(df, c("process_group", "process_key", "process_label"), c("mean_count"))
  if (!is.null(agg)) {
    agg$sd_count <- NA_real_
  }
  agg
}

combine_dispersal_routes_across_clades <- function(zip_paths, clade_names = NULL) {
  df <- read_clade_bundle_tables(zip_paths, clade_names, "bsm_dispersal_routes")
  aggregate_across_clades(df, c("route_type", "source_region", "target_region"), c("mean_count"))
}

combine_region_budgets_across_clades <- function(zip_paths, clade_names = NULL) {
  df <- read_clade_bundle_tables(zip_paths, clade_names, "region_process_budgets")
  agg <- aggregate_across_clades(df, c("region"), c("immigration", "emigration", "local_extinction", "total_dispersal"))
  if (!is.null(agg)) {
    agg$net_dispersal_flux <- agg$immigration - agg$emigration
  }
  agg
}

combine_event_summary_across_clades <- function(zip_paths, clade_names = NULL) {
  df <- read_clade_bundle_tables(zip_paths, clade_names, "bsm_event_summary")
  aggregate_across_clades(df, c("event_type", "event_label"), c("mean_count"))
}

combine_event_times_across_clades <- function(zip_paths, clade_names = NULL) {
  df <- read_clade_bundle_tables(zip_paths, clade_names, "bsm_event_times")
  if (is.null(df) || nrow(df) == 0L) {
    return(NULL)
  }
  df$model <- "All clades"
  df
}

# Sum each clade's region exchange matrix (dispersal off-diagonal + in-situ
# diagonal) across clades. Per clade uses its own map count normalisation.
combine_exchange_matrix_across_clades <- function(zip_paths, clade_names = NULL) {
  zip_paths <- as.character(zip_paths %||% character())
  if (length(zip_paths) == 0L) {
    return(NULL)
  }
  clade_names <- as.character(clade_names %||% rep(NA_character_, length(zip_paths)))
  pieces <- lapply(seq_along(zip_paths), function(i) {
    routes <- keep_first_model(read_bundle_table(zip_paths[[i]], "bsm_dispersal_routes"))
    events <- keep_first_model(read_bundle_table(zip_paths[[i]], "bsm_events"))
    esum <- read_bundle_table(zip_paths[[i]], "bsm_event_summary")
    long <- summarize_region_exchange_matrix(list(
      bsm_dispersal_routes = routes, bsm_events = events, bsm_event_summary = esum
    ))
    if (is.null(long) || nrow(long) == 0L) {
      return(NULL)
    }
    lab <- if (!is.na(clade_names[[i]]) && nzchar(clade_names[[i]])) clade_names[[i]] else derive_clade_name(zip_paths[[i]])
    long$clade <- lab
    long
  })
  pieces <- pieces[!vapply(pieces, is.null, logical(1))]
  if (length(pieces) == 0L) {
    return(NULL)
  }
  keep <- c("clade", "source_region", "recipient_region", "kind", "mean_count")
  do.call(rbind, lapply(pieces, function(d) d[, intersect(keep, names(d)), drop = FALSE]))
}

#' Plot process rates through time across clades
#'
#' Draw process rates through time with one panel per biogeographic process and
#' one coloured curve per clade, from a table produced by
#' [combine_process_rates_across_clades()].
#'
#' @param combined_rates A data frame from
#'   [combine_process_rates_across_clades()].
#' @param process Optional process key(s) or label(s) to restrict the plot to.
#' @return A ggplot object.
#' @export
plot_process_rates_across_clades <- function(combined_rates, process = NULL) {
  required <- c("clade", "process_label", "bin_midpoint", "mean_count")
  missing <- setdiff(required, names(combined_rates))
  if (length(missing) > 0L) {
    stop("combined_rates is missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  data <- combined_rates
  if (!is.null(process)) {
    keep <- data$process_label %in% process
    if ("process_key" %in% names(data)) {
      keep <- keep | data$process_key %in% process
    }
    data <- data[keep, , drop = FALSE]
  }
  if (nrow(data) == 0L) {
    stop("combined_rates has no rows to plot.", call. = FALSE)
  }

  plot <- ggplot2::ggplot(data, ggplot2::aes(x = bin_midpoint, y = mean_count, colour = clade))
  has_ci <- all(c("ci_lower", "ci_upper") %in% names(data)) &&
    any(is.finite(data$ci_lower)) && any(is.finite(data$ci_upper))
  subtitle <- "One panel per process; each clade shown as a separate curve"
  if (has_ci) {
    plot <- plot + ggplot2::geom_ribbon(
      ggplot2::aes(ymin = ci_lower, ymax = ci_upper, fill = clade),
      alpha = 0.15, colour = NA
    )
    subtitle <- "One panel per process; curve = mean, band = 95% CI across stochastic maps"
  }
  plot <- plot +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 1.3) +
    ggplot2::scale_x_reverse() +
    scale_colour_bgs() +
    ggplot2::facet_wrap(stats::as.formula("~ process_label"), scales = "free_y") +
    ggplot2::labs(
      x = "Time before present",
      y = "Mean events per stochastic map",
      colour = "Clade", fill = "Clade",
      title = "Cross-clade process rates through time",
      subtitle = subtitle
    ) +
    theme_bgs()
  if (has_ci) {
    plot <- plot + scale_fill_bgs() + ggplot2::guides(fill = "none")
  }
  plot
}

#' Combine region-resolved process rates through time across clades
#'
#' Read several single-clade `region_process_rates_through_time.csv` files (one
#' per analysed clade) and combine them into one tidy table with a `clade`
#' column, so the regional timing of each biogeographic process can be compared
#' across clades.
#'
#' @param files Character vector of paths to
#'   `region_process_rates_through_time.csv` files from different clade analyses.
#' @param clade_names Optional character vector of clade labels, one per file.
#'   When omitted, each label is taken from the file name, falling back to the
#'   analysis directory name.
#' @return A data frame combining the inputs with an added `clade` column.
#'   Returns an empty table when no readable input files are supplied.
#' @export
combine_region_process_rates_across_clades <- function(files, clade_names = NULL) {
  empty <- empty_combined_region_rates_table()
  required <- c("region", "process_label", "bin_midpoint", "mean_count")
  combine_clade_rate_files(files, clade_names, required, empty)
}

combine_clade_rate_files <- function(files, clade_names, required, empty) {
  files <- as.character(files %||% character())
  if (length(files) == 0L) {
    return(empty)
  }
  if (!is.null(clade_names)) {
    clade_names <- as.character(clade_names)
  }

  pieces <- lapply(seq_along(files), function(i) {
    file <- files[[i]]
    if (is.na(file) || !nzchar(file) || !file.exists(file)) {
      return(NULL)
    }
    df <- tryCatch(utils::read.csv(file, stringsAsFactors = FALSE), error = function(e) NULL)
    if (is.null(df) || nrow(df) == 0L || !all(required %in% names(df))) {
      return(NULL)
    }
    df <- keep_first_model(df)
    clade <- if (!is.null(clade_names) && length(clade_names) >= i && !is.na(clade_names[[i]]) && nzchar(clade_names[[i]])) {
      clade_names[[i]]
    } else {
      derive_clade_name(file)
    }
    df$clade <- clade
    df
  })
  pieces <- pieces[!vapply(pieces, is.null, logical(1))]
  if (length(pieces) == 0L) {
    return(empty)
  }

  labels <- vapply(pieces, function(d) d$clade[[1L]], character(1))
  labels <- make.unique(labels, sep = " ")
  pieces <- Map(function(d, lab) {
    d$clade <- lab
    d
  }, pieces, labels)

  common <- Reduce(intersect, lapply(pieces, names))
  common <- c("clade", setdiff(common, "clade"))
  out <- do.call(rbind, lapply(pieces, function(d) d[, common, drop = FALSE]))
  sort_cols <- intersect(c("clade", "process_label", "region", "time_bin"), names(out))
  out <- out[do.call(order, out[sort_cols]), , drop = FALSE]
  row.names(out) <- NULL
  out
}

# Re-bin per-clade rate histograms onto a shared absolute-time grid before
# pooling. Every clade has its own time bins (each tree has a different root
# age), so summing by raw bin_midpoint pools non-aligned points and the curves
# degrade into vertical spikes. Distributing each clade bin's count across the
# common grid bins it overlaps (proportional to overlap, so totals are
# conserved) puts all clades on one axis and restores smooth curves - the same
# fixed-bin approach used in the source literature (e.g. 5-Ma bins).
rebin_rates_to_common_grid <- function(data, bin_width = 5,
                                        group_cols = c("region", "process_label"),
                                        value_col = "mean_count") {
  bin_width <- suppressWarnings(as.numeric(bin_width))
  if (length(bin_width) != 1L || !is.finite(bin_width) || bin_width <= 0) {
    bin_width <- 5
  }
  if (!all(c("bin_start", "bin_end") %in% names(data))) {
    data$bin_start <- data$bin_midpoint
    data$bin_end <- data$bin_midpoint
  }
  data[[value_col]] <- suppressWarnings(as.numeric(data[[value_col]]))
  data <- data[!is.na(data[[value_col]]) & !is.na(data$bin_start) & !is.na(data$bin_end), , drop = FALSE]
  if (nrow(data) == 0L) {
    return(data.frame())
  }
  max_age <- max(data$bin_end, data$bin_start, na.rm = TRUE)
  n_bins <- max(1L, ceiling((max_age + 1e-9) / bin_width))
  grid_starts <- seq(0, by = bin_width, length.out = n_bins)

  pieces <- lapply(seq_len(nrow(data)), function(i) {
    s <- min(data$bin_start[i], data$bin_end[i])
    e <- max(data$bin_start[i], data$bin_end[i])
    val <- data[[value_col]][i]
    if (e > s) {
      overlap <- pmax(0, pmin(e, grid_starts + bin_width) - pmax(s, grid_starts))
      frac <- overlap / (e - s)
    } else {
      frac <- as.numeric(grid_starts <= s & s < grid_starts + bin_width)
      if (sum(frac) == 0) frac[n_bins] <- 1
    }
    keep <- frac > 0
    if (!any(keep)) {
      return(NULL)
    }
    g <- data[i, group_cols, drop = FALSE]
    cbind(
      g[rep(1L, sum(keep)), , drop = FALSE],
      data.frame(grid_bin = which(keep), value = val * frac[keep]),
      row.names = NULL
    )
  })
  pieces <- pieces[!vapply(pieces, is.null, logical(1))]
  if (length(pieces) == 0L) {
    return(data.frame())
  }
  long <- do.call(rbind, pieces)
  agg <- stats::aggregate(
    list(mean_count = long$value),
    by = c(long[, group_cols, drop = FALSE], list(grid_bin = long$grid_bin)),
    FUN = function(x) sum(x, na.rm = TRUE)
  )
  agg$bin_midpoint <- (agg$grid_bin - 0.5) * bin_width
  agg
}

#' Plot region-resolved process rates through time across clades
#'
#' Draw region-resolved process rates through time with one panel per region
#' and, within each panel, one coloured curve per biogeographic process
#' (in-situ speciation, immigration, emigration), pooled across all clades.
#' Clades are placed on a shared absolute-time grid (`bin_width`-wide bins)
#' before pooling, so heterogeneous trees combine into clean curves rather than
#' spikes. This layout mirrors the region-through-time figures in the
#' multi-clade literature.
#'
#' @param combined_region_rates A data frame from
#'   [combine_region_process_rates_across_clades()].
#' @param process Optional process key(s) or label(s) to restrict the plot to.
#' @param regions Optional character vector of regions to include. When omitted,
#'   every region is shown; pass a subset to focus (e.g. drop the non-Asian
#'   continents).
#' @param bin_width Width, in time units (millions of years), of the shared
#'   time bins clades are pooled into. Defaults to 5.
#' @return A ggplot object.
#' @export
plot_region_process_rates_across_clades <- function(combined_region_rates, process = NULL,
                                                     regions = NULL, bin_width = 5) {
  required <- c("clade", "region", "process_label", "bin_midpoint", "mean_count")
  missing <- setdiff(required, names(combined_region_rates))
  if (length(missing) > 0L) {
    stop("combined_region_rates is missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  data <- combined_region_rates
  if (!is.null(process)) {
    keep <- data$process_label %in% process
    if ("process_key" %in% names(data)) {
      keep <- keep | data$process_key %in% process
    }
    data <- data[keep, , drop = FALSE]
  }
  if (!is.null(regions)) {
    data <- data[data$region %in% regions, , drop = FALSE]
  }
  if (nrow(data) == 0L) {
    stop("combined_region_rates has no rows to plot.", call. = FALSE)
  }

  pooled <- rebin_rates_to_common_grid(data, bin_width = bin_width,
                                       group_cols = c("region", "process_label"))
  if (nrow(pooled) == 0L) {
    stop("combined_region_rates has no rows to plot.", call. = FALSE)
  }

  ggplot2::ggplot(pooled, ggplot2::aes(x = bin_midpoint, y = mean_count, colour = process_label)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 1.1) +
    ggplot2::scale_x_reverse() +
    scale_colour_bgs() +
    ggplot2::facet_wrap(stats::as.formula("~ region"), scales = "free_y") +
    ggplot2::labs(
      x = "Time before present (Ma)",
      y = "Mean events per stochastic map (summed across clades)",
      colour = "Process",
      title = "Cross-clade region-resolved process rates through time",
      subtitle = sprintf("One panel per region; each process a curve, pooled across clades on %g-Ma bins", bin_width)
    ) +
    theme_bgs()
}

# Standard geological periods, oldest to youngest (Ma before present). Used to
# slice the dispersal network by geological time.
geological_periods <- function() {
  data.frame(
    period = c("Paleogene", "Neogene", "Quaternary"),
    from = c(66, 23.03, 2.58),
    to = c(23.03, 2.58, 0),
    stringsAsFactors = FALSE
  )
}

# Names offered in the network period selector; "Total" spans all time.
geological_period_choices <- function() {
  c("Total", geological_periods()$period)
}

# Figure subtitle describing a period's time span, e.g. "Neogene (23-2.58 Ma)".
period_network_subtitle <- function(period) {
  if (is.null(period) || identical(period, "Total")) {
    return("All dispersals, all time")
  }
  w <- bgs_period_window(period)
  if (!is.finite(w$from)) {
    return(period)
  }
  sprintf("%s (%g-%g Ma)", period, w$from, w$to)
}

# Time window [to, from) in Ma for a named period; "Total" (or anything
# unrecognised) means all time.
bgs_period_window <- function(period) {
  periods <- geological_periods()
  if (is.null(period) || !is.character(period) || !(period %in% periods$period)) {
    return(list(from = Inf, to = -Inf))
  }
  row <- periods[periods$period == period, , drop = FALSE]
  list(from = row$from[[1L]], to = row$to[[1L]])
}

# Build a dispersal-route table (as consumed by plot_bsm_dispersal_network) from
# the combined per-event table, optionally restricted to a time window
# [to, from] in Ma before present. Counts are normalised to a mean per
# stochastic map per clade (dividing by that clade's replicate count) and then
# summed across clades, matching how bsm_dispersal_routes is scaled.
dispersal_routes_from_event_times <- function(event_times, from = Inf, to = -Inf) {
  cols <- c("source_region", "target_region", "event_time_before_present")
  if (is.null(event_times) || nrow(event_times) == 0L || !all(cols %in% names(event_times))) {
    return(NULL)
  }
  d <- event_times
  d <- d[!is.na(d$source_region) & nzchar(d$source_region) &
           !is.na(d$target_region) & nzchar(d$target_region) &
           d$source_region != d$target_region, , drop = FALSE]
  if (nrow(d) == 0L) {
    return(NULL)
  }
  # Replicate count per clade must be taken over ALL of a clade's events, not
  # just those in the time window - otherwise a clade with events in only some
  # replicates within a window would be scaled up and periods would not sum to
  # the total.
  clade_all <- if ("clade" %in% names(d)) d$clade else rep("all", nrow(d))
  rep_all <- if ("replicate" %in% names(d)) d$replicate else rep(1L, nrow(d))
  n_rep <- tapply(rep_all, clade_all, function(x) length(unique(x)))

  # Half-open [to, from) so an event on a period boundary is counted once: the
  # older period is exclusive at its young edge, the younger inclusive.
  t <- suppressWarnings(as.numeric(d$event_time_before_present))
  keep <- !is.na(t) & t >= to & t < from
  d <- d[keep, , drop = FALSE]
  if (nrow(d) == 0L) {
    return(NULL)
  }
  clade_col <- if ("clade" %in% names(d)) d$clade else rep("all", nrow(d))
  d$per_map <- 1 / pmax(1L, n_rep[as.character(clade_col)])
  routes <- stats::aggregate(
    list(mean_count = d$per_map),
    by = list(source_region = d$source_region, target_region = d$target_region),
    FUN = function(x) sum(x, na.rm = TRUE)
  )
  routes$route_type <- "all_dispersal"
  routes$model <- "All clades"
  routes[, c("model", "route_type", "source_region", "target_region", "mean_count")]
}

empty_combined_rates_table <- function() {
  tbl <- empty_process_rates_table()
  cbind(clade = character(), tbl)
}

empty_combined_region_rates_table <- function() {
  tbl <- empty_region_process_rates_table()
  cbind(clade = character(), tbl)
}

# Summed mean events per process, pooled across all clades (overall summary).
xclade_process_summary <- function(df) {
  req <- c("process_label", "mean_count")
  if (is.null(df) || !all(req %in% names(df)) || nrow(df) == 0L) {
    return(NULL)
  }
  agg <- stats::aggregate(
    list(events = suppressWarnings(as.numeric(df$mean_count))),
    by = list(Process = df$process_label),
    FUN = function(x) round(sum(x, na.rm = TRUE), 2)
  )
  agg <- agg[order(-agg$events), , drop = FALSE]
  names(agg)[names(agg) == "events"] <- "Summed mean events (all clades)"
  row.names(agg) <- NULL
  agg
}

# Summed mean events per region and process, pooled across all clades.
xclade_region_summary <- function(df) {
  req <- c("region", "process_label", "mean_count")
  if (is.null(df) || !all(req %in% names(df)) || nrow(df) == 0L) {
    return(NULL)
  }
  agg <- stats::aggregate(
    list(events = suppressWarnings(as.numeric(df$mean_count))),
    by = list(Region = df$region, Process = df$process_label),
    FUN = function(x) round(sum(x, na.rm = TRUE), 2)
  )
  agg <- agg[order(agg$Region, -agg$events), , drop = FALSE]
  names(agg)[names(agg) == "events"] <- "Summed mean events (all clades)"
  row.names(agg) <- NULL
  agg
}

# Select and round the key columns of a combined rate table for the report's
# full (per-clade, per-time-bin) long table.
xclade_long_table <- function(df, cols) {
  keep <- intersect(cols, names(df))
  out <- df[, keep, drop = FALSE]
  num <- vapply(out, is.numeric, logical(1))
  out[num] <- lapply(out[num], function(x) round(x, 3))
  row.names(out) <- NULL
  out
}

# Render a self-contained HTML report of the cross-clade integrated results.
# `x` is a named list of the integrated pieces (any may be NULL/empty):
# rates, region_rates, synthesis, routes, budgets, event_summary, event_times,
# exchange_long. Figures are embedded as data URIs so the file is portable.
render_cross_clade_report <- function(x, file = NULL) {
  if (is.null(file)) {
    file <- tempfile("cross_clade_report_", fileext = ".html")
  }
  x <- x %||% list()
  has <- function(key) {
    d <- x[[key]]
    !is.null(d) && is.data.frame(d) && nrow(d) > 0L
  }
  keys <- c("synthesis", "rates", "region_rates", "exchange_long",
            "routes", "budgets", "event_times", "event_summary")
  if (!any(vapply(keys, has, logical(1)))) {
    stop(
      "No cross-clade results found in the uploaded bundles. The integrated ",
      "results are built from BSM stochastic-mapping outputs, so each bundle ",
      "must come from a single-clade run with \"Run BSM stochastic mapping\" ",
      "enabled. Re-run the clades with BSM on, then download and upload the new ",
      "bundles.",
      call. = FALSE
    )
  }

  embed_plot <- function(plot, width, height) {
    png <- tempfile(fileext = ".png")
    on.exit(unlink(png), add = TRUE)
    ggplot2::ggsave(png, plot, width = width, height = height, dpi = 150)
    uri <- base64enc::dataURI(file = png, mime = "image/png")
    sprintf('<img alt="figure" src="%s" style="max-width:100%%;height:auto;" />', uri)
  }
  figure <- function(expr, width, height) {
    tryCatch(embed_plot(expr, width, height),
      error = function(e) sprintf("<p class=\"note\">Figure unavailable: %s</p>", conditionMessage(e)))
  }
  html_escape <- function(v) {
    v <- as.character(v)
    v <- gsub("&", "&amp;", v, fixed = TRUE)
    gsub("<", "&lt;", v, fixed = TRUE)
  }
  html_table <- function(df) {
    if (is.null(df) || nrow(df) == 0L) {
      return("")
    }
    header <- paste0("<th>", html_escape(names(df)), "</th>", collapse = "")
    rows <- vapply(seq_len(nrow(df)), function(i) {
      paste0("<td>", html_escape(unlist(df[i, , drop = TRUE])), "</td>", collapse = "")
    }, character(1))
    paste0(
      "<table><thead><tr>", header, "</tr></thead><tbody><tr>",
      paste(rows, collapse = "</tr><tr>"), "</tr></tbody></table>"
    )
  }
  all_dispersal <- function(routes) {
    if ("route_type" %in% names(routes)) routes[routes$route_type == "all_dispersal", , drop = FALSE] else routes
  }

  parts <- c(
    "<!doctype html>", "<html lang=\"en\">", "<head>", "<meta charset=\"utf-8\">",
    "<title>Cross-clade synthesis report</title>",
    paste0(
      "<style>body{font-family:Arial,Helvetica,sans-serif;max-width:940px;",
      "margin:24px auto;padding:0 16px;color:#1f2937;line-height:1.5}",
      "h1{font-size:22px}h2{font-size:17px;margin-top:30px;",
      "border-bottom:1px solid #e5e7eb;padding-bottom:4px}h3{font-size:14px}",
      ".note{color:#6b7280;font-size:13px}table{border-collapse:collapse;",
      "font-size:12px;margin-top:10px}th,td{border:1px solid #d1d5db;",
      "padding:3px 7px;text-align:left}th{background:#f3f4f6}</style>"
    ),
    "</head>", "<body>",
    "<h1>Cross-clade synthesis of biogeographic events</h1>",
    sprintf(
      paste0(
        "<p class=\"note\">Generated by BioGeoSyn on %s. Counts are means per ",
        "BSM stochastic map, summed across clades. Each clade contributes a single ",
        "(best) model; clades must share comparable time units and region ",
        "definitions.</p>"
      ),
      format(Sys.time(), "%Y-%m-%d %H:%M")
    )
  )

  if (has("synthesis")) {
    parts <- c(parts, "<h2>Biogeographic process synthesis</h2>",
      figure(plot_biogeographic_process_synthesis(x$synthesis), 8, 4.8))
  }
  if (has("rates")) {
    parts <- c(parts, "<h2>Process rates through time (overall)</h2>",
      figure(plot_process_rates_across_clades(x$rates), 8.6, 5.2),
      "<h3>Combined rates per clade and time bin</h3>",
      html_table(xclade_long_table(x$rates,
        c("clade", "process_group", "process_label", "bin_midpoint", "mean_count", "ci_lower", "ci_upper"))),
      "<h3>Summed mean events per process (all clades pooled)</h3>",
      html_table(xclade_process_summary(x$rates)))
  }
  if (has("region_rates")) {
    parts <- c(parts, "<h2>Process rates through time (by region)</h2>",
      figure(plot_region_process_rates_across_clades(x$region_rates), 9, 4.8),
      "<h3>Combined rates per clade, region and time bin</h3>",
      html_table(xclade_long_table(x$region_rates,
        c("clade", "region", "process_label", "bin_midpoint", "mean_count", "ci_lower", "ci_upper"))),
      "<h3>Summed mean events per region and process (all clades pooled)</h3>",
      html_table(xclade_region_summary(x$region_rates)))
  }
  if (has("exchange_long")) {
    parts <- c(parts, "<h2>Source-to-recipient exchange matrix</h2>",
      "<p class=\"note\">Diagonal = in-situ speciation; off-diagonal = dispersal source (row) to recipient (column). Total (out) = emigration, Total (in) = immigration.</p>",
      html_table(format_region_exchange_matrix(x$exchange_long)))
  }
  if (has("event_times")) {
    # Per-period networks sliced from the event times, plus the all-time total.
    parts <- c(parts, "<h2>Dispersal network by geological period</h2>",
      "<p class=\"note\">Arrows run source to recipient; width is the mean number of dispersals per stochastic map, summed across clades. Periods partition the total (Paleogene 66-23, Neogene 23-2.58, Quaternary 2.58-0 Ma).</p>")
    for (period in geological_period_choices()) {
      w <- bgs_period_window(period)
      routes <- dispersal_routes_from_event_times(x$event_times, w$from, w$to)
      net_fig <- if (is.null(routes) || nrow(routes) == 0L) {
        "<p class=\"note\">No between-region dispersals in this period.</p>"
      } else {
        figure(plot_bsm_dispersal_network(routes, subtitle = period_network_subtitle(period)), 6.5, 5.5)
      }
      parts <- c(parts, sprintf("<h3>%s</h3>", html_escape(period)), net_fig)
    }
  } else if (has("routes")) {
    parts <- c(parts, "<h2>Dispersal network</h2>",
      figure(plot_bsm_dispersal_network(all_dispersal(x$routes)), 6.5, 5.5))
  }
  if (has("budgets")) {
    parts <- c(parts, "<h2>Regional dispersal budget (immigration / emigration)</h2>",
      figure(plot_region_process_budget(x$budgets), 7.5, 4.5))
  }
  if (has("event_summary")) {
    parts <- c(parts, "<h2>Event statistics</h2>",
      html_table(xclade_long_table(x$event_summary, c("event_type", "event_label", "mean_count"))))
  }

  parts <- c(parts, "</body>", "</html>")
  writeLines(paste(parts, collapse = "\n"), file)
  invisible(file)
}

# Bundle all integrated cross-clade tables (and key figures) into one zip.
write_cross_clade_full_bundle <- function(file, x) {
  x <- x %||% list()
  tmp <- tempfile("bgs-xclade-full-")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  written <- character()
  ok_df <- function(df) !is.null(df) && is.data.frame(df) && nrow(df) > 0L
  save_csv <- function(df, name) {
    if (ok_df(df)) {
      utils::write.csv(df, file.path(tmp, paste0(name, ".csv")), row.names = FALSE, na = "")
      written <<- c(written, paste0(name, ".csv"))
    }
  }
  save_fig <- function(plot_expr, name, width, height) {
    ok <- tryCatch({
      ggplot2::ggsave(file.path(tmp, paste0(name, ".png")), plot_expr, width = width, height = height, dpi = 300)
      TRUE
    }, error = function(e) FALSE)
    if (isTRUE(ok)) written <<- c(written, paste0(name, ".png"))
  }
  all_dispersal <- function(routes) {
    if ("route_type" %in% names(routes)) routes[routes$route_type == "all_dispersal", , drop = FALSE] else routes
  }

  save_csv(x$rates, "process_rates_through_time_combined")
  save_csv(x$region_rates, "region_process_rates_through_time_combined")
  save_csv(x$synthesis, "process_synthesis_combined")
  save_csv(x$routes, "dispersal_routes_combined")
  save_csv(x$budgets, "region_budgets_combined")
  save_csv(x$event_summary, "event_summary_combined")
  save_csv(x$event_times, "event_times_combined")
  if (ok_df(x$exchange_long)) {
    save_csv(x$exchange_long, "region_exchange_long")
    save_csv(format_region_exchange_matrix(x$exchange_long), "region_exchange_matrix")
  }

  if (ok_df(x$synthesis)) save_fig(plot_biogeographic_process_synthesis(x$synthesis), "process_synthesis", 8, 4.8)
  if (ok_df(x$rates)) save_fig(plot_process_rates_across_clades(x$rates), "process_rates_through_time", 8.6, 5.2)
  if (ok_df(x$region_rates)) save_fig(plot_region_process_rates_across_clades(x$region_rates), "region_process_rates_through_time", 9, 4.8)
  if (ok_df(x$event_times)) {
    # One network per geological period, plus the all-time total.
    for (period in geological_period_choices()) {
      w <- bgs_period_window(period)
      routes <- dispersal_routes_from_event_times(x$event_times, w$from, w$to)
      if (!is.null(routes) && nrow(routes) > 0L) {
        save_fig(plot_bsm_dispersal_network(routes, subtitle = period_network_subtitle(period)),
                 paste0("dispersal_network_", tolower(period)), 6.5, 5.5)
      }
    }
  } else if (ok_df(x$routes)) {
    save_fig(plot_bsm_dispersal_network(all_dispersal(x$routes)), "dispersal_network", 6.5, 5.5)
  }
  if (ok_df(x$budgets)) save_fig(plot_region_process_budget(x$budgets), "region_dispersal_budget", 7.5, 4.5)

  if (length(written) == 0L) {
    stop("No cross-clade results to bundle; upload clade bundles first.", call. = FALSE)
  }
  zip_relative_files(tmp, file, written)
  invisible(file)
}
