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
    scale_colour_ibgb() +
    ggplot2::facet_wrap(stats::as.formula("~ process_label"), scales = "free_y") +
    ggplot2::labs(
      x = "Time before present",
      y = "Mean events per stochastic map",
      colour = "Clade", fill = "Clade",
      title = "Cross-clade process rates through time",
      subtitle = subtitle
    ) +
    theme_ibgb()
  if (has_ci) {
    plot <- plot + scale_fill_ibgb() + ggplot2::guides(fill = "none")
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

#' Plot region-resolved process rates through time across clades
#'
#' Draw region-resolved process rates through time with one panel per
#' biogeographic process and one coloured curve per region, pooled across all
#' clades, from a table produced by
#' [combine_region_process_rates_across_clades()]. This is the region-resolved
#' counterpart of [plot_process_rates_across_clades()]: clades are summed
#' together (not shown separately), and within each process panel each region is
#' a separate curve.
#'
#' @param combined_region_rates A data frame from
#'   [combine_region_process_rates_across_clades()].
#' @param process Optional process key(s) or label(s) to restrict the plot to.
#' @return A ggplot object.
#' @export
plot_region_process_rates_across_clades <- function(combined_region_rates, process = NULL) {
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
  if (nrow(data) == 0L) {
    stop("combined_region_rates has no rows to plot.", call. = FALSE)
  }

  # Pool across clades: one curve per region within each process panel.
  data$mean_count <- suppressWarnings(as.numeric(data$mean_count))
  pooled <- stats::aggregate(
    list(mean_count = data$mean_count),
    by = list(
      process_label = data$process_label,
      region = data$region,
      bin_midpoint = data$bin_midpoint
    ),
    FUN = function(x) sum(x, na.rm = TRUE)
  )

  ggplot2::ggplot(pooled, ggplot2::aes(x = bin_midpoint, y = mean_count, colour = region)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 1.3) +
    ggplot2::scale_x_reverse() +
    scale_colour_ibgb() +
    ggplot2::facet_wrap(stats::as.formula("~ process_label"), scales = "free_y") +
    ggplot2::labs(
      x = "Time before present",
      y = "Mean events per stochastic map (summed across clades)",
      colour = "Region",
      title = "Cross-clade region-resolved process rates through time",
      subtitle = "One panel per process; each region a curve, pooled across all clades"
    ) +
    theme_ibgb()
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

# Render a self-contained HTML report of the cross-clade integrated results
# (overall and/or region-resolved), with figures embedded as data URIs so the
# file is portable. Returns the path to the written HTML file.
render_cross_clade_report <- function(overall_combined = NULL,
                                      region_combined = NULL,
                                      file = NULL) {
  if (is.null(file)) {
    file <- tempfile("cross_clade_report_", fileext = ".html")
  }
  has_overall <- !is.null(overall_combined) && nrow(overall_combined) > 0L
  has_region <- !is.null(region_combined) && nrow(region_combined) > 0L
  if (!has_overall && !has_region) {
    stop("No cross-clade results to report; upload clade rate files first.", call. = FALSE)
  }

  embed_plot <- function(plot, width, height) {
    png <- tempfile(fileext = ".png")
    on.exit(unlink(png), add = TRUE)
    ggplot2::ggsave(png, plot, width = width, height = height, dpi = 150)
    uri <- base64enc::dataURI(file = png, mime = "image/png")
    sprintf('<img alt="figure" src="%s" style="max-width:100%%;height:auto;" />', uri)
  }
  html_escape <- function(x) {
    x <- as.character(x)
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    gsub("<", "&lt;", x, fixed = TRUE)
  }
  html_table <- function(df) {
    header <- paste0("<th>", html_escape(names(df)), "</th>", collapse = "")
    rows <- vapply(seq_len(nrow(df)), function(i) {
      paste0("<td>", html_escape(unlist(df[i, , drop = TRUE])), "</td>", collapse = "")
    }, character(1))
    paste0(
      "<table><thead><tr>", header, "</tr></thead><tbody><tr>",
      paste(rows, collapse = "</tr><tr>"), "</tr></tbody></table>"
    )
  }

  parts <- c(
    "<!doctype html>", "<html lang=\"en\">", "<head>", "<meta charset=\"utf-8\">",
    "<title>Cross-clade synthesis report</title>",
    paste0(
      "<style>body{font-family:Arial,Helvetica,sans-serif;max-width:940px;",
      "margin:24px auto;padding:0 16px;color:#1f2937;line-height:1.5}",
      "h1{font-size:22px}h2{font-size:17px;margin-top:30px;",
      "border-bottom:1px solid #e5e7eb;padding-bottom:4px}",
      ".note{color:#6b7280;font-size:13px}table{border-collapse:collapse;",
      "font-size:12px;margin-top:10px}th,td{border:1px solid #d1d5db;",
      "padding:3px 7px;text-align:left}th{background:#f3f4f6}</style>"
    ),
    "</head>", "<body>",
    "<h1>Cross-clade synthesis of biogeographic events</h1>",
    sprintf(
      paste0(
        "<p class=\"note\">Generated by iBiogeobears on %s. Curves show mean ",
        "events per stochastic map; bands are the 95%% interval (2.5-97.5%% ",
        "percentiles) across maps. Clades must share comparable time units and ",
        "region definitions; full tables are in the accompanying CSV downloads.</p>"
      ),
      format(Sys.time(), "%Y-%m-%d %H:%M")
    )
  )

  if (has_overall) {
    n <- length(unique(overall_combined$clade))
    parts <- c(
      parts,
      "<h2>Overall process rates through time</h2>",
      sprintf("<p class=\"note\">%d clades integrated.</p>", n),
      tryCatch(
        embed_plot(plot_process_rates_across_clades(overall_combined), 8.6, 5.2),
        error = function(e) sprintf("<p class=\"note\">Figure unavailable: %s</p>", conditionMessage(e))
      ),
      "<h3>Combined rates per clade and time bin</h3>",
      html_table(xclade_long_table(
        overall_combined,
        c("clade", "process_group", "process_label", "bin_midpoint", "mean_count", "ci_lower", "ci_upper")
      ))
    )
    summ <- xclade_process_summary(overall_combined)
    if (!is.null(summ)) {
      parts <- c(parts, "<h3>Summed mean events per process (all clades pooled)</h3>", html_table(summ))
    }
  }

  if (has_region) {
    parts <- c(
      parts,
      "<h2>Region-resolved process rates through time</h2>",
      tryCatch(
        embed_plot(plot_region_process_rates_across_clades(region_combined), 9, 6),
        error = function(e) sprintf("<p class=\"note\">Figure unavailable: %s</p>", conditionMessage(e))
      ),
      "<h3>Combined rates per clade, region and time bin</h3>",
      html_table(xclade_long_table(
        region_combined,
        c("clade", "region", "process_label", "bin_midpoint", "mean_count", "ci_lower", "ci_upper")
      ))
    )
    summ <- xclade_region_summary(region_combined)
    if (!is.null(summ)) {
      parts <- c(parts, "<h3>Summed mean events per region and process (all clades pooled)</h3>", html_table(summ))
    }
  }

  parts <- c(parts, "</body>", "</html>")
  writeLines(paste(parts, collapse = "\n"), file)
  invisible(file)
}
