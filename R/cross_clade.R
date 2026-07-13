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

  ggplot2::ggplot(data, ggplot2::aes(x = bin_midpoint, y = mean_count, colour = clade)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 1.3) +
    ggplot2::scale_x_reverse() +
    scale_colour_ibgb() +
    ggplot2::facet_wrap(stats::as.formula("~ process_label"), scales = "free_y") +
    ggplot2::labs(
      x = "Time before present",
      y = "Mean events per stochastic map",
      colour = "Clade",
      title = "Cross-clade process rates through time",
      subtitle = "One panel per process; each clade shown as a separate curve"
    ) +
    theme_ibgb()
}

empty_combined_rates_table <- function() {
  tbl <- empty_process_rates_table()
  cbind(clade = character(), tbl)
}
