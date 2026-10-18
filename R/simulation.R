# ---- Simulated event histories and bin-width sensitivity ----
#
# The cross-clade synthesis (re-binning, pooling, weighting, uncertainty) works
# downstream of BioGeoBEARS, so it can be validated on event histories whose
# generating rates are known. simulate_event_histories() places events of one
# process along the branches of a tree as a Poisson process with a known
# per-lineage rate lambda(t), independently for each "stochastic map", and
# returns them in the form the pooling functions take.

#' Simulate event histories with a known per-lineage rate
#'
#' @param tree_nodes A `tree_nodes.csv`-style table (see [tree_lineage_myr()]),
#'   or an `ape` `phylo` object.
#' @param rate Per-lineage event rate (events per lineage-Myr): a number, or a
#'   function of time before present returning the rate (vectorised).
#' @param n_maps Number of independent realisations ("stochastic maps").
#' @param process Process label given to the events.
#' @param resolution Time step used to integrate a time-varying rate.
#' @param seed Optional integer seed.
#' @return A list with `events` (`replicate`, `process_label`,
#'   `event_time_before_present`), `n_maps`, `replicates` and `tree_nodes`,
#'   accepted by [pool_rates_by_map_resampling()] and
#'   [pool_per_lineage_rates()].
#' @export
simulate_event_histories <- function(tree_nodes, rate, n_maps = 50L, process = "Range expansion",
                                     resolution = 0.1, seed = NULL) {
  if (inherits(tree_nodes, "phylo")) {
    f <- tempfile(fileext = ".tre")
    on.exit(unlink(f), add = TRUE)
    ape::write.tree(tree_nodes, f)
    tree_nodes <- make_node_lookup(f)
  }
  rate_fun <- if (is.function(rate)) rate else local({
    r <- as.numeric(rate)
    function(t) rep(r, length(t))
  })
  if (!is.null(seed)) {
    old <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv()) else NULL
    on.exit(if (is.null(old)) suppressWarnings(rm(".Random.seed", envir = globalenv())) else assign(".Random.seed", old, envir = globalenv()), add = TRUE)
    set.seed(as.integer(seed))
  }
  br <- tree_branches(tree_nodes)
  root <- max(br$old)
  n_cells <- max(1L, ceiling(root / resolution))
  ov <- interval_bin_overlap(br$young, br$old, n_cells, resolution)
  mid <- (seq_len(n_cells) - 0.5) * resolution
  expct <- sweep(ov, 2L, pmax(0, rate_fun(mid)), `*`)
  total <- rowSums(expct)
  cum <- t(apply(expct, 1L, cumsum))
  if (n_cells == 1L) cum <- matrix(cum, ncol = 1L)
  starts <- (seq_len(n_cells) - 1L) * resolution

  rows <- vector("list", n_maps)
  for (m in seq_len(n_maps)) {
    k <- stats::rpois(length(total), total)
    if (sum(k) == 0L) next
    b <- rep(seq_along(k), k)
    u <- stats::runif(length(b)) * total[b]
    cell <- vapply(seq_along(b), function(i) findInterval(u[[i]], c(0, cum[b[[i]], ]), rightmost.closed = TRUE), integer(1))
    cell <- pmin(pmax(cell, 1L), n_cells)
    lo <- pmax(br$young[b], starts[cell])
    hi <- pmin(br$old[b], starts[cell] + resolution)
    rows[[m]] <- data.frame(replicate = m, process_label = process,
                            event_time_before_present = lo + stats::runif(length(b)) * pmax(0, hi - lo))
  }
  events <- do.call(rbind, rows[!vapply(rows, is.null, logical(1))])
  if (is.null(events)) {
    events <- data.frame(replicate = integer(), process_label = character(), event_time_before_present = numeric())
  }
  list(events = events, n_maps = as.integer(n_maps), replicates = seq_len(n_maps), tree_nodes = tree_nodes)
}

#' Expected event counts and rates under a known per-lineage rate
#'
#' The truth against which simulated pooled curves are compared: for each
#' shared time bin, the expected number of events (rate integrated over the
#' tree's branch time) and the corresponding per-lineage rate.
#'
#' @inheritParams simulate_event_histories
#' @param n_bins,bin_width Shared time grid.
#' @return A data frame with `grid_bin`, `bin_midpoint`, `lineage_myr`,
#'   `expected_count` and `true_rate`.
#' @export
expected_event_counts <- function(tree_nodes, rate, n_bins, bin_width, resolution = 0.05) {
  rate_fun <- if (is.function(rate)) rate else local({
    r <- as.numeric(rate)
    function(t) rep(r, length(t))
  })
  fine <- tree_lineage_myr(tree_nodes, ceiling(n_bins * bin_width / resolution), resolution)
  mid <- (seq_along(fine) - 0.5) * resolution
  e_fine <- fine * pmax(0, rate_fun(mid))
  bin <- pmin(n_bins, floor((mid) / bin_width) + 1L)
  lm <- tree_lineage_myr(tree_nodes, n_bins, bin_width)
  ec <- as.numeric(tapply(e_fine, factor(bin, levels = seq_len(n_bins)), sum))
  ec[is.na(ec)] <- 0
  data.frame(grid_bin = seq_len(n_bins), bin_midpoint = (seq_len(n_bins) - 0.5) * bin_width,
             lineage_myr = lm, expected_count = ec, true_rate = ifelse(lm > 0, ec / lm, NA_real_))
}

#' Sensitivity of pooled curves to the time-bin width
#'
#' Recompute pooled per-lineage rates (and counts) for several bin widths and
#' report, for each process and width, where the pooled rate peaks. Pulses
#' that move or vanish as the width changes should not be interpreted.
#'
#' @param clade_data As for [pool_per_lineage_rates()].
#' @param bin_widths Bin widths to compare.
#' @param weighting Which pooled rate to summarise for the peaks.
#' @param ... Passed to [pool_per_lineage_rates()] (e.g. `n_draws`, `by_region`).
#' @return A list with `rates` (all pooled tables, with a `bin_width` column)
#'   and `peaks` (one row per process and width: `peak_start`, `peak_end`,
#'   `peak_rate`, `peak_to_median`).
#' @export
bin_width_sensitivity <- function(clade_data, bin_widths = c(1, 2, 5, 10),
                                  weighting = c("clade", "lineage"), ...) {
  weighting <- match.arg(weighting)
  col <- if (weighting == "clade") "clade_rate" else "rate"
  rates <- do.call(rbind, lapply(bin_widths, function(w) {
    out <- pool_per_lineage_rates(clade_data, bin_width = w, ...)
    if (is.null(out)) return(NULL)
    out$bin_width <- w
    out
  }))
  if (is.null(rates)) return(NULL)
  key <- intersect(c("process_label", "region"), names(rates))
  peaks <- do.call(rbind, lapply(split(rates, rates[c(key, "bin_width")], drop = TRUE), function(d) {
    v <- d[[col]]
    if (all(is.na(v))) return(NULL)
    i <- which.max(v)
    out <- d[i, c(key, "bin_width"), drop = FALSE]
    out$peak_start <- d$bin_start[[i]]
    out$peak_end <- d$bin_end[[i]]
    out$peak_rate <- v[[i]]
    out$peak_to_median <- v[[i]] / stats::median(v, na.rm = TRUE)
    out
  }))
  row.names(peaks) <- NULL
  list(rates = rates, peaks = peaks[do.call(order, peaks[c(key, "bin_width")]), , drop = FALSE])
}

#' Plot pooled rates for several bin widths
#'
#' @param sensitivity A result of [bin_width_sensitivity()].
#' @param weighting `"clade"` or `"lineage"`.
#' @return A ggplot object.
#' @export
plot_bin_width_sensitivity <- function(sensitivity, weighting = c("clade", "lineage")) {
  weighting <- match.arg(weighting)
  d <- sensitivity$rates
  y <- if (weighting == "clade") "clade_rate" else "rate"
  d$y <- d[[y]]
  d <- d[!is.na(d$y), , drop = FALSE]
  d$width <- factor(paste0(d$bin_width, "-Ma bins"), levels = paste0(sort(unique(d$bin_width)), "-Ma bins"))
  ggplot2::ggplot(d, ggplot2::aes(x = bin_midpoint, y = y, colour = width)) +
    ggplot2::geom_line(linewidth = 0.7) + ggplot2::geom_point(size = 1) +
    ggplot2::scale_x_reverse() +
    ggplot2::facet_wrap(stats::as.formula("~ process_label"), scales = "free_y") +
    scale_colour_bgs() +
    ggplot2::labs(x = "Time before present (Ma)", y = "Events per lineage per Myr", colour = NULL,
                  title = "Sensitivity of pooled rates to the time-bin width",
                  subtitle = if (weighting == "clade") "Clades weighted equally" else "Lineage-weighted") +
    theme_bgs()
}
