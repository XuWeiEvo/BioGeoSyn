#' Lineages through space and time, pooled across clades
#'
#' Counts how many lineages occupy each region through time (Skeels 2019),
#' from the ranges sampled in every stochastic map. The number of lineages in a
#' region in a time bin is the branch time spent in that region during the bin
#' divided by the bin width, that is, the time-averaged number of lineages
#' present. A widespread lineage counts in every region it occupies. Counts are
#' added across clades, and the 95% interval comes from resampling one
#' stochastic map per clade (and one tree first, for clades analysed on
#' several trees), as for the other pooled outputs.
#'
#' @param clade_data A named list with one element per clade (or per tree of a
#'   clade), as returned by `read_clade_events_for_pooling(..., by_region =
#'   TRUE, with_tree = TRUE)`. Each element needs `tree_nodes` and either an
#'   `occupancy` array (maps x regions x bins, in lineage-Myr) or the
#'   `all_events` table from which it is reconstructed.
#' @param bin_width Bin width in Myr. Narrow bins (1 Ma, the default) give a
#'   near-continuous curve.
#' @param n_draws Number of resampling draws.
#' @param seed Random seed, or `NULL`.
#' @param groups Optional clade grouping for tree replicates; by default
#'   bundles named `<clade>__treeNNN` are grouped by clade.
#' @return A data frame with one row per region and bin: `region`,
#'   `bin_start`, `bin_end`, `bin_midpoint`, `mean_lineages`, `ci_lower`,
#'   `ci_upper` and `n_clades` (clades with lineages in that region and bin).
#' @export
lineages_through_space_time <- function(clade_data, bin_width = 1, n_draws = 1000L, seed = 1L,
                                        groups = NULL) {
  clade_data <- clade_data[!vapply(clade_data, function(x) is.null(x) || is.null(x$tree_nodes), logical(1))]
  if (length(clade_data) == 0L) return(NULL)
  if (length(bin_width) != 1L || !is.finite(bin_width) || bin_width <= 0) bin_width <- 1
  n_draws <- max(1L, as.integer(n_draws))
  tree_groups <- resolve_tree_groups(clade_data, groups)
  root_age <- max(vapply(clade_data, function(x) max(as.numeric(x$tree_nodes$time_before_present), na.rm = TRUE),
                         numeric(1)))
  n_bins <- max(1L, ceiling((root_age + 1e-9) / bin_width))

  occ <- lapply(clade_data, function(x) {
    o <- x$occupancy
    if (is.null(o)) {
      o <- map_region_occupancy(x$all_events %||% x$events, x$tree_nodes, x$replicates, n_bins, bin_width,
                                code_to_name = x$code_to_name)
    }
    if (dim(o)[[3L]] < n_bins) {
      pad <- array(0, dim = c(dim(o)[1:2], n_bins - dim(o)[[3L]]))
      o <- array(c(o, pad), dim = c(dim(o)[1:2], n_bins), dimnames = list(NULL, dimnames(o)[[2L]], NULL))
    }
    keep <- apply(o, 1L, function(m) !anyNA(m))
    o[keep, , seq_len(n_bins), drop = FALSE]
  })
  regions <- sort(unique(unlist(lapply(occ, function(o) dimnames(o)[[2L]]))))
  if (length(regions) == 0L) return(NULL)
  as_matrix <- function(o) {
    m <- matrix(0, nrow = dim(o)[[1L]], ncol = length(regions) * n_bins)
    for (k in seq_along(regions)) {
      if (regions[[k]] %in% dimnames(o)[[2L]]) {
        m[, (k - 1L) * n_bins + seq_len(n_bins)] <- matrix(o[, regions[[k]], , drop = TRUE], nrow = dim(o)[[1L]])
      }
    }
    m / bin_width
  }
  mats <- lapply(occ, as_matrix)
  if (any(vapply(mats, nrow, integer(1)) == 0L)) {
    stop("Every clade needs at least one stochastic map with sampled ranges.", call. = FALSE)
  }
  per_clade <- stack_tree_replicates(mats, tree_groups)

  if (!is.null(seed) && !is.na(seed)) {
    old_seed <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv()) else NULL
    on.exit({
      if (is.null(old_seed)) {
        if (exists(".Random.seed", envir = globalenv())) rm(".Random.seed", envir = globalenv())
      } else {
        assign(".Random.seed", old_seed, envir = globalenv())
      }
    }, add = TRUE)
    set.seed(as.integer(seed))
  }
  n_cells <- length(regions) * n_bins
  draws <- matrix(0, nrow = n_draws, ncol = n_cells)
  mean_l <- numeric(n_cells)
  present <- numeric(n_cells)
  for (pc in per_clade) {
    pick <- sample.int(nrow(pc$m), n_draws, replace = TRUE, prob = pc$w)
    draws <- draws + pc$m[pick, , drop = FALSE]
    mc <- colSums(pc$m * pc$w)
    mean_l <- mean_l + mc
    present <- present + (mc > 0)
  }
  ci <- t(apply(draws, 2L, stats::quantile, probs = c(0.025, 0.975), names = FALSE, type = 7))
  bin <- rep(seq_len(n_bins), times = length(regions))
  data.frame(
    region = rep(regions, each = n_bins),
    bin_start = (bin - 1) * bin_width, bin_end = bin * bin_width, bin_midpoint = (bin - 0.5) * bin_width,
    mean_lineages = mean_l, ci_lower = ci[, 1L], ci_upper = ci[, 2L], n_clades = as.integer(present),
    stringsAsFactors = FALSE
  )
}

#' @rdname lineages_through_space_time
#' @param zip_paths Result bundles, one per clade (or per tree of a clade).
#' @param clade_names Optional clade names.
#' @export
lineages_through_space_time_from_bundles <- function(zip_paths, clade_names = NULL, bin_width = 1,
                                                     n_draws = 1000L, seed = 1L) {
  clade_data <- read_clade_events_for_pooling(zip_paths, clade_names, by_region = TRUE, with_tree = TRUE)
  lineages_through_space_time(clade_data, bin_width = bin_width, n_draws = n_draws, seed = seed)
}

#' Plot lineages through space and time
#'
#' @param ltst A table from [lineages_through_space_time()].
#' @param regions Optional regions to show.
#' @param log_y Use a log10 y-axis (lineages + 1).
#' @param facet Draw one panel per region instead of one curve per region.
#' @return A ggplot object.
#' @export
plot_lineages_through_space_time <- function(ltst, regions = NULL, log_y = TRUE, facet = FALSE) {
  d <- ltst
  if (is.null(d) || nrow(d) == 0L) stop("ltst has no rows to plot.", call. = FALSE)
  if (!is.null(regions)) d <- d[d$region %in% regions, , drop = FALSE]
  bw <- stats::median(d$bin_end - d$bin_start)
  d <- d[d$bin_end <= max(d$bin_end[d$mean_lineages > 0]), , drop = FALSE]
  tr <- if (isTRUE(log_y)) function(v) log10(v + 1) else identity
  p <- ggplot2::ggplot(d, ggplot2::aes(x = bin_midpoint, y = tr(mean_lineages), colour = region, fill = region)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = tr(ci_lower), ymax = tr(ci_upper)), alpha = 0.15, colour = NA) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::scale_x_reverse() +
    scale_colour_bgs() + scale_fill_bgs() +   # same region colours as the dispersal networks
    ggplot2::guides(fill = "none") +
    ggplot2::labs(
      x = "Time before present (Ma)",
      y = if (isTRUE(log_y)) expression(log[10](lineages + 1)) else "Lineages",
      colour = "Region",
      title = "Lineages through space and time",
      subtitle = sprintf("Mean lineages per region summed across clades; %g-Ma bins; band = 95%% interval over resampled stochastic maps", bw)
    ) +
    theme_bgs()
  if (isTRUE(facet)) p <- p + ggplot2::facet_wrap(stats::as.formula("~ region")) + ggplot2::theme(legend.position = "none")
  p
}
