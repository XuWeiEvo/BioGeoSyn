# ---- Per-lineage (per-capita) rates and clade weighting ----
#
# Event counts grow with the number of lineages alive, so a curve of counts
# mixes the tempo of a process with the accumulation of lineages, and a pooled
# count is dominated by species-rich clades. Following the per-capita rates of
# Xing & Ree (2017, PNAS) and Ding et al. (2025, Science Advances), BioGeoSyn
# divides event counts by the amount of lineage time available for the event,
# measured in lineage-Myr (the summed length of branches inside a time bin).
#
# Denominators
# - Whole-tree processes: lineage-Myr of the clade's tree in the bin. It is
#   fixed by the tree and identical across stochastic maps.
# - Region-resolved in-situ speciation and emigration: lineage-Myr spent in the
#   region, i.e. branch time whose sampled range includes the region. It is
#   reconstructed separately for every stochastic map.
# - Region-resolved immigration: lineage-Myr spent outside the region (the
#   lineages that could colonise it), again per stochastic map.
#
# Pooling across clades
# - "lineage" weighting: summed events / summed lineage-Myr. This is the mean
#   per-lineage rate of the assemblage; large clades weigh more because they
#   contribute more lineages.
# - "clade" weighting: each clade's own rate, averaged across the clades that
#   have at least `min_lineage_myr` in the bin, so every clade counts equally.
# Uncertainty uses the same resampling as pool_rates_by_map_resampling(): in
# each draw one stochastic map is sampled per clade, and the numerator and the
# map-specific denominator come from that same map.

#' Pool per-lineage process rates across clades
#'
#' Convert each clade's stochastic-map event counts into per-lineage rates
#' (events per lineage-Myr) and pool them across clades, either weighting by
#' lineages or weighting clades equally. See the Details for the denominators.
#'
#' @details Whole-tree process rates divide by the lineage-Myr of the clade's
#'   tree in each bin. Region-resolved in-situ speciation and emigration divide
#'   by the lineage-Myr spent in the region in that stochastic map, and
#'   immigration by the lineage-Myr spent outside it. Lineage-weighted rates are
#'   summed events over summed lineage-Myr; clade-weighted rates are the mean of
#'   the clades' own rates. Rates are reported only where the denominator is at
#'   least `min_lineage_myr` (for clade weighting, per clade), because rates
#'   from very little lineage time are dominated by noise.
#'
#' @param clade_data A named list, one element per clade, each a list with
#'   `events` (as for [pool_rates_by_map_resampling()]), `n_maps`,
#'   `replicates` (the stochastic-map identifiers, in order), `tree_nodes`
#'   (a `tree_nodes.csv` table) and, when `by_region = TRUE`, `occupancy`
#'   (from [map_region_occupancy()]).
#' @param bin_width Width of the shared time bins.
#' @param n_draws Number of resampling draws for the intervals.
#' @param seed Optional integer seed.
#' @param by_region Logical. Region-resolved rates instead of whole-tree rates.
#' @param min_lineage_myr Minimum lineage-Myr in a bin for a rate to be
#'   reported. Defaults to `bin_width`, i.e. on average one lineage through the
#'   whole bin.
#' @param groups Clade identity of each element, as in
#'   [pool_rates_by_map_resampling()]. Trees of the same clade are resampled
#'   jointly (tree first, then map), and each tree contributes its own
#'   denominator.
#' @return A data frame with one row per process (and region) and time bin,
#'   with `lineage_myr`, `rate`, `rate_ci_lower`, `rate_ci_upper`
#'   (lineage-weighted), `clade_rate`, `clade_rate_ci_lower`,
#'   `clade_rate_ci_upper`, `n_clades_rate` (clade-weighted), and
#'   `denominator`.
#' @export
pool_per_lineage_rates <- function(clade_data, bin_width = 5, n_draws = 1000L, seed = 1L,
                                   by_region = FALSE, min_lineage_myr = bin_width, groups = NULL) {
  clade_data <- clade_data[!vapply(clade_data, function(x) {
    is.null(x) || is.null(x$events) || nrow(x$events) == 0L || is.null(x$tree_nodes)
  }, logical(1))]
  if (length(clade_data) == 0L) {
    return(NULL)
  }
  if (length(bin_width) != 1L || !is.finite(bin_width) || bin_width <= 0) bin_width <- 5
  n_draws <- max(1L, as.integer(n_draws))
  min_lineage_myr <- suppressWarnings(as.numeric(min_lineage_myr))
  if (length(min_lineage_myr) != 1L || !is.finite(min_lineage_myr)) min_lineage_myr <- bin_width
  tree_groups <- resolve_tree_groups(clade_data, groups)

  group_cols <- if (isTRUE(by_region)) c("process_label", "region") else "process_label"
  all_ev <- do.call(rbind, lapply(clade_data, function(x) x$events[, c(group_cols, "event_time_before_present"), drop = FALSE]))
  root_age <- max(vapply(clade_data, function(x) max(as.numeric(x$tree_nodes$time_before_present), na.rm = TRUE), numeric(1)))
  max_age <- max(c(all_ev$event_time_before_present, root_age), na.rm = TRUE)
  n_bins <- max(1L, ceiling((max_age + 1e-9) / bin_width))
  groups <- unique(all_ev[, group_cols, drop = FALSE])
  groups <- groups[do.call(order, groups), , drop = FALSE]
  group_key <- do.call(paste, c(groups, sep = "\r"))
  n_cells <- length(group_key) * n_bins
  cell_group <- rep(seq_along(group_key), each = n_bins)
  cell_bin <- rep(seq_len(n_bins), times = length(group_key))

  per_clade <- lapply(clade_data, function(x) {
    counts <- clade_map_count_matrix(x, group_cols, group_key, n_bins, bin_width)
    denom <- clade_denominator_matrix(x, groups, n_bins, bin_width, by_region, nrow(counts))
    list(counts = counts, denom = denom)
  })
  per_clade <- stack_tree_replicates(lapply(per_clade, `[[`, "counts"), tree_groups,
                                     extra = lapply(per_clade, `[[`, "denom"))

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

  num_sum <- den_sum <- cw_sum <- cw_n <- matrix(0, nrow = n_draws, ncol = n_cells)
  mean_num <- mean_den <- cw_point_sum <- cw_point_n <- numeric(n_cells)
  for (pc in per_clade) {
    pick <- sample.int(nrow(pc$m), n_draws, replace = TRUE, prob = pc$w)
    N <- pc$m[pick, , drop = FALSE]
    D <- pc$d[pick, , drop = FALSE]
    D[is.na(D)] <- 0
    num_sum <- num_sum + N
    den_sum <- den_sum + D
    ok <- D >= min_lineage_myr
    r <- ifelse(ok, N / pmax(D, 1e-12), 0)
    cw_sum <- cw_sum + r
    cw_n <- cw_n + ok

    mn <- colSums(pc$m * pc$w)
    dd <- pc$d
    dd[is.na(dd)] <- 0
    md <- colSums(dd * pc$w)
    mean_num <- mean_num + mn
    mean_den <- mean_den + md
    okp <- md >= min_lineage_myr
    cw_point_sum <- cw_point_sum + ifelse(okp, mn / pmax(md, 1e-12), 0)
    cw_point_n <- cw_point_n + okp
  }
  lin_draws <- ifelse(den_sum >= min_lineage_myr, num_sum / pmax(den_sum, 1e-12), NA_real_)
  cw_draws <- ifelse(cw_n > 0, cw_sum / pmax(cw_n, 1), NA_real_)
  q <- function(m) {
    t(apply(m, 2L, function(v) {
      if (all(is.na(v))) c(NA_real_, NA_real_) else stats::quantile(v, c(0.025, 0.975), names = FALSE, na.rm = TRUE, type = 7)
    }))
  }
  lin_ci <- q(lin_draws)
  cw_ci <- q(cw_draws)

  out <- groups[cell_group, , drop = FALSE]
  row.names(out) <- NULL
  out$grid_bin <- cell_bin
  out$bin_start <- (cell_bin - 1L) * bin_width
  out$bin_end <- cell_bin * bin_width
  out$bin_midpoint <- (cell_bin - 0.5) * bin_width
  out$mean_count <- mean_num
  out$lineage_myr <- mean_den
  out$rate <- ifelse(mean_den >= min_lineage_myr, mean_num / pmax(mean_den, 1e-12), NA_real_)
  out$rate_ci_lower <- ifelse(is.na(out$rate), NA_real_, lin_ci[, 1L])
  out$rate_ci_upper <- ifelse(is.na(out$rate), NA_real_, lin_ci[, 2L])
  out$clade_rate <- ifelse(cw_point_n > 0, cw_point_sum / pmax(cw_point_n, 1), NA_real_)
  out$clade_rate_ci_lower <- ifelse(is.na(out$clade_rate), NA_real_, cw_ci[, 1L])
  out$clade_rate_ci_upper <- ifelse(is.na(out$clade_rate), NA_real_, cw_ci[, 2L])
  out$n_clades_rate <- as.integer(cw_point_n)
  out$n_clades <- length(per_clade)
  out$n_trees <- length(clade_data)
  out$n_draws <- n_draws
  out$denominator <- denominator_label(out$process_label, by_region)
  out$rate_unit <- "events per lineage-Myr"
  out
}

denominator_label <- function(process_label, by_region) {
  if (!isTRUE(by_region)) {
    return(rep("lineage-Myr of the whole tree", length(process_label)))
  }
  ifelse(process_label == "Immigration", "lineage-Myr outside the region", "lineage-Myr in the region")
}

# Denominator matrix aligned with clade_map_count_matrix(): rows = stochastic
# maps (in `x$replicates` order), columns = group x bin cells.
clade_denominator_matrix <- function(x, groups, n_bins, bin_width, by_region, n_rows) {
  tree_lm <- tree_lineage_myr(x$tree_nodes, n_bins, bin_width)
  if (!isTRUE(by_region)) {
    row <- rep(tree_lm, times = nrow(groups))
    return(matrix(row, nrow = n_rows, ncol = length(row), byrow = TRUE))
  }
  occ <- x$occupancy
  out <- matrix(NA_real_, nrow = n_rows, ncol = nrow(groups) * n_bins)
  for (g in seq_len(nrow(groups))) {
    cols <- (g - 1L) * n_bins + seq_len(n_bins)
    region <- groups$region[[g]]
    occ_r <- if (!is.null(occ) && region %in% dimnames(occ)[[2L]]) {
      occ[, region, , drop = TRUE]
    } else {
      matrix(0, nrow = n_rows, ncol = n_bins)
    }
    occ_r <- matrix(occ_r, nrow = n_rows)
    if (ncol(occ_r) < n_bins) {
      occ_r <- cbind(occ_r, matrix(0, nrow = n_rows, ncol = n_bins - ncol(occ_r)))
    }
    occ_r <- occ_r[, seq_len(n_bins), drop = FALSE]
    out[, cols] <- if (identical(groups$process_label[[g]], "Immigration")) {
      pmax(0, matrix(tree_lm, nrow = n_rows, ncol = n_bins, byrow = TRUE) - occ_r)
    } else {
      occ_r
    }
  }
  out
}

# Overlap (in time units) of intervals [young, old] with the shared bins.
interval_bin_overlap <- function(young, old, n_bins, bin_width) {
  starts <- (seq_len(n_bins) - 1L) * bin_width
  ends <- starts + bin_width
  if (n_bins > 0L) ends[n_bins] <- max(ends[n_bins], max(old, na.rm = TRUE) + 1e-9)
  lo <- outer(young, starts, pmax)
  hi <- outer(old, ends, pmin)
  ov <- hi - lo
  ov[ov < 0] <- 0
  ov
}

#' Lineage-Myr of a tree in shared time bins
#'
#' Sum the branch lengths of a tree that fall inside each time bin, measured
#' back from the present. The root edge is not counted.
#'
#' @param tree_nodes A `tree_nodes.csv` table with `node_index`,
#'   `parent_node_index` and `time_before_present`.
#' @param n_bins Number of bins.
#' @param bin_width Width of each bin.
#' @return A numeric vector of length `n_bins`.
#' @export
tree_lineage_myr <- function(tree_nodes, n_bins, bin_width) {
  br <- tree_branches(tree_nodes)
  if (nrow(br) == 0L) {
    return(numeric(n_bins))
  }
  colSums(interval_bin_overlap(br$young, br$old, n_bins, bin_width))
}

tree_branches <- function(tree_nodes) {
  tn <- tree_nodes
  tn$time_before_present <- pmax(0, suppressWarnings(as.numeric(tn$time_before_present)))
  has_parent <- !is.na(tn$parent_node_index)
  child <- tn[has_parent, , drop = FALSE]
  parent_time <- tn$time_before_present[match(child$parent_node_index, tn$node_index)]
  data.frame(
    node = child$node_index,
    parent = child$parent_node_index,
    young = child$time_before_present,
    old = parent_time,
    is_tip = child$node_type == "tip",
    stringsAsFactors = FALSE
  )
}

#' Region occupancy (lineage-Myr) per stochastic map
#'
#' Reconstruct, for every stochastic map, the sampled range of every branch
#' through time and sum the branch time spent in each region per time bin. The
#' range at the young end of a branch is the node's sampled state (or, for a
#' tip, the state after its last anagenetic event); walking back through the
#' branch's anagenetic events gives its state history. Tip branches with no
#' anagenetic event take the remaining daughter range of their parent's
#' cladogenetic event.
#'
#' @param events A `bsm_events.csv` table for one model.
#' @param tree_nodes A `tree_nodes.csv` table.
#' @param replicates Stochastic-map identifiers, in the row order wanted.
#' @param n_bins,bin_width Shared time grid.
#' @param code_to_name Optional named vector mapping area codes to region names.
#' @return An array `[map, region, bin]` of lineage-Myr.
#' @export
map_region_occupancy <- function(events, tree_nodes, replicates, n_bins, bin_width, code_to_name = NULL) {
  br <- tree_branches(tree_nodes)
  if (is.null(code_to_name)) code_to_name <- region_code_name_map(events)
  states <- unique(c(events$parent_state, unlist(strsplit(as.character(events$child_state), ",", fixed = TRUE))))
  codes <- sort(unique(unlist(strsplit(gsub("[^A-Za-z]", "", states), ""))))
  names_r <- unname(code_to_name[codes])
  names_r[is.na(names_r)] <- codes[is.na(names_r)]
  occ <- array(0, dim = c(length(replicates), length(codes), n_bins),
               dimnames = list(NULL, names_r, NULL))
  if (nrow(br) == 0L || length(codes) == 0L) {
    return(occ)
  }
  events$event_time_before_present <- suppressWarnings(as.numeric(events$event_time_before_present))
  by_map <- split(events, events$replicate)
  for (i in seq_along(replicates)) {
    ev <- by_map[[as.character(replicates[[i]])]]
    if (is.null(ev) || nrow(ev) == 0L) {
      occ[i, , ] <- NA_real_
      next
    }
    seg <- map_branch_segments(ev, br)
    if (is.null(seg) || nrow(seg) == 0L) next
    ov <- interval_bin_overlap(seg$young, seg$old, n_bins, bin_width)
    for (k in seq_along(codes)) {
      has <- grepl(codes[[k]], seg$state, fixed = TRUE)
      if (any(has)) occ[i, k, ] <- colSums(ov[has, , drop = FALSE])
    }
  }
  occ
}

# State history of every branch in one stochastic map, as segments
# (young, old, state).
map_branch_segments <- function(ev, br) {
  clado <- ev[ev$event_class == "cladogenetic", , drop = FALSE]
  ana <- ev[ev$event_class == "anagenetic", , drop = FALSE]
  node_state <- stats::setNames(as.character(clado$parent_state), clado$node_index)
  daughters <- stats::setNames(lapply(strsplit(as.character(clado$child_state), ",", fixed = TRUE), function(d) {
    if (length(d) == 1L) c(d, d) else d
  }), clado$node_index)
  ana <- ana[order(ana$node_index, -ana$event_time_before_present), , drop = FALSE]
  ana_by_node <- split(ana, ana$node_index)

  n <- nrow(br)
  start <- top <- rep(NA_character_, n)
  for (j in seq_len(n)) {
    a <- ana_by_node[[as.character(br$node[[j]])]]
    if (!br$is_tip[[j]]) top[[j]] <- unname(node_state[as.character(br$node[[j]])])
    if (!is.null(a) && nrow(a) > 0L) {
      start[[j]] <- as.character(a$parent_state[[1L]])
      if (br$is_tip[[j]]) top[[j]] <- as.character(a$child_state[[nrow(a)]])
    } else if (!br$is_tip[[j]]) {
      start[[j]] <- top[[j]]
    }
  }
  # Tip branches without anagenetic events: assign the parent's remaining
  # daughter range(s).
  for (p in unique(br$parent[is.na(start)])) {
    kids <- which(br$parent == p)
    pool <- daughters[[as.character(p)]] %||% character()
    for (k in kids[!is.na(start[kids])]) {
      hit <- match(start[[k]], pool)
      if (!is.na(hit)) pool <- pool[-hit]
    }
    for (k in kids[is.na(start[kids])]) {
      if (length(pool) == 0L) break
      start[[k]] <- pool[[1L]]
      pool <- pool[-1L]
      if (is.na(top[[k]])) top[[k]] <- start[[k]]
    }
  }

  segs <- vector("list", n)
  for (j in seq_len(n)) {
    if (is.na(start[[j]])) next
    a <- ana_by_node[[as.character(br$node[[j]])]]
    if (is.null(a) || nrow(a) == 0L) {
      segs[[j]] <- data.frame(young = br$young[[j]], old = br$old[[j]], state = start[[j]], stringsAsFactors = FALSE)
    } else {
      t <- pmin(pmax(a$event_time_before_present, br$young[[j]]), br$old[[j]])
      edges <- c(br$old[[j]], t, br$young[[j]])
      st <- c(as.character(a$parent_state[[1L]]), as.character(a$child_state))
      segs[[j]] <- data.frame(young = edges[-1L], old = edges[-length(edges)], state = st, stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, segs[!vapply(segs, is.null, logical(1))])
}

#' Pool per-lineage rates from result bundles
#'
#' Read each clade's `bsm_events.csv`, `bsm_event_summary.csv` and
#' `tree_nodes.csv` from its result bundle and pool per-lineage rates with
#' [pool_per_lineage_rates()].
#'
#' @inheritParams pool_process_rates_from_bundles
#' @inheritParams pool_per_lineage_rates
#' @return A pooled per-lineage rate table, or `NULL` when no bundle has the
#'   required tables.
#' @export
pool_per_lineage_rates_from_bundles <- function(zip_paths, clade_names = NULL, bin_width = 5,
                                                n_draws = 1000L, seed = 1L, by_region = FALSE,
                                                min_lineage_myr = bin_width) {
  clade_data <- read_clade_events_for_pooling(zip_paths, clade_names, by_region = by_region,
                                              with_tree = TRUE)
  clade_data <- clade_data[!vapply(clade_data, function(x) is.null(x$tree_nodes), logical(1))]
  if (length(clade_data) == 0L) {
    return(NULL)
  }
  if (isTRUE(by_region)) {
    root_age <- max(vapply(clade_data, function(x) max(as.numeric(x$tree_nodes$time_before_present), na.rm = TRUE), numeric(1)))
    max_ev <- max(vapply(clade_data, function(x) max(x$events$event_time_before_present, na.rm = TRUE), numeric(1)))
    n_bins <- max(1L, ceiling((max(root_age, max_ev) + 1e-9) / bin_width))
    clade_data <- lapply(clade_data, function(x) {
      x$occupancy <- map_region_occupancy(x$all_events, x$tree_nodes, x$replicates, n_bins, bin_width,
                                           code_to_name = x$code_to_name)
      x
    })
  }
  pool_per_lineage_rates(clade_data, bin_width = bin_width, n_draws = n_draws, seed = seed,
                         by_region = by_region, min_lineage_myr = min_lineage_myr)
}

#' Plot per-lineage rates through time across clades
#'
#' @param per_lineage A table from [pool_per_lineage_rates()] or
#'   [pool_per_lineage_rates_from_bundles()].
#' @param weighting `"lineage"` (summed events over summed lineage-Myr) or
#'   `"clade"` (mean of the clades' own rates).
#' @param process Optional process label(s) to keep.
#' @param regions Optional region(s) to keep (region-resolved tables only).
#' @return A ggplot object.
#' @export
plot_per_lineage_rates_across_clades <- function(per_lineage, weighting = c("lineage", "clade"),
                                                 process = NULL, regions = NULL) {
  weighting <- match.arg(weighting)
  d <- per_lineage
  if (is.null(d) || nrow(d) == 0L) stop("per_lineage has no rows to plot.", call. = FALSE)
  if (!is.null(process)) d <- d[d$process_label %in% process, , drop = FALSE]
  if (!is.null(regions) && "region" %in% names(d)) d <- d[d$region %in% regions, , drop = FALSE]
  cols <- if (weighting == "lineage") c("rate", "rate_ci_lower", "rate_ci_upper") else c("clade_rate", "clade_rate_ci_lower", "clade_rate_ci_upper")
  d$y <- d[[cols[[1L]]]]
  d$lo <- d[[cols[[2L]]]]
  d$hi <- d[[cols[[3L]]]]
  d <- d[!is.na(d$y), , drop = FALSE]
  if (nrow(d) == 0L) stop("No time bin has enough lineage time to report a rate.", call. = FALSE)
  bw <- stats::median(d$bin_end - d$bin_start, na.rm = TRUE)
  sub <- if (weighting == "lineage") {
    "Summed events / summed lineage-Myr across clades"
  } else {
    "Mean of the clades' own rates (each clade weighted equally)"
  }
  p <- ggplot2::ggplot(d, ggplot2::aes(x = bin_midpoint, y = y, colour = process_label)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lo, ymax = hi, fill = process_label), alpha = 0.12, colour = NA) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 1.2) +
    ggplot2::scale_x_reverse() +
    scale_colour_bgs() + scale_fill_bgs() + ggplot2::guides(fill = "none") +
    ggplot2::labs(
      x = "Time before present (Ma)",
      y = "Events per lineage per Myr",
      colour = "Process",
      title = "Cross-clade per-lineage rates through time",
      subtitle = sprintf("%s; %g-Ma bins; band = 95%% interval over resampled stochastic maps", sub, bw)
    ) +
    theme_bgs()
  if ("region" %in% names(d)) {
    p <- p + ggplot2::facet_wrap(stats::as.formula("~ region"), scales = "free_y")
  }
  p
}
