# ---- Cross-clade uncertainty from stochastic-map realisations ----
#
# Pooled cross-clade curves are sums over independent clade analyses. Summing
# each clade's marginal 2.5% and 97.5% limits does not give a 95% interval for
# the sum: it assumes that every clade sits at its own extreme in the same
# draw, so the band is too wide and widens linearly with the number of clades.
# Here the interval is computed from the distribution of the sum itself. In each
# draw one stochastic map is sampled at random from every clade, the per-bin
# event counts of those maps are added, and the 2.5% and 97.5% quantiles are
# taken over draws. The mean is the exact sum of the clades' per-map means.

#' Pool process rates across clades by resampling stochastic maps
#'
#' Bin every clade's individually timed BSM events directly onto one shared
#' absolute-time grid and pool them across clades. The pooled mean is the sum of
#' the clades' mean counts per stochastic map. The 95% interval is the 2.5% and
#' 97.5% quantile of the pooled count over `n_draws` draws, where each draw adds
#' the counts of one stochastic map sampled at random from every clade. Because
#' clades are independent analyses whose maps have no correspondence, maps are
#' sampled independently per clade (with replacement).
#'
#' @param clade_events A named list with one element per clade. Each element is
#'   a list with `events` (a data frame with at least `replicate`,
#'   `process_label`, `event_time_before_present`, and `region` when
#'   `by_region = TRUE`) and `n_maps` (the number of stochastic maps the clade
#'   produced, including maps with no event of a given kind).
#' @param bin_width Width of the shared time bins (time units of the trees,
#'   usually millions of years).
#' @param n_draws Number of resampling draws used for the interval.
#' @param seed Optional integer seed so the interval is reproducible.
#' @param by_region Logical. Pool by process and region instead of by process.
#' @param groups Clade identity of each element of `clade_events`. Elements that
#'   share a group are analyses of the same clade on different trees (for
#'   example posterior trees). Within a group, each draw first samples a tree
#'   uniformly and then one of its stochastic maps, so the interval includes
#'   topological and age uncertainty, and the mean is the average over trees.
#'   Defaults to the element names with any `__treeNNN` suffix removed.
#' @return A data frame with one row per process (and region), and shared time
#'   bin, with `bin_start`, `bin_end`, `bin_midpoint`, `mean_count`, `ci_lower`,
#'   `ci_upper`, `n_clades`, `n_trees`, `n_draws` and `ci_method`.
#' @export
pool_rates_by_map_resampling <- function(clade_events, bin_width = 5, n_draws = 1000L,
                                         seed = 1L, by_region = FALSE, groups = NULL) {
  empty <- empty_pooled_rates_table(by_region)
  clade_events <- clade_events[!vapply(clade_events, function(x) {
    is.null(x) || is.null(x$events) || nrow(x$events) == 0L
  }, logical(1))]
  if (length(clade_events) == 0L) {
    return(empty)
  }
  if (length(bin_width) != 1L || !is.finite(bin_width) || bin_width <= 0) {
    bin_width <- 5
  }
  n_draws <- max(1L, as.integer(n_draws))
  tree_groups <- resolve_tree_groups(clade_events, groups)

  group_cols <- if (isTRUE(by_region)) c("process_label", "region") else "process_label"
  all_ev <- do.call(rbind, lapply(clade_events, function(x) x$events[, c(group_cols, "event_time_before_present"), drop = FALSE]))
  max_age <- max(all_ev$event_time_before_present, na.rm = TRUE)
  if (!is.finite(max_age)) {
    return(empty)
  }
  n_bins <- max(1L, ceiling((max_age + 1e-9) / bin_width))
  groups <- unique(all_ev[, group_cols, drop = FALSE])
  groups <- groups[do.call(order, groups), , drop = FALSE]
  group_key <- do.call(paste, c(groups, sep = "\r"))
  n_cells <- length(group_key) * n_bins

  # One matrix per analysed tree: rows = stochastic maps, columns = group x bin
  # cells; trees of the same clade are then stacked with sampling weights.
  map_matrices <- lapply(clade_events, function(x) {
    clade_map_count_matrix(x, group_cols, group_key, n_bins, bin_width)
  })
  stacked <- stack_tree_replicates(map_matrices, tree_groups)

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

  pooled_draws <- matrix(0, nrow = n_draws, ncol = n_cells)
  pooled_mean <- numeric(n_cells)
  for (st in stacked) {
    pick <- sample.int(nrow(st$m), n_draws, replace = TRUE, prob = st$w)
    pooled_draws <- pooled_draws + st$m[pick, , drop = FALSE]
    pooled_mean <- pooled_mean + colSums(st$m * st$w)
  }
  ci <- apply(pooled_draws, 2L, stats::quantile, probs = c(0.025, 0.975), names = FALSE, type = 7)

  cell_group <- rep(seq_along(group_key), each = n_bins)
  cell_bin <- rep(seq_len(n_bins), times = length(group_key))
  out <- groups[cell_group, , drop = FALSE]
  row.names(out) <- NULL
  out$grid_bin <- cell_bin
  out$bin_start <- (cell_bin - 1L) * bin_width
  out$bin_end <- cell_bin * bin_width
  out$bin_midpoint <- (cell_bin - 0.5) * bin_width
  out$mean_count <- pooled_mean
  out$ci_lower <- ci[1L, ]
  out$ci_upper <- ci[2L, ]
  out$n_clades <- length(stacked)
  out$n_trees <- length(map_matrices)
  out$n_draws <- n_draws
  out$ci_method <- if (length(map_matrices) > length(stacked)) "tree_and_map_resampling" else "map_resampling"
  out
}

#' Clade name without a tree-replicate suffix
#'
#' Strip a trailing `_results` and a tree-replicate tag such as `__tree003`,
#' `_tree3` or `-tree-12`, so analyses of one clade on several trees are
#' recognised as the same clade.
#'
#' @param x Character vector of clade or bundle names.
#' @return Character vector of clade names.
#' @export
clade_group_name <- function(x) {
  x <- sub("_results$", "", as.character(x))
  sub("[_ -]+tree[_ -]?[0-9]+$", "", x, ignore.case = TRUE)
}

resolve_tree_groups <- function(clade_events, groups) {
  if (is.null(groups)) {
    nm <- names(clade_events) %||% rep("", length(clade_events))
    nm[!nzchar(nm)] <- paste0("clade", which(!nzchar(nm)))
    return(clade_group_name(nm))
  }
  groups <- as.character(groups)
  if (length(groups) != length(clade_events)) {
    stop("`groups` must have one entry per clade analysis.", call. = FALSE)
  }
  groups
}

# Stack the per-tree matrices of each clade with sampling weights that give
# every tree the same total weight (1 / number of trees) and every map within a
# tree an equal share of it. A clade analysed on one tree reduces to equal
# weights over its maps.
stack_tree_replicates <- function(mats, groups, extra = NULL) {
  idx <- split(seq_along(mats), factor(groups, levels = unique(groups)))
  lapply(idx, function(ii) {
    n_t <- length(ii)
    w <- unlist(lapply(ii, function(i) rep(1 / (n_t * nrow(mats[[i]])), nrow(mats[[i]]))))
    out <- list(m = do.call(rbind, mats[ii]), w = w)
    if (!is.null(extra)) out$d <- do.call(rbind, extra[ii])
    out
  })
}

# Per-map count matrix for one clade on the shared grid. Maps that produced no
# event of a given kind are kept as zero rows, so the clade's map count (not just
# the maps with events) defines its distribution.
clade_map_count_matrix <- function(x, group_cols, group_key, n_bins, bin_width) {
  ev <- x$events
  ev <- ev[!is.na(ev$event_time_before_present) & !is.na(ev$replicate), , drop = FALSE]
  replicates <- if (!is.null(x$replicates)) x$replicates else sort(unique(ev$replicate))
  n_maps <- suppressWarnings(as.integer(x$n_maps %||% NA_integer_))
  if (length(n_maps) != 1L || is.na(n_maps) || n_maps < length(replicates)) {
    n_maps <- max(1L, length(replicates))
  }
  m <- matrix(0, nrow = n_maps, ncol = length(group_key) * n_bins)
  if (nrow(ev) == 0L) {
    return(m)
  }
  map_row <- match(ev$replicate, replicates)
  bin <- pmin(n_bins, pmax(1L, floor(ev$event_time_before_present / bin_width) + 1L))
  g <- match(do.call(paste, c(ev[, group_cols, drop = FALSE], sep = "\r")), group_key)
  col <- (g - 1L) * n_bins + bin
  keep <- !is.na(map_row) & !is.na(col)
  tab <- table(factor(map_row[keep], levels = seq_len(n_maps)), factor(col[keep], levels = seq_len(ncol(m))))
  m[] <- as.numeric(tab)
  m
}

# Read the per-event BSM table of each bundle (first model only, as elsewhere in
# the cross-clade synthesis) and attach the process label each event maps to.
read_clade_events_for_pooling <- function(zip_paths, clade_names = NULL, by_region = FALSE,
                                          with_tree = FALSE) {
  zip_paths <- as.character(zip_paths %||% character())
  if (length(zip_paths) == 0L) {
    return(list())
  }
  clade_names <- as.character(clade_names %||% rep(NA_character_, length(zip_paths)))
  taxonomy <- biogeographic_process_taxonomy()
  # A clade whose events never move into or out of a region cannot name that
  # region from its own dispersal records, so area codes are resolved against
  # the names seen across all bundles (cross-clade synthesis already assumes a
  # shared region coding).
  global_codes <- if (isTRUE(by_region) || isTRUE(with_tree)) shared_region_code_map(zip_paths) else character()
  pieces <- lapply(seq_along(zip_paths), function(i) {
    events <- keep_first_model(read_bundle_table(zip_paths[[i]], "bsm_events"))
    if (is.null(events) || nrow(events) == 0L ||
        !all(c("model", "replicate", "event_type", "event_time_before_present") %in% names(events))) {
      return(NULL)
    }
    esum <- read_bundle_table(zip_paths[[i]], "bsm_event_summary")
    model <- as.character(events$model[[1L]])
    n_maps <- region_n_maps(esum, events, model)
    events$event_time_before_present <- suppressWarnings(as.numeric(events$event_time_before_present))
    if (isTRUE(by_region)) {
      ev <- attribute_region_events(events)
      if (is.null(ev) || nrow(ev) == 0L) {
        return(NULL)
      }
      ev$region <- ev$event_region
      fill <- ev$region %in% names(global_codes)
      ev$region[fill] <- unname(global_codes[ev$region[fill]])
      ev <- ev[!is.na(ev$region) & nzchar(ev$region), , drop = FALSE]
    } else {
      ev <- events[events$event_type %in% taxonomy$bsm_event_type, , drop = FALSE]
      idx <- match(ev$event_type, taxonomy$bsm_event_type)
      ev$process_label <- taxonomy$process_label[idx]
      ev$process_key <- taxonomy$process_key[idx]
    }
    lab <- if (length(clade_names) >= i && !is.na(clade_names[[i]]) && nzchar(clade_names[[i]])) {
      clade_names[[i]]
    } else {
      derive_clade_name(zip_paths[[i]])
    }
    replicates <- sort(unique(events$replicate[!is.na(events$replicate)]))
    if (length(replicates) < n_maps) {
      replicates <- c(replicates, max(c(replicates, 0)) + seq_len(n_maps - length(replicates)))
    }
    out <- list(clade = lab, events = ev, n_maps = n_maps, replicates = replicates)
    if (isTRUE(with_tree)) {
      out$tree_nodes <- read_bundle_table(zip_paths[[i]], "tree_nodes")
      out$all_events <- events
      own <- region_code_name_map(events)
      out$code_to_name <- c(own, global_codes[setdiff(names(global_codes), names(own))])
    }
    out
  })
  pieces <- pieces[!vapply(pieces, is.null, logical(1))]
  if (length(pieces) == 0L) {
    return(list())
  }
  labels <- make.unique(vapply(pieces, function(p) p$clade, character(1)), sep = " ")
  stats::setNames(pieces, labels)
}

# Area code -> region name pairs seen across all bundles' dispersal records.
# Conflicting names for one code are reported and the first is kept.
shared_region_code_map <- function(zip_paths) {
  maps <- lapply(zip_paths, function(z) {
    ev <- read_bundle_table(z, "bsm_events")
    if (is.null(ev) || nrow(ev) == 0L) character() else region_code_name_map(ev)
  })
  all <- unlist(maps)
  if (length(all) == 0L) {
    return(character())
  }
  conflicts <- tapply(all, names(all), function(v) length(unique(v)) > 1L)
  if (any(conflicts)) {
    warning("Area codes map to different region names across clades: ",
            paste(names(conflicts)[conflicts], collapse = ", "),
            ". Cross-clade region results assume a shared region coding.", call. = FALSE)
  }
  all[!duplicated(names(all))]
}

#' Pool cross-clade process rates from result bundles
#'
#' Convenience wrapper that reads the per-event BSM tables from each clade's
#' result bundle and pools them with [pool_rates_by_map_resampling()].
#'
#' @param zip_paths Paths to single-clade result bundles (`.zip`).
#' @param clade_names Optional clade labels, one per bundle.
#' @inheritParams pool_rates_by_map_resampling
#' @return A pooled rates table (see [pool_rates_by_map_resampling()]), or
#'   `NULL` when no bundle carries per-event BSM output.
#' @export
pool_process_rates_from_bundles <- function(zip_paths, clade_names = NULL, bin_width = 5,
                                            n_draws = 1000L, seed = 1L, by_region = FALSE) {
  clade_events <- read_clade_events_for_pooling(zip_paths, clade_names, by_region = by_region)
  if (length(clade_events) == 0L) {
    return(NULL)
  }
  pool_rates_by_map_resampling(clade_events, bin_width = bin_width, n_draws = n_draws,
                               seed = seed, by_region = by_region)
}

empty_pooled_rates_table <- function(by_region = FALSE) {
  out <- data.frame(
    process_label = character(),
    grid_bin = integer(),
    bin_start = numeric(),
    bin_end = numeric(),
    bin_midpoint = numeric(),
    mean_count = numeric(),
    ci_lower = numeric(),
    ci_upper = numeric(),
    n_clades = integer(),
    n_draws = integer(),
    ci_method = character(),
    stringsAsFactors = FALSE
  )
  if (isTRUE(by_region)) {
    out <- cbind(out[, 1L, drop = FALSE], region = character(), out[, -1L, drop = FALSE])
  }
  out
}
