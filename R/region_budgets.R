#' Summarize per-region biogeographic process budgets
#'
#' Aggregate BioGeoBEARS stochastic mapping (BSM) output into a per-region
#' budget of dispersal in and out and local extinction. Immigration and
#' emigration are mean dispersal counts into and out of each region (from all
#' dispersal routes); the net dispersal flux is immigration minus emigration, so
#' a positive value marks a net sink and a negative value a net source. Local
#' extinction is the mean number of range-contraction events lost in the region
#' per stochastic map.
#'
#' @param bsm_tables A list of standardized BSM tables containing at least
#'   `bsm_dispersal_routes`, and optionally `bsm_events` and `bsm_event_summary`,
#'   as returned in a workflow result's `bsm_tables`.
#' @return A data frame with one row per model and region, including
#'   `immigration`, `emigration`, `net_dispersal_flux`, `local_extinction`, and
#'   `total_dispersal`. Returns an empty table with the same columns when no BSM
#'   dispersal routes are available.
#' @export
summarize_region_process_budgets <- function(bsm_tables) {
  empty <- empty_region_process_budgets_table()
  bsm_tables <- bsm_tables %||% list()
  routes <- bsm_tables$bsm_dispersal_routes %||% NULL
  events <- bsm_tables$bsm_events %||% NULL
  event_summary <- bsm_tables$bsm_event_summary %||% NULL

  if (is.null(routes) || nrow(routes) == 0L) {
    return(empty)
  }
  required <- c("model", "route_type", "source_region", "target_region", "mean_count")
  if (!all(required %in% names(routes))) {
    return(empty)
  }
  disp <- routes[!is.na(routes$route_type) & routes$route_type == "all_dispersal", , drop = FALSE]
  disp <- disp[!is.na(disp$mean_count), , drop = FALSE]
  if (nrow(disp) == 0L) {
    return(empty)
  }

  out <- do.call(rbind, lapply(split(disp, disp$model), function(model_routes) {
    model <- as.character(model_routes$model[[1L]])
    regions <- sort(unique(c(model_routes$source_region, model_routes$target_region)))
    regions <- regions[!is.na(regions) & nzchar(regions)]
    if (length(regions) == 0L) {
      return(NULL)
    }
    emigration <- vapply(regions, function(r) {
      sum(model_routes$mean_count[model_routes$source_region == r], na.rm = TRUE)
    }, numeric(1))
    immigration <- vapply(regions, function(r) {
      sum(model_routes$mean_count[model_routes$target_region == r], na.rm = TRUE)
    }, numeric(1))
    local_extinction <- region_local_extinction(events, event_summary, model, regions)
    data.frame(
      model = model,
      region = regions,
      immigration = as.numeric(immigration),
      emigration = as.numeric(emigration),
      net_dispersal_flux = as.numeric(immigration - emigration),
      local_extinction = as.numeric(local_extinction),
      total_dispersal = as.numeric(immigration + emigration),
      interpretation_note = "Mean BioGeoBEARS stochastic mapping dispersal in/out and local extinction per region.",
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  }))
  if (is.null(out) || nrow(out) == 0L) {
    return(empty)
  }
  out <- out[order(out$model, -out$net_dispersal_flux, out$region), , drop = FALSE]
  row.names(out) <- NULL
  out[, names(empty), drop = FALSE]
}

region_local_extinction <- function(events, event_summary, model, regions) {
  out <- rep(0, length(regions))
  if (is.null(events) || nrow(events) == 0L || !"extirpation_region" %in% names(events)) {
    return(out)
  }
  ev <- events[
    !is.na(events$model) & events$model == model &
      !is.na(events$event_type) & events$event_type == "e",
    ,
    drop = FALSE
  ]
  if (nrow(ev) == 0L) {
    return(out)
  }
  n_maps <- region_n_maps(event_summary, events, model)
  if (is.na(n_maps) || n_maps <= 0L) {
    n_maps <- 1L
  }
  counts <- table(factor(ev$extirpation_region, levels = regions))
  as.numeric(counts) / n_maps
}

region_n_maps <- function(event_summary, events, model) {
  if (!is.null(event_summary) && all(c("replicate_count", "model") %in% names(event_summary))) {
    rc <- event_summary$replicate_count[event_summary$model == model]
    rc <- rc[!is.na(rc)]
    if (length(rc) > 0L) {
      return(max(rc))
    }
  }
  if (!is.null(events) && "replicate" %in% names(events)) {
    reps <- events$replicate[events$model == model]
    reps <- reps[!is.na(reps)]
    if (length(reps) > 0L) {
      return(length(unique(reps)))
    }
  }
  1L
}

#' Plot the regional dispersal budget
#'
#' Draw a diverging bar chart of the per-region dispersal budget: immigration
#' (positive) and emigration (negative) mean counts per region, with net
#' dispersal flux marked, so net sources and net sinks are read directly.
#'
#' @param region_budgets A data frame from [summarize_region_process_budgets()].
#' @return A ggplot object.
#' @export
plot_region_process_budget <- function(region_budgets) {
  required <- c("model", "region", "immigration", "emigration", "net_dispersal_flux")
  missing <- setdiff(required, names(region_budgets))
  if (length(missing) > 0L) {
    stop("region_budgets is missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (nrow(region_budgets) == 0L) {
    stop("region_budgets must contain at least one row.", call. = FALSE)
  }

  region_order <- unique(region_budgets$region[order(region_budgets$net_dispersal_flux)])
  region_budgets$region <- factor(region_budgets$region, levels = region_order)

  long <- rbind(
    data.frame(
      model = region_budgets$model, region = region_budgets$region,
      direction = "Immigration (in)", count = region_budgets$immigration,
      stringsAsFactors = FALSE
    ),
    data.frame(
      model = region_budgets$model, region = region_budgets$region,
      direction = "Emigration (out)", count = -region_budgets$emigration,
      stringsAsFactors = FALSE
    )
  )

  ggplot2::ggplot(long, ggplot2::aes(x = region, y = count, fill = direction)) +
    ggplot2::geom_col(width = 0.7, colour = bgs_palette()$outline, linewidth = 0.25) +
    ggplot2::geom_hline(yintercept = 0, colour = bgs_palette()$muted, linewidth = 0.4) +
    ggplot2::geom_point(
      data = region_budgets,
      ggplot2::aes(x = region, y = net_dispersal_flux),
      inherit.aes = FALSE, size = 2, colour = bgs_palette()$ink
    ) +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(stats::as.formula("~ model")) +
    ggplot2::scale_fill_manual(values = c(
      "Immigration (in)" = "#0072B2",
      "Emigration (out)" = "#D55E00"
    )) +
    ggplot2::labs(
      x = NULL,
      y = "Mean dispersal count per stochastic map (out < 0 < in)",
      fill = NULL,
      title = "Regional dispersal budget",
      subtitle = "Net dispersal flux (point) marks net sources (< 0) and net sinks (> 0)"
    ) +
    theme_bgs() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}

empty_region_process_budgets_table <- function() {
  data.frame(
    model = character(),
    region = character(),
    immigration = numeric(),
    emigration = numeric(),
    net_dispersal_flux = numeric(),
    local_extinction = numeric(),
    total_dispersal = numeric(),
    interpretation_note = character(),
    stringsAsFactors = FALSE
  )
}

#' Summarize a region source-to-recipient exchange matrix
#'
#' Build a long table of biogeographic exchange between regions: off-diagonal
#' entries are the mean number of dispersal events from a source region to a
#' recipient region, and diagonal entries (source == recipient) are the mean
#' number of in-situ (sympatric) speciation events in that region. This is the
#' data behind the source-by-recipient matrix (cf. summary tables of in-situ
#' speciation and exchange events). Counts are means per BSM stochastic map.
#'
#' @param bsm_tables A list of standardized BSM tables with `bsm_dispersal_routes`
#'   and `bsm_events` (and optionally `bsm_event_summary`).
#' @return A data frame with `model`, `source_region`, `recipient_region`,
#'   `kind` (`dispersal` or `in_situ`), and `mean_count`.
#' @export
summarize_region_exchange_matrix <- function(bsm_tables) {
  bsm_tables <- bsm_tables %||% list()
  routes <- bsm_tables$bsm_dispersal_routes %||% NULL
  events <- bsm_tables$bsm_events %||% NULL
  event_summary <- bsm_tables$bsm_event_summary %||% NULL
  empty <- data.frame(
    model = character(), source_region = character(), recipient_region = character(),
    kind = character(), mean_count = numeric(), stringsAsFactors = FALSE
  )

  pieces <- list()
  routes_cols <- c("model", "route_type", "source_region", "target_region", "mean_count")
  if (!is.null(routes) && nrow(routes) > 0L && all(routes_cols %in% names(routes))) {
    disp <- routes[
      !is.na(routes$route_type) & routes$route_type == "all_dispersal" &
        !is.na(routes$mean_count),
      ,
      drop = FALSE
    ]
    disp <- disp[as.character(disp$source_region) != as.character(disp$target_region), , drop = FALSE]
    if (nrow(disp) > 0L) {
      pieces$disp <- data.frame(
        model = as.character(disp$model),
        source_region = as.character(disp$source_region),
        recipient_region = as.character(disp$target_region),
        kind = "dispersal",
        mean_count = as.numeric(disp$mean_count),
        stringsAsFactors = FALSE
      )
    }
  }
  ins <- in_situ_counts_by_region(events, event_summary)
  if (!is.null(ins) && nrow(ins) > 0L) {
    pieces$ins <- data.frame(
      model = ins$model, source_region = ins$region, recipient_region = ins$region,
      kind = "in_situ", mean_count = ins$mean_count, stringsAsFactors = FALSE
    )
  }
  pieces <- pieces[!vapply(pieces, is.null, logical(1))]
  if (length(pieces) == 0L) {
    return(empty)
  }
  out <- do.call(rbind, pieces)
  out <- out[order(out$model, out$source_region, out$recipient_region), , drop = FALSE]
  row.names(out) <- NULL
  out
}

# Mean number of in-situ (sympatric) speciation events per region and model.
in_situ_counts_by_region <- function(events, event_summary) {
  if (is.null(events) || nrow(events) == 0L || !"event_type" %in% names(events)) {
    return(NULL)
  }
  ins <- events[!is.na(events$event_type) & events$event_type == "sympatry", , drop = FALSE]
  if (nrow(ins) == 0L) {
    return(NULL)
  }
  code_to_name <- region_code_name_map(events)
  area_code <- as.character(ins$parent_state %||% rep(NA_character_, nrow(ins)))
  region <- unname(code_to_name[area_code])
  region[is.na(region)] <- area_code[is.na(region)]
  ins$region <- region
  ins <- ins[!is.na(ins$region) & nzchar(ins$region), , drop = FALSE]
  if (nrow(ins) == 0L) {
    return(NULL)
  }
  do.call(rbind, lapply(split(ins, as.character(ins$model)), function(m) {
    model <- as.character(m$model[[1L]])
    n_maps <- region_n_maps(event_summary, events, model)
    if (is.na(n_maps) || n_maps <= 0L) {
      n_maps <- length(unique(m$replicate))
    }
    tab <- table(m$region) / n_maps
    data.frame(model = model, region = names(tab), mean_count = as.numeric(tab), stringsAsFactors = FALSE)
  }))
}

#' Format a region exchange matrix for display
#'
#' Pivot the long table from [summarize_region_exchange_matrix()] into a wide
#' source (rows) by recipient (columns) matrix, summed across any models or
#' clades present, with a diagonal of in-situ speciation, per-row emigration
#' totals, per-column immigration totals, and percentages.
#'
#' @param exchange_long A data frame from [summarize_region_exchange_matrix()]
#'   (optionally row-bound across clades).
#' @param digits Number of decimal places for counts.
#' @return A character data frame ready for display or CSV export.
#' @export
format_region_exchange_matrix <- function(exchange_long, digits = 2L) {
  cols <- c("source_region", "recipient_region", "mean_count")
  if (is.null(exchange_long) || nrow(exchange_long) == 0L || !all(cols %in% names(exchange_long))) {
    return(data.frame())
  }
  regions <- sort(unique(c(
    as.character(exchange_long$source_region), as.character(exchange_long$recipient_region)
  )))
  regions <- regions[!is.na(regions) & nzchar(regions)]
  if (length(regions) == 0L) {
    return(data.frame())
  }

  mat <- matrix(0, length(regions), length(regions), dimnames = list(regions, regions))
  for (i in seq_len(nrow(exchange_long))) {
    s <- as.character(exchange_long$source_region[[i]])
    r <- as.character(exchange_long$recipient_region[[i]])
    v <- suppressWarnings(as.numeric(exchange_long$mean_count[[i]]))
    if (!is.na(v) && s %in% regions && r %in% regions) {
      mat[s, r] <- mat[s, r] + v
    }
  }
  emig <- rowSums(mat) - diag(mat)
  imm <- colSums(mat) - diag(mat)
  tot <- sum(emig)
  fmt <- function(x) formatC(round(as.numeric(x), digits), format = "f", digits = digits)
  pct <- function(x, d) if (isTRUE(d > 0)) formatC(round(100 * x / d, 1), format = "f", digits = 1) else "0.0"

  M <- matrix("", nrow = length(regions) + 2L, ncol = length(regions) + 3L)
  colnames(M) <- c("Source \\ Recipient", regions, "Total (out)", "% out")
  for (k in seq_along(regions)) {
    s <- regions[[k]]
    M[k, 1L] <- s
    M[k, 1L + seq_along(regions)] <- fmt(mat[s, regions])
    M[k, length(regions) + 2L] <- fmt(emig[[s]])
    M[k, length(regions) + 3L] <- pct(emig[[s]], tot)
  }
  ti <- length(regions) + 1L
  M[ti, 1L] <- "Total (in)"
  M[ti, 1L + seq_along(regions)] <- fmt(imm[regions])
  po <- length(regions) + 2L
  M[po, 1L] <- "% in"
  M[po, 1L + seq_along(regions)] <- vapply(regions, function(r) pct(imm[[r]], sum(imm)), character(1))

  as.data.frame(M, stringsAsFactors = FALSE, check.names = FALSE)
}
