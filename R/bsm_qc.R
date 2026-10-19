#' Summarize BSM reliability quality-control checks
#'
#' Run the internal consistency checks that establish the biogeographic process
#' outputs faithfully re-present the underlying BioGeoBEARS stochastic mapping,
#' and report each as a pass/warning/fail row. The checks are: per-process
#' counts reconcile with class totals recounted from sampled event rows
#' (`all_clado`, `all_ana`, `total_events`); differences from the preserved
#' upstream summaries are reported separately; regional net dispersal flux sums to zero; process rates
#' summed across time bins equal the per-process synthesis mean; region-resolved
#' rates summed across regions equal the overall per-process rate in each bin;
#' and the requested stochastic maps completed.
#'
#' @param bsm_tables A list of standardized BSM tables, as returned in a workflow
#'   result's `bsm_tables`.
#' @return A data frame with one row per check and model, including `status`
#'   (`Pass`, `Warning`, or `Fail`), `observed`, `expected`, and `detail`.
#'   Returns an empty table when no BSM results are available.
#' @export
summarize_bsm_qc <- function(bsm_tables) {
  empty <- empty_bsm_qc_table()
  bsm_tables <- bsm_tables %||% list()
  event_summary <- bsm_tables$bsm_event_summary %||% NULL
  process <- bsm_tables$biogeographic_process_summary %||% NULL
  budgets <- bsm_tables$region_process_budgets %||% NULL
  rates <- bsm_tables$process_rates_through_time %||% NULL
  region_rates <- bsm_tables$region_process_rates_through_time %||% NULL
  run_status <- bsm_tables$bsm_run_status %||% NULL

  models <- character()
  if (!is.null(process) && nrow(process) > 0L) {
    models <- unique(process$model)
  } else if (!is.null(event_summary) && nrow(event_summary) > 0L) {
    models <- unique(event_summary$model)
  }
  if (length(models) == 0L) {
    return(empty)
  }

  tol <- 1e-6
  rows <- list()
  add <- function(check, model, status, observed, expected, detail) {
    rows[[length(rows) + 1L]] <<- data.frame(
      check = check, model = model, status = status,
      observed = observed, expected = expected, detail = detail,
      stringsAsFactors = FALSE
    )
  }

  for (m in models) {
    events <- bsm_tables$bsm_events
    replicates <- bsm_tables$bsm_replicate_counts
    if (!is.null(events) && !is.null(replicates) && nrow(replicates) > 0L &&
        all(c("model", "replicate", "event_type") %in% names(events))) {
      n_maps <- region_n_maps(event_summary, events, m)
      expected_replicates <- bsm_counts_from_events(m, events[events$model == m, , drop = FALSE], n_maps)
      comparison <- bsm_compare_event_counts(
        expected_replicates, replicates[replicates$model == m, , drop = FALSE],
        bsm_summary_from_replicates(expected_replicates), event_summary[event_summary$model == m, , drop = FALSE])
      ok <- nrow(comparison) > 0L && all(comparison$status == "Match")
      add("Derived counts match sampled event rows", m, if (ok) "Pass" else "Fail",
          if (ok) "matched" else "mismatch", "matched",
          "Each map and its mean, SD and summed counts are checked, including maps with zero events.")
    }
    upstream <- bsm_tables$bsm_count_reconciliation
    if (!is.null(upstream) && nrow(upstream) > 0L) {
      check <- upstream[upstream$model == m, , drop = FALSE]
      if (nrow(check) > 0L) {
        changed <- unique(check$event_type[check$status != "Match"])
        add("Derived counts compared with preserved upstream summaries", m,
            if (length(changed)) "Warning" else "Pass",
            if (length(changed)) paste(changed, collapse = ", ") else "matched", "matched",
            if (length(changed)) paste(
              "Derived counts use sampled event rows. Upstream discrepancies are retained in",
              "bsm_count_reconciliation.csv; raw BioGeoBEARS objects are unchanged."
            ) else "Upstream counts agree with the independently recounted event rows.")
      }
    }
    if (!is.null(process) && !is.null(event_summary)) {
      clado <- sum(process$mean_count[process$model == m & process$process_group == "cladogenetic"], na.rm = TRUE)
      ana <- sum(process$mean_count[process$model == m & process$process_group == "anagenetic"], na.rm = TRUE)
      all_clado <- bsm_qc_es_mean(event_summary, m, "all_clado")
      all_ana <- bsm_qc_es_mean(event_summary, m, "all_ana")
      total_events <- bsm_qc_es_mean(event_summary, m, "total_events")
      ok <- bsm_qc_equal(clado, all_clado, tol) &&
        bsm_qc_equal(ana, all_ana, tol) &&
        bsm_qc_equal(clado + ana, total_events, tol)
      add(
        "Process counts reconcile with event-row class totals", m,
        if (ok) "Pass" else "Fail",
        bsm_qc_round(clado + ana), bsm_qc_round(total_events),
        sprintf(
          "cladogenetic %s vs all_clado %s; anagenetic %s vs all_ana %s",
          bsm_qc_round(clado), bsm_qc_round(all_clado),
          bsm_qc_round(ana), bsm_qc_round(all_ana)
        )
      )
    }

    if (!is.null(budgets) && nrow(budgets) > 0L) {
      net <- sum(budgets$net_dispersal_flux[budgets$model == m], na.rm = TRUE)
      add(
        "Regional net dispersal flux sums to zero", m,
        if (abs(net) < tol) "Pass" else "Fail",
        bsm_qc_round(net), "0",
        "Global immigration equals global emigration."
      )
    }

    if (!is.null(rates) && !is.null(process)) {
      cons <- bsm_qc_rates_conservation(rates, process, m, tol)
      if (!is.na(cons)) {
        add(
          "Process rates sum across time bins to the synthesis mean", m,
          if (cons) "Pass" else "Fail", if (cons) "matched" else "mismatch", "matched",
          "Binned mean counts reconstruct the per-process synthesis mean."
        )
      }
    }

    if (!is.null(region_rates) && nrow(region_rates) > 0L && !is.null(rates)) {
      cons <- bsm_qc_region_rates(region_rates, rates, m, tol)
      if (!is.na(cons)) {
        add(
          "Region-resolved rates sum across regions to the overall rate", m,
          if (cons) "Pass" else "Fail", if (cons) "matched" else "mismatch", "matched",
          "Per-region rates reconstruct the overall per-process rate in each bin."
        )
      }
    }

    if (!is.null(run_status) && nrow(run_status) > 0L) {
      rs <- run_status[run_status$model == m, , drop = FALSE]
      if (nrow(rs) > 0L) {
        requested <- suppressWarnings(as.integer(rs$requested_maps[[1L]]))
        completed <- suppressWarnings(as.integer(rs$completed_maps[[1L]]))
        status <- if (!is.na(completed) && !is.na(requested) && completed >= requested) "Pass" else "Warning"
        add(
          "Requested stochastic maps completed", m, status,
          if (is.na(completed)) "NA" else as.character(completed),
          if (is.na(requested)) "NA" else as.character(requested),
          "Completed stochastic maps versus the requested number."
        )
      }
    }
  }

  out <- do.call(rbind, rows)
  if (is.null(out) || nrow(out) == 0L) {
    return(empty)
  }
  status_rank <- c(Fail = 1L, Warning = 2L, Pass = 3L)
  out <- out[order(status_rank[out$status], out$model, out$check), , drop = FALSE]
  row.names(out) <- NULL
  out[, names(empty), drop = FALSE]
}

bsm_qc_es_mean <- function(event_summary, model, event_type) {
  if (is.null(event_summary) || !all(c("model", "event_type", "mean_count") %in% names(event_summary))) {
    return(NA_real_)
  }
  v <- event_summary$mean_count[event_summary$model == model & event_summary$event_type == event_type]
  if (length(v) == 0L) NA_real_ else suppressWarnings(as.numeric(v[[1L]]))
}

bsm_qc_equal <- function(a, b, tol) {
  if (is.na(a) || is.na(b)) {
    return(FALSE)
  }
  isTRUE(all.equal(a, b, tolerance = tol))
}

bsm_qc_round <- function(x) {
  if (length(x) == 0L || is.na(x)) "NA" else as.character(round(x, 4))
}

bsm_qc_rates_conservation <- function(rates, process, model, tol) {
  if (!all(c("model", "process_key", "mean_count") %in% names(rates))) {
    return(NA)
  }
  procs <- unique(rates$process_key[rates$model == model])
  if (length(procs) == 0L) {
    return(NA)
  }
  all(vapply(procs, function(pk) {
    rt <- sum(rates$mean_count[rates$model == model & rates$process_key == pk], na.rm = TRUE)
    syn <- process$mean_count[process$model == model & process$process_key == pk]
    if (length(syn) == 0L) {
      return(TRUE)
    }
    bsm_qc_equal(rt, suppressWarnings(as.numeric(syn[[1L]])), tol)
  }, logical(1)))
}

bsm_qc_region_rates <- function(region_rates, rates, model, tol) {
  rr <- region_rates[region_rates$model == model, , drop = FALSE]
  if (nrow(rr) == 0L || !all(c("process_key", "time_bin", "mean_count") %in% names(rr))) {
    return(NA)
  }
  agg <- stats::aggregate(mean_count ~ process_key + time_bin, data = rr, FUN = sum)
  overall_process <- c(immigration = "range_expansion", emigration = "range_expansion",
                       in_situ_speciation = "in_situ_speciation")
  for (i in seq_len(nrow(agg))) {
    pk <- agg$process_key[[i]]
    if (pk %in% names(overall_process)) pk <- overall_process[[pk]]
    overall <- rates$mean_count[
      rates$model == model &
        rates$process_key == pk &
        rates$time_bin == agg$time_bin[[i]]
    ]
    if (length(overall) > 0L && !bsm_qc_equal(agg$mean_count[[i]], suppressWarnings(as.numeric(overall[[1L]])), tol)) {
      return(FALSE)
    }
  }
  TRUE
}

empty_bsm_qc_table <- function() {
  data.frame(
    check = character(),
    model = character(),
    status = character(),
    observed = character(),
    expected = character(),
    detail = character(),
    stringsAsFactors = FALSE
  )
}
