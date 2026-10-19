# Count sampled events independently of BioGeoBEARS' aggregate count objects.
# In BioGeoBEARS 1.1.3, extinction rows can be omitted from e/all_ana/total_events.
# Keep the upstream objects unchanged and record comparisons in a separate table.
bsm_counts_from_events <- function(model, events, replicate_count) {
  if (length(replicate_count) != 1L || is.na(replicate_count) ||
      replicate_count < 0 || replicate_count != as.integer(replicate_count)) {
    stop("replicate_count must be a non-negative integer.", call. = FALSE)
  }
  n <- as.integer(replicate_count)
  required <- c("model", "replicate", "event_type")
  if (!all(required %in% names(events))) {
    stop("Event recount requires model, replicate and event_type columns.", call. = FALSE)
  }
  taxonomy <- biogeographic_process_taxonomy()
  if (nrow(events) > 0L) {
    if (anyNA(events[, required]) || any(events$model != model) ||
        any(!events$replicate %in% seq_len(n))) {
      stop("Event rows must belong to the model and its completed maps.", call. = FALSE)
    }
    types <- match(events$event_type, taxonomy$bsm_event_type)
    if (anyNA(types)) {
      stop("Unrecognized BSM event type; cannot recount all events.", call. = FALSE)
    }
    if ("event_class" %in% names(events) &&
        any(is.na(events$event_class) | events$event_class != taxonomy$process_group[types])) {
      stop("BSM event type does not match its event class.", call. = FALSE)
    }
  }
  if (n == 0L) return(empty_bsm_replicate_counts_table())
  out <- data.frame(model = rep(model, n), replicate = seq_len(n), stringsAsFactors = FALSE)
  for (type in taxonomy$bsm_event_type) {
    out[[type]] <- tabulate(events$replicate[events$event_type == type], nbins = n)
  }
  out$ana_disp <- out$a + out$d
  out$ALL_disp <- out$founder + out$ana_disp
  out$all_ana <- out$ana_disp + out$e
  out$all_clado <- out$founder + out$subset + out$vicariance + out$sympatry
  out$total_events <- out$all_ana + out$all_clado
  out[, names(empty_bsm_replicate_counts_table()), drop = FALSE]
}

bsm_summary_from_replicates <- function(replicates) {
  if (nrow(replicates) == 0L) return(empty_bsm_event_summary_table())
  types <- setdiff(names(empty_bsm_replicate_counts_table()), c("model", "replicate"))
  out <- do.call(rbind, lapply(split(replicates, replicates$model), function(rows) {
    data.frame(
      model = rows$model[[1L]], event_type = types, event_label = bsm_event_labels(types),
      mean_count = vapply(rows[types], mean, numeric(1)),
      sd_count = vapply(rows[types], stats::sd, numeric(1)),
      sum_count = vapply(rows[types], sum, numeric(1)), replicate_count = nrow(rows),
      interpretation_note = paste(
        "Counts derived from sampled BioGeoBEARS event rows, including zero-event maps.",
        "Upstream counts are retained separately and compared in bsm_count_reconciliation.csv."
      ), stringsAsFactors = FALSE, row.names = NULL
    )
  }))
  out <- out[order(out$model, -out$mean_count, out$event_type), , drop = FALSE]
  row.names(out) <- NULL
  out
}

empty_bsm_count_reconciliation_table <- function() {
  data.frame(model = character(), event_type = character(), statistic = character(),
             replicate = integer(), upstream_count = numeric(), derived_count = numeric(),
             difference = numeric(), status = character(), stringsAsFactors = FALSE)
}

bsm_compare_event_counts <- function(derived_replicates, upstream_replicates,
                                     derived_summary, upstream_summary) {
  long <- function(replicates, summary) {
    types <- setdiff(names(empty_bsm_replicate_counts_table()), c("model", "replicate"))
    pieces <- lapply(intersect(types, names(replicates)), function(type) {
      if (nrow(replicates) == 0L) return(NULL)
      data.frame(model = replicates$model, event_type = type, statistic = "per_map",
                 replicate = replicates$replicate, count = replicates[[type]], stringsAsFactors = FALSE)
    })
    if (nrow(summary) > 0L) {
      pieces <- c(pieces, lapply(c("mean_count", "sd_count", "sum_count"), function(statistic) {
        data.frame(model = summary$model, event_type = summary$event_type, statistic = statistic,
                   replicate = NA_integer_, count = summary[[statistic]], stringsAsFactors = FALSE)
      }))
    }
    out <- do.call(rbind, pieces)
    if (is.null(out)) out <- data.frame(model = character(), event_type = character(),
      statistic = character(), replicate = integer(), count = numeric(), stringsAsFactors = FALSE)
    out
  }
  d <- long(derived_replicates, derived_summary)
  u <- long(upstream_replicates, upstream_summary)
  names(d)[names(d) == "count"] <- "derived_count"
  names(u)[names(u) == "count"] <- "upstream_count"
  out <- merge(u, d, by = c("model", "event_type", "statistic", "replicate"), all = TRUE)
  if (nrow(out) == 0L) return(empty_bsm_count_reconciliation_table())
  out$difference <- out$derived_count - out$upstream_count
  # SD is undefined for a single map; matching NA values are not a discrepancy.
  both_na_sd <- out$statistic == "sd_count" & is.na(out$derived_count) & is.na(out$upstream_count)
  out$status <- ifelse(both_na_sd, "Match", ifelse(is.na(out$difference), "Unavailable",
    ifelse(abs(out$difference) <= 1e-8, "Match", "Different")))
  out[, names(empty_bsm_count_reconciliation_table()), drop = FALSE]
}
