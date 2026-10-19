test_that("event-row recount retains extinction and zero-event maps without changing raw data", {
  events <- data.frame(model = "DEC", replicate = c(1L, 1L, 1L, 2L),
    event_type = c("e", "e", "d", "sympatry"),
    event_class = c("anagenetic", "anagenetic", "anagenetic", "cladogenetic"))
  before <- serialize(events, NULL)
  reps <- bsm_counts_from_events("DEC", events, 3L)
  expect_identical(serialize(events, NULL), before)
  expect_equal(reps$e, c(2, 0, 0))
  expect_equal(reps$all_ana, c(3, 0, 0))
  expect_equal(reps$total_events, c(3, 1, 0))
  summary <- bsm_summary_from_replicates(reps)
  e <- summary[summary$event_type == "e", ]
  expect_equal(e$mean_count, 2 / 3)
  expect_equal(e$sd_count, sd(c(2, 0, 0)))
  expect_equal(e$sum_count, 2)
  expect_equal(e$replicate_count, 3)
  expect_equal(reps$ALL_disp, c(1, 0, 0))
})

test_that("all seven processes and aggregates are counted from their rows", {
  types <- biogeographic_process_taxonomy()$bsm_event_type
  events <- data.frame(model = "BAYAREALIKE+J", replicate = 1L, event_type = types)
  reps <- bsm_counts_from_events("BAYAREALIKE+J", events, 1L)
  expect_equal(unname(unlist(reps[types])), rep(1, 7))
  expect_equal(reps$all_clado, 4)
  expect_equal(reps$all_ana, 3)
  expect_equal(reps$total_events, 7)
  expect_equal(reps$ana_disp, 2)
  expect_equal(reps$ALL_disp, 3)
  expect_true(all(is.na(bsm_summary_from_replicates(reps)$sd_count)))
})

test_that("zero-event maps remain in the recount and invalid event codes fail", {
  events <- data.frame(model = character(), replicate = integer(), event_type = character())
  reps <- bsm_counts_from_events("DEC", events, 2L)
  expect_equal(reps$total_events, c(0, 0))
  expect_equal(bsm_summary_from_replicates(reps)$mean_count, rep(0, 12))
  invalid <- data.frame(model = "DEC", replicate = 1L, event_type = "unknown")
  expect_error(bsm_counts_from_events("DEC", invalid, 1L), "Unrecognized")
  invalid$event_type <- "e"; invalid$replicate <- 2L
  expect_error(bsm_counts_from_events("DEC", invalid, 1L), "completed maps")
  invalid$replicate <- 1L; invalid$event_class <- "cladogenetic"
  expect_error(bsm_counts_from_events("DEC", invalid, 1L), "event class")
})

test_that("upstream zero extinctions are retained and a misleading mean is detected", {
  events <- data.frame(model = "DEC", replicate = c(1L, 2L), event_type = "e")
  derived <- bsm_counts_from_events("DEC", events, 2L)
  upstream <- derived; upstream$e <- upstream$all_ana <- upstream$total_events <- 0
  ds <- bsm_summary_from_replicates(derived); us <- bsm_summary_from_replicates(upstream)
  before <- serialize(upstream, NULL)
  check <- bsm_compare_event_counts(derived, upstream, ds, us)
  expect_identical(serialize(upstream, NULL), before)
  expect_setequal(unique(check$event_type[check$status == "Different"]), c("e", "all_ana", "total_events"))
  tables <- list(bsm_events = events, bsm_replicate_counts = derived, bsm_event_summary = ds,
    biogeographic_process_summary = summarize_biogeographic_processes(list(bsm_event_summary = ds)),
    bsm_count_reconciliation = check)
  qc <- summarize_bsm_qc(tables)
  expect_equal(qc$status[qc$check == "Derived counts match sampled event rows"], "Pass")
  expect_equal(qc$status[grepl("preserved upstream", qc$check)], "Warning")
  tables$bsm_event_summary$mean_count[tables$bsm_event_summary$event_type == "e"] <- 0
  qc <- summarize_bsm_qc(tables)
  expect_equal(qc$status[qc$check == "Derived counts match sampled event rows"], "Fail")
})

test_that("regional counts use the full event time grid and check dispersal attribution", {
  events <- data.frame(model = "DEC", replicate = 1L,
    event_type = c("vicariance", "sympatry", "d"), event_time_before_present = c(10, 4, 3),
    parent_state = c("AB", "A", "A"), source_region = c(NA, NA, "A"),
    target_region = c(NA, NA, "B"))
  summary <- bsm_summary_from_replicates(bsm_counts_from_events("DEC", events, 1L))
  tables <- list(bsm_events = events, bsm_event_summary = summary)
  overall <- summarize_process_rates_through_time(tables, n_bins = 2L)
  regional <- summarize_region_process_rates_through_time(tables, n_bins = 2L)
  expect_equal(sort(unique(regional$bin_end)), c(5, 10))
  expect_true(bsm_qc_region_rates(regional, overall, "DEC", 1e-6))
  regional$mean_count[regional$process_key == "immigration" & regional$time_bin == 1L] <- 2
  expect_false(bsm_qc_region_rates(regional, overall, "DEC", 1e-6))
})

test_that("standardization corrects upstream extinction loss and leaves the raw object intact", {
  empty <- data.frame()
  ana <- data.frame(node = 1L, event_type = "e", event_txt = "AB->A",
    abs_event_time = 1, event_time = 0.5, extirpation_from = "B")
  raw <- list(
    bsm_output = list(RES_clado_events_tables = list(empty, empty)),
    source_area_output = list(clado_events_tables = list(empty, empty),
      ana_events_tables = list(ana, empty)),
    counts = list(e_totals_list = c(0, 0), ana_totals_list = c(0, 0),
      all_totals_list = c(0, 0), summary_counts_BSMs = data.frame(
        e = c(0, 0, 0), all_ana = c(0, 0, 0), total_events = c(0, 0, 0),
        row.names = c("means", "stdevs", "sums"))))
  before <- serialize(raw, NULL)
  out <- standardize_bsm_result("DEC", raw,
    list(areas = c("A", "B"), region_metadata = NULL))
  expect_identical(serialize(raw, NULL), before)
  expect_equal(out$bsm_replicate_counts$e, c(1, 0))
  expect_equal(out$bsm_event_summary$mean_count[out$bsm_event_summary$event_type == "e"], 0.5)
  expect_equal(out$bsm_upstream_replicate_counts$e, c(0, 0))
  expect_equal(out$bsm_upstream_event_summary$mean_count, c(0, 0, 0))
  expect_true(any(out$bsm_count_reconciliation$status == "Different"))
})

test_that("cross-clade imports never select preserved obsolete summaries", {
  root <- tempfile("bgs-recount-bundle-")
  dir.create(file.path(root, "tables"), recursive = TRUE)
  dir.create(file.path(root, "provenance", "before_event_recount", "tables"), recursive = TRUE)
  current <- data.frame(model = "DEC", event_type = "e", mean_count = 1.5)
  old <- current; old$mean_count <- 0
  active <- "tables/bsm_event_summary.csv"
  historical <- "provenance/before_event_recount/tables/bsm_event_summary.csv"
  write.csv(current, file.path(root, active), row.names = FALSE)
  write.csv(old, file.path(root, historical), row.names = FALSE)
  archive <- tempfile(fileext = ".zip")
  zip_relative_files(root, archive, c(historical, active))
  expect_equal(read_bundle_table(archive, "bsm_event_summary"), current)
  expect_equal(combine_event_summary_across_clades(archive)$mean_count, 1.5)
})
