# Rebuild derived BSM outputs without fitting a model or sampling new maps.
# Usage: BGS_DEV=<checkout> Rscript tools/recount-bsm-bundles.R input.zip output.zip
# Original archives and raw_biogeobears/ members are never changed.
recount_bsm_bundle <- function(input, output) {
  if (file.exists(output)) stop("Output already exists: ", output)
  input <- normalizePath(input, winslash = "/", mustWork = TRUE)
  dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
  output <- file.path(normalizePath(dirname(output), winslash = "/"), basename(output))
  members <- utils::unzip(input, list = TRUE)$Name
  if (any(grepl("^[/\\\\]|^[A-Za-z]:|(^|[/\\\\])[.][.]([/\\\\]|$)", members))) {
    stop("Archive contains an unsafe member path.")
  }
  workspace <- tempfile("bgs-recount-")
  dir.create(workspace)
  on.exit(unlink(workspace, recursive = TRUE), add = TRUE)
  utils::unzip(input, exdir = workspace)
  markers <- list.files(workspace, "^bsm_events[.]csv$", recursive = TRUE, full.names = TRUE)
  if (length(markers) != 1L) stop("Expected exactly one bsm_events.csv table.")
  root <- dirname(dirname(markers[[1L]]))
  fun <- function(name) getFromNamespace(name, "BioGeoSyn")
  table_files <- list.files(file.path(root, "tables"), "[.]csv$", full.names = TRUE)
  tables <- stats::setNames(lapply(table_files, function(p) utils::read.csv(p, check.names = FALSE,
    stringsAsFactors = FALSE)), sub("[.]csv$", "", basename(table_files)))
  events <- tables$bsm_events
  upstream_summary <- tables$bsm_upstream_event_summary %||% tables$bsm_event_summary
  upstream_replicates <- tables$bsm_upstream_replicate_counts %||% tables$bsm_replicate_counts
  models <- unique(upstream_summary$model)
  raw_files <- list.files(file.path(root, "raw_biogeobears"), "_bsm_result[.]rds$", recursive = TRUE, full.names = TRUE)
  raw_verified <- character()
  new_replicates <- lapply(models, function(model) {
    n <- unique(tables$bsm_event_summary$replicate_count[tables$bsm_event_summary$model == model])
    if (length(n) != 1L || is.na(n) || n < 1L) stop("No unambiguous completed-map count for ", model)
    ev <- events[events$model == model, , drop = FALSE]
    reps <- fun("bsm_counts_from_events")(model, ev, n)
    # Where the bundle includes raw objects, independently check every event code
    # and map against those objects before rebuilding any derived output.
    for (raw_file in raw_files) {
      raw <- readRDS(raw_file)
      if (!identical(raw$model, model)) next
      if (fun("bsm_completed_map_count")(raw) != n) stop("Raw map count differs for ", model)
      # Region labels do not affect event counting; retain codes for this check.
      labels <- stats::setNames(character(), character())
      raw_events <- fun("standardize_bsm_event_tables")(model,
        raw$source_area_output$clado_events_tables, raw$source_area_output$ana_events_tables, labels)
      raw_reps <- fun("bsm_counts_from_events")(model, raw_events, n)
      if (!isTRUE(all.equal(reps, raw_reps, check.attributes = FALSE))) stop("Event rows differ from raw maps.")
      raw_verified <<- c(raw_verified, model)
    }
    reps
  })
  tables$bsm_replicate_counts <- do.call(rbind, new_replicates)
  tables$bsm_event_summary <- fun("bsm_summary_from_replicates")(tables$bsm_replicate_counts)
  tables$bsm_upstream_event_summary <- upstream_summary
  tables$bsm_upstream_replicate_counts <- upstream_replicates
  tables$bsm_count_reconciliation <- fun("bsm_compare_event_counts")(
    tables$bsm_replicate_counts, upstream_replicates, tables$bsm_event_summary, upstream_summary)
  tables$biogeographic_process_summary <- BioGeoSyn::summarize_biogeographic_processes(tables)
  # The overall and regional bin grids must have the same extent.
  n_bins <- if (!is.null(tables$process_rates_through_time)) max(tables$process_rates_through_time$time_bin) else 10L
  tables$process_rates_through_time <- BioGeoSyn::summarize_process_rates_through_time(tables, n_bins = n_bins)
  tables$region_process_rates_through_time <- BioGeoSyn::summarize_region_process_rates_through_time(tables, n_bins = n_bins)
  tables$region_process_budgets <- BioGeoSyn::summarize_region_process_budgets(tables)
  tables$bsm_qc <- BioGeoSyn::summarize_bsm_qc(tables)
  failures <- tables$bsm_qc[tables$bsm_qc$status == "Fail", , drop = FALSE]
  if (nrow(failures)) stop("Recount failed QC: ", paste(failures$check, collapse = "; "))
  derived <- c("bsm_event_summary", "bsm_replicate_counts", "bsm_upstream_event_summary",
    "bsm_upstream_replicate_counts", "bsm_count_reconciliation", "biogeographic_process_summary",
    "region_process_budgets", "process_rates_through_time", "region_process_rates_through_time", "bsm_qc")
  changed <- character()
  backup <- file.path(root, "provenance", "before_event_recount")
  dir.create(backup, recursive = TRUE, showWarnings = FALSE)
  figures <- list(
    bsm_event_summary = list(table = "bsm_event_summary", plot = fun("plot_bsm_event_summary")),
    biogeographic_process_synthesis = list(table = "biogeographic_process_summary", plot = BioGeoSyn::plot_biogeographic_process_synthesis),
    process_rates_through_time = list(table = "process_rates_through_time", plot = BioGeoSyn::plot_process_rates_through_time),
    region_process_rates_through_time = list(table = "region_process_rates_through_time", plot = BioGeoSyn::plot_region_process_rates_through_time)
  )
  for (name in derived) {
    path <- file.path(root, "tables", paste0(name, ".csv"))
    previous <- if (file.exists(path)) utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE) else NULL
    equal <- !is.null(previous) && isTRUE(all.equal(previous, tables[[name]], check.attributes = FALSE, tolerance = 1e-10))
    if (equal) next
    changed <- c(changed, name)
    if (file.exists(path)) {
      dir.create(file.path(backup, "tables"), showWarnings = FALSE)
      file.copy(path, file.path(backup, "tables", basename(path)), overwrite = FALSE)
    }
    utils::write.csv(tables[[name]], path, row.names = FALSE)
  }
  for (name in names(figures)) {
    spec <- figures[[name]]
    if (!spec$table %in% changed) next
    old_figures <- list.files(file.path(root, "figures"), paste0("^", name, "[.](png|pdf|svg)$"), full.names = TRUE)
    if (!length(old_figures)) next
    dir.create(file.path(backup, "figures"), showWarnings = FALSE)
    file.copy(old_figures, file.path(backup, "figures", basename(old_figures)), overwrite = FALSE)
    formats <- tools::file_ext(old_figures)
    rendered <- fun("save_plot_outputs")(spec$plot(tables[[spec$table]]), name, file.path(root, "figures"), formats)
    if (any(rendered$status != "created")) stop("Figure regeneration failed: ", name)
  }
  dir.create(file.path(root, "logs"), showWarnings = FALSE)
  utils::write.csv(tables$bsm_count_reconciliation, file.path(root, "logs", "event_recount_comparison.csv"), row.names = FALSE)
  writeLines(c(
    "Derived BSM counts rebuilt from the saved sampled event rows.",
    paste("BioGeoSyn version:", utils::packageVersion("BioGeoSyn")),
    paste("Source archive:", basename(input)), paste("Source archive MD5:", tools::md5sum(input)),
    paste("Raw-object cross-check:", if (length(raw_verified)) paste(raw_verified, collapse = ", ") else "raw objects not bundled; archived event rows used"),
    "No model fitting, stochastic mapping, source-area sampling, or raw-data modification was performed.",
    "Earlier derived tables and figures are retained under provenance/before_event_recount/.",
    paste("Rebuilt tables:", paste(changed, collapse = ", "))
  ), file.path(root, "logs", "event_recount.txt"))
  writeLines(capture.output(utils::sessionInfo()), file.path(root, "logs", "event_recount_session.txt"))
  BioGeoSyn::bundle_results(root, output, refresh_manifest = TRUE)
  list(output = output, changed_tables = changed, raw_verified = raw_verified,
       summary = tables$bsm_event_summary, processes = tables$biogeographic_process_summary,
       reconciliation = tables$bsm_count_reconciliation, qc = tables$bsm_qc)
}

`%||%` <- function(x, y) if (is.null(x)) y else x
if (sys.nframe() == 0L) {
  dev <- Sys.getenv("BGS_DEV")
  if (nzchar(dev)) pkgload::load_all(dev, quiet = TRUE) else library(BioGeoSyn)
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 2L) stop("Usage: Rscript recount-bsm-bundles.R input.zip output.zip")
  print(recount_bsm_bundle(args[[1L]], args[[2L]])$changed_tables)
}
