test_that("planned_model_table summarizes configured models", {
  config <- list(models = list(run = c("DEC", "DEC+J", "BAYAREALIKE")))

  plan <- planned_model_table(config)

  expect_equal(plan$model, c("DEC", "DEC+J", "BAYAREALIKE"))
  expect_equal(plan$model_family, c("DEC", "DEC", "BAYAREALIKE"))
  expect_equal(plan$has_j, c(FALSE, TRUE, FALSE))
  expect_equal(plan$status, rep("planned", 3L))
})

test_that("create_iBGB_shiny_app builds a Shiny app when shiny is installed", {
  testthat::skip_if_not_installed("shiny")

  app <- create_iBGB_shiny_app()

  expect_s3_class(app, "shiny.appobj")
})

test_that("Shiny sidebar helper builds grouped controls", {
  testthat::skip_if_not_installed("shiny")

  section <- shiny_control_section(
    "Workflow",
    shiny_action_grid(shiny::actionButton("validate", "Validate"))
  )

  html <- as.character(section)

  expect_match(html, "ibgb-control-section", fixed = TRUE)
  expect_match(html, "ibgb-control-title", fixed = TRUE)
  expect_match(html, "ibgb-action-grid", fixed = TRUE)
  expect_match(html, "Workflow", fixed = TRUE)
})

test_that("resolve_shiny_config_path prefers uploaded YAML files", {
  uploaded <- tempfile(fileext = ".yml")
  writeLines("project:\n  name: uploaded", uploaded)
  input <- list(
    config_upload = data.frame(
      name = "analysis.yml",
      size = file.info(uploaded)$size,
      type = "text/yaml",
      datapath = uploaded,
      stringsAsFactors = FALSE
    ),
    config_path = "fallback.yml"
  )

  expect_equal(resolve_shiny_config_path(input), uploaded)
})

test_that("resolve_shiny_config_path requires a config source", {
  input <- list(config_upload = NULL, config_path = "")

  expect_error(
    resolve_shiny_config_path(input),
    "Provide an analysis.yml"
  )
})

test_that("Shiny config editor applies GUI overrides and writes runnable YAML", {
  project <- create_example_project(tempfile("ibgb-shiny-config-editor-"))
  cfg <- read_config(project$config)
  input <- list(
    use_config_editor = TRUE,
    project_name = "edited_clade",
    tree_file = "data/tree.nwk",
    geography_file = "data/geography.csv",
    regions_file = "data/regions.csv",
    max_range_size = "2",
    models_run = c("DEC", "DEC+J")
  )

  edited <- apply_shiny_config_overrides(cfg, input, output_dir = file.path(project$root, "edited-results"))
  config_path <- write_shiny_workflow_config(edited, source_config = project$config)
  roundtrip <- read_config(config_path)

  expect_equal(edited$project$name, "edited_clade")
  expect_equal(edited$inputs$max_range_size, 2L)
  expect_equal(edited$models$run, c("DEC", "DEC+J"))
  expect_true(grepl("edited-results$", edited$project$output_dir))
  expect_true(file.exists(roundtrip$inputs$tree_file))
  expect_true(file.exists(roundtrip$inputs$geography_file))
  expect_true(file.exists(roundtrip$inputs$regions_file))
  expect_equal(roundtrip$models$run, c("DEC", "DEC+J"))
})

test_that("run_app_action captures errors in state messages", {
  state <- new.env(parent = emptyenv())
  state$message <- "Ready."
  state$messages <- "Ready."
  state$status_type <- "info"

  value <- run_app_action(state, {
    stop("broken", call. = FALSE)
  })

  expect_null(value)
  expect_equal(state$status_type, "error")
  expect_match(state$message, "Error: broken", fixed = TRUE)
  expect_true(any(grepl("Error: broken", state$messages, fixed = TRUE)))
})

test_that("Shiny message helpers record staged workflow progress", {
  state <- new.env(parent = emptyenv())
  state$message <- "Ready."
  state$messages <- "Ready."
  state$status_type <- "info"

  append_app_stage(state, "Workflow", "model status ready", "completed: 2")

  expect_equal(state$message, "Workflow: model status ready - completed: 2")
  expect_true(any(grepl("Workflow: model status ready - completed: 2", state$messages, fixed = TRUE)))
  expect_equal(
    workflow_validation_label(data.frame(ok = c(TRUE, TRUE, FALSE, NA))),
    "2 passed, 1 failed"
  )
  expect_equal(
    workflow_model_status_label(data.frame(status = c("completed", "planned", "completed"))),
    "completed: 2, planned: 1"
  )
})

test_that("download file helpers resolve and copy report files", {
  out <- tempfile("ibgb-shiny-download-")
  paths <- create_project(out)
  report <- file.path(paths$reports, "summary_report.html")
  writeLines("<html></html>", report)
  state <- new.env(parent = emptyenv())
  state$report <- NULL
  state$result <- list(project_paths = paths)

  resolved <- resolve_report_file(state)
  copied <- tempfile(fileext = ".html")
  copy_download_file(resolved, copied)

  expect_equal(basename(resolved), "summary_report.html")
  expect_true(file.exists(copied))
  expect_equal(report_preview_path(state), as_path(report))
})

test_that("download file helper reports missing files clearly", {
  expect_error(
    require_existing_file(NULL, "missing file"),
    "missing file"
  )
  state <- new.env(parent = emptyenv())
  state$report <- NULL
  state$result <- NULL
  expect_null(report_preview_path(state))
})

test_that("load_existing_workflow_result rebuilds Shiny state from output files", {
  out <- tempfile("ibgb-shiny-load-results-")
  paths <- create_project(out)
  report <- file.path(paths$reports, "summary_report.html")
  model_plot <- file.path(paths$figures, "model_comparison.png")
  writeLines("<html></html>", report)
  writeBin(as.raw(c(0x89, 0x50, 0x4e, 0x47)), model_plot)

  utils::write.csv(data.frame(check = "tree_file", ok = TRUE), file.path(paths$tables, "input_validation.csv"), row.names = FALSE)
  utils::write.csv(data.frame(model = "DEC", status = "completed", warning_count = 0L), file.path(paths$tables, "model_run_status.csv"), row.names = FALSE)
  utils::write.csv(data.frame(model = "DEC", AICc = 10, delta_aicc = 0), file.path(paths$tables, "model_comparison.csv"), row.names = FALSE)
  utils::write.csv(data.frame(section = "Summary", display_label = "Best model", answer = "DEC"), file.path(paths$tables, "model_sensitivity.csv"), row.names = FALSE)
  utils::write.csv(data.frame(model = "DEC", location = "branch_top_at_node", node_index = 1L, best_state = "A", best_probability = 0.9), file.path(paths$tables, "node_state_summary.csv"), row.names = FALSE)
  utils::write.csv(data.frame(figure = "model_comparison", format = "png", path = model_plot, status = "created"), file.path(paths$figures, "figure_manifest.csv"), row.names = FALSE)

  result <- load_existing_workflow_result(out)
  state <- new.env(parent = emptyenv())
  state$result <- result
  state$manifest <- result$workflow_manifest

  expect_s3_class(result, "iBGB_workflow_result")
  expect_equal(result$model_run_status$model, "DEC")
  expect_equal(result$model_comparison$model, "DEC")
  expect_equal(result$model_sensitivity_table$answer, "DEC")
  expect_equal(result$standardized_tables$node_state_summary$best_state, "A")
  expect_true(any(result$workflow_manifest$relative_path == "tables/model_comparison.csv"))
  expect_equal(shiny_named_figure_path(state, "model_comparison"), as_path(model_plot))
})

test_that("shiny_summary_table reports workflow status", {
  out <- tempfile("ibgb-shiny-summary-")
  paths <- create_project(out)
  report <- file.path(paths$reports, "summary_report.html")
  bundle <- tempfile(fileext = ".zip")
  writeLines("<html></html>", report)
  writeLines("zip", bundle)

  state <- new.env(parent = emptyenv())
  state$validation <- data.frame(check = c("a", "b"), ok = c(TRUE, TRUE))
  state$model_table <- data.frame(status = c("completed", "planned"), warning_count = c(2L, 0L))
  state$result <- list(project_paths = paths, dry_run = FALSE, validation_failed = FALSE)
  state$report <- report
  state$bundle <- bundle

  summary <- shiny_summary_table(state)

  expect_equal(summary$value[match("Validation", summary$item)], "passed")
  expect_equal(summary$value[match("Run mode", summary$item)], "executed")
  expect_equal(summary$value[match("Completed models", summary$item)], "1 of 2")
  expect_equal(summary$value[match("Warning count", summary$item)], "2")
  expect_equal(summary$value[match("Report", summary$item)], "available")
  expect_equal(summary$value[match("Bundle", summary$item)], "available")
})

test_that("shiny about and citation helpers expose software status", {
  out <- tempfile("ibgb-shiny-about-")
  paths <- create_project(out)
  session_info <- file.path(paths$logs, "session_info.txt")
  citation_log <- file.path(paths$logs, "biogeobears_citation.txt")
  writeLines("session", session_info)
  writeLines("citation", citation_log)

  state <- new.env(parent = emptyenv())
  state$result <- list(project_paths = paths)
  fake_bgb <- list(
    available = TRUE,
    version = "1.1.2",
    path = "C:/R/BioGeoBEARS",
    citation = "BioGeoBEARS citation text",
    install_help = "install help"
  )

  about <- shiny_about_table(state, bgb_check = fake_bgb)
  citation <- shiny_biogeobears_citation_text(fake_bgb)
  missing <- shiny_biogeobears_citation_text(list(
    available = FALSE,
    version = NA_character_,
    path = NA_character_,
    citation = NA_character_,
    install_help = "install BioGeoBEARS"
  ))

  expect_equal(about$value[match("Package", about$item)], "iBiogeobears")
  expect_equal(about$value[match("License", about$item)], "GPL (>= 2)")
  expect_equal(about$value[match("BioGeoBEARS available", about$item)], "yes")
  expect_equal(about$value[match("BioGeoBEARS version", about$item)], "1.1.2")
  expect_equal(about$value[match("BioGeoBEARS citation command", about$item)], "citation(\"BioGeoBEARS\")")
  expect_equal(about$value[match("Session info log", about$item)], as_path(session_info))
  expect_equal(about$value[match("BioGeoBEARS citation log", about$item)], as_path(citation_log))
  expect_match(citation, "BioGeoBEARS citation text", fixed = TRUE)
  expect_match(missing, "BioGeoBEARS is not bundled", fixed = TRUE)
  expect_match(missing, "citation(\"BioGeoBEARS\")", fixed = TRUE)
})

test_that("shiny_run_summary_table handles empty and fitted result states", {
  empty_state <- new.env(parent = emptyenv())
  empty_state$result <- NULL
  empty_state$model_table <- NULL
  empty_state$report <- NULL
  empty_state$bundle <- NULL

  empty_summary <- shiny_run_summary_table(empty_state)
  expect_equal(empty_summary$value[match("Best statistical model", empty_summary$item)], "not available")
  expect_equal(empty_summary$value[match("Output directory", empty_summary$item)], "not available")

  out <- tempfile("ibgb-shiny-run-summary-")
  paths <- create_project(out)
  report <- file.path(paths$reports, "summary_report.html")
  writeLines("<html></html>", report)

  state <- new.env(parent = emptyenv())
  state$result <- list(
    project_paths = paths,
    model_comparison = data.frame(
      model = c("DEC", "DEC+J", "DIVALIKE"),
      has_j = c(FALSE, TRUE, FALSE),
      AICc = c(12, 10, 14),
      delta_aicc = c(2, 0, 4),
      stringsAsFactors = FALSE
    ),
    model_sensitivity_table = data.frame(
      section = "Caution",
      display_label = "Best model includes +J",
      answer = "yes; report sensitivity",
      stringsAsFactors = FALSE
    )
  )
  state$model_table <- data.frame(
    model = c("DEC", "DEC+J", "DIVALIKE"),
    status = c("completed", "completed", "completed"),
    warning_count = c(0L, 2L, 1L),
    stringsAsFactors = FALSE
  )
  state$report <- report
  state$bundle <- NULL

  summary <- shiny_run_summary_table(state)

  expect_equal(summary$value[match("Fitted models", summary$item)], "3 of 3")
  expect_equal(summary$value[match("Failed models", summary$item)], "none")
  expect_equal(summary$value[match("Best statistical model", summary$item)], "DEC+J (delta AICc 0)")
  expect_equal(summary$value[match("Best non-+J model", summary$item)], "DEC (delta AICc 2)")
  expect_equal(summary$value[match("Best +J model", summary$item)], "DEC+J (delta AICc 0)")
  expect_equal(summary$value[match("+J interpretation caution", summary$item)], "yes; report sensitivity")
  expect_equal(summary$value[match("Captured warnings", summary$item)], "3")
  expect_equal(summary$value[match("Report", summary$item)], as_path(report))
  expect_equal(summary$value[match("Output directory", summary$item)], paths$root)

  summary_path <- persist_shiny_run_summary(state)
  copied <- tempfile(fileext = ".csv")
  copy_download_file(resolve_run_summary_file(state), copied)
  summary_csv <- utils::read.csv(summary_path, stringsAsFactors = FALSE)

  expect_equal(summary_path, file.path(paths$tables, "shiny_run_summary.csv"))
  expect_true(file.exists(copied))
  expect_equal(summary_csv$value[match("Best statistical model", summary_csv$item)], "DEC+J (delta AICc 0)")
})

test_that("shiny_run_summary_cards renders readable status cards", {
  testthat::skip_if_not_installed("shiny")

  state <- new.env(parent = emptyenv())
  state$result <- list(
    project_paths = list(root = "results/example_clade", tables = tempfile()),
    model_comparison = data.frame(
      model = c("DEC", "DEC+J"),
      has_j = c(FALSE, TRUE),
      delta_aicc = c(1, 0),
      stringsAsFactors = FALSE
    ),
    model_sensitivity_table = data.frame(
      section = "Caution",
      display_label = "Best model includes +J",
      answer = "yes; report sensitivity",
      stringsAsFactors = FALSE
    )
  )
  state$model_table <- data.frame(status = c("completed", "completed"), warning_count = c(0L, 2L))
  state$report <- NULL
  state$bundle <- NULL

  html <- as.character(shiny_run_summary_cards(state))

  expect_match(html, "ibgb-run-summary-grid", fixed = TRUE)
  expect_match(html, "Best statistical model", fixed = TRUE)
  expect_match(html, "DEC\\+J \\(delta AICc 0\\)")
  expect_match(html, "yes; report sensitivity", fixed = TRUE)
  expect_match(html, "Failed models", fixed = TRUE)
  expect_match(html, "ibgb-run-summary-card warning", fixed = TRUE)
})

test_that("shiny_key_files_table lists common workflow outputs", {
  empty_state <- new.env(parent = emptyenv())
  empty_state$result <- NULL
  empty_state$manifest <- NULL
  empty_state$bundle <- NULL
  empty_state$diagnostic_bundle <- NULL

  empty_files <- shiny_key_files_table(empty_state)
  expect_true(all(empty_files$status == "Missing"))
  expect_equal(empty_files$next_step[match("Report", empty_files$file)], "Click Render report.")
  expect_equal(empty_files$next_step[match("Result bundle", empty_files$file)], "Click Create bundle if missing.")
  expect_equal(empty_files$next_step[match("Diagnostic bundle", empty_files$file)], "Click Create diagnostic bundle.")

  out <- tempfile("ibgb-shiny-key-files-")
  paths <- create_project(out)
  report <- file.path(paths$reports, "summary_report.html")
  bundle <- tempfile(fileext = ".zip")
  diagnostic_bundle <- tempfile(fileext = ".zip")
  writeLines("<html></html>", report)
  writeLines("zip", bundle)
  writeLines("diagnostics", diagnostic_bundle)
  utils::write.csv(data.frame(item = "Best statistical model", value = "DEC"), file.path(paths$tables, "shiny_run_summary.csv"), row.names = FALSE)
  utils::write.csv(data.frame(model = "DEC", delta_aicc = 0), file.path(paths$tables, "model_comparison.csv"), row.names = FALSE)
  utils::write.csv(data.frame(section = "Caution", answer = "not triggered"), file.path(paths$tables, "model_sensitivity.csv"), row.names = FALSE)
  manifest <- create_workflow_manifest(paths$root, write = TRUE)

  state <- new.env(parent = emptyenv())
  state$result <- list(project_paths = paths, workflow_manifest = manifest)
  state$manifest <- manifest
  state$report <- report
  state$bundle <- bundle
  state$diagnostic_bundle <- diagnostic_bundle

  key_files <- shiny_key_files_table(state)

  expect_equal(key_files$status[match("Run summary CSV", key_files$file)], "Available")
  expect_equal(key_files$status[match("Model comparison CSV", key_files$file)], "Available")
  expect_equal(key_files$status[match("+J sensitivity CSV", key_files$file)], "Available")
  expect_equal(key_files$status[match("Workflow manifest CSV", key_files$file)], "Available")
  expect_equal(key_files$status[match("Report", key_files$file)], "Available")
  expect_equal(key_files$status[match("Result bundle", key_files$file)], "Available")
  expect_equal(key_files$status[match("Diagnostic bundle", key_files$file)], "Available")
  expect_equal(key_files$next_step[match("Report", key_files$file)], "")
  expect_equal(key_files$path[match("Report", key_files$file)], as_path(report))
  expect_equal(key_files$path[match("Result bundle", key_files$file)], as_path(bundle))
  expect_equal(key_files$path[match("Diagnostic bundle", key_files$file)], as_path(diagnostic_bundle))
})

test_that("Shiny result helpers expose comparison, sensitivity, and warnings", {
  state <- new.env(parent = emptyenv())
  state$result <- list(
    model_comparison = data.frame(
      model = c("DEC", "DEC+J"),
      model_family = c("DEC", "DEC"),
      has_j = c(FALSE, TRUE),
      AICc = c(10, 9),
      delta_aicc = c(1, 0),
      caution_flag = c("none", "plus_j_supported_check_sensitivity"),
      interpretation_note = c("baseline", "check sensitivity")
    ),
    model_sensitivity_table = data.frame(
      section = "Caution",
      display_label = "Best model includes +J",
      answer = "yes",
      models = "DEC+J",
      model_count = 1L,
      evidence = "delta AICc = 0",
      interpretation_note = "Report sensitivity."
    ),
    standardized_tables = list(
      node_state_summary = data.frame(
        model = "DEC+J",
        location = "branch_top_at_node",
        node_index = 2L,
        node_type = "internal",
        node_label = "node_2",
        best_state = "AB",
        best_probability = 0.8,
        state_count = 4L
      )
    ),
    node_state_sensitivity = data.frame(
      location = "branch_top_at_node",
      node_index = 2L,
      node_type = "internal",
      node_label = "node_2",
      non_j_model = "DEC",
      non_j_state = "A",
      non_j_probability = 0.7,
      plus_j_model = "DEC+J",
      plus_j_state = "AB",
      plus_j_probability = 0.8,
      state_differs = TRUE,
      probability_difference = 0.1,
      probability_difference_abs = 0.1
    )
  )
  state$model_table <- data.frame(
    model = c("DEC", "DEC+J", "BAYAREALIKE"),
    status = c("completed", "completed", "failed"),
    warning_count = c(0L, 2L, 0L),
    warning_messages = c(NA, "optimizer warning", NA),
    error_message = c(NA, NA, "optimizer failed"),
    log_file = c("dec.log", "decj.log", "bayarealike.log")
  )

  comparison <- shiny_model_comparison_table(state)
  sensitivity <- shiny_model_sensitivity_table(state)
  warnings <- shiny_warnings_table(state)
  failed <- shiny_failed_models_table(state)
  node_states <- shiny_node_state_summary_table(state)
  node_sensitivity <- shiny_node_state_sensitivity_table(state)

  expect_equal(comparison$model, c("DEC", "DEC+J"))
  expect_true("interpretation_note" %in% names(comparison))
  expect_equal(sensitivity$display_label, "Best model includes +J")
  expect_equal(warnings$model, c("DEC+J", "BAYAREALIKE"))
  expect_equal(warnings$warning_count, c(2L, 0L))
  expect_equal(warnings$error_message[match("BAYAREALIKE", warnings$model)], "optimizer failed")
  expect_equal(failed$model, "BAYAREALIKE")
  expect_equal(failed$log_file, "bayarealike.log")
  expect_equal(node_states$best_state, "AB")
  expect_equal(node_sensitivity$plus_j_state, "AB")
})

test_that("Shiny failed-model diagnostics summarize errors and logs", {
  state <- new.env(parent = emptyenv())
  state$result <- list(project_paths = list(tables = tempfile(), root = tempfile()))
  state$model_table <- data.frame(
    model = c("DEC", "DIVALIKE+J"),
    status = c("completed", "failed"),
    error_message = c(NA, "BioGeoBEARS optimizer stopped"),
    log_file = c("dec.log", "divalikej.log"),
    warning_count = c(0L, 0L),
    warning_messages = c(NA, NA),
    stringsAsFactors = FALSE
  )

  summary <- shiny_run_summary_table(state)
  failed <- shiny_failed_models_table(state)
  warning_summary <- shiny_warning_summary_table(state)

  expect_equal(summary$value[match("Failed models", summary$item)], "DIVALIKE+J")
  expect_equal(failed$model, "DIVALIKE+J")
  expect_equal(failed$error_message, "BioGeoBEARS optimizer stopped")
  expect_equal(warning_summary$value[match("Failed models", warning_summary$item)], "DIVALIKE+J")
  expect_match(
    warning_summary$value[match("Recommended next step", warning_summary$item)],
    "Inspect failed model error messages",
    fixed = TRUE
  )
})

test_that("Shiny result summaries make model fit, +J sensitivity, and warnings readable", {
  state <- new.env(parent = emptyenv())
  state$result <- list(
    model_comparison = data.frame(
      model = c("DEC", "DEC+J", "DIVALIKE"),
      model_family = c("DEC", "DEC", "DIVALIKE"),
      has_j = c(FALSE, TRUE, FALSE),
      AICc = c(11, 10, 16),
      delta_aicc = c(1, 0, 6),
      caution_flag = c("none", "plus_j_supported_check_sensitivity", "none"),
      interpretation_note = c("baseline", "check sensitivity", "weaker fit"),
      stringsAsFactors = FALSE
    ),
    model_sensitivity_table = data.frame(
      section = c("Sensitivity", "Sensitivity"),
      display_label = c("Best model includes +J", "Automatic biological conclusion"),
      answer = c("yes; report sensitivity", "disabled"),
      evidence = c("DEC+J has delta AICc 0", "auto declaration disabled"),
      interpretation_note = c("Report sensitivity.", "Do not declare a simple answer."),
      stringsAsFactors = FALSE
    )
  )
  state$model_table <- data.frame(
    model = c("DEC", "DEC+J", "DIVALIKE"),
    status = c("completed", "completed", "completed"),
    warning_count = c(0L, 3L, 1L),
    warning_messages = c(NA, "optimizer warning", "boundary warning"),
    log_file = c("dec.log", "decj.log", "divalike.log"),
    stringsAsFactors = FALSE
  )

  fit <- shiny_model_fit_summary_table(state)
  plus_j <- shiny_plus_j_summary_table(state)
  warning_summary <- shiny_warning_summary_table(state)

  expect_equal(fit$value[match("Best statistical model", fit$item)], "DEC+J (delta AICc 0)")
  expect_match(fit$value[match("Models within delta AICc <= 2", fit$item)], "DEC\\+J \\(delta AICc 0\\)")
  expect_match(fit$value[match("Models within delta AICc <= 2", fit$item)], "DEC \\(delta AICc 1\\)")
  expect_equal(plus_j$answer[match("Is +J best or near-best?", plus_j$question)], "yes: DEC+J (delta AICc 0)")
  expect_equal(
    plus_j$answer[match("Recommended next step", plus_j$question)],
    "Report +J sensitivity and compare with the best non-+J model."
  )
  expect_equal(warning_summary$value[match("Captured warnings", warning_summary$item)], "4")
  expect_equal(warning_summary$value[match("Affected models", warning_summary$item)], "DEC+J, DIVALIKE")
  expect_equal(warning_summary$value[match("Highest warning count", warning_summary$item)], "3")
})

test_that("Shiny result helpers can read workflow CSV tables", {
  out <- tempfile("ibgb-shiny-result-tables-")
  paths <- create_project(out)
  utils::write.csv(
    data.frame(model = "DEC", AICc = 10, delta_aicc = 0, interpretation_note = "ok"),
    file.path(paths$tables, "model_comparison.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    data.frame(section = "Summary", display_label = "Best model", answer = "DEC"),
    file.path(paths$tables, "model_sensitivity.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    data.frame(model = "DEC", status = "completed", warning_count = 0L, warning_messages = NA),
    file.path(paths$tables, "model_run_status.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    data.frame(
      model = "DEC",
      location = "branch_top_at_node",
      node_index = 1L,
      node_type = "tip",
      node_label = "sp1",
      best_state = "A",
      best_probability = 0.9,
      state_count = 4L
    ),
    file.path(paths$tables, "node_state_summary.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    data.frame(
      location = "branch_top_at_node",
      node_index = 1L,
      node_type = "tip",
      node_label = "sp1",
      non_j_model = "DEC",
      non_j_state = "A",
      non_j_probability = 0.9,
      plus_j_model = "DEC+J",
      plus_j_state = "B",
      plus_j_probability = 0.8,
      state_differs = TRUE,
      probability_difference = -0.1,
      probability_difference_abs = 0.1
    ),
    file.path(paths$tables, "node_state_sensitivity.csv"),
    row.names = FALSE
  )

  state <- new.env(parent = emptyenv())
  state$result <- list(project_paths = paths)
  state$model_table <- NULL

  expect_equal(shiny_model_comparison_table(state)$model, "DEC")
  expect_equal(shiny_model_sensitivity_table(state)$answer, "DEC")
  expect_equal(shiny_warnings_table(state)$model, "No captured warnings or failed models")
  expect_equal(shiny_node_state_summary_table(state)$best_state, "A")
  expect_equal(shiny_node_state_sensitivity_table(state)$plus_j_state, "B")
})

test_that("table preview helpers discover and read CSV outputs", {
  out <- tempfile("ibgb-shiny-tables-")
  paths <- create_project(out)
  table_path <- file.path(paths$tables, "model_comparison.csv")
  utils::write.csv(data.frame(model = "DEC", AICc = 10), table_path, row.names = FALSE)
  manifest <- data.frame(
    category = "tables",
    relative_path = "tables/model_comparison.csv",
    file_name = "model_comparison.csv",
    extension = "csv",
    stringsAsFactors = FALSE
  )
  result <- list(project_paths = paths)

  choices <- table_preview_choices(result, manifest)
  state <- new.env(parent = emptyenv())
  state$result <- result
  state$manifest <- manifest
  input <- list(table_preview = unname(choices[[1L]]))
  preview <- read_table_preview(input, state)

  expect_equal(names(choices), "tables/model_comparison.csv")
  expect_equal(resolve_table_preview_path(input, state), as_path(table_path))
  expect_equal(preview$model, "DEC")
  expect_equal(preview$AICc, 10)
})

test_that("table status helpers report key CSV availability and next steps", {
  out <- tempfile("ibgb-shiny-table-status-")
  paths <- create_project(out)
  utils::write.csv(data.frame(model = "DEC", AICc = 10), file.path(paths$tables, "model_comparison.csv"), row.names = FALSE)
  utils::write.csv(data.frame(model = "DEC", status = "completed"), file.path(paths$tables, "model_run_status.csv"), row.names = FALSE)
  manifest <- data.frame(
    category = "tables",
    relative_path = c("tables/model_comparison.csv", "tables/node_state_summary.csv"),
    file_name = c("model_comparison.csv", "node_state_summary.csv"),
    extension = "csv",
    stringsAsFactors = FALSE
  )

  state <- new.env(parent = emptyenv())
  state$result <- list(project_paths = paths, workflow_manifest = manifest)
  state$manifest <- manifest

  status <- shiny_table_status_table(state)
  model_row <- status[status$table == "Model comparison", , drop = FALSE]
  node_row <- status[status$table == "Node states", , drop = FALSE]
  root_row <- status[status$table == "Root states", , drop = FALSE]

  expect_equal(model_row$status, "Available")
  expect_equal(model_row$rows, 1L)
  expect_equal(model_row$columns, 2L)
  expect_equal(model_row$missing_reason, "")
  expect_equal(node_row$status, "Missing")
  expect_match(node_row$missing_reason, "Workflow manifest lists this table", fixed = TRUE)
  expect_match(node_row$next_step, "Refresh key files", fixed = TRUE)
  expect_equal(root_row$status, "Missing")
  expect_match(root_row$missing_reason, "Expected CSV was not found", fixed = TRUE)
})

test_that("table preview helpers return empty data when no tables exist", {
  paths <- create_project(tempfile("ibgb-shiny-no-tables-"))
  state <- new.env(parent = emptyenv())
  state$result <- list(project_paths = paths)
  state$manifest <- data.frame()
  input <- list(table_preview = "")

  expect_length(table_preview_choices(state$result, state$manifest), 0L)
  expect_null(resolve_table_preview_path(input, state))
  expect_equal(nrow(read_table_preview(input, state)), 0L)
})

test_that("figure preview helpers discover PNG outputs", {
  out <- tempfile("ibgb-shiny-figures-")
  paths <- create_project(out)
  png_path <- file.path(paths$figures, "model_comparison.png")
  writeBin(as.raw(c(0x89, 0x50, 0x4e, 0x47)), png_path)
  manifest <- data.frame(
    category = "figures",
    relative_path = "figures/model_comparison.png",
    file_name = "model_comparison.png",
    extension = "png",
    stringsAsFactors = FALSE
  )
  result <- list(project_paths = paths)

  choices <- figure_preview_choices(result, manifest)
  state <- new.env(parent = emptyenv())
  state$result <- result
  state$manifest <- manifest
  input <- list(figure_preview = unname(choices[[1L]]))

  expect_equal(names(choices), "figures/model_comparison.png")
  expect_equal(resolve_figure_preview_path(input, state), as_path(png_path))
})

test_that("figure dashboard helpers expose named workflow figures", {
  out <- tempfile("ibgb-shiny-figure-dashboard-")
  paths <- create_project(out)
  png_path <- file.path(paths$figures, "model_comparison.png")
  writeBin(as.raw(c(0x89, 0x50, 0x4e, 0x47)), png_path)

  state <- new.env(parent = emptyenv())
  state$result <- list(
    project_paths = paths,
    figure_manifest = data.frame(
      figure = "model_comparison",
      format = "png",
      path = png_path,
      status = "created",
      stringsAsFactors = FALSE
    )
  )
  state$manifest <- NULL

  dashboard <- shiny_figure_dashboard_table(state)
  image <- shiny_named_figure_image(state, "model_comparison")

  expect_equal(shiny_named_figure_path(state, "model_comparison"), as_path(png_path))
  expect_equal(dashboard$status[match("Model Comparison", dashboard$figure)], "Available")
  expect_equal(dashboard$preview[match("Model Comparison", dashboard$figure)], "Shown below")
  expect_equal(dashboard$status[match("Root State Probabilities", dashboard$figure)], "Missing")
  expect_match(
    dashboard$missing_reason[match("Root State Probabilities", dashboard$figure)],
    "Expected PNG was not found",
    fixed = TRUE
  )
  expect_equal(image$src, as_path(png_path))
  expect_equal(image$contentType, "image/png")
})

test_that("figure dashboard reports failed figure generation with next steps", {
  out <- tempfile("ibgb-shiny-figure-dashboard-failed-")
  paths <- create_project(out)

  state <- new.env(parent = emptyenv())
  state$result <- list(
    project_paths = paths,
    figure_manifest = data.frame(
      figure = "node_state_sensitivity",
      format = "png",
      path = file.path(paths$figures, "node_state_sensitivity.png"),
      status = "failed",
      error_message = "missing node_state_sensitivity.csv",
      stringsAsFactors = FALSE
    )
  )
  state$manifest <- NULL

  dashboard <- shiny_figure_dashboard_table(state)
  row <- dashboard[dashboard$figure == "Node-State Sensitivity", , drop = FALSE]

  expect_equal(row$status, "Failed")
  expect_equal(row$preview, "Not shown")
  expect_match(row$missing_reason, "Figure generation failed: missing node_state_sensitivity.csv", fixed = TRUE)
  expect_match(row$next_step, "Inspect figure_manifest.csv", fixed = TRUE)
})

test_that("figure preview helpers return NULL when no figures exist", {
  paths <- create_project(tempfile("ibgb-shiny-no-figures-"))
  state <- new.env(parent = emptyenv())
  state$result <- list(project_paths = paths)
  state$manifest <- data.frame()
  input <- list(figure_preview = "")

  expect_length(figure_preview_choices(state$result, state$manifest), 0L)
  expect_null(resolve_figure_preview_path(input, state))
})

test_that("Shiny server validates and dry-runs a workflow", {
  testthat::skip_if_not_installed("shiny")

  project <- create_example_project(tempfile("ibgb-shiny-server-"))

  shiny::testServer(iBGB_shiny_server, {
    session$setInputs(
      config_path = project$config,
      output_dir = project$output_dir,
      dry_run = TRUE,
      require_biogeobears = FALSE,
      force = FALSE,
      report_format = "source"
    )

    session$setInputs(validate = 1)
    state <- session$userData$state
    expect_true(all(state$validation$ok))
    expect_true(nrow(state$model_table) > 0L)
    expect_match(state$message, "Validation passed", fixed = TRUE)
    expect_true(any(grepl("Validation: started", state$messages, fixed = TRUE)))
    expect_true(any(grepl("Validation: model plan ready", state$messages, fixed = TRUE)))

    session$setInputs(run = 1)
    expect_s3_class(state$result, "iBGB_workflow_result")
    expect_true(isTRUE(state$result$dry_run))
    expect_true(file.exists(file.path(state$result$project_paths$tables, "workflow_manifest.csv")))
    expect_match(state$message, "Dry run completed", fixed = TRUE)
    expect_true(any(grepl("Workflow: dry run started", state$messages, fixed = TRUE)))
    expect_true(any(grepl("Workflow: validation complete", state$messages, fixed = TRUE)))
    expect_true(any(grepl("Workflow: model status ready", state$messages, fixed = TRUE)))
    expect_true(any(grepl("Workflow: failed models - none", state$messages, fixed = TRUE)))
    expect_true(any(grepl("Workflow: outputs refreshed", state$messages, fixed = TRUE)))
  })
})

test_that("Shiny server uses GUI config editor overrides", {
  testthat::skip_if_not_installed("shiny")

  project <- create_example_project(tempfile("ibgb-shiny-server-editor-"))
  edited_output <- file.path(project$root, "results", "edited_clade")

  shiny::testServer(iBGB_shiny_server, {
    session$setInputs(
      config_path = project$config,
      output_dir = edited_output,
      use_config_editor = TRUE,
      project_name = "edited_clade",
      tree_file = "data/tree.nwk",
      geography_file = "data/geography.csv",
      regions_file = "data/regions.csv",
      max_range_size = "2",
      models_run = "DEC",
      dry_run = TRUE,
      require_biogeobears = FALSE,
      force = FALSE,
      report_format = "source"
    )

    session$setInputs(validate = 1)
    state <- session$userData$state
    expect_true(all(state$validation$ok))
    expect_equal(state$model_table$model, "DEC")

    session$setInputs(run = 1)
    expect_equal(state$result$config$project$name, "edited_clade")
    expect_equal(state$result$config$inputs$max_range_size, 2L)
    expect_equal(state$result$config$models$run, "DEC")
    expect_equal(state$result$project_paths$root, edited_output)
  })
})

test_that("Shiny server creates an example project from the GUI", {
  testthat::skip_if_not_installed("shiny")

  example_dir <- tempfile("ibgb-shiny-created-example-")

  shiny::testServer(iBGB_shiny_server, {
    session$setInputs(example_project_dir = example_dir)
    session$setInputs(create_example = 1)

    state <- session$userData$state
    expect_true(file.exists(file.path(example_dir, "analysis.yml")))
    expect_match(state$message, "Example project: ready", fixed = TRUE)
    expect_true(any(grepl("Example project: started", state$messages, fixed = TRUE)))

    session$setInputs(
      config_path = file.path(example_dir, "analysis.yml"),
      output_dir = file.path(example_dir, "results", "example_clade")
    )
    session$setInputs(validate = 1)

    expect_true(all(state$validation$ok))
    expect_match(state$message, "Validation passed", fixed = TRUE)
  })
})

test_that("Shiny server loads existing result directories", {
  testthat::skip_if_not_installed("shiny")

  paths <- create_project(tempfile("ibgb-shiny-existing-results-"))
  utils::write.csv(data.frame(check = "tree_file", ok = TRUE), file.path(paths$tables, "input_validation.csv"), row.names = FALSE)
  utils::write.csv(data.frame(model = "DEC", status = "completed", warning_count = 0L), file.path(paths$tables, "model_run_status.csv"), row.names = FALSE)
  utils::write.csv(data.frame(model = "DEC", AICc = 10, delta_aicc = 0), file.path(paths$tables, "model_comparison.csv"), row.names = FALSE)

  shiny::testServer(iBGB_shiny_server, {
    session$setInputs(output_dir = paths$root)
    session$setInputs(load_results = 1)

    state <- session$userData$state
    expect_s3_class(state$result, "iBGB_workflow_result")
    expect_equal(state$model_table$model, "DEC")
    expect_equal(state$result$model_comparison$model, "DEC")
    expect_true(any(grepl("Load existing results: started", state$messages, fixed = TRUE)))
    expect_true(any(grepl("Load existing results: ready", state$messages, fixed = TRUE)))
    expect_true(any(state$manifest$relative_path == "tables/model_comparison.csv"))
    expect_true(any(state$manifest$relative_path == "tables/shiny_run_summary.csv"))
    expect_true(file.exists(file.path(paths$tables, "shiny_run_summary.csv")))
    expect_match(state$message, "Load existing results: ready", fixed = TRUE)
  })
})

test_that("Shiny server renders reports and bundles dry-run results", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if(Sys.which("zip") == "", "zip utility is not available")

  project <- create_example_project(tempfile("ibgb-shiny-render-"))

  shiny::testServer(iBGB_shiny_server, {
    session$setInputs(
      config_path = project$config,
      output_dir = project$output_dir,
      dry_run = TRUE,
      require_biogeobears = FALSE,
      force = FALSE,
      report_format = "source"
    )

    session$setInputs(run = 1)
    state <- session$userData$state
    expect_s3_class(state$result, "iBGB_workflow_result")
    expect_true(file.exists(file.path(state$result$project_paths$tables, "shiny_run_summary.csv")))

    session$setInputs(render_report = 1)
    expect_true(file.exists(state$report))
    expect_match(state$message, "Report ready:", fixed = TRUE)
    expect_equal(report_preview_path(state), as_path(state$report))
    expect_equal(shiny_key_files_table(state)$status[match("Report", shiny_key_files_table(state)$file)], "Available")
    expect_true(any(grepl("Report: render started", state$messages, fixed = TRUE)))

    session$setInputs(refresh_key_files = 1)
    expect_true(file.exists(file.path(state$result$project_paths$tables, "shiny_run_summary.csv")))
    expect_true(any(state$manifest$relative_path == "tables/shiny_run_summary.csv"))
    expect_null(state$bundle)
    expect_match(state$message, "Key files refreshed:", fixed = TRUE)
    expect_true(any(grepl("Key files: refresh started", state$messages, fixed = TRUE)))

    session$setInputs(bundle = 1)
    expect_true(file.exists(state$bundle))
    first_bundle <- state$bundle
    expect_match(state$message, "Bundle ready:", fixed = TRUE)
    expect_equal(shiny_key_files_table(state)$status[match("Result bundle", shiny_key_files_table(state)$file)], "Available")
    expect_true(any(grepl("Bundle: refreshing key files", state$messages, fixed = TRUE)))
    expect_true(any(grepl("Bundle: creating archive", state$messages, fixed = TRUE)))

    session$setInputs(bundle = 2)
    expect_equal(state$bundle, first_bundle)
    expect_true(file.exists(state$bundle))
    expect_true(any(grepl("Bundle: using existing archive", state$messages, fixed = TRUE)))

    session$setInputs(diagnostic_bundle = 1)
    expect_true(file.exists(state$diagnostic_bundle))
    expect_match(state$message, "Diagnostic bundle ready:", fixed = TRUE)
    expect_equal(shiny_key_files_table(state)$status[match("Diagnostic bundle", shiny_key_files_table(state)$file)], "Available")
    expect_true(any(grepl("Diagnostics: creating archive", state$messages, fixed = TRUE)))

    copied <- tempfile(fileext = ".zip")
    copy_download_file(resolve_diagnostic_bundle_file(state), copied)
    expect_true(file.exists(copied))
  })
})
