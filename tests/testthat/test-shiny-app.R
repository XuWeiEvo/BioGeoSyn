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
  expect_equal(summary$value[match("Best statistical model", summary$item)], "DEC+J (delta AICc 0)")
  expect_equal(summary$value[match("Best non-+J model", summary$item)], "DEC (delta AICc 2)")
  expect_equal(summary$value[match("Best +J model", summary$item)], "DEC+J (delta AICc 0)")
  expect_equal(summary$value[match("+J interpretation caution", summary$item)], "yes; report sensitivity")
  expect_equal(summary$value[match("Captured warnings", summary$item)], "3")
  expect_equal(summary$value[match("Report", summary$item)], as_path(report))
  expect_equal(summary$value[match("Output directory", summary$item)], paths$root)
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
    model = c("DEC", "DEC+J"),
    status = c("completed", "completed"),
    warning_count = c(0L, 2L),
    warning_messages = c(NA, "optimizer warning"),
    log_file = c("dec.log", "decj.log")
  )

  comparison <- shiny_model_comparison_table(state)
  sensitivity <- shiny_model_sensitivity_table(state)
  warnings <- shiny_warnings_table(state)
  node_states <- shiny_node_state_summary_table(state)
  node_sensitivity <- shiny_node_state_sensitivity_table(state)

  expect_equal(comparison$model, c("DEC", "DEC+J"))
  expect_true("interpretation_note" %in% names(comparison))
  expect_equal(sensitivity$display_label, "Best model includes +J")
  expect_equal(warnings$model, "DEC+J")
  expect_equal(warnings$warning_count, 2L)
  expect_equal(node_states$best_state, "AB")
  expect_equal(node_sensitivity$plus_j_state, "AB")
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
  expect_equal(shiny_warnings_table(state)$model, "No captured warnings")
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
  expect_equal(dashboard$status[match("Model Comparison", dashboard$figure)], "available")
  expect_equal(dashboard$status[match("Root State Probabilities", dashboard$figure)], "not available")
  expect_equal(image$src, as_path(png_path))
  expect_equal(image$contentType, "image/png")
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

    session$setInputs(run = 1)
    expect_s3_class(state$result, "iBGB_workflow_result")
    expect_true(isTRUE(state$result$dry_run))
    expect_true(file.exists(file.path(state$result$project_paths$tables, "workflow_manifest.csv")))
    expect_match(state$message, "Dry run completed", fixed = TRUE)
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
    expect_match(state$message, "Example project:", fixed = TRUE)

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
    expect_true(any(state$manifest$relative_path == "tables/model_comparison.csv"))
    expect_match(state$message, "Loaded existing results:", fixed = TRUE)
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

    session$setInputs(render_report = 1)
    expect_true(file.exists(state$report))
    expect_match(state$message, "Report:", fixed = TRUE)
    expect_equal(report_preview_path(state), as_path(state$report))

    session$setInputs(bundle = 1)
    expect_true(file.exists(state$bundle))
    expect_match(state$message, "Bundle:", fixed = TRUE)
  })
})
