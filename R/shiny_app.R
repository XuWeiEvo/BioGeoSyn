#' Launch the iBiogeobears Shiny application
#'
#' @param config Optional path to an `analysis.yml` file. When omitted, a
#'   complete temporary example project is prepared and loaded automatically.
#' @param output_dir Optional workflow output directory override.
#' @param launch.browser Passed to [shiny::runApp()].
#' @param ... Additional arguments passed to [shiny::runApp()].
#' @return The value returned by [shiny::runApp()].
#' @export
launch_app <- function(config = NULL, output_dir = NULL, launch.browser = TRUE, ...) {
  check_shiny_available()
  app <- create_iBGB_shiny_app(config = config, output_dir = output_dir)
  shiny::runApp(app, launch.browser = launch.browser, ...)
}

create_iBGB_shiny_app <- function(config = NULL, output_dir = NULL) {
  check_shiny_available()

  startup <- prepare_shiny_startup(config, output_dir)
  default_config <- startup$config
  default_output <- startup$output_dir

  shiny::shinyApp(
    ui = iBGB_app_ui(default_config, default_output, startup$example_project_dir),
    server = iBGB_shiny_server
  )
}

shiny_control_section <- function(title, ...) {
  shiny::tags$div(
    class = "ibgb-control-section",
    shiny::tags$div(class = "ibgb-control-title", title),
    ...
  )
}

shiny_collapsible_section <- function(title, ..., open = FALSE) {
  args <- c(
    list(class = "ibgb-collapsible"),
    if (isTRUE(open)) list(open = "open") else list(),
    list(shiny::tags$summary(title)),
    list(...)
  )
  do.call(shiny::tags$details, args)
}

shiny_action_grid <- function(...) {
  shiny::tags$div(class = "ibgb-action-grid", ...)
}

shiny_home_guidance_body <- function() {
  shiny::tagList(
    shiny::uiOutput("home_next_action"),
    shiny::tags$div(class = "ibgb-key-files-title", "\u5f15\u5bfc\u6d41\u7a0b"),
    shiny::tableOutput("guided_workflow_table"),
    shiny_collapsible_section(
      "\u8be6\u7ec6\u72b6\u6001",
      shiny::tableOutput("first_steps_table")
    )
  )
}

shiny_primary_results_body <- function() {
  shiny::tagList(
    shiny::uiOutput("run_summary_cards"),
    shiny::tags$div(
      class = "ibgb-primary-result",
      shiny::tags$h4("1. \u7956\u5148\u5206\u5e03\u91cd\u5efa\u56fe"),
      shiny::tags$p("\u5148\u770b\u6700\u4f73\u7edf\u8ba1\u6a21\u578b\u4e0b\u7684\u7956\u5148\u5206\u5e03\u91cd\u5efa\u56fe\uff0c\u518d\u7ed3\u5408\u6a21\u578b\u6bd4\u8f83\u548c +J \u63d0\u793a\u89e3\u91ca\u3002"),
      shiny::div(class = "ibgb-preview", shiny::imageOutput("primary_figure_node_best"))
    ),
    shiny::tags$div(
      class = "ibgb-primary-result",
      shiny::tags$h4("2. \u6a21\u578b\u6bd4\u8f83\u8868"),
      shiny::tableOutput("primary_model_comparison_table"),
      shiny::div(class = "ibgb-preview", shiny::imageOutput("primary_figure_model_comparison"))
    ),
    shiny::tags$div(
      class = "ibgb-primary-result",
      shiny::tags$h4("3. \u751f\u7269\u5730\u7406\u8fc7\u7a0b\u7efc\u5408"),
      shiny::tags$p("\u628a BSM \u968f\u673a\u6620\u5c04\u7ed3\u679c\u7ffb\u8bd1\u6210\u53ef\u89e3\u91ca\u7684\u751f\u7269\u5730\u7406\u8fc7\u7a0b\uff1a\u5206\u652f\u5f62\u6210\u4e8b\u4ef6\uff08\u539f\u5730/\u540c\u57df\u7269\u79cd\u5f62\u6210\u3001subset sympatry\u3001vicariance\u3001founder-event \u8df3\u8dc3\u7269\u79cd\u5f62\u6210\uff09\u548c\u652f\u7cfb\u5185\u5206\u5e03\u533a\u6f14\u5316\uff08range expansion\u3001\u5c40\u90e8\u706d\u7edd\u3001range switching\uff09\u3002\u4ec5\u5728\u8fd0\u884c BSM \u540e\u663e\u793a\u3002"),
      shiny::div(class = "ibgb-preview", shiny::imageOutput("primary_figure_process_synthesis")),
      shiny::tableOutput("primary_process_summary_table")
    ),
    shiny::tags$div(
      class = "ibgb-primary-result",
      shiny::tags$h4("4. \u4e8b\u4ef6\u7edf\u8ba1\u660e\u7ec6"),
      shiny::tags$p("\u5982\u679c\u8fd0\u884c\u4e86 BSM \u968f\u673a\u6620\u5c04\uff0c\u8fd9\u91cc\u4f18\u5148\u663e\u793a BSM \u7ed3\u679c\uff1b\u5426\u5219\u663e\u793a\u57fa\u4e8e\u6700\u9ad8\u6982\u7387\u7956\u5148\u72b6\u6001\u7684\u786e\u5b9a\u6027\u4e8b\u4ef6\u7edf\u8ba1\u4f5c\u4e3a\u66ff\u4ee3\u3002"),
      shiny::tableOutput("primary_bsm_event_summary_table"),
      shiny::div(class = "ibgb-preview", shiny::imageOutput("primary_figure_bsm_event_summary")),
      shiny::tags$h4("BSM \u4e8b\u4ef6\u65f6\u95f4\u4e0e\u65b9\u5411"),
      shiny::tableOutput("primary_bsm_event_times_table"),
      shiny::tags$h4("BSM \u533a\u57df\u95f4\u6269\u6563\u7f51\u7edc"),
      shiny::tags$p("\u6709\u5411\u7bad\u5934\u4ece\u6e90\u533a\u6307\u5411\u76ee\u6807\u533a\uff0c\u7bad\u5934\u7c97\u7ec6\u53cd\u6620\u5e73\u5747\u6269\u6563\u6b21\u6570\uff08mean count\uff09\u3002"),
      shiny::div(class = "ibgb-preview", shiny::imageOutput("primary_figure_bsm_dispersal_network")),
      shiny::tags$h4("BSM \u6269\u6563\u8def\u7ebf\u70ed\u56fe"),
      shiny::tags$p("\u540c\u4e00\u4efd\u6269\u6563\u6570\u636e\u7684\u70ed\u56fe\u89c6\u89d2\uff1a\u884c=\u6e90\u533a\u3001\u5217=\u76ee\u6807\u533a\uff0c\u989c\u8272\u6df1\u6d45\u53cd\u6620\u5e73\u5747\u6269\u6563\u6b21\u6570\u3002"),
      shiny::div(class = "ibgb-preview", shiny::imageOutput("primary_figure_bsm_dispersal_routes")),
      shiny::tags$h4("\u786e\u5b9a\u6027\u66ff\u4ee3\u4e8b\u4ef6\u7edf\u8ba1"),
      shiny::tableOutput("primary_event_summary_table"),
      shiny::tags$h4("\u6700\u4f18\u6a21\u578b\u4e8b\u4ef6\u65f6\u95f4\u548c\u65b9\u5411"),
      shiny::tags$p("\u65f6\u95f4\u4e3a\u5206\u652f\u4e2d\u70b9\u8fd1\u4f3c\u503c\uff0c\u5355\u4f4d\u4e0e\u8f93\u5165\u6811\u5206\u652f\u957f\u5ea6\u4e00\u81f4\uff1b\u65b9\u5411\u6765\u81ea\u6700\u9ad8\u6982\u7387\u7956\u5148\u72b6\u6001\u7684\u53d8\u5316\u3002"),
      shiny::tableOutput("primary_best_fit_events_table"),
      shiny::div(class = "ibgb-preview", shiny::imageOutput("primary_figure_event_summary"))
    )
  )
}

shiny_constraint_fields <- function() {
  data.frame(
    field = c(
      "times_file",
      "dists_file",
      "dispersal_multipliers_file",
      "areas_allowed_file",
      "areas_adjacency_file",
      "area_of_areas_file"
    ),
    label = c(
      "\u65f6\u95f4\u5206\u5c42\u6587\u4ef6",
      "\u8ddd\u79bb\u6587\u4ef6",
      "\u6269\u6563\u4e58\u6570\u6587\u4ef6",
      "\u5141\u8bb8\u5206\u5e03\u533a\u6587\u4ef6",
      "\u533a\u57df\u76f8\u90bb\u6587\u4ef6",
      "\u533a\u57df\u9762\u79ef\u6587\u4ef6"
    ),
    template = c(
      "times.txt",
      "distances.txt",
      "dispersal_multipliers.txt",
      "areas_allowed.txt",
      "areas_adjacency.txt",
      "area_of_areas.txt"
    ),
    stringsAsFactors = FALSE
  )
}

shiny_wizard_constraint_inputs <- function() {
  fields <- shiny_constraint_fields()
  shiny::tagList(lapply(seq_len(nrow(fields)), function(i) {
    shiny::tags$div(
      class = "ibgb-upload-row",
      shiny::fileInput(
        inputId = paste0("wizard_constraint_", fields$field[[i]]),
        label = fields$label[[i]],
        accept = c(".txt", ".tsv", ".csv")
      ),
      shiny::downloadButton(paste0("download_constraint_", fields$field[[i]]), "\u6a21\u677f")
    )
  }))
}

constraint_template_path <- function(field) {
  fields <- shiny_constraint_fields()
  idx <- match(field, fields$field)
  if (length(idx) != 1L || is.na(idx)) {
    stop("Unknown constraint template: ", paste(field, collapse = ", "), call. = FALSE)
  }
  path <- system.file("example_data", "constraints", fields$template[[idx]], package = "iBiogeobears")
  if (!file.exists(path)) {
    stop("Installed constraint template could not be found: ", fields$template[[idx]], call. = FALSE)
  }
  path
}

shiny_figure_panel <- function(title, output_id) {
  shiny::tags$section(
    shiny::tags$h4(title),
    shiny::imageOutput(output_id)
  )
}

iBGB_shiny_server <- function(input, output, session) {
  state <- shiny::reactiveValues(
        result = NULL,
        validation = NULL,
        model_table = NULL,
        manifest = NULL,
        report = NULL,
        bundle = NULL,
        diagnostic_bundle = NULL,
        installation = check_installation(),
        install_plan = biogeobears_install_plan(),
        message = "\u914d\u7f6e\u5c31\u7eea\u3002\u8fd0\u884c\u524d\u8bf7\u5148\u68c0\u67e5\u8f93\u5165\u3002",
        messages = "\u914d\u7f6e\u5c31\u7eea\u3002\u8fd0\u884c\u524d\u8bf7\u5148\u68c0\u67e5\u8f93\u5165\u3002",
        status_type = "info"
      )
  session$userData$state <- state

      current_config_path <- shiny::reactive({
        resolve_shiny_config_path(input)
      })

      current_output_dir <- shiny::reactive({
        value <- trimws(input$output_dir %||% "")
        if (nzchar(value)) value else NULL
      })

      shiny::observeEvent(input$choose_output_dir, {
        run_app_action(state, {
          selected <- choose_output_directory(current_output_dir())
          if (!is.null(selected) && nzchar(selected)) {
            shiny::updateTextInput(session, "output_dir", value = selected)
            append_app_message(state, paste("Output directory selected:", selected))
          }
        })
      })

      current_config <- shiny::reactive({
        cfg <- apply_shiny_wizard_overrides(read_config(current_config_path()), input)
        apply_shiny_config_overrides(
          cfg,
          input = input,
          output_dir = current_output_dir()
        )
      })

      current_workflow_config_path <- shiny::reactive({
        write_shiny_workflow_config(current_config(), source_config = current_config_path())
      })

      current_input_summary <- shiny::reactive({
        tryCatch(summarize_input_data(current_config()), error = function(e) NULL)
      })

      shiny::observeEvent(input$refresh_setup, {
        run_app_action(state, {
          state$installation <- check_installation()
          state$install_plan <- biogeobears_install_plan()
          append_app_message(state, "Setup checks refreshed.")
        })
      })

      shiny::observeEvent(input$show_install_plan, {
        run_app_action(state, {
          state$install_plan <- biogeobears_install_plan()
          missing_count <- sum(state$install_plan$status != "Ready")
          append_app_message(
            state,
            paste("BioGeoBEARS install plan refreshed:", missing_count, "package(s) need action.")
          )
        })
      })

      shiny::observeEvent(input$open_user_guide, {
        run_app_action(state, {
          guide <- open_user_guide(browse = TRUE, language = "zh-CN")
          append_app_message(state, paste("User guide:", guide))
        })
      })

      shiny::observeEvent(input$install_biogeobears, {
        shiny::showModal(biogeobears_install_modal())
      })

      shiny::observeEvent(input$retry_failed_only, {
        if (isTRUE(input$retry_failed_only)) {
          shiny::updateCheckboxInput(session, "resume_completed_models", value = TRUE)
        }
      })

      shiny::observeEvent(input$confirm_install_biogeobears, {
        shiny::removeModal()
        run_app_action(state, {
          shiny::withProgress(message = "Installing BioGeoBEARS", value = 0, {
            append_app_message(
              state,
              "BioGeoBEARS installation started from CRAN and nmatzke/BioGeoBEARS."
            )
            installation_result <- install_biogeobears(execute = TRUE)
            shiny::incProgress(0.9)
            state$installation <- check_installation()
            state$install_plan <- installation_result$plan
            append_app_message(
              state,
              paste("BioGeoBEARS installation ready:", installation_result$biogeobears$version)
            )
            shiny::incProgress(0.1)
          })
        })
      })

      shiny::observeEvent(input$validate, {
        run_app_action(state, {
          shiny::withProgress(message = "Validating", value = 0, {
          append_app_stage(state, "Validation", "started", current_config_path())
          cfg <- current_config()
          state$validation <- validate_inputs(cfg)
          state$model_table <- planned_model_table(cfg)
          append_app_stage(state, "Validation", "model plan ready", paste(nrow(state$model_table), "model(s)"))
          append_app_message(state, if (all(state$validation$ok)) "Validation passed." else "Validation failed.")
          shiny::incProgress(1)
          })
        })
      })

      shiny::observeEvent(input$run, {
        run_app_action(state, {
          shiny::withProgress(message = "Running workflow", value = 0, {
          append_app_stage(
            state,
            "Workflow",
            if (isTRUE(input$dry_run)) "dry run started" else "real run started",
            current_output_dir() %||% "configured output directory"
          )
          result <- run_workflow(
            config = current_workflow_config_path(),
            output_dir = NULL,
            dry_run = isTRUE(input$dry_run),
            require_biogeobears = isTRUE(input$require_biogeobears),
            force = isTRUE(input$force),
            resume_completed_models = isTRUE(input$resume_completed_models %||% TRUE),
            retry_failed_only = isTRUE(input$retry_failed_only)
          )
          state$result <- result
          state$validation <- result$validation
          state$model_table <- result$model_run_status
          state$manifest <- result$workflow_manifest
          append_app_stage(state, "Workflow", "validation complete", workflow_validation_label(result$validation))
          append_app_stage(state, "Workflow", "model status ready", workflow_model_status_label(result$model_run_status))
          append_app_stage(state, "Workflow", "model actions", workflow_model_action_label(result$model_run_status))
          append_app_stage(state, "Workflow", "failed models", workflow_failed_models_label(result$model_run_status))
          refresh_shiny_result_exports(session, state)
          append_app_stage(state, "Workflow", "outputs refreshed", result$project_paths$root)
          append_app_message(state, if (isTRUE(result$dry_run)) "Dry run completed." else "Workflow completed.")
          shiny::incProgress(1)
          })
        })
      })

      shiny::observeEvent(input$render_report, {
        run_app_action(state, {
          require_workflow_result(state$result)
          shiny::withProgress(message = "Rendering report", value = 0, {
          append_app_stage(state, "Report", "render started", state$result$project_paths$root)
          report <- render_report(state$result, format = "html")
          state$report <- report
          refresh_shiny_result_exports(session, state)
          append_app_message(state, paste("Report ready:", report))
          shiny::incProgress(1)
          })
        })
      })

      shiny::observeEvent(input$bundle, {
        run_app_action(state, {
          require_workflow_result(state$result)
          shiny::withProgress(message = "Bundling results", value = 0, {
          append_app_stage(state, "Bundle", "refreshing key files", state$result$project_paths$root)
          refresh_shiny_result_exports(session, state)
          if (is.null(state$bundle) || !file.exists(state$bundle)) {
            append_app_stage(state, "Bundle", "creating archive", state$result$project_paths$root)
            state$bundle <- bundle_results(state$result, overwrite = TRUE)
            state$manifest <- create_workflow_manifest(state$result, write = TRUE)
            update_table_preview_choices(session, state)
            update_figure_preview_choices(session, state)
          } else {
            append_app_stage(state, "Bundle", "using existing archive", state$bundle)
          }
          append_app_message(state, paste("Bundle ready:", state$bundle))
          shiny::incProgress(1)
          })
        })
      })

      shiny::observeEvent(input$diagnostic_bundle, {
        run_app_action(state, {
          require_workflow_result(state$result)
          shiny::withProgress(message = "Bundling diagnostics", value = 0, {
          append_app_stage(state, "Diagnostics", "refreshing key files", state$result$project_paths$root)
          refresh_shiny_result_exports(session, state)
          append_app_stage(state, "Diagnostics", "creating archive", state$result$project_paths$root)
          state$diagnostic_bundle <- bundle_diagnostics(state$result, overwrite = TRUE)
          append_app_message(state, paste("Diagnostic bundle ready:", state$diagnostic_bundle))
          shiny::incProgress(1)
          })
        })
      })

      shiny::observeEvent(input$refresh_key_files, {
        run_app_action(state, {
          require_workflow_result(state$result)
          shiny::withProgress(message = "Refreshing key files", value = 0, {
          append_app_stage(state, "Key files", "refresh started", state$result$project_paths$root)
          refresh_shiny_result_exports(session, state)
          append_app_message(state, "Key files refreshed: workflow manifest, run summary, table previews, and figure previews.")
          shiny::incProgress(1)
          })
        })
      })

      shiny::observeEvent(input$open_report, {
        run_app_action(state, {
          report <- resolve_report_file(state)
          utils::browseURL(report)
          append_app_message(state, paste("Opened report:", report))
        })
      })

      output$status <- shiny::renderUI({
        shiny::tags$div(class = paste("ibgb-status", state$status_type), state$message)
      })

      output$summary_table <- shiny::renderTable({
        shiny_summary_table(state)
      }, striped = TRUE, bordered = TRUE, na = "")

      output$first_steps_table <- shiny::renderTable({
        shiny_first_steps_table(
          state,
          config_path = tryCatch(current_config_path(), error = function(e) NULL),
          output_dir = current_output_dir(),
          dry_run = isTRUE(input$dry_run),
          require_biogeobears = isTRUE(input$require_biogeobears)
        )
      }, striped = TRUE, bordered = TRUE, na = "")

      output$guided_workflow_table <- shiny::renderTable({
        shiny_guided_workflow_table(
          state,
          start_choice = input$workflow_start_choice %||% "example",
          config_path = tryCatch(current_config_path(), error = function(e) NULL),
          output_dir = current_output_dir(),
          dry_run = isTRUE(input$dry_run),
          require_biogeobears = isTRUE(input$require_biogeobears)
        )
      }, striped = TRUE, bordered = TRUE, na = "")

      output$home_next_action <- shiny::renderUI({
        workflow <- shiny_guided_workflow_table(
          state,
          start_choice = input$workflow_start_choice %||% "example",
          config_path = tryCatch(current_config_path(), error = function(e) NULL),
          output_dir = current_output_dir(),
          dry_run = isTRUE(input$dry_run),
          require_biogeobears = isTRUE(input$require_biogeobears)
        )
        shiny_home_next_action(workflow)
      })

      output$data_overview_table <- shiny::renderTable({
        shiny_data_overview_table(current_input_summary())
      }, striped = TRUE, bordered = TRUE, na = "")

      output$region_occupancy_table <- shiny::renderTable({
        shiny_region_occupancy_table(current_input_summary())
      }, striped = TRUE, bordered = TRUE, na = "")

      output$range_size_table <- shiny::renderTable({
        shiny_range_size_table(current_input_summary())
      }, striped = TRUE, bordered = TRUE, na = "")

      output$installation_table <- shiny::renderTable({
        shiny_installation_table(state$installation)
      }, striped = TRUE, bordered = TRUE, na = "")

      output$biogeobears_install_plan_table <- shiny::renderTable({
        shiny_biogeobears_install_plan(state$install_plan)
      }, striped = TRUE, bordered = TRUE, na = "")

      output$run_summary_table <- shiny::renderTable({
        shiny_run_summary_table(state)
      }, striped = TRUE, bordered = TRUE, na = "")

      output$run_summary_cards <- shiny::renderUI({
        shiny_run_summary_cards(state)
      })

      output$key_files_table <- shiny::renderTable({
        shiny_key_files_table(state)
      }, striped = TRUE, bordered = TRUE, na = "")

      output$validation_table <- shiny::renderTable({
        shiny_validation_table(state$validation)
      }, striped = TRUE, bordered = TRUE, na = "")

      output$model_table <- shiny::renderTable({
        table_head(state$model_table, 20L)
      }, striped = TRUE, bordered = TRUE, na = "")

      output$failed_models_table <- shiny::renderTable({
        shiny_failed_models_table(state)
      }, striped = TRUE, bordered = TRUE, na = "")

      output$model_fit_summary_table <- shiny::renderTable({
        shiny_model_fit_summary_table(state)
      }, striped = TRUE, bordered = TRUE, na = "")

      output$model_comparison_table <- shiny::renderTable({
        table_head(shiny_model_comparison_table(state), 30L)
      }, striped = TRUE, bordered = TRUE, na = "")

      output$primary_model_comparison_table <- shiny::renderTable({
        table_head(shiny_primary_model_comparison_table(state), 12L)
      }, striped = TRUE, bordered = TRUE, na = "")

      output$plus_j_summary_table <- shiny::renderTable({
        shiny_plus_j_summary_table(state)
      }, striped = TRUE, bordered = TRUE, na = "")

      output$model_sensitivity_table <- shiny::renderTable({
        table_head(shiny_model_sensitivity_table(state), 30L)
      }, striped = TRUE, bordered = TRUE, na = "")

      output$warning_summary_table <- shiny::renderTable({
        shiny_warning_summary_table(state)
      }, striped = TRUE, bordered = TRUE, na = "")

      output$warnings_table <- shiny::renderTable({
        table_head(shiny_warnings_table(state), 30L)
      }, striped = TRUE, bordered = TRUE, na = "")

      output$node_state_summary_table <- shiny::renderTable({
        table_head(shiny_node_state_summary_table(state), 50L)
      }, striped = TRUE, bordered = TRUE, na = "")

      output$node_state_sensitivity_table <- shiny::renderTable({
        table_head(shiny_node_state_sensitivity_table(state), 50L)
      }, striped = TRUE, bordered = TRUE, na = "")

      output$primary_event_summary_table <- shiny::renderTable({
        table_head(shiny_primary_event_summary_table(state), 20L)
      }, striped = TRUE, bordered = TRUE, na = "")

      output$primary_best_fit_events_table <- shiny::renderTable({
        table_head(shiny_primary_best_fit_events_table(state), 20L)
      }, striped = TRUE, bordered = TRUE, na = "")

      output$range_change_events_table <- shiny::renderTable({
        table_head(shiny_range_change_events_table(state), 50L)
      }, striped = TRUE, bordered = TRUE, na = "")

      output$best_fit_events_table <- shiny::renderTable({
        table_head(shiny_best_fit_events_table(state), 80L)
      }, striped = TRUE, bordered = TRUE, na = "")

      output$bsm_run_status_table <- shiny::renderTable({
        table_head(shiny_bsm_run_status_table(state), 20L)
      }, striped = TRUE, bordered = TRUE, na = "")

      output$bsm_qc_table <- shiny::renderTable({
        table_head(shiny_bsm_qc_table(state), 30L)
      }, striped = TRUE, bordered = TRUE, na = "")

      output$bsm_event_summary_table <- shiny::renderTable({
        table_head(shiny_bsm_event_summary_table(state), 30L)
      }, striped = TRUE, bordered = TRUE, na = "")

      output$bsm_dispersal_routes_table <- shiny::renderTable({
        table_head(shiny_bsm_dispersal_routes_table(state), 50L)
      }, striped = TRUE, bordered = TRUE, na = "")

      output$bsm_event_times_table <- shiny::renderTable({
        table_head(shiny_bsm_event_times_table(state), 80L)
      }, striped = TRUE, bordered = TRUE, na = "")

      output$primary_bsm_event_summary_table <- shiny::renderTable({
        table_head(shiny_primary_bsm_event_summary_table(state), 20L)
      }, striped = TRUE, bordered = TRUE, na = "")

      output$primary_bsm_event_times_table <- shiny::renderTable({
        table_head(shiny_primary_bsm_event_times_table(state), 20L)
      }, striped = TRUE, bordered = TRUE, na = "")

      output$primary_process_summary_table <- shiny::renderTable({
        table_head(shiny_biogeographic_process_summary_table(state), 20L)
      }, striped = TRUE, bordered = TRUE, na = "")

      cross_clade_combined <- shiny::reactive({
        files <- input$cross_clade_files
        if (is.null(files) || nrow(files) == 0L) {
          return(NULL)
        }
        clade_names <- tools::file_path_sans_ext(files$name)
        combine_process_rates_across_clades(files$datapath, clade_names = clade_names)
      })

      output$cross_clade_status <- shiny::renderUI({
        combined <- cross_clade_combined()
        if (is.null(combined)) {
          return(shiny::tags$div(class = "ibgb-home-note", "\u5c1a\u672a\u4e0a\u4f20\u6587\u4ef6\u3002"))
        }
        if (nrow(combined) == 0L) {
          return(shiny::tags$div(
            class = "ibgb-status error",
            "\u4e0a\u4f20\u7684\u6587\u4ef6\u91cc\u6ca1\u6709\u53ef\u7528\u7684\u901f\u7387\u6570\u636e\u3002\u8bf7\u786e\u8ba4\u4e0a\u4f20\u7684\u662f process_rates_through_time.csv\u3002"
          ))
        }
        n <- length(unique(combined$clade))
        shiny::tags$div(class = "ibgb-status info", paste0("\u5df2\u6574\u5408 ", n, " \u4e2a\u7c7b\u7fa4\u3002"))
      })

      output$cross_clade_plot <- shiny::renderImage({
        combined <- cross_clade_combined()
        shiny::req(combined)
        shiny::validate(shiny::need(nrow(combined) > 0, "\u65e0\u53ef\u7ed8\u5236\u7684\u6570\u636e\u3002"))
        path <- tempfile(fileext = ".png")
        ggplot2::ggsave(
          path, plot_process_rates_across_clades(combined),
          width = 8.6, height = 5.2, dpi = 150
        )
        list(src = path, contentType = "image/png", width = "100%")
      }, deleteFile = TRUE)

      output$cross_clade_table <- shiny::renderTable({
        combined <- cross_clade_combined()
        if (is.null(combined) || nrow(combined) == 0L) {
          return(data.frame())
        }
        cols <- intersect(
          c("clade", "process_label", "process_group", "time_bin", "bin_midpoint", "mean_count", "ci_lower", "ci_upper", "rate"),
          names(combined)
        )
        table_head(combined[, cols, drop = FALSE], 60L)
      }, striped = TRUE, bordered = TRUE, na = "")

      output$download_cross_clade <- shiny::downloadHandler(
        filename = function() "cross_clade_process_rates.csv",
        content = function(file) {
          combined <- cross_clade_combined()
          if (is.null(combined) || nrow(combined) == 0L) {
            stop("Upload clade rate files before downloading the combined result.", call. = FALSE)
          }
          utils::write.csv(combined, file, row.names = FALSE, na = "")
        }
      )

      cross_clade_region_combined <- shiny::reactive({
        files <- input$cross_clade_region_files
        if (is.null(files) || nrow(files) == 0L) {
          return(NULL)
        }
        clade_names <- tools::file_path_sans_ext(files$name)
        combine_region_process_rates_across_clades(files$datapath, clade_names = clade_names)
      })

      output$cross_clade_region_status <- shiny::renderUI({
        combined <- cross_clade_region_combined()
        if (is.null(combined)) {
          return(shiny::tags$div(class = "ibgb-home-note", "\u5c1a\u672a\u4e0a\u4f20\u6587\u4ef6\u3002"))
        }
        if (nrow(combined) == 0L) {
          return(shiny::tags$div(
            class = "ibgb-status error",
            "\u4e0a\u4f20\u7684\u6587\u4ef6\u91cc\u6ca1\u6709\u53ef\u7528\u7684\u5206\u533a\u57df\u901f\u7387\u6570\u636e\u3002\u8bf7\u786e\u8ba4\u4e0a\u4f20\u7684\u662f region_process_rates_through_time.csv\u3002"
          ))
        }
        n <- length(unique(combined$clade))
        r <- length(unique(combined$region))
        shiny::tags$div(class = "ibgb-status info", paste0("\u5df2\u6574\u5408 ", n, " \u4e2a\u7c7b\u7fa4\u3001", r, " \u4e2a\u5730\u533a\u3002"))
      })

      output$cross_clade_region_plot <- shiny::renderImage({
        combined <- cross_clade_region_combined()
        shiny::req(combined)
        shiny::validate(shiny::need(nrow(combined) > 0, "\u65e0\u53ef\u7ed8\u5236\u7684\u6570\u636e\u3002"))
        path <- tempfile(fileext = ".png")
        ggplot2::ggsave(
          path, plot_region_process_rates_across_clades(combined),
          width = 9.0, height = 6.0, dpi = 150
        )
        list(src = path, contentType = "image/png", width = "100%")
      }, deleteFile = TRUE)

      output$cross_clade_region_table <- shiny::renderTable({
        combined <- cross_clade_region_combined()
        if (is.null(combined) || nrow(combined) == 0L) {
          return(data.frame())
        }
        cols <- intersect(
          c("clade", "region", "process_label", "time_bin", "bin_midpoint", "mean_count", "ci_lower", "ci_upper"),
          names(combined)
        )
        table_head(combined[, cols, drop = FALSE], 60L)
      }, striped = TRUE, bordered = TRUE, na = "")

      output$download_cross_clade_region <- shiny::downloadHandler(
        filename = function() "cross_clade_region_process_rates.csv",
        content = function(file) {
          combined <- cross_clade_region_combined()
          if (is.null(combined) || nrow(combined) == 0L) {
            stop("Upload clade region-rate files before downloading the combined result.", call. = FALSE)
          }
          utils::write.csv(combined, file, row.names = FALSE, na = "")
        }
      )

      output$manifest_table <- shiny::renderTable({
        table_head(state$manifest, 30L)
      }, striped = TRUE, bordered = TRUE, na = "")

      output$output_file_legend_table <- shiny::renderTable({
        output_file_legend()
      }, striped = TRUE, bordered = TRUE, na = "")

      output$paths_text <- shiny::renderText({
        if (is.null(state$result)) {
          return("")
        }
        paste(utils::capture.output(utils::str(state$result$project_paths)), collapse = "\n")
      })

      output$messages_text <- shiny::renderText({
        paste(state$messages, collapse = "\n")
      })

      output$report_path_text <- shiny::renderText({
        path <- report_preview_path(state)
        if (is.null(path)) {
          "No report has been rendered yet."
        } else {
          path
        }
      })

      output$figure_dashboard_table <- shiny::renderTable({
        shiny_figure_dashboard_table(state)
      }, striped = TRUE, bordered = TRUE, na = "")

      output$figure_model_comparison <- shiny::renderImage({
        shiny_named_figure_image(state, "model_comparison")
      }, deleteFile = FALSE)

      output$primary_figure_model_comparison <- shiny::renderImage({
        shiny_named_figure_image(state, "model_comparison")
      }, deleteFile = FALSE)

      output$figure_root_states <- shiny::renderImage({
        shiny_named_figure_image(state, "root_state_probabilities")
      }, deleteFile = FALSE)

      output$figure_node_best <- shiny::renderImage({
        shiny_named_figure_image(state, "node_state_summary_best_model")
      }, deleteFile = FALSE)

      output$primary_figure_node_best <- shiny::renderImage({
        shiny_named_figure_image(state, "node_state_summary_best_model")
      }, deleteFile = FALSE)

      output$figure_node_non_j <- shiny::renderImage({
        shiny_named_figure_image(state, "node_state_summary_best_non_j")
      }, deleteFile = FALSE)

      output$figure_node_plus_j <- shiny::renderImage({
        shiny_named_figure_image(state, "node_state_summary_best_plus_j")
      }, deleteFile = FALSE)

      output$figure_node_sensitivity <- shiny::renderImage({
        shiny_named_figure_image(state, "node_state_sensitivity")
      }, deleteFile = FALSE)

      output$figure_event_summary <- shiny::renderImage({
        shiny_named_figure_image(state, "event_summary")
      }, deleteFile = FALSE)

      output$primary_figure_event_summary <- shiny::renderImage({
        shiny_named_figure_image(state, "event_summary")
      }, deleteFile = FALSE)

      output$figure_bsm_event_summary <- shiny::renderImage({
        shiny_named_figure_image(state, "bsm_event_summary")
      }, deleteFile = FALSE)

      output$primary_figure_bsm_event_summary <- shiny::renderImage({
        shiny_named_figure_image(state, "bsm_event_summary")
      }, deleteFile = FALSE)

      output$primary_figure_process_synthesis <- shiny::renderImage({
        shiny_named_figure_image(state, "biogeographic_process_synthesis")
      }, deleteFile = FALSE)

      output$figure_bsm_event_times <- shiny::renderImage({
        shiny_named_figure_image(state, "bsm_event_times")
      }, deleteFile = FALSE)

      output$figure_bsm_dispersal_routes <- shiny::renderImage({
        shiny_named_figure_image(state, "bsm_dispersal_routes")
      }, deleteFile = FALSE)

      output$figure_bsm_dispersal_network <- shiny::renderImage({
        shiny_named_figure_image(state, "bsm_dispersal_network")
      }, deleteFile = FALSE)

      output$primary_figure_bsm_dispersal_network <- shiny::renderImage({
        shiny_named_figure_image(state, "bsm_dispersal_network")
      }, deleteFile = FALSE)

      output$primary_figure_bsm_dispersal_routes <- shiny::renderImage({
        shiny_named_figure_image(state, "bsm_dispersal_routes")
      }, deleteFile = FALSE)

      output$table_preview_output <- shiny::renderTable({
        table_head(read_table_preview(input, state), 50L)
      }, striped = TRUE, bordered = TRUE, na = "")

      output$table_status_table <- shiny::renderTable({
        shiny_table_status_table(state)
      }, striped = TRUE, bordered = TRUE, na = "")

      output$table_path_text <- shiny::renderText({
        path <- resolve_table_preview_path(input, state)
        if (is.null(path)) {
          "No CSV table is available for preview."
        } else {
          path
        }
      })

      output$figure_image <- shiny::renderImage({
        path <- resolve_figure_preview_path(input, state)
        if (is.null(path)) {
          return(NULL)
        }
        list(
          src = path,
          contentType = "image/png",
          alt = basename(path),
          width = "100%"
        )
      }, deleteFile = FALSE)

      output$figure_path_text <- shiny::renderText({
        path <- resolve_figure_preview_path(input, state)
        if (is.null(path)) {
          "No PNG figure is available for preview."
        } else {
          path
        }
      })

      output$about_table <- shiny::renderTable({
        shiny_about_table(state)
      }, striped = TRUE, bordered = TRUE, na = "")

      output$report_environment_table <- shiny::renderTable({
        shiny_report_environment_table()
      }, striped = TRUE, bordered = TRUE, na = "")

      output$citation_text <- shiny::renderText({
        shiny_biogeobears_citation_text()
      })

      output$download_report <- shiny::downloadHandler(
        filename = function() {
          basename(resolve_report_file(state))
        },
        content = function(file) {
          src <- resolve_report_file(state)
          copy_download_file(src, file)
        }
      )

      output$download_run_summary <- shiny::downloadHandler(
        filename = function() {
          basename(resolve_run_summary_file(state))
        },
        content = function(file) {
          src <- resolve_run_summary_file(state)
          copy_download_file(src, file)
        }
      )

      output$download_bundle <- shiny::downloadHandler(
        filename = function() {
          basename(resolve_bundle_file(state))
        },
        content = function(file) {
          src <- resolve_bundle_file(state)
          copy_download_file(src, file)
        }
      )

      output$download_diagnostic_bundle <- shiny::downloadHandler(
        filename = function() {
          basename(resolve_diagnostic_bundle_file(state))
        },
        content = function(file) {
          src <- resolve_diagnostic_bundle_file(state)
          copy_download_file(src, file)
        }
      )

      output$download_tree_template <- shiny::downloadHandler(
        filename = function() "tree_template.nwk",
        content = function(file) {
          copy_download_file(input_template_path("tree"), file)
        }
      )

      output$download_geography_template <- shiny::downloadHandler(
        filename = function() "geography_template.csv",
        content = function(file) {
          copy_download_file(input_template_path("geography"), file)
        }
      )

      output$download_regions_template <- shiny::downloadHandler(
        filename = function() "regions_template.csv",
        content = function(file) {
          copy_download_file(input_template_path("regions"), file)
        }
      )

      for (constraint_field in shiny_constraint_fields()$field) {
        local({
          field <- constraint_field
          fields <- shiny_constraint_fields()
          template_name <- fields$template[fields$field == field]
          output[[paste0("download_constraint_", field)]] <- shiny::downloadHandler(
            filename = function() template_name,
            content = function(file) {
              copy_download_file(constraint_template_path(field), file)
            }
          )
        })
      }
}

shiny_installation_table <- function(checks = check_installation()) {
  if (is.null(checks) || nrow(checks) == 0L) {
    return(data.frame())
  }
  out <- checks
  names(out) <- c("Component", "Required for", "Required", "Status", "Version", "Next step")
  out
}

shiny_biogeobears_install_plan <- function(plan = biogeobears_install_plan()) {
  if (is.null(plan) || nrow(plan) == 0L) {
    return(data.frame())
  }
  out <- plan[order(plan$status == "Ready", plan$package), , drop = FALSE]
  row.names(out) <- NULL
  names(out) <- c("Package", "Source", "Status", "Version", "Next step")
  out
}

biogeobears_install_modal <- function() {
  shiny::modalDialog(
    title = "Install BioGeoBEARS",
    shiny::tags$p(
      "This will install missing CRAN dependencies and BioGeoBEARS from ",
      shiny::tags$code("nmatzke/BioGeoBEARS"),
      " into the first writable R library."
    ),
    shiny::tags$p("The installation requires internet access and may take several minutes."),
    footer = shiny::tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton("confirm_install_biogeobears", "Install")
    ),
    easyClose = FALSE
  )
}

shiny_validation_table <- function(validation) {
  if (is.null(validation) || nrow(validation) == 0L) {
    return(data.frame())
  }
  if (!all(c("label", "status", "next_step") %in% names(validation))) {
    validation <- format_validation_results(validation)
  }
  out <- validation[c("label", "status", "detail", "next_step")]
  names(out) <- c("Check", "Status", "Detail", "How to fix")
  out
}

shiny_data_overview_table <- function(summary) {
  if (is.null(summary)) {
    return(data.frame())
  }
  items <- character(0)
  values <- character(0)
  add <- function(item, value) {
    items[[length(items) + 1L]] <<- item
    values[[length(values) + 1L]] <<- as.character(value)
  }
  tr <- summary$tree
  if (!is.null(tr)) {
    add("\u7cfb\u7edf\u6811 tip \u6570", tr$n_tips)
    if (isTRUE(tr$has_branch_lengths) && !is.na(tr$root_age)) {
      add("\u6811\u6839\u5e74\u9f84\uff08\u6811\u9ad8\uff09", formatC(tr$root_age, format = "fg", digits = 3))
    }
    if (!is.na(tr$is_ultrametric)) {
      add("\u8d85\u5ea6\u91cf\u6811", if (isTRUE(tr$is_ultrametric)) "\u662f" else "\u5426")
    }
  }
  g <- summary$geography
  if (!is.null(g)) {
    add("\u5730\u533a\u6570", g$n_areas)
    add("\u7269\u79cd\u6570", g$n_species)
    if (!is.na(g$max_range_size_setting)) {
      add("\u6700\u5927\u5206\u5e03\u533a\u6570\u91cf\uff08\u8bbe\u5b9a\uff09", g$max_range_size_setting)
    }
    add("\u89c2\u6d4b\u5230\u7684\u6700\u5927\u5206\u5e03\u533a\u6570", g$max_range_size_observed)
    add("\u5e73\u5747\u5206\u5e03\u533a\u6570", formatC(g$mean_range_size, format = "fg", digits = 3))
    add("\u5e7f\u5e03\u79cd\uff08\u5206\u5e03\u533a >1\uff09", g$widespread_species)
  }
  tm <- summary$taxon_match
  if (!is.null(tm)) {
    add(
      "\u6811\u4e0e\u5206\u5e03\u540d\u79f0\u5339\u914d",
      if (isTRUE(tm$all_match)) {
        "\u662f"
      } else {
        paste0(
          "\u5426\uff08\u5206\u5e03\u7f3a ", length(tm$missing_from_geography),
          "\uff0c\u6811\u7f3a ", length(tm$missing_from_tree), "\uff09"
        )
      }
    )
  }
  if (length(items) == 0L) {
    return(data.frame())
  }
  out <- data.frame(item = items, value = values, stringsAsFactors = FALSE)
  names(out) <- c("\u9879\u76ee", "\u6570\u503c")
  out
}

shiny_region_occupancy_table <- function(summary) {
  if (is.null(summary)) {
    return(data.frame())
  }
  occ <- summary$region_occupancy
  if (is.null(occ) || nrow(occ) == 0L) {
    return(data.frame())
  }
  out <- data.frame(
    region = occ$region,
    label = occ$label,
    n_species = occ$n_species,
    n_endemic = occ$n_endemic,
    proportion = paste0(formatC(100 * occ$proportion, format = "f", digits = 1), "%"),
    stringsAsFactors = FALSE
  )
  names(out) <- c("\u5730\u533a", "\u540d\u79f0", "\u7269\u79cd\u6570", "\u7279\u6709\u79cd", "\u5360\u6bd4")
  out
}

shiny_range_size_table <- function(summary) {
  if (is.null(summary)) {
    return(data.frame())
  }
  dist <- summary$range_size_distribution
  if (is.null(dist) || nrow(dist) == 0L) {
    return(data.frame())
  }
  out <- data.frame(
    range_size = dist$range_size,
    n_species = dist$n_species,
    proportion = paste0(formatC(100 * dist$proportion, format = "f", digits = 1), "%"),
    stringsAsFactors = FALSE
  )
  names(out) <- c("\u5206\u5e03\u533a\u6570", "\u7269\u79cd\u6570", "\u5360\u6bd4")
  out
}

input_template_path <- function(kind) {
  files <- c(
    tree = "tree.nwk",
    geography = "geography.csv",
    regions = "regions.csv"
  )
  if (length(kind) != 1L || is.na(kind) || !kind %in% names(files)) {
    stop("Unknown input template: ", paste(kind, collapse = ", "), call. = FALSE)
  }
  path <- system.file("example_data", files[[kind]], package = "iBiogeobears")
  if (!file.exists(path)) {
    stop("Installed input template could not be found: ", files[[kind]], call. = FALSE)
  }
  path
}

default_project_parent <- function() {
  as_path(file.path(path.expand("~"), "iBiogeobears-projects"))
}

shiny_upload_path <- function(upload, label) {
  if (is.null(upload) || nrow(upload) == 0L || !"datapath" %in% names(upload)) {
    stop(label, " is required.", call. = FALSE)
  }
  path <- upload$datapath[[1L]]
  if (is.null(path) || is.na(path) || !file.exists(path)) {
    stop(label, " upload is not available.", call. = FALSE)
  }
  path
}

prepare_shiny_startup <- function(config = NULL, output_dir = NULL) {
  if (!is.null(config)) {
    config <- as_path(config)
    if (!file.exists(config)) {
      stop("Shiny config file does not exist: ", config, call. = FALSE)
    }
    return(list(
      config = config,
      output_dir = output_dir %||% "",
      example_project_dir = ""
    ))
  }

  example <- create_example_project(tempfile("iBiogeobears-welcome-"))
  list(
    config = example$config,
    output_dir = output_dir %||% example$output_dir,
    example_project_dir = example$root
  )
}

run_app_action <- function(state, expr) {
  tryCatch(
    {
      state$status_type <- "info"
      force(expr)
    },
    error = function(e) {
      append_app_message(state, paste("Error:", conditionMessage(e)), status_type = "error")
      NULL
    }
  )
}

append_app_message <- function(state, message, status_type = "info") {
  message <- as.character(message)
  state$message <- message
  state$status_type <- status_type
  existing <- state$messages %||% character()
  state$messages <- c(existing, paste(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), message))
  invisible(message)
}

append_app_stage <- function(state, action, stage, detail = NULL) {
  message <- paste0(action, ": ", stage)
  detail <- detail %||% NULL
  if (!is.null(detail) && length(detail) > 0L && !is.na(detail[[1L]]) && nzchar(as.character(detail[[1L]]))) {
    message <- paste0(message, " - ", as.character(detail[[1L]]))
  }
  append_app_message(state, message)
}

workflow_validation_label <- function(validation) {
  if (is.null(validation) || nrow(validation) == 0L || !"ok" %in% names(validation)) {
    return("not available")
  }
  ok <- validation$ok
  passed <- sum(!is.na(ok) & ok)
  failed <- sum(!is.na(ok) & !ok)
  paste0(passed, " passed, ", failed, " failed")
}

workflow_model_status_label <- function(model_table) {
  if (is.null(model_table) || nrow(model_table) == 0L || !"status" %in% names(model_table)) {
    return("not available")
  }
  statuses <- sort(table(model_table$status), decreasing = TRUE)
  paste(paste0(names(statuses), ": ", as.integer(statuses)), collapse = ", ")
}

workflow_model_action_label <- function(model_table) {
  if (is.null(model_table) || nrow(model_table) == 0L || !"run_action" %in% names(model_table)) {
    return("not available")
  }
  actions <- sort(table(model_table$run_action), decreasing = TRUE)
  paste(paste0(names(actions), ": ", as.integer(actions)), collapse = ", ")
}

workflow_failed_models_label <- function(model_table) {
  label <- failed_models_label(model_table)
  if (identical(label, "none")) {
    return("none")
  }
  label
}

resolve_report_file <- function(state) {
  path <- report_preview_path(state)
  require_existing_file(path, "Render a report before downloading it.")
}

report_preview_path <- function(state) {
  path <- state$report
  if (is.null(path) && !is.null(state$result)) {
    candidates <- file.path(state$result$project_paths$reports, c("summary_report.html", "summary_report.pdf", "summary_report.qmd"))
    candidates <- candidates[file.exists(candidates)]
    path <- candidates[1L] %||% NULL
  }
  if (is.null(path) || length(path) == 0L || is.na(path) || !file.exists(path)) {
    return(NULL)
  }
  as_path(path)
}

resolve_bundle_file <- function(state) {
  if (is.null(state$bundle)) {
    require_workflow_result(state$result)
    state$bundle <- bundle_results(state$result, overwrite = TRUE)
  }
  require_existing_file(state$bundle, "Bundle results before downloading them.")
}

resolve_diagnostic_bundle_file <- function(state) {
  if (is.null(state$diagnostic_bundle)) {
    require_workflow_result(state$result)
    state$diagnostic_bundle <- bundle_diagnostics(state$result, overwrite = TRUE)
  }
  require_existing_file(state$diagnostic_bundle, "Bundle diagnostics before downloading them.")
}

require_existing_file <- function(path, message) {
  if (is.null(path) || length(path) == 0L || is.na(path) || !file.exists(path)) {
    stop(message, call. = FALSE)
  }
  as_path(path)
}

copy_download_file <- function(src, dest) {
  ok <- file.copy(src, dest, overwrite = TRUE)
  if (!isTRUE(ok)) {
    stop("Unable to prepare download file: ", src, call. = FALSE)
  }
  invisible(dest)
}

resolve_run_summary_file <- function(state) {
  path <- persist_shiny_run_summary(state)
  require_existing_file(path, "Run or load workflow results before downloading the run summary.")
}

persist_shiny_run_summary <- function(state) {
  if (is.null(state$result) || is.null(state$result$project_paths$tables)) {
    return(NULL)
  }
  table <- shiny_run_summary_table(state)
  path <- file.path(state$result$project_paths$tables, "shiny_run_summary.csv")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write_csv_base(table, path)
  as_path(path)
}

refresh_shiny_result_exports <- function(session, state) {
  persist_shiny_run_summary(state)
  if (!is.null(state$result)) {
    state$manifest <- create_workflow_manifest(state$result, write = TRUE)
  }
  update_table_preview_choices(session, state)
  update_figure_preview_choices(session, state)
  invisible(state$manifest)
}

load_existing_workflow_result <- function(output_dir, refresh_manifest = TRUE) {
  output_dir <- trimws(output_dir %||% "")
  if (!nzchar(output_dir)) {
    stop("Provide an output directory before loading existing results.", call. = FALSE)
  }
  if (!dir.exists(output_dir)) {
    stop("Workflow output directory does not exist: ", output_dir, call. = FALSE)
  }

  project_paths <- workflow_project_paths(output_dir)
  workflow_manifest <- create_workflow_manifest(project_paths$root, write = isTRUE(refresh_manifest))
  validation <- read_existing_output_table(project_paths, "input_validation.csv") %||% data.frame()
  model_run_status <- read_existing_output_table(project_paths, "model_run_status.csv") %||%
    read_existing_output_table(project_paths, "model_run_plan.csv") %||%
    data.frame()
  model_comparison <- read_existing_output_table(project_paths, "model_comparison.csv")
  model_sensitivity_table <- read_existing_output_table(project_paths, "model_sensitivity.csv")
  node_state_sensitivity <- read_existing_output_table(project_paths, "node_state_sensitivity.csv")
  best_fit_events <- read_existing_output_table(project_paths, "best_fit_events.csv")
  range_change_events <- read_existing_output_table(project_paths, "range_change_events.csv")
  event_summary <- read_existing_output_table(project_paths, "event_summary.csv")
  bsm_run_status <- read_existing_output_table(project_paths, "bsm_run_status.csv")
  bsm_event_summary <- read_existing_output_table(project_paths, "bsm_event_summary.csv")
  bsm_replicate_counts <- read_existing_output_table(project_paths, "bsm_replicate_counts.csv")
  bsm_dispersal_routes <- read_existing_output_table(project_paths, "bsm_dispersal_routes.csv")
  bsm_events <- read_existing_output_table(project_paths, "bsm_events.csv")
  bsm_event_times <- read_existing_output_table(project_paths, "bsm_event_times.csv")
  figure_manifest <- read_existing_figure_manifest(project_paths)

  standardized_tables <- list(
    geographic_states = read_existing_output_table(project_paths, "geographic_states.csv") %||% data.frame(),
    tree_nodes = read_existing_output_table(project_paths, "tree_nodes.csv") %||% data.frame(),
    parameter_table = read_existing_output_table(project_paths, "model_parameters.csv") %||% data.frame(),
    ancestral_state_probabilities = read_existing_output_table(project_paths, "ancestral_state_probabilities.csv") %||% data.frame(),
    root_state_probabilities = read_existing_output_table(project_paths, "root_state_probabilities.csv") %||% data.frame(),
    node_state_summary = read_existing_output_table(project_paths, "node_state_summary.csv") %||% data.frame(),
    node_state_sensitivity = node_state_sensitivity %||% data.frame(),
    best_fit_events = best_fit_events %||% data.frame(),
    range_change_events = range_change_events %||% data.frame(),
    event_summary = event_summary %||% data.frame(),
    bsm_run_status = bsm_run_status %||% data.frame(),
    bsm_event_summary = bsm_event_summary %||% data.frame(),
    bsm_replicate_counts = bsm_replicate_counts %||% data.frame(),
    bsm_dispersal_routes = bsm_dispersal_routes %||% data.frame(),
    bsm_events = bsm_events %||% data.frame(),
    bsm_event_times = bsm_event_times %||% data.frame()
  )

  result <- list(
    config = NULL,
    project_paths = project_paths,
    validation = validation,
    biogeobears = NULL,
    model_plan = model_run_status,
    model_run_status = model_run_status,
    model_comparison = model_comparison,
    model_sensitivity = NULL,
    model_sensitivity_table = model_sensitivity_table,
    node_state_sensitivity = node_state_sensitivity,
    best_fit_events = best_fit_events,
    bsm_tables = list(
      bsm_run_status = bsm_run_status %||% data.frame(),
      bsm_event_summary = bsm_event_summary %||% data.frame(),
      bsm_replicate_counts = bsm_replicate_counts %||% data.frame(),
      bsm_dispersal_routes = bsm_dispersal_routes %||% data.frame(),
      bsm_events = bsm_events %||% data.frame(),
      bsm_event_times = bsm_event_times %||% data.frame()
    ),
    standardized_tables = standardized_tables,
    figure_manifest = figure_manifest,
    workflow_manifest = workflow_manifest,
    dry_run = is.null(model_comparison) || nrow(model_comparison) == 0L,
    force = FALSE,
    validation_failed = if (nrow(validation) > 0L && "ok" %in% names(validation)) any(!validation$ok) else FALSE
  )
  class(result) <- c("iBGB_workflow_result", "list")
  result
}

workflow_project_paths <- function(output_dir) {
  output_dir <- as_path(output_dir)
  list(
    root = output_dir,
    inputs = file.path(output_dir, "inputs"),
    raw_biogeobears = file.path(output_dir, "raw_biogeobears"),
    tables = file.path(output_dir, "tables"),
    figures = file.path(output_dir, "figures"),
    reports = file.path(output_dir, "reports"),
    logs = file.path(output_dir, "logs")
  )
}

read_existing_output_table <- function(project_paths, filename) {
  path <- file.path(project_paths$tables, filename)
  if (!file.exists(path)) {
    return(NULL)
  }
  utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
}

read_existing_figure_manifest <- function(project_paths) {
  path <- file.path(project_paths$figures, "figure_manifest.csv")
  if (!file.exists(path)) {
    return(data.frame())
  }
  utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
}

shiny_summary_table <- function(state) {
  validation_status <- "not run"
  if (!is.null(state$validation) && nrow(state$validation) > 0L) {
    validation_status <- if (all(state$validation$ok)) "passed" else "failed"
  }

  run_status <- "not run"
  run_mode <- "not run"
  completed_models <- "not available"
  reused_models <- "not available"
  warning_count <- "not available"
  if (!is.null(state$result)) {
    run_mode <- if (isTRUE(state$result$dry_run)) "dry run" else "executed"
    run_status <- if (isTRUE(state$result$validation_failed)) "validation failed" else "completed"
  }
  if (!is.null(state$model_table) && nrow(state$model_table) > 0L) {
    if ("status" %in% names(state$model_table)) {
      completed_models <- paste0(sum(state$model_table$status == "completed", na.rm = TRUE), " of ", nrow(state$model_table))
    } else {
      completed_models <- as.character(nrow(state$model_table))
    }
    if ("run_action" %in% names(state$model_table)) {
      reused_models <- as.character(sum(state$model_table$run_action == "reused", na.rm = TRUE))
    }
    if ("warning_count" %in% names(state$model_table)) {
      warning_count <- as.character(sum(state$model_table$warning_count, na.rm = TRUE))
    }
  }

  data.frame(
    item = c("Validation", "Run mode", "Run status", "Completed models", "Reused models", "Warning count", "Report", "Bundle"),
    value = c(
      validation_status,
      run_mode,
      run_status,
      completed_models,
      reused_models,
      warning_count,
      if (!is.null(report_preview_path(state))) "available" else "not available",
      if (!is.null(state$bundle) && file.exists(state$bundle)) "available" else "not available"
    ),
    stringsAsFactors = FALSE
  )
}

shiny_upload_preview_table <- function(input) {
  rows <- list(
    shiny_upload_preview_tree(input$wizard_tree %||% NULL),
    shiny_upload_preview_csv(input$wizard_geography %||% NULL, "\u5206\u5e03\u77e9\u9635 CSV", expected = "\u7b2c\u4e00\u5217\u4e3a\u7269\u79cd\u540d\uff0c\u5176\u4f59\u5217\u4e3a\u5730\u7406\u533a\u57df\uff0c\u53d6\u503c\u901a\u5e38\u4e3a 0/1\u3002"),
    shiny_upload_preview_csv(input$wizard_regions %||% NULL, "\u533a\u57df\u4fe1\u606f CSV", expected = "\u81f3\u5c11\u5305\u542b\u533a\u57df\u7f16\u53f7\u6216\u533a\u57df\u540d\u79f0\uff0c\u7528\u6765\u89e3\u91ca\u5206\u5e03\u77e9\u9635\u5217\u540d\u3002")
  )
  do.call(rbind, rows)
}

shiny_upload_preview_missing <- function(label, next_step) {
  shiny_upload_preview_row(label, "\u7b49\u5f85\u4e0a\u4f20", "", next_step)
}

shiny_upload_preview_row <- function(label, status, summary, next_step) {
  stats::setNames(
    data.frame(
      file = label,
      status = status,
      summary = summary,
      next_step = next_step,
      check.names = FALSE,
      stringsAsFactors = FALSE
    ),
    c("\u6587\u4ef6", "\u72b6\u6001", "\u6458\u8981", "\u4e0b\u4e00\u6b65")
  )
}

shiny_upload_preview_tree <- function(upload) {
  path <- shiny_uploaded_datapath(upload)
  if (is.null(path)) {
    return(shiny_upload_preview_missing("\u7cfb\u7edf\u6811\u6587\u4ef6", "\u4e0a\u4f20 Newick \u683c\u5f0f\u7684\u6811\u6587\u4ef6\u3002"))
  }
  text <- tryCatch(
    paste(readLines(path, n = 5L, warn = FALSE), collapse = " "),
    error = function(e) NA_character_
  )
  if (is.na(text) || !nzchar(text)) {
    return(shiny_upload_preview_row("\u7cfb\u7edf\u6811\u6587\u4ef6", "\u9700\u8981\u68c0\u67e5", "\u6587\u4ef6\u4e3a\u7a7a\u6216\u65e0\u6cd5\u8bfb\u53d6\u3002", "\u91cd\u65b0\u4e0a\u4f20 Newick \u6811\u6587\u4ef6\u3002"))
  }
  looks_newick <- grepl("\\(", text) && grepl(";", text)
  status <- if (looks_newick) "\u53ef\u8bfb\u53d6" else "\u9700\u8981\u68c0\u67e5"
  next_step <- if (looks_newick) "\u7ee7\u7eed\u4e0a\u4f20\u5206\u5e03\u77e9\u9635\u548c\u533a\u57df\u4fe1\u606f\u3002" else "\u68c0\u67e5\u6811\u6587\u4ef6\u662f\u5426\u4e3a Newick \u683c\u5f0f\uff0c\u4e14\u4ee5\u5206\u53f7\u7ed3\u5c3e\u3002"
  shiny_upload_preview_row(
    "\u7cfb\u7edf\u6811\u6587\u4ef6",
    status,
    paste0("\u524d\u51e0\u884c\u5171 ", nchar(text), " \u4e2a\u5b57\u7b26\u3002"),
    next_step
  )
}

shiny_upload_preview_csv <- function(upload, label, expected) {
  path <- shiny_uploaded_datapath(upload)
  if (is.null(path)) {
    return(shiny_upload_preview_missing(label, paste0("\u4e0a\u4f20 ", label, "\u3002")))
  }
  table <- tryCatch(
    utils::read.csv(path, nrows = 5L, check.names = FALSE, stringsAsFactors = FALSE),
    error = function(e) e
  )
  if (inherits(table, "error")) {
    return(shiny_upload_preview_row(label, "\u9700\u8981\u68c0\u67e5", conditionMessage(table), expected))
  }
  if (ncol(table) == 0L) {
    return(shiny_upload_preview_row(label, "\u9700\u8981\u68c0\u67e5", "CSV \u6ca1\u6709\u53ef\u8bfb\u53d6\u7684\u5217\u3002", expected))
  }
  summary <- paste0(
    "\u53ef\u8bfb\u53d6\uff1b\u9884\u89c8\u5230 ",
    nrow(table),
    " \u884c\u3001",
    ncol(table),
    " \u5217\uff1a",
    paste(utils::head(names(table), 6L), collapse = ", ")
  )
  shiny_upload_preview_row(label, "\u53ef\u8bfb\u53d6", summary, "\u70b9\u51fb\u201c\u521b\u5efa\u81ea\u5df1\u7684\u5206\u6790\u9879\u76ee\u201d\u540e\u4f1a\u8fdb\u884c\u5b8c\u6574\u9a8c\u8bc1\u3002")
}

shiny_uploaded_datapath <- function(upload) {
  if (is.null(upload) || nrow(upload) == 0L || !"datapath" %in% names(upload)) {
    return(NULL)
  }
  path <- upload$datapath[[1L]]
  if (is.null(path) || is.na(path) || !nzchar(path) || !file.exists(path)) {
    return(NULL)
  }
  path
}

shiny_guided_workflow_table <- function(
    state,
    start_choice = "example",
    config_path = NULL,
    output_dir = NULL,
    dry_run = TRUE,
    require_biogeobears = FALSE) {
  start_choice <- shiny_workflow_start_choice(start_choice)
  config_path <- shiny_text_or_blank(config_path)
  output_dir <- shiny_text_or_blank(output_dir)
  if (!nzchar(output_dir) && !is.null(state$result) && !is.null(state$result$project_paths$root)) {
    output_dir <- state$result$project_paths$root
  }

  config_ready <- nzchar(config_path) && file.exists(config_path)
  validation <- shiny_validation_progress(state$validation)
  bgb_ready <- shiny_installation_component_ready(state$installation, "BioGeoBEARS")
  has_result <- !is.null(state$result)
  dry_run_ready <- has_result && isTRUE(state$result$dry_run) && !isTRUE(state$result$validation_failed)
  real_run_ready <- has_result && !isTRUE(state$result$dry_run) && !isTRUE(state$result$validation_failed)
  comparison_ready <- has_result && nrow(shiny_model_comparison_table(state)) > 0L
  primary_results_ready <- real_run_ready && comparison_ready
  result_bundle_ready <- !is.null(state$bundle) && file.exists(state$bundle)
  diagnostic_bundle_ready <- !is.null(state$diagnostic_bundle) && file.exists(state$diagnostic_bundle)
  export_ready <- result_bundle_ready || diagnostic_bundle_ready

  data_ready <- switch(
    start_choice,
    example = config_ready,
    own = has_result || validation$available,
    existing = has_result,
    config_ready
  )

  data_next <- switch(
    start_choice,
    example = if (data_ready) "\u70b9\u51fb\u201c\u68c0\u67e5\u8f93\u5165\u201d\u3002" else "\u70b9\u51fb\u201c\u521b\u5efa\u793a\u4f8b\u9879\u76ee\u201d\u3002",
    own = if (data_ready) "\u70b9\u51fb\u201c\u68c0\u67e5\u8f93\u5165\u201d\u3002" else "\u4e0a\u4f20\u7cfb\u7edf\u6811\u3001\u5206\u5e03\u77e9\u9635\u548c\u533a\u57df\u4fe1\u606f\uff0c\u7136\u540e\u70b9\u51fb\u201c\u521b\u5efa\u81ea\u5df1\u7684\u5206\u6790\u9879\u76ee\u201d\u3002",
    existing = if (data_ready) "\u6253\u5f00\u201c\u7ed3\u679c\u201d\u3002" else "\u5728\u201c\u9ad8\u7ea7\uff1a\u5df2\u6709\u9879\u76ee\u548c YAML\u201d\u91cc\u586b\u5199\u7ed3\u679c\u76ee\u5f55\uff0c\u7136\u540e\u70b9\u51fb\u201c\u52a0\u8f7d\u5df2\u6709\u7ed3\u679c\u201d\u3002",
    "\u5148\u9009\u62e9\u5f00\u59cb\u65b9\u5f0f\u3002"
  )
  data_detail <- switch(
    start_choice,
    example = if (config_ready) as_path(config_path) else "\u8fd8\u6ca1\u6709\u521b\u5efa\u793a\u4f8b\u9879\u76ee\u3002",
    own = if (data_ready) as_path(config_path) else "\u4f7f\u7528\u201c\u4f7f\u7528\u81ea\u5df1\u7684\u6570\u636e\u201d\u91cc\u7684\u4e0a\u4f20\u63a7\u4ef6\u3002",
    existing = if (has_result) output_dir else "\u9700\u8981\u5148\u6307\u5b9a\u5df2\u6709\u7ed3\u679c\u76ee\u5f55\u3002",
    ""
  )

  validation_status <- if (!data_ready) {
    "\u7b49\u5f85"
  } else if (validation$failed) {
    "\u9700\u8981\u5904\u7406"
  } else if (validation$passed) {
    "\u5df2\u5c31\u7eea"
  } else {
    "\u9700\u8981\u64cd\u4f5c"
  }
  validation_next <- if (!data_ready) {
    "\u5148\u9009\u62e9\u6216\u521b\u5efa\u4e00\u4e2a\u9879\u76ee\u3002"
  } else if (validation$failed) {
    "\u6253\u5f00\u201c\u9ad8\u7ea7\u7ed3\u679c > Validation\u201d\uff0c\u6309\u63d0\u793a\u4fee\u590d\u5931\u8d25\u9879\u3002"
  } else if (validation$passed) {
    "\u8fd0\u884c\u4e00\u6b21 dry run\u3002"
  } else {
    "\u70b9\u51fb\u201c\u68c0\u67e5\u8f93\u5165\u201d\u3002"
  }

  dry_status <- if (dry_run_ready || real_run_ready) {
    "\u5df2\u5c31\u7eea"
  } else if (validation$failed || !validation$passed) {
    "\u7b49\u5f85"
  } else {
    "\u9700\u8981\u64cd\u4f5c"
  }
  dry_next <- if (dry_run_ready) {
    if (bgb_ready) "\u53d6\u6d88\u52fe\u9009 Dry run\uff0c\u51c6\u5907\u771f\u5b9e\u8fd0\u884c\u3002" else "\u5b89\u88c5 BioGeoBEARS \u540e\u518d\u771f\u5b9e\u8fd0\u884c\u3002"
  } else if (real_run_ready) {
    "\u6253\u5f00\u201c\u7ed3\u679c\u201d\u3002"
  } else if (validation$failed) {
    "\u5148\u4fee\u590d\u8f93\u5165\u68c0\u67e5\u9519\u8bef\u3002"
  } else if (validation$passed) {
    "\u4fdd\u6301 Dry run \u52fe\u9009\uff0c\u7136\u540e\u70b9\u51fb\u201c\u8fd0\u884c\u6d41\u7a0b\u201d\u3002"
  } else {
    "\u5148\u70b9\u51fb\u201c\u68c0\u67e5\u8f93\u5165\u201d\u3002"
  }

  real_status <- if (real_run_ready) {
    "\u5df2\u5c31\u7eea"
  } else if (dry_run_ready && bgb_ready) {
    "\u9700\u8981\u64cd\u4f5c"
  } else {
    "\u7b49\u5f85"
  }
  real_next <- if (real_run_ready) {
    "\u67e5\u770b\u201c\u7ed3\u679c\u201d\u3002"
  } else if (dry_run_ready && bgb_ready) {
    "\u53d6\u6d88\u52fe\u9009 Dry run\uff0c\u7136\u540e\u70b9\u51fb\u201c\u8fd0\u884c\u6d41\u7a0b\u201d\u3002"
  } else if (dry_run_ready && !bgb_ready) {
    if (isTRUE(dry_run) && !isTRUE(require_biogeobears)) {
      "\u5b89\u88c5 BioGeoBEARS \u540e\u624d\u80fd\u771f\u5b9e\u8fd0\u884c\u3002"
    } else {
      "\u5b89\u88c5 BioGeoBEARS\uff0c\u7136\u540e\u91cd\u65b0\u8fd0\u884c\u3002"
    }
  } else {
    "\u5148\u5b8c\u6210 dry run\u3002"
  }

  result_status <- if (primary_results_ready) {
    "\u5df2\u5c31\u7eea"
  } else if (real_run_ready) {
    "\u9700\u8981\u5904\u7406"
  } else {
    "\u7b49\u5f85"
  }
  result_next <- if (primary_results_ready) {
    "\u6253\u5f00\u201c\u7ed3\u679c\u201d\uff0c\u67e5\u770b\u7956\u5148\u5206\u5e03\u91cd\u5efa\u56fe\u3001\u6a21\u578b\u6bd4\u8f83\u8868\u548c\u4e8b\u4ef6\u7edf\u8ba1\u3002"
  } else if (real_run_ready) {
    "\u70b9\u51fb\u201c\u5237\u65b0\u5173\u952e\u6587\u4ef6\u201d\uff0c\u6216\u6253\u5f00\u201c\u6392\u9519\u201d\u67e5\u770b\u539f\u56e0\u3002"
  } else {
    "\u5148\u771f\u5b9e\u8fd0\u884c\uff0c\u6216\u52a0\u8f7d\u5df2\u6709\u771f\u5b9e\u8fd0\u884c\u7ed3\u679c\u3002"
  }

  export_status <- if (export_ready) {
    if (result_bundle_ready && diagnostic_bundle_ready) "\u5df2\u5c31\u7eea" else "\u90e8\u5206\u5b8c\u6210"
  } else if (primary_results_ready) {
    "\u9700\u8981\u64cd\u4f5c"
  } else {
    "\u7b49\u5f85"
  }
  export_next <- if (result_bundle_ready && diagnostic_bundle_ready) {
    "\u4e0b\u8f7d\u7ed3\u679c\u538b\u7f29\u5305\u6216\u8bca\u65ad\u538b\u7f29\u5305\u3002"
  } else if (result_bundle_ready) {
    "\u5982\u679c\u9700\u8981\u522b\u4eba\u5e2e\u5fd9\u6392\u9519\uff0c\u518d\u751f\u6210\u8bca\u65ad\u538b\u7f29\u5305\u3002"
  } else if (primary_results_ready) {
    "\u70b9\u51fb\u201c\u751f\u6210\u7ed3\u679c\u538b\u7f29\u5305\u201d\u3002"
  } else {
    "\u5148\u67e5\u770b\u7ed3\u679c\u3002"
  }

  stats::setNames(
    data.frame(
      step = c("\u6570\u636e\u6765\u6e90", "\u8f93\u5165\u68c0\u67e5", "Dry run", "\u771f\u5b9e\u8fd0\u884c", "\u67e5\u770b\u7ed3\u679c", "\u5bfc\u51fa\u5206\u4eab"),
      status = c(
        if (data_ready) "\u5df2\u5c31\u7eea" else "\u9700\u8981\u64cd\u4f5c",
        validation_status,
        dry_status,
        real_status,
        result_status,
        export_status
      ),
      next_action = c(
        data_next,
        validation_next,
        dry_next,
        real_next,
        result_next,
        export_next
      ),
      detail = c(
        data_detail,
        validation$detail,
        if (has_result) workflow_model_status_label(state$model_table) else "",
        shiny_installation_component_detail(state$installation, "BioGeoBEARS"),
        if (comparison_ready) "\u6a21\u578b\u6bd4\u8f83\u8868\u5df2\u751f\u6210\u3002" else "\u8fd8\u6ca1\u6709\u6a21\u578b\u6bd4\u8f83\u8868\u3002",
        shiny_export_detail(result_bundle_ready, diagnostic_bundle_ready, state)
      ),
      check.names = FALSE,
      stringsAsFactors = FALSE
    ),
    c("\u6b65\u9aa4", "\u72b6\u6001", "\u4e0b\u4e00\u6b65", "\u8bf4\u660e")
  )
}

shiny_workflow_start_choice <- function(start_choice) {
  value <- tolower(shiny_text_or_blank(start_choice))
  if (value %in% c("example", "example data")) {
    return("example")
  }
  if (value %in% c("own", "my own data", "user", "user data")) {
    return("own")
  }
  if (value %in% c("existing", "existing results", "load")) {
    return("existing")
  }
  "example"
}

shiny_validation_progress <- function(validation) {
  if (is.null(validation) || nrow(validation) == 0L || !"ok" %in% names(validation)) {
    return(list(
      available = FALSE,
      passed = FALSE,
      failed = FALSE,
      detail = "\u5c1a\u672a\u68c0\u67e5\u3002"
    ))
  }
  failed <- sum(!is.na(validation$ok) & !validation$ok)
  passed <- sum(!is.na(validation$ok) & validation$ok)
  list(
    available = TRUE,
    passed = failed == 0L,
    failed = failed > 0L,
    detail = paste0(passed, " \u9879\u901a\u8fc7\uff0c", failed, " \u9879\u5931\u8d25")
  )
}

shiny_home_next_action <- function(workflow) {
  if (is.null(workflow) || nrow(workflow) == 0L) {
    return(shiny::tags$div(class = "ibgb-next-action", "Choose a data source."))
  }
  status <- workflow[["\u72b6\u6001"]] %||% workflow[["Status"]]
  actionable <- workflow[status %in% c("\u9700\u8981\u64cd\u4f5c", "\u9700\u8981\u5904\u7406", "\u7b49\u5f85", "\u90e8\u5206\u5b8c\u6210", "Action needed", "Needs attention", "Waiting", "Partial"), , drop = FALSE]
  next_row <- if (nrow(actionable) > 0L) actionable[1L, , drop = FALSE] else workflow[nrow(workflow), , drop = FALSE]
  next_step <- (next_row[["\u6b65\u9aa4"]] %||% next_row[["Step"]])[[1L]]
  next_text <- (next_row[["\u4e0b\u4e00\u6b65"]] %||% next_row[["Next action"]])[[1L]]
  shiny::tags$div(
    class = "ibgb-next-action",
    shiny::tags$div(class = "ibgb-next-action-title", "\u4e0b\u4e00\u6b65"),
    shiny::tags$div(class = "ibgb-next-action-step", next_step),
    shiny::tags$div(class = "ibgb-next-action-detail", next_text)
  )
}

shiny_first_steps_table <- function(
    state,
    config_path = NULL,
    output_dir = NULL,
    dry_run = TRUE,
    require_biogeobears = FALSE) {
  config_path <- shiny_text_or_blank(config_path)
  output_dir <- shiny_text_or_blank(output_dir)
  if (!nzchar(output_dir) && !is.null(state$result) && !is.null(state$result$project_paths$root)) {
    output_dir <- state$result$project_paths$root
  }

  config_ready <- nzchar(config_path) && file.exists(config_path)
  core_missing <- shiny_missing_required_components(state$installation, exclude = "BioGeoBEARS")
  core_ready <- length(core_missing) == 0L
  bgb_ready <- shiny_installation_component_ready(state$installation, "BioGeoBEARS")

  validation_status <- "Not run"
  validation_next <- "Click Validate."
  validation_detail <- ""
  if (!is.null(state$validation) && nrow(state$validation) > 0L && "ok" %in% names(state$validation)) {
    failed <- sum(!is.na(state$validation$ok) & !state$validation$ok)
    passed <- sum(!is.na(state$validation$ok) & state$validation$ok)
    validation_detail <- paste0(passed, " passed, ", failed, " failed")
    if (failed > 0L) {
      validation_status <- "Needs attention"
      validation_next <- "Open Validation and follow How to fix."
    } else {
      validation_status <- "Passed"
      validation_next <- "Click Run workflow with Dry run checked."
    }
  }

  workflow_status <- "Not run"
  workflow_next <- "Run a dry workflow first."
  workflow_detail <- ""
  if (!is.null(state$result)) {
    workflow_detail <- workflow_model_status_label(state$model_table)
    if (isTRUE(state$result$validation_failed)) {
      workflow_status <- "Needs attention"
      workflow_next <- "Fix validation errors before real execution."
    } else if (!identical(failed_models_label(state$model_table), "none")) {
      workflow_status <- "Needs attention"
      workflow_next <- "Open Run Status, inspect failed model logs, then retry failed models."
    } else if (isTRUE(state$result$dry_run)) {
      workflow_status <- "Dry run complete"
      workflow_next <- if (bgb_ready) {
        "Uncheck Dry run for real BioGeoBEARS execution."
      } else {
        "Install BioGeoBEARS before real execution."
      }
    } else {
      workflow_status <- "Complete"
      workflow_next <- "Render report and create result bundle."
    }
  }

  report <- report_preview_path(state)
  report_ready <- !is.null(report)
  result_bundle_ready <- !is.null(state$bundle) && file.exists(state$bundle)
  diagnostic_bundle_ready <- !is.null(state$diagnostic_bundle) && file.exists(state$diagnostic_bundle)

  data.frame(
    step = c(
      "Project config",
      "Setup checks",
      "BioGeoBEARS",
      "Validation",
      "Workflow run",
      "Report",
      "Export"
    ),
    status = c(
      if (config_ready) "Ready" else "Action needed",
      if (core_ready) "Ready" else "Action needed",
      if (bgb_ready) "Ready" else if (isTRUE(dry_run) && !isTRUE(require_biogeobears)) "Needed for real run" else "Action needed",
      validation_status,
      workflow_status,
      if (report_ready) "Ready" else if (is.null(state$result)) "Not ready" else "Action needed",
      shiny_export_status(result_bundle_ready, diagnostic_bundle_ready, state$result)
    ),
    next_step = c(
      if (config_ready) "Click Validate." else "Create example project, create analysis project, upload analysis.yml, or enter a config path.",
      if (core_ready) "Proceed to validation." else paste("Install or repair:", paste(core_missing, collapse = ", ")),
      if (bgb_ready) "Real model execution is available." else "Keep Dry run checked until BioGeoBEARS is installed.",
      validation_next,
      workflow_next,
      if (report_ready) "Open or download report." else "Click Render report after workflow completes.",
      shiny_export_next_step(result_bundle_ready, diagnostic_bundle_ready, state$result)
    ),
    detail = c(
      if (config_ready) as_path(config_path) else "",
      if (core_ready) "R, core packages, and Shiny ready." else paste(core_missing, collapse = ", "),
      shiny_installation_component_detail(state$installation, "BioGeoBEARS"),
      validation_detail,
      workflow_detail,
      report %||% "",
      shiny_export_detail(result_bundle_ready, diagnostic_bundle_ready, state)
    ),
    stringsAsFactors = FALSE
  )
}

shiny_text_or_blank <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x[[1L]])) {
    return("")
  }
  trimws(as.character(x[[1L]]))
}

choose_output_directory <- function(default = NULL) {
  default <- shiny_text_or_blank(default)
  if (!nzchar(default)) {
    default <- getwd()
  }
  if (.Platform$OS.type == "windows" && exists("choose.dir", envir = asNamespace("utils"), inherits = FALSE)) {
    selected <- utils::choose.dir(default = default, caption = "Choose iBiogeobears result directory")
    return(shiny_text_or_blank(selected))
  }
  stop(
    "Folder chooser is not available in this R session. Paste the full output directory path into the result directory box.",
    call. = FALSE
  )
}

shiny_missing_required_components <- function(installation, exclude = character()) {
  if (is.null(installation) || nrow(installation) == 0L ||
      !all(c("component", "required", "status") %in% names(installation))) {
    return("setup checks")
  }
  rows <- installation[
    installation$required == "yes" &
      installation$status != "Ready" &
      !installation$component %in% exclude,
    ,
    drop = FALSE
  ]
  rows$component
}

shiny_installation_component_ready <- function(installation, component) {
  if (is.null(installation) || nrow(installation) == 0L ||
      !all(c("component", "status") %in% names(installation))) {
    return(FALSE)
  }
  rows <- installation[installation$component == component, , drop = FALSE]
  nrow(rows) > 0L && identical(rows$status[[1L]], "Ready")
}

shiny_installation_component_detail <- function(installation, component) {
  if (is.null(installation) || nrow(installation) == 0L ||
      !all(c("component", "version", "next_step") %in% names(installation))) {
    return("Run setup checks.")
  }
  rows <- installation[installation$component == component, , drop = FALSE]
  if (nrow(rows) == 0L) {
    return("Run setup checks.")
  }
  if (identical(rows$status[[1L]], "Ready")) {
    version <- rows$version[[1L]]
    if (!is.na(version) && nzchar(version)) {
      return(paste("Version", version))
    }
    return("Ready.")
  }
  rows$next_step[[1L]]
}

shiny_export_status <- function(result_bundle_ready, diagnostic_bundle_ready, result) {
  if (isTRUE(result_bundle_ready) && isTRUE(diagnostic_bundle_ready)) {
    return("Ready")
  }
  if (is.null(result)) {
    return("Not ready")
  }
  if (isTRUE(result_bundle_ready) || isTRUE(diagnostic_bundle_ready)) {
    return("Partial")
  }
  "Action needed"
}

shiny_export_next_step <- function(result_bundle_ready, diagnostic_bundle_ready, result) {
  if (is.null(result)) {
    return("Run or load workflow results.")
  }
  if (!isTRUE(result_bundle_ready)) {
    return("Click Create bundle if missing.")
  }
  if (!isTRUE(diagnostic_bundle_ready)) {
    return("Click Create diagnostic bundle.")
  }
  "Download result or diagnostic bundle."
}

shiny_export_detail <- function(result_bundle_ready, diagnostic_bundle_ready, state) {
  details <- c(
    if (isTRUE(result_bundle_ready)) paste("Result:", as_path(state$bundle)) else "Result bundle missing",
    if (isTRUE(diagnostic_bundle_ready)) paste("Diagnostics:", as_path(state$diagnostic_bundle)) else "Diagnostic bundle missing"
  )
  paste(details, collapse = "; ")
}

shiny_about_table <- function(state, bgb_check = check_biogeobears(required = FALSE)) {
  data.frame(
    item = c(
      "Package",
      "Package version",
      "License",
      "BioGeoBEARS available",
      "BioGeoBEARS version",
      "BioGeoBEARS path",
      "BioGeoBEARS citation command",
      "Session info log",
      "BioGeoBEARS citation log"
    ),
    value = c(
      "iBiogeobears",
      shiny_package_version_label(),
      "GPL (>= 2)",
      if (isTRUE(bgb_check$available)) "yes" else "no",
      shiny_text_or_not_available(bgb_check$version),
      shiny_text_or_not_available(bgb_check$path),
      "citation(\"BioGeoBEARS\")",
      shiny_log_path(state, "session_info.txt") %||% "not available",
      shiny_log_path(state, "biogeobears_citation.txt") %||% "not available"
    ),
    stringsAsFactors = FALSE
  )
}

shiny_report_environment_table <- function(env = check_report_environment(c("source", "html", "pdf"))) {
  if (is.null(env) || nrow(env) == 0L) {
    return(data.frame())
  }
  out <- env
  logical_cols <- intersect(c("available", "quarto_package", "quarto_cli", "latex_available"), names(out))
  for (col in logical_cols) {
    out[[col]] <- ifelse(isTRUE(out[[col]]) | out[[col]] == TRUE, "yes", "no")
  }
  out
}

shiny_text_or_not_available <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x[[1L]]) || !nzchar(as.character(x[[1L]]))) {
    return("not available")
  }
  as.character(x[[1L]])
}

shiny_package_version_label <- function() {
  tryCatch(
    as.character(utils::packageVersion("iBiogeobears")),
    error = function(e) {
      desc <- tryCatch(utils::packageDescription("iBiogeobears", fields = "Version"), error = function(e) NA_character_)
      if (!is.na(desc) && nzchar(desc)) {
        desc
      } else {
        "not available"
      }
    }
  )
}

shiny_log_path <- function(state, filename) {
  if (is.null(state$result) || is.null(state$result$project_paths$logs)) {
    return(NULL)
  }
  path <- file.path(state$result$project_paths$logs, filename)
  if (file.exists(path)) {
    return(as_path(path))
  }
  NULL
}

shiny_biogeobears_citation_text <- function(bgb_check = check_biogeobears(required = FALSE)) {
  citation <- shiny_text_or_not_available(bgb_check$citation)
  if (isTRUE(bgb_check$available) && citation != "not available") {
    return(citation)
  }
  paste(
    "BioGeoBEARS is not bundled with iBiogeobears.",
    "Install BioGeoBEARS separately for real model execution.",
    "When BioGeoBEARS is installed, run citation(\"BioGeoBEARS\") and cite it directly.",
    bgb_check$install_help %||% "",
    sep = "\n"
  )
}

shiny_run_summary_table <- function(state) {
  comparison <- shiny_model_comparison_table(state)
  sensitivity <- shiny_model_sensitivity_table(state)
  warnings <- shiny_warnings_table(state)

  best_overall <- best_model_label(comparison)
  best_non_j <- best_model_label(filter_model_comparison_by_j(comparison, has_j = FALSE))
  best_plus_j <- best_model_label(filter_model_comparison_by_j(comparison, has_j = TRUE))
  plus_j_caution <- plus_j_caution_label(comparison, sensitivity)
  warning_count <- warning_count_label(state$model_table, warnings)
  failed_models <- failed_models_label(state$model_table)
  output_dir <- if (!is.null(state$result)) state$result$project_paths$root %||% "not available" else "not available"

  data.frame(
    item = c(
      "Fitted models",
      "Failed models",
      "Best statistical model",
      "Best non-+J model",
      "Best +J model",
      "+J interpretation caution",
      "Captured warnings",
      "Report",
      "Output directory"
    ),
    value = c(
      fitted_models_label(state$model_table, comparison),
      failed_models,
      best_overall,
      best_non_j,
      best_plus_j,
      plus_j_caution,
      warning_count,
      report_preview_path(state) %||% "not available",
      output_dir
    ),
    stringsAsFactors = FALSE
  )
}

shiny_run_summary_cards <- function(state) {
  summary <- shiny_run_summary_table(state)
  featured <- summary[summary$item %in% shiny_run_summary_card_items(), , drop = FALSE]
  featured$item <- factor(featured$item, levels = shiny_run_summary_card_items())
  featured <- featured[order(featured$item), , drop = FALSE]

  shiny::tags$div(
    class = "ibgb-run-summary-grid",
    lapply(seq_len(nrow(featured)), function(i) {
      item <- as.character(featured$item[[i]])
      value <- as.character(featured$value[[i]])
      shiny::tags$div(
        class = paste("ibgb-run-summary-card", shiny_run_summary_card_class(item, value)),
        shiny::tags$div(class = "ibgb-run-summary-label", item),
        shiny::tags$div(class = "ibgb-run-summary-value", value)
      )
    })
  )
}

shiny_run_summary_card_items <- function() {
  c(
    "Fitted models",
    "Failed models",
    "Best statistical model",
    "+J interpretation caution",
    "Captured warnings",
    "Report",
    "Output directory"
  )
}

shiny_run_summary_card_class <- function(item, value) {
  value <- tolower(value %||% "")
  if (!nzchar(value) || value == "not available") {
    return("muted")
  }
  if (identical(item, "+J interpretation caution")) {
    return(if (value == "not triggered") "good" else "warning")
  }
  if (identical(item, "Captured warnings")) {
    warning_count <- suppressWarnings(as.numeric(value))
    return(if (!is.na(warning_count) && warning_count > 0) "warning" else "good")
  }
  if (identical(item, "Failed models")) {
    return(if (value == "none") "good" else "warning")
  }
  if (identical(item, "Report")) {
    return("good")
  }
  "info"
}

output_file_legend <- function() {
  rows <- function(category, ...) {
    kv <- c(...)
    data.frame(
      category = category,
      file = names(kv),
      description = unname(kv),
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, list(
    rows(
      "\u8868\u683c tables/",
      "model_comparison.csv" = "\u5404\u6a21\u578b\u7684 logLik\u3001\u53c2\u6570\u6570\u3001AIC/AICc\u3001\u0394AICc \u548c Akaike \u6743\u91cd\u6bd4\u8f83\u3002",
      "input_validation.csv" = "\u8f93\u5165\u6570\u636e\uff08\u6811\u3001\u5206\u5e03\u77e9\u9635\u3001\u533a\u57df\u3001\u6a21\u578b\u8bbe\u7f6e\uff09\u7684\u4e00\u81f4\u6027\u6821\u9a8c\u7ed3\u679c\u3002",
      "biogeographic_process_summary.csv" = "\u5404\u751f\u7269\u5730\u7406\u8fc7\u7a0b\uff08\u539f\u5730/\u540c\u57df\u3001subset\u3001vicariance\u3001founder \u8df3\u8dc3\u3001range expansion\u3001\u5c40\u90e8\u706d\u7edd\u3001range switching\uff09\u7684\u5e73\u5747\u4e8b\u4ef6\u6570\u3002",
      "region_process_budgets.csv" = "\u6bcf\u4e2a\u5730\u533a\u7684\u6269\u6563\u6536\u652f\uff1a\u8fc1\u5165\u3001\u8fc1\u51fa\u3001\u51c0\u901a\u91cf\u3001\u5c40\u90e8\u706d\u7edd\u3002",
      "process_rates_through_time.csv" = "\u5404\u8fc7\u7a0b\u901f\u7387\u968f\u65f6\u95f4\uff08\u5206\u7bb1\uff09\u7684\u5747\u503c\u3001\u6807\u51c6\u5dee\u548c 95% CI\u3002\u8de8\u7c7b\u7fa4\u6574\u5408\u7528\u7684\u5c31\u662f\u8fd9\u4e2a\u6587\u4ef6\u3002",
      "region_process_rates_through_time.csv" = "\u5206\u5730\u533a\u7684\u8fc7\u7a0b\u901f\u7387\u968f\u65f6\u95f4\u3002\u8de8\u7c7b\u7fa4\u5206\u533a\u57df\u6574\u5408\u7528\u7684\u5c31\u662f\u8fd9\u4e2a\u6587\u4ef6\u3002",
      "bsm_qc.csv" = "BSM \u53ef\u9760\u6027\u68c0\u67e5\uff1a\u5404\u5185\u90e8\u4e00\u81f4\u6027\u4e0d\u53d8\u91cf\u7684 Pass/Warning/Fail\u3002",
      "bsm_event_summary.csv" = "BSM \u5404\u4e8b\u4ef6\u7c7b\u522b\u7684\u5e73\u5747/\u5408\u8ba1\u6b21\u6570\u3002",
      "bsm_dispersal_routes.csv" = "BSM \u5404 source-target \u6269\u6563\u8def\u7ebf\u7684\u5e73\u5747\u6b21\u6570\u3002",
      "bsm_event_times.csv" = "BSM \u6bcf\u4e2a\u4e8b\u4ef6\u7684\u65f6\u95f4\u548c\u7c7b\u578b\uff08\u9010\u6b21\u8bb0\u5f55\uff09\u3002"
    ),
    rows(
      "\u56fe\u7247 figures/",
      "node_state_summary_best_model" = "\u6700\u4f18\u6a21\u578b\u4e0b\u5404\u8282\u70b9\u7684\u7956\u5148\u5206\u5e03\u91cd\u5efa\uff08\u4e3b\u56fe\uff09\u3002",
      "biogeographic_process_synthesis" = "\u751f\u7269\u5730\u7406\u8fc7\u7a0b\u7efc\u5408\u56fe\uff08\u53d1\u8868\u6838\u5fc3\u56fe\uff09\u3002",
      "model_comparison" = "\u6a21\u578b\u6bd4\u8f83\u56fe\u3002",
      "region_process_budget" = "\u5404\u5730\u533a\u6269\u6563\u6536\u652f\u7684\u5206\u6b67\u6761\u5f62\u56fe\u3002",
      "process_rates_through_time" = "\u8fc7\u7a0b\u901f\u7387\u968f\u65f6\u95f4\u56fe\u3002",
      "region_process_rates_through_time" = "\u5206\u5730\u533a\u8fc7\u7a0b\u901f\u7387\u968f\u65f6\u95f4\u56fe\u3002",
      "bsm_dispersal_network" = "BSM \u533a\u57df\u95f4\u6269\u6563\u6709\u5411\u7bad\u5934\u7f51\u7edc\uff08\u7bad\u5934\u7c97\u7ec6=\u5e73\u5747\u6b21\u6570\uff09\u3002",
      "bsm_dispersal_routes" = "BSM \u6269\u6563\u8def\u7ebf\u70ed\u56fe\u3002",
      "bsm_event_summary / bsm_event_times / event_summary" = "BSM \u4e0e\u786e\u5b9a\u6027\u4e8b\u4ef6\u7684\u6458\u8981\u548c\u65f6\u95f4\u56fe\u3002"
    ),
    rows(
      "\u62a5\u544a\u4e0e\u5176\u5b83",
      "reports/summary_report.*" = "\u6c47\u603b\u62a5\u544a\uff08html/pdf/qmd\uff09\uff0c\u628a\u4e0a\u9762\u7684\u7ed3\u679c\u4e32\u6210\u56fe\u6587\u62a5\u544a\u3002",
      "workflow_manifest.csv" = "\u672c\u6b21\u8fd0\u884c\u4ea7\u51fa\u7684\u6240\u6709\u6587\u4ef6\u6e05\u5355\u3002",
      "config_used.yml" = "\u672c\u6b21\u8fd0\u884c\u5b9e\u9645\u4f7f\u7528\u7684\u914d\u7f6e\uff0c\u7528\u4e8e\u590d\u73b0\u3002",
      "logs/" = "\u8fd0\u884c\u65e5\u5fd7\uff0c\u6392\u9519\u7528\u3002"
    )
  ))
}

shiny_key_files_table <- function(state) {
  specs <- shiny_key_file_specs()
  paths <- vapply(specs$relative_path, function(relative_path) {
    resolve_key_file_path(state, relative_path) %||% NA_character_
  }, character(1))
  paths <- unname(paths)
  available <- !is.na(paths)

  data.frame(
    file = specs$display_label,
    status = ifelse(available, "Available", "Missing"),
    next_step = ifelse(available, "", specs$missing_action),
    path = paths,
    stringsAsFactors = FALSE
  )
}

shiny_key_file_specs <- function() {
  data.frame(
    display_label = c(
      "Run summary CSV",
      "Model comparison CSV",
      "Event summary CSV",
      "Best-fit events CSV",
      "BSM event summary CSV",
      "BSM event times CSV",
      "+J sensitivity CSV",
      "Workflow manifest CSV",
      "Report",
      "Result bundle",
      "Diagnostic bundle"
    ),
    relative_path = c(
      "tables/shiny_run_summary.csv",
      "tables/model_comparison.csv",
      "tables/event_summary.csv",
      "tables/best_fit_events.csv",
      "tables/bsm_event_summary.csv",
      "tables/bsm_event_times.csv",
      "tables/model_sensitivity.csv",
      "tables/workflow_manifest.csv",
      "reports/summary_report.html",
      "bundle:result",
      "bundle:diagnostic"
    ),
    missing_action = c(
      "Run or load workflow results, then refresh key files.",
      "Run or load workflow results.",
      "Run or load workflow results with ancestral-state outputs.",
      "Run or load workflow results with ancestral-state outputs.",
      "Enable BSM stochastic mapping, then run workflow.",
      "Enable BSM stochastic mapping, then run workflow.",
      "Run or load workflow results.",
      "Run or load workflow results, then refresh key files.",
      "Click Render report.",
      "Click Create bundle if missing.",
      "Click Create diagnostic bundle."
    ),
    stringsAsFactors = FALSE
  )
}

resolve_key_file_path <- function(state, relative_path) {
  result <- state$result
  if (is.null(result) || is.null(result$project_paths$root)) {
    return(NULL)
  }
  root <- result$project_paths$root

  if (identical(relative_path, "bundle:result")) {
    bundle <- state$bundle %||% NULL
    if (!is.null(bundle) && file.exists(bundle)) {
      return(as_path(bundle))
    }
    return(NULL)
  }

  if (identical(relative_path, "bundle:diagnostic")) {
    bundle <- state$diagnostic_bundle %||% NULL
    if (!is.null(bundle) && file.exists(bundle)) {
      return(as_path(bundle))
    }
    return(NULL)
  }

  if (identical(relative_path, "reports/summary_report.html")) {
    report <- report_preview_path(state)
    if (!is.null(report)) {
      return(report)
    }
  }

  manifest <- state$manifest %||% result$workflow_manifest %||% NULL
  if (!is.null(manifest) && nrow(manifest) > 0L && "relative_path" %in% names(manifest) &&
      relative_path %in% manifest$relative_path) {
    path <- file.path(root, relative_path)
    if (file.exists(path)) {
      return(as_path(path))
    }
  }

  path <- file.path(root, relative_path)
  if (file.exists(path)) {
    return(as_path(path))
  }

  NULL
}

fitted_models_label <- function(model_table, comparison) {
  if (!is.null(model_table) && nrow(model_table) > 0L && "status" %in% names(model_table)) {
    return(paste0(sum(model_table$status == "completed", na.rm = TRUE), " of ", nrow(model_table)))
  }
  if (!is.null(comparison) && nrow(comparison) > 0L) {
    return(as.character(nrow(comparison)))
  }
  "not available"
}

failed_models_label <- function(model_table) {
  failed <- failed_model_rows(model_table)
  if (nrow(failed) == 0L) {
    return("none")
  }
  if (!"model" %in% names(failed)) {
    return(as.character(nrow(failed)))
  }
  paste(failed$model, collapse = ", ")
}

failed_model_rows <- function(model_table) {
  if (is.null(model_table) || nrow(model_table) == 0L || !"status" %in% names(model_table)) {
    return(data.frame())
  }
  model_table[!is.na(model_table$status) & tolower(model_table$status) == "failed", , drop = FALSE]
}

best_model_label <- function(comparison) {
  if (is.null(comparison) || nrow(comparison) == 0L || !"model" %in% names(comparison)) {
    return("not available")
  }
  if ("delta_aicc" %in% names(comparison)) {
    values <- comparison$delta_aicc
    if (all(is.na(values))) {
      return("not available")
    }
    rows <- comparison[values == min(values, na.rm = TRUE), , drop = FALSE]
  } else if ("AICc" %in% names(comparison)) {
    values <- comparison$AICc
    if (all(is.na(values))) {
      return("not available")
    }
    rows <- comparison[values == min(values, na.rm = TRUE), , drop = FALSE]
  } else {
    rows <- comparison[1L, , drop = FALSE]
  }
  if (nrow(rows) == 0L) {
    return("not available")
  }
  suffix <- ""
  if ("delta_aicc" %in% names(rows) && !is.na(rows$delta_aicc[[1L]])) {
    suffix <- paste0(" (delta AICc ", format(round(rows$delta_aicc[[1L]], 3L), trim = TRUE), ")")
  } else if ("AICc" %in% names(rows) && !is.na(rows$AICc[[1L]])) {
    suffix <- paste0(" (AICc ", format(round(rows$AICc[[1L]], 3L), trim = TRUE), ")")
  }
  paste(paste(rows$model, collapse = ", "), suffix, sep = "")
}

near_best_models_label <- function(comparison, threshold = 2) {
  if (is.null(comparison) || nrow(comparison) == 0L ||
      !all(c("model", "delta_aicc") %in% names(comparison))) {
    return("not available")
  }
  rows <- comparison[!is.na(comparison$delta_aicc) & comparison$delta_aicc <= threshold, , drop = FALSE]
  if (nrow(rows) == 0L) {
    return("none")
  }
  rows <- rows[order(rows$delta_aicc), , drop = FALSE]
  paste(
    paste0(rows$model, " (delta AICc ", format(round(rows$delta_aicc, 3L), trim = TRUE), ")"),
    collapse = "; "
  )
}

near_best_plus_j_label <- function(comparison, threshold = 2) {
  if (is.null(comparison) || nrow(comparison) == 0L ||
      !all(c("has_j", "delta_aicc") %in% names(comparison))) {
    return("not available")
  }
  rows <- comparison[!is.na(comparison$has_j) & comparison$has_j &
    !is.na(comparison$delta_aicc) & comparison$delta_aicc <= threshold, , drop = FALSE]
  if (nrow(rows) == 0L) {
    return("no")
  }
  paste0("yes: ", near_best_models_label(rows, threshold = threshold))
}

filter_model_comparison_by_j <- function(comparison, has_j) {
  if (is.null(comparison) || nrow(comparison) == 0L || !"has_j" %in% names(comparison)) {
    return(data.frame())
  }
  comparison[!is.na(comparison$has_j) & comparison$has_j == has_j, , drop = FALSE]
}

plus_j_caution_label <- function(comparison, sensitivity) {
  if (!is.null(sensitivity) && nrow(sensitivity) > 0L) {
    section <- if ("section" %in% names(sensitivity)) sensitivity$section else rep("", nrow(sensitivity))
    display_label <- if ("display_label" %in% names(sensitivity)) sensitivity$display_label else rep("", nrow(sensitivity))
    caution_rows <- sensitivity[
      grepl("caution|interpret", section, ignore.case = TRUE) |
        grepl("caution|interpret", display_label, ignore.case = TRUE),
      ,
      drop = FALSE
    ]
    if (nrow(caution_rows) > 0L && "answer" %in% names(caution_rows)) {
      answer <- caution_rows$answer[[1L]]
      if (!is.na(answer) && nzchar(answer)) {
        return(as.character(answer))
      }
    }
  }
  if (!is.null(comparison) && nrow(comparison) > 0L && all(c("has_j", "delta_aicc") %in% names(comparison))) {
    near_best_j <- comparison$has_j & comparison$delta_aicc <= 2
    if (any(near_best_j, na.rm = TRUE)) {
      return("+J model is best or near-best; interpret with sensitivity checks")
    }
  }
  "not triggered"
}

plus_j_next_step <- function(caution, near_best_j) {
  caution <- tolower(caution %||% "")
  near_best_j <- tolower(near_best_j %||% "")
  if (grepl("^yes|sensitivity|caution|best|near-best|\\+j model", caution) ||
      grepl("^yes", near_best_j)) {
    return("Report +J sensitivity and compare with the best non-+J model.")
  }
  if (identical(caution, "not triggered") || identical(near_best_j, "no")) {
    return("Report standard model comparison and keep +J sensitivity table available.")
  }
  "Run or load workflow results to evaluate +J sensitivity."
}

warning_count_label <- function(model_table, warnings) {
  if (!is.null(model_table) && nrow(model_table) > 0L && "warning_count" %in% names(model_table)) {
    return(as.character(sum(model_table$warning_count, na.rm = TRUE)))
  }
  if (!is.null(warnings) && nrow(warnings) > 0L && "warning_count" %in% names(warnings)) {
    return(as.character(sum(warnings$warning_count, na.rm = TRUE)))
  }
  "not available"
}

affected_warning_models_label <- function(warnings) {
  if (is.null(warnings) || nrow(warnings) == 0L || !"model" %in% names(warnings)) {
    return("not available")
  }
  rows <- warnings[
    !is.na(warnings$model) &
      !warnings$model %in% c("No captured warnings", "No captured warnings or failed models"),
    ,
    drop = FALSE
  ]
  if (nrow(rows) == 0L) {
    return("none")
  }
  paste(unique(rows$model), collapse = ", ")
}

max_warning_count_label <- function(warnings) {
  if (is.null(warnings) || nrow(warnings) == 0L || !"warning_count" %in% names(warnings)) {
    return("not available")
  }
  counts <- suppressWarnings(as.numeric(warnings$warning_count))
  if (all(is.na(counts))) {
    return("not available")
  }
  as.character(max(counts, na.rm = TRUE))
}

warning_next_step <- function(total, failed = "none") {
  if (!is.null(failed) && !identical(failed, "none") && !identical(failed, "not available")) {
    return("Inspect failed model error messages and log paths before interpreting results.")
  }
  total_num <- suppressWarnings(as.numeric(total))
  if (is.na(total_num)) {
    return("Run or load workflow results to inspect optimization warnings.")
  }
  if (total_num > 0) {
    return("Inspect warning messages and linked model logs before interpreting results.")
  }
  "No captured warnings; keep logs with the archived result bundle."
}

shiny_model_fit_summary_table <- function(state) {
  comparison <- shiny_model_comparison_table(state)
  sensitivity <- shiny_model_sensitivity_table(state)

  data.frame(
    item = c(
      "Best statistical model",
      "Models within delta AICc <= 2",
      "Best non-+J model",
      "Best +J model",
      "+J interpretation caution",
      "Interpretation boundary"
    ),
    value = c(
      best_model_label(comparison),
      near_best_models_label(comparison),
      best_model_label(filter_model_comparison_by_j(comparison, has_j = FALSE)),
      best_model_label(filter_model_comparison_by_j(comparison, has_j = TRUE)),
      plus_j_caution_label(comparison, sensitivity),
      "Statistical fit is shown separately from biological interpretation."
    ),
    stringsAsFactors = FALSE
  )
}

shiny_plus_j_summary_table <- function(state) {
  comparison <- shiny_model_comparison_table(state)
  sensitivity <- shiny_model_sensitivity_table(state)
  caution <- plus_j_caution_label(comparison, sensitivity)
  near_best_j <- near_best_plus_j_label(comparison)

  data.frame(
    question = c(
      "Is +J best or near-best?",
      "Best +J model",
      "Best non-+J model",
      "Interpretation caution",
      "Recommended next step"
    ),
    answer = c(
      near_best_j,
      best_model_label(filter_model_comparison_by_j(comparison, has_j = TRUE)),
      best_model_label(filter_model_comparison_by_j(comparison, has_j = FALSE)),
      caution,
      plus_j_next_step(caution, near_best_j)
    ),
    stringsAsFactors = FALSE
  )
}

shiny_warning_summary_table <- function(state) {
  warnings <- shiny_warnings_table(state)
  model_table <- state$model_table %||% read_workflow_table(state$result, "model_run_status.csv")
  total <- warning_count_label(model_table, warnings)
  affected <- affected_warning_models_label(warnings)
  failed <- failed_models_label(model_table)

  data.frame(
    item = c("Captured warnings", "Affected models", "Failed models", "Highest warning count", "Recommended next step"),
    value = c(
      total,
      affected,
      failed,
      max_warning_count_label(warnings),
      warning_next_step(total, failed)
    ),
    stringsAsFactors = FALSE
  )
}

shiny_model_comparison_table <- function(state) {
  table <- state$result$model_comparison %||% read_workflow_table(state$result, "model_comparison.csv")
  if (is.null(table) || nrow(table) == 0L) {
    return(data.frame())
  }
  cols <- c(
    "model", "model_family", "has_j", "logLik", "num_params", "AIC",
    "AICc", "delta_aicc", "aicc_weight", "caution_flag",
    "interpretation_note"
  )
  table[, intersect(cols, names(table)), drop = FALSE]
}

shiny_primary_model_comparison_table <- function(state) {
  table <- shiny_model_comparison_table(state)
  if (nrow(table) == 0L) {
    return(data.frame(
      result = "No model comparison available yet",
      next_step = "Run a real workflow or load existing results.",
      stringsAsFactors = FALSE
    ))
  }
  if ("delta_aicc" %in% names(table)) {
    table <- table[order(table$delta_aicc), , drop = FALSE]
  } else if ("AICc" %in% names(table)) {
    table <- table[order(table$AICc), , drop = FALSE]
  }
  cols <- c("model", "logLik", "num_params", "AICc", "delta_aicc", "aicc_weight", "caution_flag")
  table[, intersect(cols, names(table)), drop = FALSE]
}

shiny_model_sensitivity_table <- function(state) {
  table <- state$result$model_sensitivity_table %||% read_workflow_table(state$result, "model_sensitivity.csv")
  if (is.null(table) || nrow(table) == 0L) {
    return(data.frame())
  }
  cols <- c("section", "display_label", "answer", "models", "model_count", "evidence", "interpretation_note")
  table[, intersect(cols, names(table)), drop = FALSE]
}

shiny_warnings_table <- function(state) {
  table <- state$model_table %||% read_workflow_table(state$result, "model_run_status.csv")
  if (is.null(table) || nrow(table) == 0L || !"warning_count" %in% names(table)) {
    return(data.frame())
  }
  warning_rows <- !is.na(table$warning_count) & table$warning_count > 0L
  failed_rows <- if ("status" %in% names(table)) !is.na(table$status) & tolower(table$status) == "failed" else rep(FALSE, nrow(table))
  rows <- table[warning_rows | failed_rows, , drop = FALSE]
  if (nrow(rows) == 0L) {
    return(data.frame(model = "No captured warnings or failed models", warning_count = 0L, warning_messages = ""))
  }
  cols <- c("model", "status", "warning_count", "warning_messages", "error_message", "log_file")
  rows[, intersect(cols, names(rows)), drop = FALSE]
}

shiny_failed_models_table <- function(state) {
  table <- state$model_table %||% read_workflow_table(state$result, "model_run_status.csv")
  rows <- failed_model_rows(table)
  if (nrow(rows) == 0L) {
    return(data.frame(model = "No failed models", status = "", error_message = "", log_file = ""))
  }
  cols <- c("model", "status", "error_message", "log_file", "raw_output_dir", "result_file")
  rows[, intersect(cols, names(rows)), drop = FALSE]
}

shiny_node_state_summary_table <- function(state) {
  table <- state$result$standardized_tables$node_state_summary %||%
    read_workflow_table(state$result, "node_state_summary.csv")
  if (is.null(table) || nrow(table) == 0L) {
    return(data.frame())
  }
  if (all(c("model", "location", "node_index") %in% names(table))) {
    table <- table[order(table$model, table$location, table$node_index), , drop = FALSE]
  }
  cols <- c(
    "model", "location", "node_index", "node_type", "node_label",
    "best_state", "best_probability", "state_count"
  )
  table[, intersect(cols, names(table)), drop = FALSE]
}

shiny_node_state_sensitivity_table <- function(state) {
  table <- state$result$node_state_sensitivity %||%
    state$result$standardized_tables$node_state_sensitivity %||%
    read_workflow_table(state$result, "node_state_sensitivity.csv")
  if (is.null(table) || nrow(table) == 0L) {
    return(data.frame())
  }
  if (all(c("state_differs", "probability_difference_abs") %in% names(table))) {
    table <- table[order(-table$state_differs, -table$probability_difference_abs), , drop = FALSE]
  }
  cols <- c(
    "location", "node_index", "node_type", "node_label",
    "non_j_model", "non_j_state", "non_j_probability",
    "plus_j_model", "plus_j_state", "plus_j_probability",
    "state_differs", "probability_difference", "probability_difference_abs"
  )
  table[, intersect(cols, names(table)), drop = FALSE]
}

shiny_event_summary_table <- function(state) {
  table <- state$result$standardized_tables$event_summary %||%
    read_workflow_table(state$result, "event_summary.csv")
  if (is.null(table) || nrow(table) == 0L) {
    return(data.frame())
  }
  if (all(c("model", "location", "event_count") %in% names(table))) {
    table <- table[order(table$model, table$location, -table$event_count), , drop = FALSE]
  }
  cols <- c("model", "location", "event_label", "event_count", "changed_edges", "interpretation_note")
  table[, intersect(cols, names(table)), drop = FALSE]
}

shiny_primary_event_summary_table <- function(state) {
  table <- shiny_event_summary_table(state)
  if (nrow(table) == 0L) {
    return(data.frame(
      event = "No event summary available yet",
      count = NA_integer_,
      next_step = "Run a real workflow. Event summary is derived from ancestral range changes.",
      stringsAsFactors = FALSE
    ))
  }

  comparison <- shiny_model_comparison_table(state)
  best_model <- best_model_name(comparison)
  if (!is.null(best_model) && "model" %in% names(table)) {
    filtered <- table[table$model == best_model, , drop = FALSE]
    if (nrow(filtered) > 0L) {
      table <- filtered
    }
  }
  if ("location" %in% names(table) && "branch_top_at_node" %in% table$location) {
    table <- table[table$location == "branch_top_at_node", , drop = FALSE]
  }
  if ("event_label" %in% names(table)) {
    changed <- table[table$event_label != "No range change", , drop = FALSE]
    if (nrow(changed) > 0L) {
      table <- changed
    }
  }
  if ("event_count" %in% names(table)) {
    table <- table[order(-table$event_count), , drop = FALSE]
  }
  cols <- c("model", "event_label", "event_count", "changed_edges", "interpretation_note")
  table[, intersect(cols, names(table)), drop = FALSE]
}

shiny_best_fit_events_table <- function(state) {
  table <- state$result$best_fit_events %||%
    state$result$standardized_tables$best_fit_events %||%
    read_workflow_table(state$result, "best_fit_events.csv")
  if (is.null(table) || nrow(table) == 0L) {
    return(data.frame())
  }
  if ("event_time_midpoint" %in% names(table)) {
    table <- table[order(-table$event_time_midpoint, table$event_index %||% seq_len(nrow(table))), , drop = FALSE]
  }
  cols <- c(
    "event_index", "model", "event_time_midpoint", "event_time_min", "event_time_max",
    "direction_label", "direction", "event_label", "parent_state", "child_state",
    "source_region_label", "target_region_label", "node_label", "parent_node_index",
    "node_index", "parent_probability", "child_probability", "event_time_note",
    "interpretation_note"
  )
  table[, intersect(cols, names(table)), drop = FALSE]
}

shiny_primary_best_fit_events_table <- function(state) {
  table <- shiny_best_fit_events_table(state)
  if (nrow(table) == 0L) {
    return(data.frame(
      event = "No best-fit event table available yet",
      time = NA_real_,
      direction = "Run a real workflow with ancestral-state outputs.",
      stringsAsFactors = FALSE
    ))
  }
  cols <- c(
    "event_index", "model", "event_time_midpoint", "direction_label",
    "direction", "event_label", "parent_state", "child_state", "node_label"
  )
  table[, intersect(cols, names(table)), drop = FALSE]
}

shiny_range_change_events_table <- function(state) {
  table <- state$result$standardized_tables$range_change_events %||%
    read_workflow_table(state$result, "range_change_events.csv")
  if (is.null(table) || nrow(table) == 0L) {
    return(data.frame())
  }
  if (all(c("model", "location", "node_index") %in% names(table))) {
    table <- table[order(table$model, table$location, table$node_index), , drop = FALSE]
  }
  cols <- c(
    "model", "location", "parent_node_index", "node_index", "node_label",
    "parent_state", "child_state", "event_label", "gained_areas",
    "lost_areas", "direction_label", "event_time_midpoint",
    "parent_probability", "child_probability",
    "interpretation_note"
  )
  table[, intersect(cols, names(table)), drop = FALSE]
}

shiny_bsm_qc_table <- function(state) {
  table <- state$result$bsm_tables$bsm_qc %||%
    state$result$standardized_tables$bsm_qc %||%
    read_workflow_table(state$result, "bsm_qc.csv")
  if (is.null(table) || nrow(table) == 0L) {
    return(data.frame(
      check = "No reliability checks available",
      next_step = "Enable BSM stochastic mapping and run a real workflow to generate reliability checks.",
      stringsAsFactors = FALSE
    ))
  }
  cols <- c("check", "model", "status", "observed", "expected", "detail")
  table[, intersect(cols, names(table)), drop = FALSE]
}

shiny_bsm_run_status_table <- function(state) {
  table <- state$result$bsm_tables$bsm_run_status %||%
    state$result$standardized_tables$bsm_run_status %||%
    read_workflow_table(state$result, "bsm_run_status.csv")
  if (is.null(table) || nrow(table) == 0L) {
    return(data.frame(
      status = "BSM not run",
      next_step = "Enable Run BSM stochastic mapping in Advanced run options, then run a real workflow.",
      stringsAsFactors = FALSE
    ))
  }
  cols <- c(
    "model", "status", "requested_maps", "completed_maps",
    "maxtries_per_branch", "seed", "warning_count", "error_message", "log_file"
  )
  table[, intersect(cols, names(table)), drop = FALSE]
}

shiny_bsm_event_summary_table <- function(state) {
  table <- state$result$bsm_tables$bsm_event_summary %||%
    state$result$standardized_tables$bsm_event_summary %||%
    read_workflow_table(state$result, "bsm_event_summary.csv")
  if (is.null(table) || nrow(table) == 0L) {
    return(data.frame())
  }
  if ("mean_count" %in% names(table)) {
    table <- table[order(table$model, -table$mean_count), , drop = FALSE]
  }
  cols <- c("model", "event_label", "mean_count", "sd_count", "sum_count", "replicate_count", "interpretation_note")
  table[, intersect(cols, names(table)), drop = FALSE]
}

shiny_primary_bsm_event_summary_table <- function(state) {
  table <- shiny_bsm_event_summary_table(state)
  if (nrow(table) == 0L) {
    return(data.frame(
      event = "No BSM summary available",
      mean_count = NA_real_,
      next_step = "For paper-ready event counts, enable BSM stochastic mapping and run a real workflow.",
      stringsAsFactors = FALSE
    ))
  }
  cols <- c("model", "event_label", "mean_count", "sd_count", "replicate_count")
  table[, intersect(cols, names(table)), drop = FALSE]
}

shiny_biogeographic_process_summary_table <- function(state) {
  table <- state$result$bsm_tables$biogeographic_process_summary %||%
    state$result$standardized_tables$biogeographic_process_summary %||%
    read_workflow_table(state$result, "biogeographic_process_summary.csv")
  if (is.null(table) || nrow(table) == 0L) {
    return(data.frame(
      process = "No biogeographic process synthesis available",
      next_step = "Enable BSM stochastic mapping and run a real workflow to generate the biogeographic process synthesis.",
      stringsAsFactors = FALSE
    ))
  }
  if ("mean_count" %in% names(table)) {
    group_order <- factor(table$process_group, levels = c("cladogenetic", "anagenetic"))
    table <- table[order(table$model, group_order, -table$mean_count), , drop = FALSE]
  }
  cols <- c(
    "model", "process_group", "process_label", "mean_count", "sd_count",
    "proportion_within_group", "proportion_overall"
  )
  table[, intersect(cols, names(table)), drop = FALSE]
}

shiny_bsm_dispersal_routes_table <- function(state) {
  table <- state$result$bsm_tables$bsm_dispersal_routes %||%
    state$result$standardized_tables$bsm_dispersal_routes %||%
    read_workflow_table(state$result, "bsm_dispersal_routes.csv")
  if (is.null(table) || nrow(table) == 0L) {
    return(data.frame())
  }
  if ("mean_count" %in% names(table)) {
    table <- table[!is.na(table$mean_count) & table$mean_count > 0, , drop = FALSE]
    table <- table[order(table$model, table$route_type, -table$mean_count), , drop = FALSE]
  }
  cols <- c("model", "route_type", "direction_label", "source_region", "target_region", "mean_count", "sd_count")
  table[, intersect(cols, names(table)), drop = FALSE]
}

shiny_bsm_event_times_table <- function(state) {
  table <- state$result$bsm_tables$bsm_event_times %||%
    state$result$standardized_tables$bsm_event_times %||%
    read_workflow_table(state$result, "bsm_event_times.csv")
  if (is.null(table) || nrow(table) == 0L) {
    return(data.frame())
  }
  if ("event_time_before_present" %in% names(table)) {
    table <- table[order(table$model, table$replicate, -table$event_time_before_present), , drop = FALSE]
  }
  cols <- c(
    "model", "replicate", "event_class", "event_label",
    "event_time_before_present", "direction_label", "parent_state",
    "child_state", "node_label"
  )
  table[, intersect(cols, names(table)), drop = FALSE]
}

shiny_primary_bsm_event_times_table <- function(state) {
  table <- shiny_bsm_event_times_table(state)
  if (nrow(table) == 0L) {
    return(data.frame(
      event = "No BSM event-time table available",
      time = NA_real_,
      direction = "Run BSM stochastic mapping to estimate event timing and source-target directions.",
      stringsAsFactors = FALSE
    ))
  }
  table
}

best_model_name <- function(comparison) {
  if (is.null(comparison) || nrow(comparison) == 0L || !"model" %in% names(comparison)) {
    return(NULL)
  }
  if ("delta_aicc" %in% names(comparison) && !all(is.na(comparison$delta_aicc))) {
    return(comparison$model[[which.min(comparison$delta_aicc)]])
  }
  if ("AICc" %in% names(comparison) && !all(is.na(comparison$AICc))) {
    return(comparison$model[[which.min(comparison$AICc)]])
  }
  comparison$model[[1L]]
}

read_workflow_table <- function(result, filename) {
  if (is.null(result) || is.null(result$project_paths$tables)) {
    return(NULL)
  }
  path <- file.path(result$project_paths$tables, filename)
  if (!file.exists(path)) {
    return(NULL)
  }
  utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
}

shiny_table_status_table <- function(state) {
  specs <- shiny_table_status_specs()
  rows <- lapply(seq_len(nrow(specs)), function(i) {
    relative_path <- specs$relative_path[[i]]
    path <- resolve_table_status_path(state, relative_path)
    status <- table_file_status(path)
    missing_reason <- if (identical(status$status, "Available")) "" else
      shiny_table_missing_reason(state, relative_path, status$error_message)
    next_step <- if (identical(status$status, "Available")) "" else
      shiny_table_next_step(relative_path, missing_reason, specs$missing_action[[i]])

    data.frame(
      table = specs$display_label[[i]],
      status = status$status,
      rows = status$rows,
      columns = status$columns,
      missing_reason = missing_reason,
      next_step = next_step,
      path = path %||% NA_character_,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

shiny_table_status_specs <- function() {
  data.frame(
    display_label = c(
      "Run summary",
      "Model run status",
      "Model comparison",
      "Event summary",
      "Best-fit events",
      "Range-change events",
      "BSM run status",
      "BSM event summary",
      "BSM dispersal routes",
      "BSM event times",
      "+J sensitivity",
      "Model parameters",
      "Root states",
      "Node states",
      "Node sensitivity",
      "Input validation",
      "Workflow manifest"
    ),
    relative_path = c(
      "tables/shiny_run_summary.csv",
      "tables/model_run_status.csv",
      "tables/model_comparison.csv",
      "tables/event_summary.csv",
      "tables/best_fit_events.csv",
      "tables/range_change_events.csv",
      "tables/bsm_run_status.csv",
      "tables/bsm_event_summary.csv",
      "tables/bsm_dispersal_routes.csv",
      "tables/bsm_event_times.csv",
      "tables/model_sensitivity.csv",
      "tables/model_parameters.csv",
      "tables/root_state_probabilities.csv",
      "tables/node_state_summary.csv",
      "tables/node_state_sensitivity.csv",
      "tables/input_validation.csv",
      "tables/workflow_manifest.csv"
    ),
    missing_action = c(
      "Run or load workflow results, then refresh key files.",
      "Run workflow.",
      "Run workflow with model fitting or load existing results.",
      "Run workflow with ancestral-state outputs available.",
      "Run workflow with model fitting and ancestral-state outputs available.",
      "Run workflow with ancestral-state outputs available.",
      "Enable BSM stochastic mapping, then run workflow.",
      "Enable BSM stochastic mapping, then run workflow.",
      "Enable BSM stochastic mapping, then run workflow.",
      "Enable BSM stochastic mapping, then run workflow.",
      "Run workflow with model comparison or load existing results.",
      "Run workflow with model fitting or load existing results.",
      "Run workflow with BioGeoBEARS outputs available.",
      "Run workflow with BioGeoBEARS outputs available.",
      "Run workflow with both +J and non-+J node summaries available.",
      "Validate inputs or run workflow.",
      "Refresh key files."
    ),
    stringsAsFactors = FALSE
  )
}

resolve_table_status_path <- function(state, relative_path) {
  result <- state$result
  if (is.null(result) || is.null(result$project_paths$root)) {
    return(NULL)
  }
  root <- result$project_paths$root
  path <- file.path(root, relative_path)
  if (file.exists(path)) {
    return(as_path(path))
  }
  NULL
}

table_file_status <- function(path) {
  empty <- list(status = "Missing", rows = NA_integer_, columns = NA_integer_, error_message = NA_character_)
  if (is.null(path) || is.na(path) || !file.exists(path)) {
    return(empty)
  }
  tryCatch(
    {
      table <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
      list(status = "Available", rows = nrow(table), columns = ncol(table), error_message = NA_character_)
    },
    error = function(e) {
      list(status = "Failed", rows = NA_integer_, columns = NA_integer_, error_message = conditionMessage(e))
    }
  )
}

shiny_table_missing_reason <- function(state, relative_path, error_message = NA_character_) {
  if (!is.na(error_message) && nzchar(error_message)) {
    return(paste("CSV could not be read:", error_message))
  }
  result <- state$result
  if (is.null(result) || is.null(result$project_paths$root)) {
    return("No workflow result loaded.")
  }
  if (shiny_workflow_manifest_has_path(state, relative_path)) {
    return("Workflow manifest lists this table, but the CSV file is missing.")
  }
  "Expected CSV was not found in the tables directory."
}

shiny_workflow_manifest_has_path <- function(state, relative_path) {
  manifest <- state$manifest %||% NULL
  if (is.null(manifest) || nrow(manifest) == 0L) {
    manifest <- state$result$workflow_manifest %||% NULL
  }
  !is.null(manifest) && nrow(manifest) > 0L &&
    "relative_path" %in% names(manifest) &&
    relative_path %in% manifest$relative_path
}

shiny_table_next_step <- function(relative_path, missing_reason, fallback) {
  reason <- tolower(missing_reason %||% "")
  if (grepl("no workflow result", reason)) {
    return("Run workflow or load existing results.")
  }
  if (grepl("could not be read", reason)) {
    return("Open the CSV path, fix or regenerate the table, then refresh key files.")
  }
  if (grepl("manifest lists", reason)) {
    return("Refresh key files; if still missing, rerun workflow generation for this table.")
  }
  fallback
}

shiny_figure_dashboard_table <- function(state) {
  figures <- shiny_dashboard_figures()
  paths <- vapply(figures$figure, function(figure) {
    shiny_named_figure_path(state, figure) %||% NA_character_
  }, character(1))
  reasons <- vapply(figures$figure, function(figure) {
    if (!is.na(paths[[figure]])) {
      return("")
    }
    shiny_figure_missing_reason(state, figure)
  }, character(1))
  data.frame(
    figure = figures$display_label,
    status = vapply(figures$figure, function(figure) {
      if (!is.na(paths[[figure]])) {
        return("Available")
      }
      rows <- shiny_figure_manifest_rows(state, figure)
      if (nrow(rows) > 0L && "status" %in% names(rows) &&
          any(tolower(rows$status %||% "") == "failed", na.rm = TRUE)) {
        return("Failed")
      }
      "Missing"
    }, character(1)),
    preview = ifelse(is.na(paths), "Not shown", "Shown below"),
    missing_reason = reasons,
    next_step = ifelse(
      is.na(paths),
      vapply(reasons, shiny_figure_next_step, character(1)),
      ""
    ),
    path = paths,
    stringsAsFactors = FALSE
  )
}

shiny_dashboard_figures <- function() {
  data.frame(
    figure = c(
      "model_comparison",
      "root_state_probabilities",
      "node_state_summary_best_model",
      "node_state_summary_best_non_j",
      "node_state_summary_best_plus_j",
      "node_state_sensitivity",
      "event_summary"
    ),
    display_label = c(
      "\u6a21\u578b\u6bd4\u8f83",
      "\u6839\u72b6\u6001\u6982\u7387",
      "\u6700\u4f18\u6a21\u578b\u8282\u70b9\u72b6\u6001",
      "\u6700\u4f18\u975e +J \u8282\u70b9\u72b6\u6001",
      "\u6700\u4f18 +J \u8282\u70b9\u72b6\u6001",
      "\u8282\u70b9\u72b6\u6001\u654f\u611f\u6027",
      "\u4e8b\u4ef6\u7edf\u8ba1"
    ),
    stringsAsFactors = FALSE
  )
}

shiny_figure_manifest_rows <- function(state, figure) {
  result <- state$result
  if (is.null(result) || is.null(result$figure_manifest)) {
    return(data.frame())
  }
  manifest <- result$figure_manifest
  if (is.null(manifest) || nrow(manifest) == 0L || !"figure" %in% names(manifest)) {
    return(data.frame())
  }
  rows <- manifest[manifest$figure == figure, , drop = FALSE]
  if (nrow(rows) > 0L && "format" %in% names(rows)) {
    rows <- rows[tolower(rows$format) == "png", , drop = FALSE]
  }
  rows
}

shiny_figure_missing_reason <- function(state, figure) {
  result <- state$result
  if (is.null(result)) {
    return("No workflow result loaded.")
  }

  rows <- shiny_figure_manifest_rows(state, figure)
  if (nrow(rows) > 0L) {
    status <- if ("status" %in% names(rows)) rows$status[[1L]] else "unknown"
    error_message <- first_non_empty(rows$error_message %||% character())
    if (!is.na(status) && identical(tolower(status), "failed")) {
      if (!is.null(error_message)) {
        return(paste("Figure generation failed:", error_message))
      }
      return("Figure generation failed.")
    }
    if ("path" %in% names(rows)) {
      path <- rows$path[[1L]] %||% ""
      if (is.na(path)) {
        path <- ""
      }
      if (nzchar(path) && !file.exists(path)) {
        return("Figure manifest points to a missing PNG file.")
      }
    }
    return(paste("Figure generation status:", status %||% "unknown"))
  }

  figures_dir <- result$project_paths$figures %||% NULL
  if (!is.null(figures_dir)) {
    expected <- file.path(figures_dir, paste0(figure, ".png"))
    if (!file.exists(expected)) {
      return("Expected PNG was not found in the figures directory.")
    }
  }
  "No figure manifest entry is available for this named figure."
}

shiny_figure_next_step <- function(reason) {
  reason <- tolower(reason %||% "")
  if (grepl("no workflow result", reason)) {
    return("Run workflow or load existing results.")
  }
  if (grepl("failed", reason)) {
    return("Inspect figure_manifest.csv and rerun figure generation after fixing the source table.")
  }
  if (grepl("missing png|expected png|manifest points", reason)) {
    return("Refresh key files, then rerun workflow or regenerate figures.")
  }
  "Run workflow with figure generation enabled."
}

first_non_empty <- function(x) {
  x <- as.character(x %||% character())
  x <- x[!is.na(x) & nzchar(trimws(x))]
  if (length(x) == 0L) {
    return(NULL)
  }
  x[[1L]]
}

shiny_named_figure_image <- function(state, figure) {
  path <- shiny_named_figure_path(state, figure)
  if (is.null(path)) {
    return(NULL)
  }
  list(
    src = path,
    contentType = "image/png",
    alt = figure,
    width = "100%"
  )
}

shiny_named_figure_path <- function(state, figure) {
  result <- state$result
  if (is.null(result)) {
    return(NULL)
  }

  manifest <- result$figure_manifest
  if (!is.null(manifest) && nrow(manifest) > 0L &&
      all(c("figure", "format", "status", "path") %in% names(manifest))) {
    rows <- manifest[
      manifest$figure == figure & manifest$format == "png" &
        manifest$status == "created" & file.exists(manifest$path),
      ,
      drop = FALSE
    ]
    if (nrow(rows) > 0L) {
      return(as_path(rows$path[[1L]]))
    }
  }

  figures_dir <- result$project_paths$figures %||% NULL
  if (!is.null(figures_dir)) {
    path <- file.path(figures_dir, paste0(figure, ".png"))
    if (file.exists(path)) {
      return(as_path(path))
    }
  }

  choices <- figure_preview_choices(result, state$manifest)
  if (length(choices) > 0L) {
    basenames <- tools::file_path_sans_ext(basename(unname(choices)))
    match_index <- match(figure, basenames)
    if (!is.na(match_index) && file.exists(unname(choices)[[match_index]])) {
      return(as_path(unname(choices)[[match_index]]))
    }
  }

  NULL
}

update_table_preview_choices <- function(session, state) {
  choices <- table_preview_choices(state$result, state$manifest)
  if (length(choices) == 0L) {
    shiny::updateSelectInput(session, "table_preview", choices = c("No CSV tables available" = ""), selected = "")
  } else {
    shiny::updateSelectInput(session, "table_preview", choices = choices, selected = choices[[1L]])
  }
  invisible(choices)
}

table_preview_choices <- function(result, manifest = NULL) {
  if (is.null(result) || is.null(result$project_paths$root)) {
    return(stats::setNames(character(), character()))
  }
  root <- result$project_paths$root
  paths <- character()
  labels <- character()

  if (!is.null(manifest) && nrow(manifest) > 0L && all(c("category", "relative_path") %in% names(manifest))) {
    rows <- manifest[
      manifest$category == "tables" & grepl("[.]csv$", manifest$relative_path, ignore.case = TRUE),
      ,
      drop = FALSE
    ]
    if (nrow(rows) > 0L) {
      paths <- file.path(root, rows$relative_path)
      labels <- rows$relative_path
    }
  }

  if (length(paths) == 0L) {
    tables_dir <- result$project_paths$tables %||% file.path(root, "tables")
    paths <- list.files(tables_dir, pattern = "[.]csv$", full.names = TRUE)
    labels <- file.path("tables", basename(paths))
  }

  exists <- file.exists(paths)
  paths <- as_path(paths[exists])
  labels <- labels[exists]
  if (length(paths) == 0L) {
    return(stats::setNames(character(), character()))
  }
  paths <- paths[order(labels)]
  labels <- labels[order(labels)]
  stats::setNames(paths, labels)
}

resolve_table_preview_path <- function(input, state) {
  selected <- input$table_preview %||% ""
  if (nzchar(selected) && file.exists(selected)) {
    return(as_path(selected))
  }
  choices <- table_preview_choices(state$result, state$manifest)
  if (length(choices) == 0L) {
    return(NULL)
  }
  as_path(choices[[1L]])
}

read_table_preview <- function(input, state) {
  path <- resolve_table_preview_path(input, state)
  if (is.null(path)) {
    return(data.frame())
  }
  utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
}

update_figure_preview_choices <- function(session, state) {
  choices <- figure_preview_choices(state$result, state$manifest)
  if (length(choices) == 0L) {
    shiny::updateSelectInput(session, "figure_preview", choices = c("No PNG figures available" = ""), selected = "")
  } else {
    shiny::updateSelectInput(session, "figure_preview", choices = choices, selected = choices[[1L]])
  }
  invisible(choices)
}

figure_preview_choices <- function(result, manifest = NULL) {
  if (is.null(result) || is.null(result$project_paths$root)) {
    return(stats::setNames(character(), character()))
  }
  root <- result$project_paths$root
  paths <- character()
  labels <- character()

  if (!is.null(manifest) && nrow(manifest) > 0L && all(c("category", "relative_path") %in% names(manifest))) {
    rows <- manifest[
      manifest$category == "figures" & grepl("[.]png$", manifest$relative_path, ignore.case = TRUE),
      ,
      drop = FALSE
    ]
    if (nrow(rows) > 0L) {
      paths <- file.path(root, rows$relative_path)
      labels <- rows$relative_path
    }
  }

  if (length(paths) == 0L) {
    figure_dir <- result$project_paths$figures %||% file.path(root, "figures")
    paths <- list.files(figure_dir, pattern = "[.]png$", full.names = TRUE)
    labels <- file.path("figures", basename(paths))
  }

  exists <- file.exists(paths)
  paths <- as_path(paths[exists])
  labels <- labels[exists]
  if (length(paths) == 0L) {
    return(stats::setNames(character(), character()))
  }
  paths <- paths[order(labels)]
  labels <- labels[order(labels)]
  stats::setNames(paths, labels)
}

resolve_figure_preview_path <- function(input, state) {
  selected <- input$figure_preview %||% ""
  if (nzchar(selected) && file.exists(selected)) {
    return(as_path(selected))
  }
  choices <- figure_preview_choices(state$result, state$manifest)
  if (length(choices) == 0L) {
    return(NULL)
  }
  as_path(choices[[1L]])
}

check_shiny_available <- function() {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("The shiny package is required to launch the GUI. Install it with install.packages('shiny').", call. = FALSE)
  }
  invisible(TRUE)
}

resolve_shiny_config_path <- function(input) {
  upload <- input$config_upload %||% NULL
  if (!is.null(upload) && nrow(upload) > 0L && "datapath" %in% names(upload)) {
    uploaded_path <- upload$datapath[[1L]]
    if (!is.null(uploaded_path) && file.exists(uploaded_path)) {
      return(uploaded_path)
    }
  }
  path <- trimws(input$config_path %||% "")
  if (!nzchar(path)) {
    stop("Provide an analysis.yml path or upload a YAML config file.", call. = FALSE)
  }
  path
}

apply_shiny_wizard_overrides <- function(config, input) {
  cfg <- config
  name <- shiny_trimmed_input(input, "wizard_project_name")
  if (!is.null(name)) {
    cfg$project$name <- name
  }
  tree_file <- shiny_optional_upload_path(input$wizard_tree)
  if (!is.null(tree_file)) {
    cfg$inputs$tree_file <- tree_file
  }
  geography_file <- shiny_optional_upload_path(input$wizard_geography)
  if (!is.null(geography_file)) {
    cfg$inputs$geography_file <- geography_file
  }
  regions_file <- shiny_optional_upload_path(input$wizard_regions)
  if (!is.null(regions_file)) {
    cfg$inputs$regions_file <- regions_file
  }
  max_range_size <- suppressWarnings(as.integer(input$wizard_max_range_size %||% NA_integer_))
  if (!is.na(max_range_size) && max_range_size >= 1L) {
    cfg$inputs$max_range_size <- max_range_size
  }
  models <- input$wizard_models %||% character()
  if (length(models) > 0L) {
    cfg$models$run <- as.character(models)
  }
  constraint_fields <- shiny_constraint_fields()$field
  uploaded_constraints <- list()
  for (field in constraint_fields) {
    path <- shiny_optional_upload_path(input[[paste0("wizard_constraint_", field)]])
    if (!is.null(path)) {
      uploaded_constraints[[field]] <- path
    }
  }
  if (length(uploaded_constraints) > 0L) {
    cfg$advanced <- cfg$advanced %||% list()
    cfg$advanced$constraints <- cfg$advanced$constraints %||% list()
    for (field in names(uploaded_constraints)) {
      cfg$advanced$constraints[[field]] <- uploaded_constraints[[field]]
    }
  }
  cfg
}

shiny_optional_upload_path <- function(upload) {
  if (is.null(upload) || !is.data.frame(upload) || nrow(upload) == 0L || !"datapath" %in% names(upload)) {
    return(NULL)
  }
  path <- upload$datapath[[1L]]
  if (is.null(path) || is.na(path) || !file.exists(path)) {
    return(NULL)
  }
  as_path(path)
}

apply_shiny_config_overrides <- function(config, input, output_dir = NULL) {
  cfg <- config
  if (!is.null(output_dir)) {
    cfg$project$output_dir <- output_dir
  }
  apply_shiny_run_option_overrides(cfg, input)
}

apply_shiny_run_option_overrides <- function(config, input) {
  cfg <- config
  cfg$analysis <- cfg$analysis %||% list()
  cfg$analysis$run_stochastic_mapping <- isTRUE(input$run_stochastic_mapping %||% FALSE)
  selection <- trimws(input$stochastic_mapping_model %||% "")
  if (nzchar(selection)) {
    cfg$analysis$stochastic_mapping_model <- selection
  }
  replicates <- suppressWarnings(as.integer(input$stochastic_mapping_replicates %||% NA_integer_))
  if (!is.na(replicates) && replicates > 0L) {
    cfg$analysis$stochastic_mapping_replicates <- replicates
    cfg$analysis$stochastic_mapping_max_maps_to_try <- max(replicates, ceiling(replicates * 2))
  }
  seed <- suppressWarnings(as.integer(input$stochastic_mapping_seed %||% NA_integer_))
  if (!is.na(seed)) {
    cfg$analysis$stochastic_mapping_seed <- seed
  }
  cfg
}

shiny_trimmed_input <- function(input, id) {
  value <- trimws(input[[id]] %||% "")
  if (!nzchar(value)) {
    return(NULL)
  }
  value
}

write_shiny_workflow_config <- function(config, source_config) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("The yaml package is required to write edited Shiny workflow configs.", call. = FALSE)
  }
  cfg <- absolutize_shiny_config_paths(config, base_dir = dirname(source_config))
  cfg$.config_file <- NULL
  path <- tempfile("ibgb-shiny-analysis-", fileext = ".yml")
  yaml::write_yaml(cfg, path)
  as_path(path)
}

absolutize_shiny_config_paths <- function(config, base_dir) {
  cfg <- config
  input_fields <- c("tree_file", "geography_file", "regions_file")
  for (field in input_fields) {
    if (!is.null(cfg$inputs[[field]]) && nzchar(cfg$inputs[[field]])) {
      cfg$inputs[[field]] <- resolve_config_path(cfg$inputs[[field]], base_dir)
    }
  }

  constraint_fields <- c(
    "times_file", "dists_file", "distance_file", "dispersal_multipliers_file",
    "areas_allowed_file", "areas_adjacency_file", "area_of_areas_file"
  )
  for (field in intersect(names(cfg$advanced$constraints %||% list()), constraint_fields)) {
    value <- cfg$advanced$constraints[[field]]
    if (!is.null(value) && nzchar(value)) {
      cfg$advanced$constraints[[field]] <- resolve_config_path(value, base_dir)
    }
  }
  cfg
}

planned_model_table <- function(config) {
  models <- config$models$run %||% valid_models()
  data.frame(
    model = models,
    model_family = model_family(models),
    has_j = is_j_model(models),
    status = "planned",
    stringsAsFactors = FALSE
  )
}

require_workflow_result <- function(result) {
  if (is.null(result)) {
    stop("Run the workflow before requesting this action.", call. = FALSE)
  }
  invisible(TRUE)
}

table_head <- function(x, n = 20L) {
  if (is.null(x)) {
    return(data.frame())
  }
  utils::head(x, n)
}

iBGB_head_styles <- function() {
  shiny::tags$head(
    shiny::tags$style(shiny::HTML(
      ".container-fluid{max-width:1180px} .well{border-radius:4px} ",
      ".btn,a.btn{border-radius:4px;background-color:#1868b8;border:1px solid #14528f;color:#fff;font-weight:600} ",
      ".btn:hover,.btn:focus,.btn:active,a.btn:hover,a.btn:focus,a.btn:active{background-color:#14528f;color:#fff} ",
      ".ibgb-status{font-weight:600;margin:8px 0} ",
      ".ibgb-status.info{color:#22577a} .ibgb-status.error{color:#b00020} ",
      ".ibgb-control-section{border-top:1px solid #ddd;margin-top:14px;padding-top:12px} ",
      ".ibgb-control-section:first-child{border-top:0;margin-top:0;padding-top:0} ",
      ".ibgb-control-title{font-weight:600;margin-bottom:8px} ",
      ".ibgb-action-grid{display:grid;grid-template-columns:1fr;gap:7px} ",
      ".ibgb-action-grid .btn{width:100%;text-align:left} ",
      ".ibgb-downloads{margin:0} .ibgb-downloads .btn{width:100%;text-align:left;margin-bottom:7px} ",
      ".ibgb-run-summary-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(210px,1fr));gap:10px;margin:0 0 12px 0} ",
      ".ibgb-run-summary-card{border:1px solid #d8dee4;border-left:4px solid #6b7280;border-radius:4px;padding:10px;background:#fff} ",
      ".ibgb-run-summary-card.info{border-left-color:#22577a} .ibgb-run-summary-card.warning{border-left-color:#b26a00} ",
      ".ibgb-run-summary-card.good{border-left-color:#2e7d32} .ibgb-run-summary-card.muted{border-left-color:#8c959f} ",
      ".ibgb-run-summary-label{font-size:12px;font-weight:600;color:#57606a;margin-bottom:4px} ",
      ".ibgb-run-summary-value{font-size:15px;font-weight:600;color:#24292f;overflow-wrap:anywhere} ",
      ".ibgb-key-files-title{font-weight:600;margin:12px 0 6px 0} ",
      ".ibgb-home-note{background:#f6f8fa;border:1px solid #d8dee4;border-radius:4px;padding:10px 12px;margin:8px 0 14px 0} ",
      ".ibgb-next-action{border:1px solid #bfdbfe;border-left:4px solid #2563eb;background:#eff6ff;border-radius:4px;padding:10px 12px;margin:8px 0 14px 0} ",
      ".ibgb-next-action-title{font-size:12px;font-weight:600;color:#1d4ed8;margin-bottom:4px} ",
      ".ibgb-next-action-step{font-size:15px;font-weight:600;color:#1f2937;margin-bottom:3px} ",
      ".ibgb-next-action-detail{color:#374151} ",
      ".ibgb-collapsible{border-top:1px solid #ddd;margin-top:12px;padding-top:10px} ",
      ".ibgb-collapsible>summary{font-weight:600;cursor:pointer;margin-bottom:8px;list-style:none} ",
      ".ibgb-collapsible>summary::-webkit-details-marker{display:none} ",
      ".ibgb-collapsible>summary::before{content:\"\u25b6\";color:#1868b8;font-size:11px;margin-right:8px;display:inline-block;transition:transform .15s} ",
      ".ibgb-collapsible[open]>summary::before{transform:rotate(90deg)} ",
      ".ibgb-primary-result{border-top:1px solid #e5e7eb;margin-top:18px;padding-top:16px} ",
      ".ibgb-primary-result:first-child{border-top:0;margin-top:0;padding-top:0} ",
      ".ibgb-preview img{max-width:100%;height:auto;border:1px solid #ddd;display:block} ",
      ".ibgb-preview .shiny-image-output{height:auto !important;min-height:0} ",
      ".ibgb-figure-dashboard{display:grid;grid-template-columns:1fr;gap:18px} ",
      ".ibgb-figure-dashboard h4{margin:6px 0 8px 0} ",
      ".ibgb-figure-dashboard img{max-width:100%;height:auto;border:1px solid #ddd;display:block} ",
      ".ibgb-figure-dashboard .shiny-image-output{height:auto !important;min-height:0} ",
      "#wizard_nav{margin-top:6px} ",
      "#wizard_nav.nav-tabs{border-bottom:2px solid #d0d7de} ",
      "#wizard_nav.nav-tabs>li>a{font-weight:600;color:#57606a;border:0;margin-right:2px} ",
      "#wizard_nav.nav-tabs>li.active>a{color:#0b3d66;border:0;border-bottom:3px solid #22577a;background:transparent} ",
      ".ibgb-step-intro{color:#57606a;margin:14px 0 16px 0;font-size:14px} ",
      ".tab-content>.tab-pane{padding:6px 2px 2px 2px} ",
      ".ibgb-two-col{display:grid;grid-template-columns:1fr 1fr;gap:22px} ",
      "@media (max-width:900px){.ibgb-two-col{grid-template-columns:1fr}} ",
      ".ibgb-choice-card{border:1px solid #d8dee4;border-radius:6px;padding:14px;background:#fff} ",
      ".ibgb-output-row{display:flex;gap:8px;align-items:flex-end} ",
      ".ibgb-output-row .form-group{flex:1;margin-bottom:0} ",
      ".ibgb-output-row .btn{white-space:nowrap;padding:6px 12px} ",
      ".ibgb-upload-row{display:flex;gap:10px;align-items:flex-start} ",
      ".ibgb-upload-row>.form-group{flex:1;margin-bottom:8px} ",
      ".ibgb-upload-row>.btn{white-space:nowrap;margin-top:25px}"
    ))
  )
}

wizard_env_section <- function() {
  shiny_collapsible_section(
    "\u73af\u5883\u4e0e\u5b89\u88c5\uff08\u7b2c\u4e00\u6b21\u4f7f\u7528\u53ef\u5c55\u5f00\u68c0\u67e5 BioGeoBEARS\uff09",
    shiny_action_grid(
      shiny::actionButton("refresh_setup", "\u5237\u65b0\u73af\u5883\u68c0\u67e5")
    ),
    shiny::tags$div(class = "ibgb-key-files-title", "\u5b89\u88c5\u72b6\u6001"),
    shiny::tableOutput("installation_table"),
    shiny::tags$div(class = "ibgb-key-files-title", "BioGeoBEARS \u5b89\u88c5\u8ba1\u5212"),
    shiny::tableOutput("biogeobears_install_plan_table")
  )
}

iBGB_app_ui <- function(default_config, default_output, example_project_dir) {
  shiny::fluidPage(
    iBGB_head_styles(),
    shiny::titlePanel("iBiogeobears"),
    shiny::uiOutput("status"),
    wizard_env_section(),
    shiny::tabsetPanel(
      id = "wizard_nav",
      type = "tabs",
      wizard_step_data(default_config, default_output, example_project_dir),
      wizard_step_analysis(),
      wizard_step_results(),
      wizard_step_cross_clade(),
      wizard_step_help()
    )
  )
}

wizard_step_data <- function(default_config, default_output, example_project_dir) {
  shiny::tabPanel(
    "1 \u00b7 \u6570\u636e",
    shiny::tags$div(
      class = "ibgb-choice-card",
      shiny::tags$div(class = "ibgb-control-title", "\u4f7f\u7528\u4f60\u81ea\u5df1\u7684\u6570\u636e"),
      shiny::textInput("wizard_project_name", "\u9879\u76ee\u540d", value = "my_clade"),
      shiny::tags$div(
        class = "ibgb-upload-row",
        shiny::fileInput(
          "wizard_tree",
          "\u7cfb\u7edf\u6811\u6587\u4ef6",
          accept = c(".nwk", ".newick", ".tree", ".tre")
        ),
        shiny::downloadButton("download_tree_template", "\u6a21\u677f")
      ),
      shiny::tags$div(
        class = "ibgb-upload-row",
        shiny::fileInput("wizard_geography", "\u5206\u5e03\u77e9\u9635 CSV", accept = ".csv"),
        shiny::downloadButton("download_geography_template", "\u6a21\u677f")
      ),
      shiny::tags$div(
        class = "ibgb-upload-row",
        shiny::fileInput("wizard_regions", "\u533a\u57df\u4fe1\u606f CSV", accept = ".csv"),
        shiny::downloadButton("download_regions_template", "\u6a21\u677f")
      ),
      shiny::tags$div(
        class = "ibgb-home-note",
        "\u6ca1\u6709\u6570\u636e\uff1f\u70b9\u6bcf\u4e2a\u4e0a\u4f20\u6846\u53f3\u4fa7\u7684\u201c\u6a21\u677f\u201d\u4e0b\u8f7d\u2014\u2014\u5b83\u4eec\u5c31\u662f\u5185\u7f6e\u793a\u4f8b\u6570\u636e\uff08\u51e0\u4e2a\u7269\u79cd\u3001\u51e0\u4e2a\u533a\u57df\uff09\uff0c\u6539\u6210\u4f60\u81ea\u5df1\u7684\u6570\u636e\u540e\u518d\u4e0a\u4f20\u3002"
      ),
      shiny_collapsible_section(
        "\u9ad8\u7ea7\u7ea6\u675f\uff08\u53ef\u9009\uff0c\u65f6\u95f4\u5206\u5c42\u7b49\u9ad8\u7ea7\u5206\u6790\u7528\uff09",
        shiny::tags$p(
          class = "ibgb-home-note",
          "\u8fd9\u4e9b\u6587\u4ef6\u7528\u4e8e\u65f6\u95f4\u5206\u5c42\u3001\u6269\u6563\u4e58\u6570\u3001\u533a\u57df\u76f8\u90bb\u7b49\u9ad8\u7ea7\u5206\u6790\uff0c\u4e00\u822c\u7528\u4e0d\u5230\uff0c\u53ef\u7559\u7a7a\u3002\u6bcf\u4e2a\u4e0a\u4f20\u6846\u53f3\u4fa7\u7684\u201c\u6a21\u677f\u201d\u53ef\u4e0b\u8f7d\u793a\u4f8b\u683c\u5f0f\u3002"
        ),
        shiny_wizard_constraint_inputs()
      ),
      shiny::numericInput("wizard_max_range_size", "\u6700\u5927\u5206\u5e03\u533a\u6570\u91cf", value = 3L, min = 1L, step = 1L),
      shiny::checkboxGroupInput(
        "wizard_models",
        "\u8981\u8fd0\u884c\u7684\u6a21\u578b",
        choices = valid_models(),
        selected = valid_models()
      ),
      shiny::tags$div(class = "ibgb-key-files-title", "\u7ed3\u679c\u4fdd\u5b58\u4f4d\u7f6e"),
      shiny::tags$div(
        class = "ibgb-output-row",
        shiny::textInput("output_dir", "\u6240\u6709\u7ed3\u679c\u4fdd\u5b58\u5230", value = default_output),
        shiny::actionButton("choose_output_dir", "\u9009\u62e9")
      ),
      shiny::tags$div(
        class = "ibgb-home-note",
        "\u8fd0\u884c\u540e\u4f1a\u5728\u8fd9\u4e2a\u76ee\u5f55\u4e0b\u751f\u6210 tables\u3001figures\u3001reports\u3001logs\u3002\u4e0a\u4f20\u6570\u636e\u540e\u4e0b\u65b9\u4f1a\u663e\u793a\u6982\u51b5\uff0c\u786e\u8ba4\u65e0\u8bef\u5c31\u5230\u201c\u5206\u6790\u201d\u6807\u7b7e\u8fd0\u884c\u3002"
      )
    ),
    shiny::tags$div(
      class = "ibgb-choice-card",
      shiny::tags$div(class = "ibgb-control-title", "\u6570\u636e\u6982\u51b5"),
      shiny::tags$p(
        class = "ibgb-home-note",
        "\u4e0a\u4f20\u6570\u636e\u540e\u8fd9\u91cc\u663e\u793a\u6982\u51b5\uff08\u7c7b\u4f3c RASP\uff09\uff1a\u6811\u91cc\u6709\u591a\u5c11\u4e2a tip\u3001\u6bcf\u4e2a\u533a\u57df\u6709\u591a\u5c11\u7269\u79cd\u3001\u5206\u5e03\u533a\u5927\u5c0f\u5982\u4f55\u5206\u5e03\u3002\u70b9\u201c\u68c0\u67e5\u8f93\u5165\u201d\u505a\u4e00\u81f4\u6027\u6821\u9a8c\u3002"
      ),
      shiny_action_grid(shiny::actionButton("validate", "\u68c0\u67e5\u8f93\u5165")),
      shiny::tableOutput("data_overview_table"),
      shiny::tags$div(
        class = "ibgb-two-col",
        shiny::tags$div(
          shiny::tags$div(class = "ibgb-key-files-title", "\u5404\u533a\u57df\u7269\u79cd\u6570"),
          shiny::tableOutput("region_occupancy_table")
        ),
        shiny::tags$div(
          shiny::tags$div(class = "ibgb-key-files-title", "\u5206\u5e03\u533a\u5927\u5c0f\u5206\u5e03"),
          shiny::tableOutput("range_size_table")
        )
      ),
      shiny::tags$div(class = "ibgb-key-files-title", "\u8f93\u5165\u9a8c\u8bc1"),
      shiny::tableOutput("validation_table")
    ),
    shiny::tags$div(
      style = "display:none;",
      shiny::textInput("config_path", NULL, value = default_config)
    )
  )
}

wizard_step_analysis <- function() {
  shiny::tabPanel(
    "2 \u00b7 \u5206\u6790",
    shiny_control_section(
      "\u8fd0\u884c",
      shiny_action_grid(shiny::actionButton("run", "\u70b9\u51fb\u5f00\u59cb\u5206\u6790")),
      shiny::tags$div(
        class = "ibgb-home-note",
        "\u8fd0\u884c\u7ed3\u675f\u540e\u5230\u201c3 \u00b7 \u5355\u4e00\u7c7b\u7fa4\u7ed3\u679c\u201d\u6807\u7b7e\u67e5\u770b\u7ed3\u679c\uff1b\u9700\u8981\u56fe\u6587\u62a5\u544a\u65f6\u5728\u90a3\u91cc\u70b9\u201c\u751f\u6210\u62a5\u544a\u201d\uff0c\u518d\u4e0b\u8f7d\u3002"
      )
    ),
    shiny::checkboxInput("dry_run", "\u8bd5\u8fd0\u884c\uff1a\u53ea\u68c0\u67e5\uff0c\u4e0d\u771f\u6b63\u8fd0\u884c BioGeoBEARS", value = TRUE),
    shiny_collapsible_section(
      "BSM \u968f\u673a\u6620\u5c04",
      shiny::tags$div(
        class = "ibgb-home-note",
        "\u52fe\u9009\u540e\u624d\u4f1a\u751f\u6210\u4e8b\u4ef6\u7edf\u8ba1\u3001\u751f\u7269\u5730\u7406\u8fc7\u7a0b\u7efc\u5408\u3001\u8fc7\u7a0b\u901f\u7387\u968f\u65f6\u95f4\u7b49\u7ed3\u679c\uff08\u8de8\u7c7b\u7fa4\u6574\u5408\u7528\u7684 process_rates_through_time.csv \u548c region_process_rates_through_time.csv \u4e5f\u5728\u8fd9\u91cc\u4ea7\u751f\uff09\u3002\u4e0d\u52fe\u9009\u5219\u53ea\u505a\u6a21\u578b\u62df\u5408\u548c\u7956\u5148\u5206\u5e03\u4f30\u8ba1\u3002"
      ),
      shiny::checkboxInput("run_stochastic_mapping", "\u8fd0\u884c BSM \u968f\u673a\u6620\u5c04", value = FALSE),
      shiny::selectInput(
        "stochastic_mapping_model",
        "BSM \u4f7f\u7528\u7684\u6a21\u578b",
        choices = c(
          "\u6700\u4f18\u7edf\u8ba1\u6a21\u578b" = "best",
          "\u6700\u4f18\u975e +J \u6a21\u578b" = "best_non_j",
          "\u6700\u4f18 +J \u6a21\u578b" = "best_plus_j",
          "\u6240\u6709\u5df2\u5b8c\u6210\u6a21\u578b" = "all"
        ),
        selected = "best"
      ),
      shiny::numericInput("stochastic_mapping_replicates", "BSM \u6620\u5c04\u6b21\u6570", value = 100L, min = 1L, step = 1L),
      shiny::numericInput("stochastic_mapping_seed", "BSM \u968f\u673a\u79cd\u5b50", value = 1L, min = 1L, step = 1L)
    )
  )
}

wizard_step_results <- function() {
  shiny::tabPanel(
    "3 \u00b7 \u5355\u4e00\u7c7b\u7fa4\u7ed3\u679c",
    shiny_primary_results_body(),
    shiny_control_section(
      "\u5bfc\u51fa",
      shiny::tags$div(
        class = "ibgb-downloads",
        shiny::downloadButton("download_bundle", "\u4e0b\u8f7d\u7ed3\u679c\u538b\u7f29\u5305\uff08\u5168\u90e8\u7ed3\u679c\u6587\u4ef6\uff09"),
        shiny::actionButton("render_report", "\u751f\u6210\u62a5\u544a"),
        shiny::downloadButton("download_report", "\u4e0b\u8f7d\u62a5\u544a")
      ),
      shiny::tags$div(
        class = "ibgb-home-note",
        "\u8fd0\u884c\u5b8c\u5148\u5728\u4e0a\u9762\u770b\u7ed3\u679c\u3002\u9700\u8981\u56fe\u6587\u62a5\u544a\u65f6\u70b9\u201c\u751f\u6210\u62a5\u544a\u201d\uff1a\u88c5\u4e86 Quarto \u51fa HTML\uff0c\u6ca1\u88c5\u5219\u751f\u6210 .qmd \u6e90\u6587\u4ef6\uff0c\u90fd\u53ef\u7528\u201c\u4e0b\u8f7d\u62a5\u544a\u201d\u4e0b\u8f7d\u3002"
      )
    ),
    shiny_collapsible_section(
      "\u7ed3\u679c\u6587\u4ef6\u8bf4\u660e\uff08\u4e0b\u8f7d\u538b\u7f29\u5305\u540e\u5bf9\u7167\u67e5\u770b\uff09",
      shiny::tags$p(
        class = "ibgb-home-note",
        "\u201c\u4e0b\u8f7d\u7ed3\u679c\u538b\u7f29\u5305\u201d\u91cc\u5305\u542b\u4e0b\u5217\u6587\u4ef6\u3002\u8fd9\u5f20\u8868\u8bf4\u660e\u6bcf\u4e2a\u7ed3\u679c\u6587\u4ef6\u5bf9\u5e94\u4ec0\u4e48\u7ed3\u679c\uff1b\u5b8c\u6574\u8868\u683c\u548c\u9ad8\u6e05\u56fe\u7247\u90fd\u5728\u538b\u7f29\u5305\u91cc\uff0c\u65e0\u9700\u5728\u8fd9\u91cc\u9010\u4e2a\u5c55\u5f00\u3002"
      ),
      shiny::tableOutput("output_file_legend_table")
    )
  )
}

wizard_step_cross_clade <- function() {
  shiny::tabPanel(
    "4 \u00b7 \u8de8\u7c7b\u7fa4",
    shiny::tags$div(
      class = "ibgb-next-action",
      shiny::tags$div(class = "ibgb-next-action-title", "\u9700\u8981\u5148\u8dd1 BSM"),
      shiny::tags$div(
        class = "ibgb-next-action-detail",
        "process_rates_through_time.csv \u548c region_process_rates_through_time.csv \u53ea\u6709\u5728\u201c2 \u00b7 \u5206\u6790\u201d\u91cc\u52fe\u9009\u201c\u8fd0\u884c BSM \u968f\u673a\u6620\u5c04\u201d\u5e76\u771f\u5b9e\u8fd0\u884c\u540e\u624d\u4f1a\u5199\u5165\u5404\u9879\u76ee\u7684 tables/ \u76ee\u5f55\u3002\u82e5\u4f60\u7684 tables/ \u91cc\u6ca1\u6709\u8fd9\u4e24\u4e2a\u6587\u4ef6\uff0c\u8bf4\u660e\u90a3\u6b21\u5206\u6790\u6ca1\u6709\u8dd1 BSM\uff0c\u8bf7\u91cd\u8dd1\u5e76\u52fe\u9009 BSM\u3002"
      )
    ),
    shiny::tags$div(
      class = "ibgb-choice-card",
      shiny::tags$div(class = "ibgb-control-title", "\u603b\u4f53\u8fc7\u7a0b\u901f\u7387\uff08\u8de8\u7c7b\u7fa4\uff09"),
      shiny::tags$div(
        class = "ibgb-home-note",
        "\u4e0a\u4f20\u6bcf\u4e2a\u7c7b\u7fa4\u7684 process_rates_through_time.csv\uff08\u5728\u5404\u5206\u6790\u9879\u76ee\u7684 tables/ \u76ee\u5f55\u4e0b\uff09\u3002\u5efa\u8bae\u5148\u628a\u6bcf\u4e2a\u6587\u4ef6\u91cd\u547d\u540d\u4e3a\u7c7b\u7fa4\u540d\uff08\u5982 CladeA.csv\uff09\uff0c\u7cfb\u7edf\u4f1a\u7528\u6587\u4ef6\u540d\u4f5c\u4e3a\u7c7b\u7fa4\u6807\u7b7e\u3002\u53ef\u4e00\u6b21\u591a\u9009\u6279\u91cf\u4e0a\u4f20\u3002\u5404\u7c7b\u7fa4\u9700\u4f7f\u7528\u53ef\u6bd4\u8f83\u7684\u65f6\u95f4\u5355\u4f4d\u3002"
      ),
      shiny::fileInput(
        "cross_clade_files",
        "\u4e0a\u4f20\u5404\u7c7b\u7fa4\u7684 process_rates_through_time.csv\uff08\u53ef\u591a\u9009\uff09",
        multiple = TRUE,
        accept = ".csv"
      ),
      shiny::uiOutput("cross_clade_status"),
      shiny::div(class = "ibgb-preview", shiny::imageOutput("cross_clade_plot", height = "520px")),
      shiny::tags$div(class = "ibgb-key-files-title", "\u5408\u5e76\u6570\u636e\u9884\u89c8\uff08\u66f2\u7ebf\u4e3a\u5747\u503c\uff0c\u8272\u5e26\u4e3a 95% CI\uff09"),
      shiny::tableOutput("cross_clade_table"),
      shiny::downloadButton("download_cross_clade", "\u4e0b\u8f7d\u6574\u5408\u7ed3\u679c CSV")
    ),
    shiny::tags$div(
      class = "ibgb-choice-card",
      shiny::tags$div(class = "ibgb-control-title", "\u5206\u533a\u57df\u8fc7\u7a0b\u901f\u7387\uff08\u8de8\u7c7b\u7fa4\uff09"),
      shiny::tags$div(
        class = "ibgb-home-note",
        "\u4e0a\u4f20\u6bcf\u4e2a\u7c7b\u7fa4\u7684 region_process_rates_through_time.csv\uff0c\u6bd4\u8f83\u5404\u5730\u533a\u5728\u4e0d\u540c\u7c7b\u7fa4\u91cc\u8fc7\u7a0b\u901f\u7387\u968f\u65f6\u95f4\u7684\u53d8\u5316\u3002"
      ),
      shiny::fileInput(
        "cross_clade_region_files",
        "\u4e0a\u4f20\u5404\u7c7b\u7fa4\u7684 region_process_rates_through_time.csv\uff08\u53ef\u591a\u9009\uff09",
        multiple = TRUE,
        accept = ".csv"
      ),
      shiny::uiOutput("cross_clade_region_status"),
      shiny::div(class = "ibgb-preview", shiny::imageOutput("cross_clade_region_plot", height = "560px")),
      shiny::tags$div(class = "ibgb-key-files-title", "\u5206\u533a\u57df\u5408\u5e76\u6570\u636e\u9884\u89c8"),
      shiny::tableOutput("cross_clade_region_table"),
      shiny::downloadButton("download_cross_clade_region", "\u4e0b\u8f7d\u5206\u533a\u57df\u6574\u5408\u7ed3\u679c CSV")
    )
  )
}

wizard_step_help <- function() {
  shiny::tabPanel(
    "\u5173\u4e8e\u4e0e\u5f15\u7528",
    shiny::tags$div(class = "ibgb-key-files-title", "\u8f6f\u4ef6\u72b6\u6001"),
    shiny::tableOutput("about_table"),
    shiny::tags$div(class = "ibgb-key-files-title", "\u62a5\u544a\u73af\u5883"),
    shiny::tableOutput("report_environment_table"),
    shiny::tags$div(class = "ibgb-key-files-title", "BioGeoBEARS \u5f15\u7528"),
    shiny::verbatimTextOutput("citation_text")
  )
}
