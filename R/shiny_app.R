#' Launch the BioGeoSyn Shiny application
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
  app <- create_bgs_shiny_app(config = config, output_dir = output_dir)
  shiny::runApp(app, launch.browser = launch.browser, ...)
}

# Shiny caps uploads at 5 MB by default. A single clade's result bundle
# (figures plus the raw BioGeoBEARS and BSM RDS files) already exceeds that, and
# the cross-clade tab takes several bundles at once, so the default makes those
# uploads fail with "Maximum upload size exceeded". Raise it to a generous
# local-desktop limit, overridable with options(biogeosyn.maxUploadSizeMB=).
bgs_max_upload_bytes <- function() {
  mb <- suppressWarnings(as.numeric(getOption("biogeosyn.maxUploadSizeMB", 1024)))
  if (length(mb) != 1L || !is.finite(mb) || mb <= 0) {
    mb <- 1024
  }
  # Cap so the byte count stays within R's integer range (< 2 GiB).
  as.integer(min(mb, 2000) * 1024 * 1024)
}

create_bgs_shiny_app <- function(config = NULL, output_dir = NULL) {
  check_shiny_available()

  startup <- prepare_shiny_startup(config, output_dir)
  default_config <- startup$config
  default_output <- startup$output_dir

  shiny::shinyApp(
    ui = bgs_app_ui(default_config, default_output, startup$example_project_dir),
    server = bgs_shiny_server
  )
}

shiny_control_section <- function(title, ...) {
  shiny::tags$div(
    class = "bgs-control-section",
    shiny::tags$div(class = "bgs-control-title", title),
    ...
  )
}

shiny_collapsible_section <- function(title, ..., open = FALSE) {
  args <- c(
    list(class = "bgs-collapsible"),
    if (isTRUE(open)) list(open = "open") else list(),
    list(shiny::tags$summary(title)),
    list(...)
  )
  do.call(shiny::tags$details, args)
}

shiny_action_grid <- function(...) {
  shiny::tags$div(class = "bgs-action-grid", ...)
}

shiny_home_guidance_body <- function() {
  shiny::tagList(
    shiny::uiOutput("home_next_action"),
    shiny::tags$div(class = "bgs-key-files-title", "Guided workflow"),
    shiny::tableOutput("guided_workflow_table"),
    shiny_collapsible_section(
      "Details",
      shiny::tableOutput("first_steps_table")
    )
  )
}

shiny_primary_results_body <- function() {
  shiny::tagList(
    shiny::uiOutput("run_summary_cards"),
    shiny::tags$div(
      class = "bgs-primary-result",
      shiny::tags$h4("1. Ancestral range reconstruction"),
      shiny::tags$p("Start from the reconstruction under the best-fitting model, then read it alongside the model comparison and the +J caution."),
      shiny::tags$p(
        class = "bgs-home-note",
        "Two views of the same tree. The pie version shows the full probability of every range at each node; the single-range version shows just the most likely range as one colour with its area code, which stays legible on large trees."
      ),
      shiny::div(class = "bgs-preview", shiny::imageOutput("primary_figure_node_best")),
      shiny::div(class = "bgs-preview", shiny::imageOutput("primary_figure_node_best_single"))
    ),
    shiny::tags$div(
      class = "bgs-primary-result",
      shiny::tags$h4("2. Model comparison"),
      shiny::tableOutput("primary_model_comparison_table"),
      shiny::div(class = "bgs-preview", shiny::imageOutput("primary_figure_model_comparison"))
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
      "Time-strata file",
      "Distances file",
      "Dispersal-multipliers file",
      "Areas-allowed file",
      "Areas-adjacency file",
      "Area-of-areas file"
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
      class = "bgs-upload-row",
      shiny::fileInput(
        inputId = paste0("wizard_constraint_", fields$field[[i]]),
        label = fields$label[[i]],
        accept = c(".txt", ".tsv", ".csv")
      ),
      shiny::downloadButton(paste0("download_constraint_", fields$field[[i]]), "Template")
    )
  }))
}

constraint_template_path <- function(field) {
  fields <- shiny_constraint_fields()
  idx <- match(field, fields$field)
  if (length(idx) != 1L || is.na(idx)) {
    stop("Unknown constraint template: ", paste(field, collapse = ", "), call. = FALSE)
  }
  path <- system.file("example_data", "constraints", fields$template[[idx]], package = "BioGeoSyn")
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

bgs_shiny_server <- function(input, output, session) {
  # Lift the 5 MB upload cap so result bundles can be uploaded (see
  # bgs_max_upload_bytes()). Set here so it is in force before any upload.
  options(shiny.maxRequestSize = bgs_max_upload_bytes())

  state <- shiny::reactiveValues(
        result = NULL,
        validation = NULL,
        model_table = NULL,
        manifest = NULL,
        report = NULL,
        xclade_report = NULL,
        bundle = NULL,
        diagnostic_bundle = NULL,
        installation = check_installation(),
        install_plan = biogeobears_install_plan(),
        message = "Configuration ready. Check the inputs before running.",
        messages = "Configuration ready. Check the inputs before running.",
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
          guide <- open_user_guide(browse = TRUE)
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
          # A new run (or a reload of a different project) supersedes any bundle,
          # report or diagnostics archive built from the previous result. Clear
          # them so the next download rebuilds from THIS result - otherwise every
          # download returns the first clade's archive.
          reset_derived_result_exports(state)
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
        shiny::tags$div(class = paste("bgs-status", state$status_type), state$message)
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

      output$state_space_note <- shiny::renderUI({
        summ <- current_input_summary()
        n_areas <- suppressWarnings(as.integer(tryCatch(summ$geography$n_areas, error = function(e) NA)))
        mr <- suppressWarnings(as.integer(input$wizard_max_range_size %||% NA_integer_))
        if (is.na(n_areas) || n_areas < 1L || is.na(mr) || mr < 1L) {
          return(NULL)
        }
        mr <- min(mr, n_areas)
        n_states <- sum(choose(n_areas, 0:mr))
        msg <- sprintf(
          "%d areas \u00d7 max_range=%d \u2192 about %s states.",
          n_areas, mr, format(n_states, big.mark = ",")
        )
        if (n_states > 500) {
          shiny::tags$div(class = "bgs-status error", paste0(
            msg, " The state space is large, so model fitting will be markedly slower",
            " (each likelihood evaluation scales with the square of the state count). Consider raising the CPU-core count, fitting fewer models,",
            " or lowering max_range where the data allow."
          ))
        } else if (n_states > 150) {
          shiny::tags$div(class = "bgs-status info", paste0(
            msg, " The state space is on the large side; fitting several models on one core will be slow. Raising the CPU-core count helps."
          ))
        } else {
          shiny::tags$div(class = "bgs-home-note", msg)
        }
      })

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

      clade_bundles <- shiny::reactive({
        files <- input$cross_clade_bundles
        if (is.null(files) || nrow(files) == 0L) {
          return(NULL)
        }
        list(paths = files$datapath, names = tools::file_path_sans_ext(files$name))
      })

      cc_call <- function(fun) {
        b <- clade_bundles()
        if (is.null(b)) {
          return(NULL)
        }
        fun(b$paths, b$names)
      }
      cc_rates <- shiny::reactive(cc_call(combine_process_rates_from_bundles))
      cc_rrates <- shiny::reactive(cc_call(combine_region_rates_from_bundles))
      cc_synth <- shiny::reactive(cc_call(combine_process_synthesis_across_clades))
      cc_routes <- shiny::reactive(cc_call(combine_dispersal_routes_across_clades))
      cc_budgets <- shiny::reactive(cc_call(combine_region_budgets_across_clades))
      cc_esum <- shiny::reactive(cc_call(combine_event_summary_across_clades))
      cc_etimes <- shiny::reactive(cc_call(combine_event_times_across_clades))
      cc_exlong <- shiny::reactive(cc_call(combine_exchange_matrix_across_clades))
      cc_parts <- shiny::reactive({
        list(
          rates = cc_rates(), region_rates = cc_rrates(), synthesis = cc_synth(),
          routes = cc_routes(), budgets = cc_budgets(), event_summary = cc_esum(),
          event_times = cc_etimes(), exchange_long = cc_exlong()
        )
      })

      output$cross_clade_status <- shiny::renderUI({
        b <- clade_bundles()
        if (is.null(b)) {
          return(shiny::tags$div(class = "bgs-home-note", "No result bundles uploaded yet. For each clade, upload the result bundle (.zip) downloaded from \"3. Single clade\"."))
        }
        missing <- bundles_missing_cross_clade_data(b$paths, b$names)
        if (length(missing) == length(b$paths)) {
          return(shiny::tags$div(
            class = "bgs-status error",
            paste0(
              "None of the ", length(b$paths), " uploaded bundles contain cross-clade results. ",
              "These come from BSM stochastic mapping, so each clade must be re-run with ",
              "\"Run BSM stochastic mapping\" enabled before its bundle can be integrated here."
            )
          ))
        }
        if (length(missing) > 0L) {
          return(shiny::tags$div(
            class = "bgs-status error",
            paste0(
              "Integrated ", length(b$paths) - length(missing), " of ", length(b$paths),
              " clades. These bundles have no BSM outputs and were skipped (re-run them with ",
              "\"Run BSM stochastic mapping\" enabled): ", paste(missing, collapse = ", "), "."
            )
          ))
        }
        shiny::tags$div(class = "bgs-status info", paste0("Integrated ", length(b$paths), " clades."))
      })

      cc_image <- function(react, plot_fun, width, height) {
        shiny::renderImage({
          d <- react()
          shiny::req(d)
          shiny::validate(shiny::need(nrow(d) > 0, "No data to plot."))
          plot <- tryCatch(plot_fun(d), error = function(e) NULL)
          shiny::validate(shiny::need(!is.null(plot), "The figure could not be produced."))
          path <- tempfile(fileext = ".png")
          ggplot2::ggsave(path, plot, width = width, height = height, dpi = 150)
          list(src = path, contentType = "image/png", width = "100%")
        }, deleteFile = TRUE)
      }
      output$cc_synth_plot <- cc_image(cc_synth, plot_biogeographic_process_synthesis, 8, 4.8)
      output$cc_budget_plot <- cc_image(cc_budgets, plot_region_process_budget, 7.5, 4.5)

      cc_shared_bin_width <- function() {
        bw <- suppressWarnings(as.numeric(input$cc_bin_width))
        if (length(bw) != 1L || !is.finite(bw) || bw <= 0) 5 else bw
      }

      # Overall process rates: clades pooled onto a shared time grid, one curve
      # per process (no per-clade split).
      output$cross_clade_plot <- shiny::renderImage({
        d <- cc_rates()
        shiny::req(d)
        shiny::validate(shiny::need(nrow(d) > 0, "No data to plot."))
        plot <- tryCatch(
          plot_process_rates_across_clades(d, pooled = TRUE, bin_width = cc_shared_bin_width()),
          error = function(e) NULL
        )
        shiny::validate(shiny::need(!is.null(plot), "The figure could not be produced."))
        path <- tempfile(fileext = ".png")
        ggplot2::ggsave(path, plot, width = 8.6, height = 5.2, dpi = 150)
        list(src = path, contentType = "image/png", width = "100%")
      }, deleteFile = TRUE)

      # Keep the region selectors in sync with whatever regions the uploaded
      # bundles actually contain; default to all of them selected.
      shiny::observeEvent(cc_rrates(), {
        d <- cc_rrates()
        regions <- if (is.null(d) || !"region" %in% names(d)) character() else sort(unique(d$region))
        for (id in c("cc_regions", "cc_network_regions")) {
          shiny::updateCheckboxGroupInput(
            session, id, choices = regions,
            selected = if (is.null(input[[id]])) regions else intersect(input[[id]], regions)
          )
        }
      })

      # By-region rates: one panel per region, three process curves, pooled onto
      # a shared time grid whose width and region set the user controls.
      output$cross_clade_region_plot <- shiny::renderImage({
        d <- cc_rrates()
        shiny::req(d)
        shiny::validate(shiny::need(nrow(d) > 0, "No data to plot."))
        regions <- input$cc_regions
        if (is.null(regions) || length(regions) == 0L) {
          regions <- NULL
        }
        plot <- tryCatch(
          plot_region_process_rates_across_clades(
            d, regions = regions, bin_width = cc_shared_bin_width(),
            log_y = isTRUE(input$cc_region_log)
          ),
          error = function(e) NULL
        )
        shiny::validate(shiny::need(!is.null(plot), "The figure could not be produced (no rows for the chosen regions)."))
        path <- tempfile(fileext = ".png")
        ggplot2::ggsave(path, plot, width = 9.5, height = 7.5, dpi = 150)
        list(src = path, contentType = "image/png", width = "100%")
      }, deleteFile = TRUE)

      # Dispersal network, sliced to the chosen geological period from the
      # per-event times (Total = all events).
      output$cc_network_plot <- shiny::renderImage({
        et <- cc_etimes()
        shiny::req(et)
        period <- input$cc_network_period %||% "Total"
        window <- bgs_period_window(period)
        routes <- dispersal_routes_from_event_times(et, window$from, window$to)
        regions <- input$cc_network_regions
        if (!is.null(regions) && length(regions) > 0L && !is.null(routes)) {
          routes <- routes[routes$source_region %in% regions & routes$target_region %in% regions, , drop = FALSE]
        }
        shiny::validate(shiny::need(!is.null(routes) && nrow(routes) > 0,
          "No between-region dispersal events among the chosen regions in this period."))
        plot <- tryCatch(
          plot_bsm_dispersal_network(routes, subtitle = period_network_subtitle(period)),
          error = function(e) NULL
        )
        shiny::validate(shiny::need(!is.null(plot), "The network could not be produced."))
        path <- tempfile(fileext = ".png")
        ggplot2::ggsave(path, plot, width = 6.5, height = 5.5, dpi = 150)
        list(src = path, contentType = "image/png", width = "100%")
      }, deleteFile = TRUE)

      # The exchange matrix and event statistics are not previewed in the tab
      # (they are wide/long tables best read in the report and bundle); the
      # cc_exlong() / cc_esum() reactives still feed the report and download.

      output$download_cross_clade <- shiny::downloadHandler(
        filename = function() "cross_clade_results.zip",
        content = function(file) {
          write_cross_clade_full_bundle(file, cc_parts())
        }
      )

      output$xclade_report_status <- shiny::renderText({
        if (is.null(state$xclade_report) || !file.exists(state$xclade_report)) {
          "No report yet. Upload the clades' result bundles, then click \"Build report\"."
        } else {
          state$xclade_report
        }
      })

      shiny::observeEvent(input$render_xclade_report, {
        run_app_action(state, {
          parts <- cc_parts()
          shiny::withProgress(message = "Rendering cross-clade report", value = 0, {
            state$xclade_report <- render_cross_clade_report(parts)
            append_app_message(state, paste("Cross-clade report ready:", state$xclade_report))
            shiny::incProgress(1)
          })
        })
      })

      output$download_xclade_report <- shiny::downloadHandler(
        filename = function() "cross_clade_report.html",
        content = function(file) {
          if (is.null(state$xclade_report) || !file.exists(state$xclade_report)) {
            stop("Generate the cross-clade report first.", call. = FALSE)
          }
          copy_download_file(state$xclade_report, file)
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

      output$primary_figure_node_best_single <- shiny::renderImage({
        shiny_named_figure_image(state, "node_state_summary_best_model_single")
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
    add("Tree tips", tr$n_tips)
    if (isTRUE(tr$has_branch_lengths) && !is.na(tr$root_age)) {
      add("Root age (tree height)", formatC(tr$root_age, format = "fg", digits = 3))
    }
    if (!is.na(tr$is_ultrametric)) {
      add("Ultrametric", if (isTRUE(tr$is_ultrametric)) "Yes" else "No")
    }
  }
  g <- summary$geography
  if (!is.null(g)) {
    add("Areas", g$n_areas)
    add("Species", g$n_species)
    if (!is.na(g$max_range_size_setting)) {
      add("Maximum range size (setting)", g$max_range_size_setting)
    }
    add("Maximum range size (observed)", g$max_range_size_observed)
    add("Mean range size", formatC(g$mean_range_size, format = "fg", digits = 3))
    add("Widespread species (range > 1)", g$widespread_species)
  }
  tm <- summary$taxon_match
  if (!is.null(tm)) {
    add(
      "Tree and geography names match",
      if (isTRUE(tm$all_match)) {
        "Yes"
      } else {
        paste0(
          "No (missing from geography: ", length(tm$missing_from_geography),
          "; missing from tree: ", length(tm$missing_from_tree), ")"
        )
      }
    )
  }
  if (length(items) == 0L) {
    return(data.frame())
  }
  out <- data.frame(item = items, value = values, stringsAsFactors = FALSE)
  names(out) <- c("Item", "Value")
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
  names(out) <- c("Area", "Name", "Species", "Endemics", "Share")
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
  names(out) <- c("Range size", "Species", "Share")
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
  path <- system.file("example_data", files[[kind]], package = "BioGeoSyn")
  if (!file.exists(path)) {
    stop("Installed input template could not be found: ", files[[kind]], call. = FALSE)
  }
  path
}

default_project_parent <- function() {
  as_path(file.path(path.expand("~"), "BioGeoSyn-projects"))
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

  example <- create_example_project(tempfile("BioGeoSyn-welcome-"))
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

# Archives (bundle, report, diagnostics) are cached on `state` and lazily
# rebuilt only when their slot is NULL. They are derived from `state$result`, so
# when the result changes they must be invalidated or downloads keep serving the
# archive built for the previous (e.g. first) clade.
reset_derived_result_exports <- function(state) {
  state$bundle <- NULL
  state$report <- NULL
  state$diagnostic_bundle <- NULL
  invisible(state)
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
  class(result) <- c("bgs_workflow_result", "list")
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
    shiny_upload_preview_csv(input$wizard_geography %||% NULL, "Geography matrix CSV", expected = "First column is the species name; the remaining columns are areas, usually coded 0/1."),
    shiny_upload_preview_csv(input$wizard_regions %||% NULL, "Regions CSV", expected = "Must contain at least an area code or area name, used to label the geography columns.")
  )
  do.call(rbind, rows)
}

shiny_upload_preview_missing <- function(label, next_step) {
  shiny_upload_preview_row(label, "Awaiting upload", "", next_step)
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
    c("File", "Status", "Summary", "Next step")
  )
}

shiny_upload_preview_tree <- function(upload) {
  path <- shiny_uploaded_datapath(upload)
  if (is.null(path)) {
    return(shiny_upload_preview_missing("Tree file", "Upload a tree in Newick format."))
  }
  text <- tryCatch(
    paste(readLines(path, n = 5L, warn = FALSE), collapse = " "),
    error = function(e) NA_character_
  )
  if (is.na(text) || !nzchar(text)) {
    return(shiny_upload_preview_row("Tree file", "Needs attention", "The file is empty or unreadable.", "Upload the Newick tree file again."))
  }
  looks_newick <- grepl("\\(", text) && grepl(";", text)
  status <- if (looks_newick) "Readable" else "Needs attention"
  next_step <- if (looks_newick) "Next, upload the geography matrix and the regions CSV." else "Check that the tree is Newick format and ends with a semicolon."
  shiny_upload_preview_row(
    "Tree file",
    status,
    paste0("First lines: ", nchar(text), " characters."),
    next_step
  )
}

shiny_upload_preview_csv <- function(upload, label, expected) {
  path <- shiny_uploaded_datapath(upload)
  if (is.null(path)) {
    return(shiny_upload_preview_missing(label, paste0("Upload ", label, ".")))
  }
  table <- tryCatch(
    utils::read.csv(path, nrows = 5L, check.names = FALSE, stringsAsFactors = FALSE),
    error = function(e) e
  )
  if (inherits(table, "error")) {
    return(shiny_upload_preview_row(label, "Needs attention", conditionMessage(table), expected))
  }
  if (ncol(table) == 0L) {
    return(shiny_upload_preview_row(label, "Needs attention", "The CSV has no readable columns.", expected))
  }
  summary <- paste0(
    "Readable; preview shows ",
    nrow(table),
    " rows and ",
    ncol(table),
    " columns: ",
    paste(utils::head(names(table), 6L), collapse = ", ")
  )
  shiny_upload_preview_row(label, "Readable", summary, "Full validation runs once you create your analysis project.")
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
    example = if (data_ready) "Click \"Check inputs\"." else "Click \"Create example project\".",
    own = if (data_ready) "Click \"Check inputs\"." else "Upload the tree, geography matrix and regions CSV, then create your analysis project.",
    existing = if (data_ready) "Open \"Results\"." else "Enter the results directory under \"Advanced: existing project and YAML\", then load the existing results.",
    "Choose how to start."
  )
  data_detail <- switch(
    start_choice,
    example = if (config_ready) as_path(config_path) else "The example project has not been created yet.",
    own = if (data_ready) as_path(config_path) else "Use the upload controls under \"Use your own data\".",
    existing = if (has_result) output_dir else "An existing results directory is required first.",
    ""
  )

  validation_status <- if (!data_ready) {
    "Waiting"
  } else if (validation$failed) {
    "Needs attention"
  } else if (validation$passed) {
    "Ready"
  } else {
    "Action needed"
  }
  validation_next <- if (!data_ready) {
    "Select or create a project first."
  } else if (validation$failed) {
    "Open \"Advanced results > Validation\" and fix the failed checks."
  } else if (validation$passed) {
    "Run a dry run."
  } else {
    "Click \"Check inputs\"."
  }

  dry_status <- if (dry_run_ready || real_run_ready) {
    "Ready"
  } else if (validation$failed || !validation$passed) {
    "Waiting"
  } else {
    "Action needed"
  }
  dry_next <- if (dry_run_ready) {
    if (bgb_ready) "Uncheck Dry run to prepare a real run." else "Install BioGeoBEARS before running for real."
  } else if (real_run_ready) {
    "Open \"Results\"."
  } else if (validation$failed) {
    "Fix the input-check errors first."
  } else if (validation$passed) {
    "Leave Dry run checked, then run the workflow."
  } else {
    "Click \"Check inputs\" first."
  }

  real_status <- if (real_run_ready) {
    "Ready"
  } else if (dry_run_ready && bgb_ready) {
    "Action needed"
  } else {
    "Waiting"
  }
  real_next <- if (real_run_ready) {
    "Open \"Results\"."
  } else if (dry_run_ready && bgb_ready) {
    "Uncheck Dry run, then run the workflow."
  } else if (dry_run_ready && !bgb_ready) {
    if (isTRUE(dry_run) && !isTRUE(require_biogeobears)) {
      "BioGeoBEARS must be installed to run for real."
    } else {
      "Install BioGeoBEARS, then run again."
    }
  } else {
    "Complete a dry run first."
  }

  result_status <- if (primary_results_ready) {
    "Ready"
  } else if (real_run_ready) {
    "Needs attention"
  } else {
    "Waiting"
  }
  result_next <- if (primary_results_ready) {
    "Open \"Results\" for the ancestral-range reconstruction, model comparison and event statistics."
  } else if (real_run_ready) {
    "Click \"Refresh key files\", or open \"Troubleshooting\" for the cause."
  } else {
    "Run for real first, or load results from a previous real run."
  }

  export_status <- if (export_ready) {
    if (result_bundle_ready && diagnostic_bundle_ready) "Ready" else "Partly complete"
  } else if (primary_results_ready) {
    "Action needed"
  } else {
    "Waiting"
  }
  export_next <- if (result_bundle_ready && diagnostic_bundle_ready) {
    "Download the result bundle or the diagnostic bundle."
  } else if (result_bundle_ready) {
    "Build the diagnostic bundle only if you need someone else to help troubleshoot."
  } else if (primary_results_ready) {
    "Click \"Build result bundle\"."
  } else {
    "Review the results first."
  }

  stats::setNames(
    data.frame(
      step = c("Data source", "Input check", "Dry run", "Real run", "Review results", "Export and share"),
      status = c(
        if (data_ready) "Ready" else "Action needed",
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
        if (comparison_ready) "Model comparison is available." else "No model comparison yet.",
        shiny_export_detail(result_bundle_ready, diagnostic_bundle_ready, state)
      ),
      check.names = FALSE,
      stringsAsFactors = FALSE
    ),
    c("Step", "Status", "Next step", "Detail")
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
      detail = "Not checked yet."
    ))
  }
  failed <- sum(!is.na(validation$ok) & !validation$ok)
  passed <- sum(!is.na(validation$ok) & validation$ok)
  list(
    available = TRUE,
    passed = failed == 0L,
    failed = failed > 0L,
    detail = paste0(passed, " passed, ", failed, " failed")
  )
}

shiny_home_next_action <- function(workflow) {
  if (is.null(workflow) || nrow(workflow) == 0L) {
    return(shiny::tags$div(class = "bgs-next-action", "Choose a data source."))
  }
  status <- workflow[["Status"]] %||% workflow[["Status"]]
  actionable <- workflow[status %in% c("Action needed", "Needs attention", "Waiting", "Partly complete", "Action needed", "Needs attention", "Waiting", "Partial"), , drop = FALSE]
  next_row <- if (nrow(actionable) > 0L) actionable[1L, , drop = FALSE] else workflow[nrow(workflow), , drop = FALSE]
  next_step <- (next_row[["Step"]] %||% next_row[["Step"]])[[1L]]
  next_text <- (next_row[["Next step"]] %||% next_row[["Next action"]])[[1L]]
  shiny::tags$div(
    class = "bgs-next-action",
    shiny::tags$div(class = "bgs-next-action-title", "Next step"),
    shiny::tags$div(class = "bgs-next-action-step", next_step),
    shiny::tags$div(class = "bgs-next-action-detail", next_text)
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
    selected <- utils::choose.dir(default = default, caption = "Choose BioGeoSyn result directory")
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
      "BioGeoSyn",
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
    as.character(utils::packageVersion("BioGeoSyn")),
    error = function(e) {
      desc <- tryCatch(utils::packageDescription("BioGeoSyn", fields = "Version"), error = function(e) NA_character_)
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
    "BioGeoBEARS is not bundled with BioGeoSyn.",
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
    class = "bgs-run-summary-grid",
    lapply(seq_len(nrow(featured)), function(i) {
      item <- as.character(featured$item[[i]])
      value <- as.character(featured$value[[i]])
      shiny::tags$div(
        class = paste("bgs-run-summary-card", shiny_run_summary_card_class(item, value)),
        shiny::tags$div(class = "bgs-run-summary-label", item),
        shiny::tags$div(class = "bgs-run-summary-value", value)
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
      "Tables (tables/)",
      "model_comparison.csv" = "Per-model logLik, parameter count, AIC/AICc, \u0394AICc and Akaike weights.",
      "input_validation.csv" = "Consistency checks on the inputs (tree, geography, regions, model settings).",
      "biogeographic_process_summary.csv" = "Mean event counts for each biogeographic process (in-situ/sympatric, subset, vicariance, founder jump, range expansion, local extinction, range switching).",
      "region_process_budgets.csv" = "Per-area dispersal budget: immigration, emigration, net flux and local extinction.",
      "process_rates_through_time.csv" = "Binned process rates through time with mean, SD and 95% CI. This is the file the cross-clade synthesis uses.",
      "region_process_rates_through_time.csv" = "Region-resolved process rates through time. This is the file the region-resolved cross-clade synthesis uses.",
      "bsm_qc.csv" = "BSM reliability check: pass/warning/fail for each internal-consistency invariant.",
      "bsm_event_summary.csv" = "Mean and total counts per BSM event class.",
      "bsm_dispersal_routes.csv" = "Mean counts for each BSM source-to-target dispersal route.",
      "bsm_event_times.csv" = "Time and type of every BSM event, one row per event."
    ),
    rows(
      "Figures (figures/)",
      "node_state_summary_best_model" = "Ancestral ranges at every node under the best model, as probability pies (the headline figure).",
      "node_state_summary_best_model_single" = "The same reconstruction shown as one colour + area code per node (the single most likely range).",
      "biogeographic_process_synthesis" = "Biogeographic process synthesis (the key publication figure).",
      "model_comparison" = "Model comparison figure.",
      "region_process_budget" = "Diverging bar chart of each area's dispersal budget.",
      "process_rates_through_time" = "Process rates through time.",
      "region_process_rates_through_time" = "Region-resolved process rates through time.",
      "bsm_dispersal_network" = "Directed network of BSM dispersal between areas (arrow width = mean count).",
      "bsm_dispersal_routes" = "Heatmap of BSM dispersal routes.",
      "bsm_event_summary / bsm_event_times / event_summary" = "Summary and timing of BSM and deterministic events."
    ),
    rows(
      "Report and other",
      "reports/summary_report.*" = "Summary report (html/pdf/qmd) tying the results above into one narrative.",
      "workflow_manifest.csv" = "Manifest of every file this run produced.",
      "config_used.yml" = "The configuration this run actually used, for reproduction.",
      "logs/" = "Run logs, for troubleshooting."
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
      "node_state_summary_best_model_single",
      "node_state_summary_best_non_j",
      "node_state_summary_best_plus_j",
      "node_state_sensitivity",
      "event_summary"
    ),
    display_label = c(
      "Model comparison",
      "Root-state probabilities",
      "Node states, best model (pies)",
      "Node states, best model (single range)",
      "Node states, best non-+J model",
      "Node states, best +J model",
      "Node-state sensitivity",
      "Event statistics"
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
  num_cores <- suppressWarnings(as.integer(input$wizard_num_cores %||% NA_integer_))
  if (!is.na(num_cores) && num_cores >= 1L) {
    cfg$advanced <- cfg$advanced %||% list()
    cfg$advanced$BioGeoBEARS_run_object <- cfg$advanced$BioGeoBEARS_run_object %||% list()
    cfg$advanced$BioGeoBEARS_run_object$num_cores_to_use <- num_cores
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
  path <- tempfile("bgs-shiny-analysis-", fileext = ".yml")
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

bgs_head_styles <- function() {
  shiny::tags$head(
    shiny::tags$style(shiny::HTML(
      # Design tokens. The accent and text colours are the package's Okabe-Ito
      # figure palette (see bgs_palette()), so the interface and the figures it
      # produces read as one visual system.
      ":root{",
      "--bgs-accent:#0072b2;--bgs-accent-strong:#005c90;--bgs-accent-weak:#eaf3f9;",
      "--bgs-orange:#d55e00;--bgs-good:#2e7d32;--bgs-danger:#b00020;",
      "--bgs-ink:#1f2937;--bgs-muted:#6b7280;--bgs-faint:#8b95a5;",
      "--bgs-line:#e4e9f0;--bgs-line-strong:#cfd8e3;",
      "--bgs-surface:#fff;--bgs-canvas:#f6f8fb;",
      "--bgs-r:10px;--bgs-r-sm:6px;",
      "--bgs-shadow:0 1px 2px rgba(16,24,40,.04),0 2px 6px rgba(16,24,40,.05);",
      "--bgs-shadow-lift:0 2px 4px rgba(16,24,40,.05),0 10px 24px rgba(16,24,40,.09);",
      "--bgs-ring:0 0 0 3px rgba(0,114,178,.18);} ",

      "body{background:var(--bgs-canvas);color:var(--bgs-ink);font-size:14.5px;line-height:1.62;",
      "font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',system-ui,'PingFang SC','Hiragino Sans GB','Microsoft YaHei','Noto Sans CJK SC',sans-serif;",
      "-webkit-font-smoothing:antialiased;-moz-osx-font-smoothing:grayscale} ",
      ".container-fluid{max-width:1180px;padding-bottom:40px} ",
      "h1,h2,h3,h4,h5{color:var(--bgs-ink);font-weight:650;letter-spacing:-.011em} ",
      "h2.bgs-app-title,.container-fluid>h2:first-child{font-size:26px;margin:22px 0 4px 0} ",
      "a{color:var(--bgs-accent)} a:hover,a:focus{color:var(--bgs-accent-strong)} ",
      ".well{border-radius:var(--bgs-r);border-color:var(--bgs-line);background:var(--bgs-surface);box-shadow:var(--bgs-shadow)} ",

      # Buttons: one solid accent, quiet secondary, clear focus ring.
      ".btn,a.btn{border-radius:var(--bgs-r-sm);background:var(--bgs-accent);border:1px solid var(--bgs-accent);",
      "color:#fff;font-weight:600;padding:7px 14px;box-shadow:var(--bgs-shadow);",
      "transition:background .15s ease,border-color .15s ease,box-shadow .15s ease,transform .05s ease} ",
      ".btn:hover,a.btn:hover{background:var(--bgs-accent-strong);border-color:var(--bgs-accent-strong);color:#fff} ",
      ".btn:focus-visible,a.btn:focus-visible{outline:none;box-shadow:var(--bgs-ring)} ",
      ".btn:active,a.btn:active{transform:translateY(1px);box-shadow:none} ",
      ".btn[disabled],.btn.disabled{opacity:.5;box-shadow:none} ",
      ".btn-default:not(.action-button){background:var(--bgs-surface);border-color:var(--bgs-line-strong);color:var(--bgs-ink)} ",
      ".btn-default:not(.action-button):hover{background:#f2f5f9;border-color:var(--bgs-faint);color:var(--bgs-ink)} ",

      # Form controls.
      "label,.control-label{font-weight:600;color:var(--bgs-ink);margin-bottom:5px} ",
      ".form-control{border-radius:var(--bgs-r-sm);border-color:var(--bgs-line-strong);color:var(--bgs-ink);",
      "box-shadow:none;transition:border-color .15s ease,box-shadow .15s ease} ",
      ".form-control:focus{border-color:var(--bgs-accent);box-shadow:var(--bgs-ring)} ",
      ".shiny-input-container{margin-bottom:12px} ",
      ".checkbox label,.radio label{font-weight:500} ",
      ".form-group.shiny-input-container>.shiny-options-group{padding-top:2px} ",

      ".bgs-inline-controls{display:flex;flex-wrap:wrap;gap:14px;align-items:flex-end;margin-bottom:6px} ",
      ".bgs-status{font-weight:600;margin:8px 0} ",
      ".bgs-status.info{color:var(--bgs-accent-strong)} .bgs-status.error{color:var(--bgs-danger)} ",
      ".bgs-control-section{border-top:1px solid var(--bgs-line);margin-top:14px;padding-top:12px} ",
      ".bgs-control-section:first-child{border-top:0;margin-top:0;padding-top:0} ",
      ".bgs-control-title{font-weight:650;margin-bottom:8px;letter-spacing:-.008em} ",
      ".bgs-action-grid{display:grid;grid-template-columns:1fr;gap:7px} ",
      ".bgs-action-grid .btn{width:100%;text-align:left} ",
      ".bgs-downloads{margin:0} .bgs-downloads .btn{width:100%;text-align:left;margin-bottom:7px} ",

      ".bgs-run-summary-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(210px,1fr));gap:10px;margin:0 0 12px 0} ",
      ".bgs-run-summary-card{border:1px solid var(--bgs-line);border-left:4px solid var(--bgs-faint);",
      "border-radius:var(--bgs-r-sm);padding:11px 12px;background:var(--bgs-surface);box-shadow:var(--bgs-shadow)} ",
      ".bgs-run-summary-card.info{border-left-color:var(--bgs-accent)} .bgs-run-summary-card.warning{border-left-color:var(--bgs-orange)} ",
      ".bgs-run-summary-card.good{border-left-color:var(--bgs-good)} .bgs-run-summary-card.muted{border-left-color:var(--bgs-faint)} ",
      ".bgs-run-summary-label{font-size:11.5px;font-weight:650;color:var(--bgs-muted);margin-bottom:4px;",
      "text-transform:uppercase;letter-spacing:.04em} ",
      ".bgs-run-summary-value{font-size:15px;font-weight:650;color:var(--bgs-ink);overflow-wrap:anywhere} ",

      ".bgs-key-files-title{font-weight:650;margin:16px 0 7px 0;font-size:13px;color:var(--bgs-muted);",
      "text-transform:uppercase;letter-spacing:.05em} ",
      ".bgs-home-note{background:#f3f6fa;border:1px solid var(--bgs-line);border-radius:var(--bgs-r-sm);",
      "padding:10px 13px;margin:8px 0 14px 0;color:var(--bgs-muted);font-size:13.5px} ",
      ".bgs-next-action{border:1px solid #cfe3f1;border-left:4px solid var(--bgs-accent);background:var(--bgs-accent-weak);",
      "border-radius:var(--bgs-r-sm);padding:12px 14px;margin:8px 0 16px 0} ",
      ".bgs-next-action-title{font-size:11.5px;font-weight:650;color:var(--bgs-accent-strong);margin-bottom:4px;",
      "text-transform:uppercase;letter-spacing:.05em} ",
      ".bgs-next-action-step{font-size:15px;font-weight:650;color:var(--bgs-ink);margin-bottom:3px} ",
      ".bgs-next-action-detail{color:#3b4657} ",

      ".bgs-collapsible{border-top:1px solid var(--bgs-line);margin-top:12px;padding-top:10px} ",
      ".bgs-collapsible>summary{font-weight:650;cursor:pointer;margin-bottom:8px;list-style:none;",
      "color:var(--bgs-ink);border-radius:var(--bgs-r-sm);padding:2px 2px} ",
      ".bgs-collapsible>summary:hover{color:var(--bgs-accent-strong)} ",
      ".bgs-collapsible>summary:focus-visible{outline:none;box-shadow:var(--bgs-ring)} ",
      ".bgs-collapsible>summary::-webkit-details-marker{display:none} ",
      ".bgs-collapsible>summary::before{content:'\\25b6';color:var(--bgs-accent);font-size:10px;margin-right:8px;",
      "display:inline-block;transition:transform .18s ease} ",
      ".bgs-collapsible[open]>summary::before{transform:rotate(90deg)} ",

      ".bgs-primary-result{border-top:1px solid var(--bgs-line);margin-top:18px;padding-top:16px} ",
      ".bgs-primary-result:first-child{border-top:0;margin-top:0;padding-top:0} ",
      ".bgs-preview img{max-width:100%;height:auto;border:1px solid var(--bgs-line);border-radius:var(--bgs-r-sm);",
      "display:block;background:var(--bgs-surface)} ",
      ".bgs-preview .shiny-image-output{height:auto !important;min-height:0} ",
      ".bgs-figure-dashboard{display:grid;grid-template-columns:1fr;gap:18px} ",
      ".bgs-figure-dashboard h4{margin:6px 0 8px 0;font-size:15px} ",
      ".bgs-figure-dashboard img{max-width:100%;height:auto;border:1px solid var(--bgs-line);",
      "border-radius:var(--bgs-r-sm);display:block;background:var(--bgs-surface)} ",
      ".bgs-figure-dashboard .shiny-image-output{height:auto !important;min-height:0} ",

      "#wizard_nav{margin-top:10px} ",
      "#wizard_nav.nav-tabs{border-bottom:1px solid var(--bgs-line-strong)} ",
      "#wizard_nav.nav-tabs>li>a{font-weight:600;color:var(--bgs-muted);border:0;margin-right:2px;",
      "padding:9px 14px;border-radius:var(--bgs-r-sm) var(--bgs-r-sm) 0 0;background:transparent;transition:color .15s ease,background .15s ease} ",
      "#wizard_nav.nav-tabs>li>a:hover{color:var(--bgs-ink);background:rgba(0,114,178,.06)} ",
      "#wizard_nav.nav-tabs>li.active>a,#wizard_nav.nav-tabs>li.active>a:hover,#wizard_nav.nav-tabs>li.active>a:focus{",
      "color:var(--bgs-accent-strong);border:0;box-shadow:inset 0 -2.5px 0 var(--bgs-accent);background:transparent} ",
      ".bgs-step-intro{color:var(--bgs-muted);margin:14px 0 16px 0;font-size:14px} ",
      ".tab-content>.tab-pane{padding:14px 2px 2px 2px} ",

      ".bgs-two-col{display:grid;grid-template-columns:1fr 1fr;gap:22px} ",
      "@media (max-width:900px){.bgs-two-col{grid-template-columns:1fr}} ",
      ".bgs-choice-card{border:1px solid var(--bgs-line);border-radius:var(--bgs-r);padding:16px 18px;",
      "background:var(--bgs-surface);box-shadow:var(--bgs-shadow);margin-bottom:14px;",
      "transition:box-shadow .18s ease,border-color .18s ease} ",
      ".bgs-choice-card:hover{box-shadow:var(--bgs-shadow-lift);border-color:var(--bgs-line-strong)} ",
      ".bgs-output-row{display:flex;gap:8px;align-items:flex-end} ",
      ".bgs-output-row .form-group{flex:1;margin-bottom:0} ",
      ".bgs-output-row .btn{white-space:nowrap;padding:6px 12px} ",
      ".bgs-upload-row{display:flex;gap:10px;align-items:flex-start} ",
      ".bgs-upload-row>.form-group{flex:1;margin-bottom:8px} ",
      ".bgs-upload-row>.btn{white-space:nowrap;margin-top:25px} ",

      # Tables: quiet rules, uppercase headers, right-aligned numbers.
      ".table{background:var(--bgs-surface);border-radius:var(--bgs-r-sm);overflow:hidden;font-size:13.5px} ",
      ".table>thead>tr>th{border-bottom:1px solid var(--bgs-line-strong);background:#f3f6fa;color:var(--bgs-muted);",
      "font-size:11.5px;font-weight:650;text-transform:uppercase;letter-spacing:.04em;padding:9px 10px;vertical-align:middle} ",
      ".table>tbody>tr>td{border-top:1px solid var(--bgs-line);padding:8px 10px;vertical-align:middle} ",
      ".table-striped>tbody>tr:nth-of-type(odd){background:#fafbfd} ",
      ".table-hover>tbody>tr:hover{background:var(--bgs-accent-weak)} ",
      ".table-bordered,.table-bordered>thead>tr>th,.table-bordered>tbody>tr>td{border-color:var(--bgs-line)} ",

      ".bgs-developer .bgs-developer-name{font-size:16px;font-weight:650;color:var(--bgs-ink)} ",
      ".bgs-developer-role{color:var(--bgs-ink);margin-top:2px} ",
      ".bgs-developer-affil{color:var(--bgs-muted);margin-top:1px} ",
      ".bgs-developer-links{display:flex;flex-wrap:wrap;gap:8px 18px;margin-top:10px;",
      "padding-top:10px;border-top:1px solid var(--bgs-line)} ",
      ".bgs-developer-links a{font-weight:600;font-size:13.5px} ",

      "pre,code,samp,kbd{font-family:ui-monospace,SFMono-Regular,'SF Mono',Menlo,Consolas,'Liberation Mono',monospace;font-size:12.5px} ",
      "pre{background:#f6f8fb;border:1px solid var(--bgs-line);border-radius:var(--bgs-r-sm);color:#374151} ",
      ".shiny-notification{border-radius:var(--bgs-r-sm);box-shadow:var(--bgs-shadow-lift)} ",
      "::selection{background:rgba(0,114,178,.18)}"
    ))
  )
}

wizard_env_section <- function() {
  shiny_collapsible_section(
    "Environment and installation (expand on first use to check BioGeoBEARS)",
    shiny_action_grid(
      shiny::actionButton("refresh_setup", "Re-check environment")
    ),
    shiny::tags$div(class = "bgs-key-files-title", "Installation status"),
    shiny::tableOutput("installation_table"),
    shiny::tags$div(class = "bgs-key-files-title", "BioGeoBEARS installation plan"),
    shiny::tableOutput("biogeobears_install_plan_table")
  )
}

bgs_app_ui <- function(default_config, default_output, example_project_dir) {
  shiny::fluidPage(
    bgs_head_styles(),
    shiny::titlePanel("BioGeoSyn"),
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

shiny_default_num_cores <- function() {
  n <- suppressWarnings(as.integer(parallel::detectCores()))
  if (is.na(n) || n < 1L) {
    return(1L)
  }
  max(1L, floor(n / 2))
}

wizard_step_data <- function(default_config, default_output, example_project_dir) {
  shiny::tabPanel(
    "1 \u00b7 Data",
    shiny::tags$div(
      class = "bgs-choice-card",
      shiny::tags$div(class = "bgs-control-title", "Use your own data"),
      shiny::textInput("wizard_project_name", "Project name", value = "my_clade"),
      shiny::tags$div(
        class = "bgs-upload-row",
        shiny::fileInput(
          "wizard_tree",
          "Tree file",
          accept = c(".nwk", ".newick", ".tree", ".tre")
        ),
        shiny::downloadButton("download_tree_template", "Template")
      ),
      shiny::tags$div(
        class = "bgs-upload-row",
        shiny::fileInput("wizard_geography", "Geography matrix CSV", accept = ".csv"),
        shiny::downloadButton("download_geography_template", "Template")
      ),
      shiny::tags$div(
        class = "bgs-upload-row",
        shiny::fileInput("wizard_regions", "Regions CSV", accept = ".csv"),
        shiny::downloadButton("download_regions_template", "Template")
      ),
      shiny::tags$div(
        class = "bgs-home-note",
        "No data yet? Use the \"Template\" button beside each upload \u2014 they are the built-in example data (a few species, a few areas). Edit them into your own data and upload."
      ),
      shiny_collapsible_section(
        "Advanced constraints (optional; for time-stratified and similar analyses)",
        shiny::tags$p(
          class = "bgs-home-note",
          "These files drive time stratification, dispersal multipliers, area adjacency and similar advanced analyses. Most analyses do not need them \u2014 leave blank. The \"Template\" button beside each upload downloads the expected format."
        ),
        shiny_wizard_constraint_inputs()
      ),
      shiny::numericInput("wizard_max_range_size", "Maximum range size", value = 3L, min = 1L, step = 1L),
      shiny::uiOutput("state_space_note"),
      shiny::checkboxGroupInput(
        "wizard_models",
        "Models to fit",
        choices = valid_models(),
        selected = valid_models()
      ),
      shiny::numericInput(
        "wizard_num_cores",
        "CPU cores (parallelises model fitting)",
        value = shiny_default_num_cores(), min = 1L, step = 1L
      ),
      shiny::tags$div(
        class = "bgs-home-note",
        "Extra cores speed up model fitting only. Do not exceed the machine's core count. The gain is largest for big state spaces (many areas, large max_range)."
      ),
      shiny::tags$div(class = "bgs-key-files-title", "Where results are saved"),
      shiny::tags$div(
        class = "bgs-output-row",
        shiny::textInput("output_dir", "Save all results to", value = default_output),
        shiny::actionButton("choose_output_dir", "Browse")
      ),
      shiny::tags$div(
        class = "bgs-home-note",
        "The run writes tables, figures, reports and logs into this directory. After uploading, an overview appears below \u2014 check it, then go to the Analysis tab to run."
      )
    ),
    shiny::tags$div(
      class = "bgs-choice-card",
      shiny::tags$div(class = "bgs-control-title", "Data overview"),
      shiny::tags$p(
        class = "bgs-home-note",
        "After uploading, an overview appears here (as in RASP): how many tips the tree has, how many species occupy each area, and how range sizes are distributed. Click \"Check inputs\" for the consistency checks."
      ),
      shiny_action_grid(shiny::actionButton("validate", "Check inputs")),
      shiny::tableOutput("data_overview_table"),
      shiny::tags$div(
        class = "bgs-two-col",
        shiny::tags$div(
          shiny::tags$div(class = "bgs-key-files-title", "Species per area"),
          shiny::tableOutput("region_occupancy_table")
        ),
        shiny::tags$div(
          shiny::tags$div(class = "bgs-key-files-title", "Range-size distribution"),
          shiny::tableOutput("range_size_table")
        )
      ),
      shiny::tags$div(class = "bgs-key-files-title", "Input validation"),
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
    "2 \u00b7 Analysis",
    shiny_control_section(
      "Run",
      shiny_action_grid(shiny::actionButton("run", "Start the analysis")),
      shiny::tags$div(
        class = "bgs-home-note",
        "When the run finishes, open the \"3. Single clade\" tab for the results; build and download the report from there if you need one."
      )
    ),
    shiny::checkboxInput("dry_run", "Dry run: check only, do not actually run BioGeoBEARS", value = TRUE),
    shiny_collapsible_section(
      "BSM stochastic mapping",
      shiny::tags$div(
        class = "bgs-home-note",
        "Only with this checked does the run produce event statistics, the process synthesis and rates through time (including process_rates_through_time.csv and region_process_rates_through_time.csv, which the cross-clade synthesis needs). Unchecked, the run fits models and estimates ancestral ranges only."
      ),
      shiny::checkboxInput("run_stochastic_mapping", "Run BSM stochastic mapping", value = FALSE),
      shiny::selectInput(
        "stochastic_mapping_model",
        "Model used for BSM",
        choices = c(
          "Best-fitting model" = "best",
          "Best non-+J model" = "best_non_j",
          "Best +J model" = "best_plus_j",
          "All fitted models" = "all"
        ),
        selected = "best"
      ),
      shiny::numericInput("stochastic_mapping_replicates", "BSM replicates", value = 100L, min = 1L, step = 1L),
      shiny::numericInput("stochastic_mapping_seed", "BSM random seed", value = 1L, min = 1L, step = 1L)
    )
  )
}

wizard_step_results <- function() {
  shiny::tabPanel(
    "3. Single clade",
    shiny_primary_results_body(),
    shiny_control_section(
      "Export",
      shiny::tags$div(
        class = "bgs-downloads",
        shiny::downloadButton("download_bundle", "Download result bundle (all result files)")
      ),
      shiny::tags$div(
        class = "bgs-home-note",
        "The bundle contains every result file for this clade (tables, figures, logs). Event statistics and the illustrated report live in \"4. Multi-clade synthesis\", where they are produced for the integrated results."
      )
    ),
    shiny_collapsible_section(
      "What each result file is (read alongside the downloaded bundle)",
      shiny::tags$p(
        class = "bgs-home-note",
        "The result bundle contains the files below. This table says what each one holds; the full tables and high-resolution figures are all in the bundle, so there is no need to expand them here."
      ),
      shiny::tableOutput("output_file_legend_table")
    )
  )
}

# Bundle a cross-clade combined table together with its integrated figure
# (publication PNG + PDF) into a single downloadable zip archive.
write_cross_clade_bundle <- function(file, combined, plot, stem, width, height) {
  tmp <- tempfile("bgs-xclade-")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  csv <- file.path(tmp, paste0(stem, ".csv"))
  png <- file.path(tmp, paste0(stem, ".png"))
  pdf <- file.path(tmp, paste0(stem, ".pdf"))
  utils::write.csv(combined, csv, row.names = FALSE, na = "")
  ggplot2::ggsave(png, plot, width = width, height = height, dpi = 300)
  tryCatch(
    ggplot2::ggsave(pdf, plot, width = width, height = height),
    error = function(e) unlink(pdf)
  )
  files <- basename(Filter(file.exists, c(csv, png, pdf)))
  zip_relative_files(tmp, file, files)
  invisible(file)
}

wizard_step_cross_clade <- function() {
  preview <- function(id, height = "460px") shiny::div(class = "bgs-preview", shiny::imageOutput(id, height = height))
  card <- function(title, ...) shiny::tags$div(class = "bgs-choice-card", shiny::tags$div(class = "bgs-control-title", title), ...)
  shiny::tabPanel(
    "4. Multi-clade synthesis",
    shiny::tags$div(
      class = "bgs-next-action",
      shiny::tags$div(class = "bgs-next-action-title", "Bring several clades together"),
      shiny::tags$div(
        class = "bgs-next-action-detail",
        "Download each clade's result bundle from \"3. Single clade\" first (.zip; BSM must have been enabled and run). Upload all of them below in one go and this page reads them and builds the full integrated results and report. Renaming each bundle to its clade name (e.g. Muridae.zip) is recommended."
      )
    ),
    card(
      "1 \u00b7 Upload the clades' result bundles",
      shiny::fileInput("cross_clade_bundles", "Upload result bundles (.zip; select several)", multiple = TRUE, accept = ".zip"),
      shiny::uiOutput("cross_clade_status")
    ),
    card("2 \u00b7 Biogeographic process synthesis (summed across clades)", preview("cc_synth_plot")),
    card("3 \u00b7 Process rates through time \u00b7 overall (all clades pooled)",
      shiny::tags$div(
        class = "bgs-inline-controls",
        shiny::numericInput("cc_bin_width", "Time-bin width (Ma)", value = 5, min = 1, max = 25, step = 1, width = "170px")
      ),
      preview("cross_clade_plot", "520px")),
    card("4 \u00b7 Process rates through time \u00b7 by region (one panel per region; in-situ / immigration / emigration)",
      shiny::tags$div(
        class = "bgs-inline-controls",
        shiny::checkboxInput("cc_region_log", "Log y-axis", value = TRUE)
      ),
      shiny::tags$div(class = "bgs-home-note", "Shares the time-bin width above. Uncheck regions to focus the figure (e.g. drop the non-Asian continents)."),
      shiny::checkboxGroupInput("cc_regions", NULL, choices = character(), inline = TRUE),
      preview("cross_clade_region_plot", "620px")),
    card("5 \u00b7 Dispersal between areas (network and per-area budget)",
      shiny::tags$div(
        class = "bgs-inline-controls",
        shiny::selectInput("cc_network_period", "Geological period",
          choices = geological_period_choices(), selected = "Total", width = "200px")
      ),
      shiny::tags$div(class = "bgs-home-note", "Uncheck regions to restrict the network to a subset of areas."),
      shiny::checkboxGroupInput("cc_network_regions", NULL, choices = character(), inline = TRUE),
      shiny::fluidRow(
        shiny::column(7, shiny::tags$div(class = "bgs-control-title", "Dispersal network"), preview("cc_network_plot", "460px")),
        shiny::column(5, shiny::tags$div(class = "bgs-control-title", "Immigration / emigration per area"), preview("cc_budget_plot", "460px"))
      )),
    shiny::tags$div(
      class = "bgs-choice-card",
      shiny::tags$div(class = "bgs-control-title", "Export and report"),
      shiny::tags$div(
        class = "bgs-home-note",
        "The report and the downloadable bundle also include the source-to-recipient exchange matrix and the event statistics table. Download every integrated result (CSVs and figures), or build a shareable HTML report. Build the report first, then download it."
      ),
      shiny::tags$div(
        class = "bgs-downloads",
        shiny::downloadButton("download_cross_clade", "Download all integrated results (CSV + figures)"),
        shiny::actionButton("render_xclade_report", "Build report"),
        shiny::downloadButton("download_xclade_report", "Download report")
      ),
      shiny::verbatimTextOutput("xclade_report_status")
    )
  )
}

wizard_step_help <- function() {
  link <- function(label, href) {
    shiny::tags$a(href = href, target = "_blank", rel = "noopener noreferrer", label)
  }
  shiny::tabPanel(
    "About and citation",
    shiny::tags$div(class = "bgs-key-files-title", "Software status"),
    shiny::tableOutput("about_table"),
    shiny::tags$div(class = "bgs-key-files-title", "Developer"),
    shiny::tags$div(
      class = "bgs-choice-card bgs-developer",
      shiny::tags$div(class = "bgs-developer-name", "Wei Xu"),
      shiny::tags$div(class = "bgs-developer-role", "Ph.D., Postdoctoral researcher"),
      shiny::tags$div(
        class = "bgs-developer-affil",
        "School of Zoology, Tel Aviv University, Israel"
      ),
      shiny::tags$div(
        class = "bgs-developer-links",
        link("xuwei.evo@gmail.com", "mailto:xuwei.evo@gmail.com"),
        link("Google Scholar", "https://scholar.google.com/citations?user=YOxNTyAAAAAJ&hl=en"),
        link("ResearchGate", "https://www.researchgate.net/profile/Wei-Xu-312"),
        link("GitHub", "https://github.com/XuWeiEvo/BioGeoSyn")
      )
    ),
    shiny::tags$div(class = "bgs-key-files-title", "BioGeoBEARS citation"),
    shiny::verbatimTextOutput("citation_text")
  )
}
