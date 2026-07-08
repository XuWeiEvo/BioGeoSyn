#' Launch the iBiogeobears Shiny application
#'
#' @param config Optional path to an `analysis.yml` file.
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

  default_config <- config %||% system.file("templates", "analysis.yml", package = "iBiogeobears")
  default_output <- output_dir %||% ""

  shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::tags$head(
        shiny::tags$style(shiny::HTML(
          ".container-fluid{max-width:1180px} .well{border-radius:4px} ",
          ".btn{border-radius:4px} .ibgb-status{font-weight:600;margin:8px 0} ",
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
          ".ibgb-preview img{max-width:100%;height:auto;border:1px solid #ddd} ",
          ".ibgb-figure-dashboard{display:grid;grid-template-columns:1fr;gap:18px} ",
          ".ibgb-figure-dashboard h4{margin:6px 0 8px 0} ",
          ".ibgb-figure-dashboard img{max-width:100%;height:auto;border:1px solid #ddd}"
        ))
      ),
      shiny::titlePanel("iBiogeobears"),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny_control_section(
            "Project",
            shiny::textInput("config_path", "analysis.yml", value = default_config),
            shiny::fileInput("config_upload", "Upload analysis.yml", accept = c(".yml", ".yaml")),
            shiny::textInput("output_dir", "Output directory", value = default_output),
            shiny::textInput("example_project_dir", "Example project directory", value = ""),
            shiny_action_grid(
              shiny::actionButton("create_example", "Create example project"),
              shiny::actionButton("load_results", "Load existing results")
            )
          ),
          shiny_control_section(
            "Run options",
            shiny::checkboxInput("dry_run", "Dry run", value = TRUE),
            shiny::checkboxInput("require_biogeobears", "Require BioGeoBEARS", value = FALSE),
            shiny::checkboxInput("force", "Force execution after validation failure", value = FALSE)
          ),
          shiny_control_section(
            "Workflow",
            shiny_action_grid(
              shiny::actionButton("validate", "Validate"),
              shiny::actionButton("run", "Run workflow"),
              shiny::actionButton("open_output", "Open output directory")
            )
          ),
          shiny_control_section(
            "Report and export",
            shiny::selectInput("report_format", "Report format", choices = c("source", "html", "pdf"), selected = "html"),
            shiny_action_grid(
              shiny::actionButton("render_report", "Render report"),
              shiny::actionButton("open_report", "Open report"),
              shiny::actionButton("refresh_key_files", "Refresh key files"),
              shiny::actionButton("bundle", "Create bundle if missing")
            ),
            shiny::tags$div(
              class = "ibgb-downloads",
              shiny::downloadButton("download_run_summary", "Download run summary"),
              shiny::downloadButton("download_report", "Download report"),
              shiny::downloadButton("download_bundle", "Download bundle")
            )
          )
        ),
        shiny::mainPanel(
          shiny::uiOutput("status"),
          shiny::tableOutput("summary_table"),
          shiny::tabsetPanel(
            shiny::tabPanel(
              "Run Summary",
              shiny::uiOutput("run_summary_cards"),
              shiny::tags$div(class = "ibgb-key-files-title", "Key files"),
              shiny::tableOutput("key_files_table"),
              shiny::tableOutput("run_summary_table")
            ),
            shiny::tabPanel("Validation", shiny::tableOutput("validation_table")),
            shiny::tabPanel("Run Status", shiny::tableOutput("model_table")),
            shiny::tabPanel(
              "Model Comparison",
              shiny::tags$div(class = "ibgb-key-files-title", "Fit summary"),
              shiny::tableOutput("model_fit_summary_table"),
              shiny::tags$div(class = "ibgb-key-files-title", "Model comparison details"),
              shiny::tableOutput("model_comparison_table")
            ),
            shiny::tabPanel(
              "+J Sensitivity",
              shiny::tags$div(class = "ibgb-key-files-title", "+J sensitivity summary"),
              shiny::tableOutput("plus_j_summary_table"),
              shiny::tags$div(class = "ibgb-key-files-title", "+J sensitivity details"),
              shiny::tableOutput("model_sensitivity_table")
            ),
            shiny::tabPanel(
              "Warnings",
              shiny::tags$div(class = "ibgb-key-files-title", "Warning summary"),
              shiny::tableOutput("warning_summary_table"),
              shiny::tags$div(class = "ibgb-key-files-title", "Warning details"),
              shiny::tableOutput("warnings_table")
            ),
            shiny::tabPanel("Node States", shiny::tableOutput("node_state_summary_table")),
            shiny::tabPanel("Node Sensitivity", shiny::tableOutput("node_state_sensitivity_table")),
            shiny::tabPanel("Manifest", shiny::tableOutput("manifest_table")),
            shiny::tabPanel("Report", shiny::verbatimTextOutput("report_path_text")),
            shiny::tabPanel(
              "Figure Dashboard",
              shiny::tableOutput("figure_dashboard_table"),
              shiny::tags$div(
                class = "ibgb-figure-dashboard",
                shiny_figure_panel("Model Comparison", "figure_model_comparison"),
                shiny_figure_panel("Root State Probabilities", "figure_root_states"),
                shiny_figure_panel("Best Model Node States", "figure_node_best"),
                shiny_figure_panel("Best Non-+J Node States", "figure_node_non_j"),
                shiny_figure_panel("Best +J Node States", "figure_node_plus_j"),
                shiny_figure_panel("Node-State Sensitivity", "figure_node_sensitivity")
              )
            ),
            shiny::tabPanel(
              "Tables",
              shiny::tags$div(class = "ibgb-key-files-title", "Table status"),
              shiny::tableOutput("table_status_table"),
              shiny::selectInput("table_preview", "Table", choices = c("No CSV tables available" = "")),
              shiny::tableOutput("table_preview_output"),
              shiny::verbatimTextOutput("table_path_text")
            ),
            shiny::tabPanel(
              "Figures",
              shiny::selectInput("figure_preview", "Figure", choices = c("No PNG figures available" = "")),
              shiny::div(class = "ibgb-preview", shiny::imageOutput("figure_image")),
              shiny::verbatimTextOutput("figure_path_text")
            ),
            shiny::tabPanel(
              "About/Citation",
              shiny::tags$div(class = "ibgb-key-files-title", "Software status"),
              shiny::tableOutput("about_table"),
              shiny::tags$div(class = "ibgb-key-files-title", "BioGeoBEARS citation"),
              shiny::verbatimTextOutput("citation_text")
            ),
            shiny::tabPanel("Paths", shiny::verbatimTextOutput("paths_text")),
            shiny::tabPanel("Messages", shiny::verbatimTextOutput("messages_text"))
          )
        )
      )
    ),
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

shiny_action_grid <- function(...) {
  shiny::tags$div(class = "ibgb-action-grid", ...)
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
        message = "Ready.",
        messages = "Ready.",
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

      shiny::observeEvent(input$create_example, {
        run_app_action(state, {
          target <- trimws(input$example_project_dir %||% "")
          if (!nzchar(target)) {
            target <- tempfile("ibgb-example-project-")
          }
          append_app_stage(state, "Example project", "started", target)
          example <- create_example_project(target)
          shiny::updateTextInput(session, "config_path", value = example$config)
          shiny::updateTextInput(session, "output_dir", value = example$output_dir)
          append_app_stage(state, "Example project", "ready", example$root)
        })
      })

      shiny::observeEvent(input$validate, {
        run_app_action(state, {
          shiny::withProgress(message = "Validating", value = 0, {
          append_app_stage(state, "Validation", "started", current_config_path())
          cfg <- read_config(current_config_path())
          if (!is.null(current_output_dir())) {
            cfg$project$output_dir <- current_output_dir()
          }
          state$validation <- validate_inputs(cfg)
          state$model_table <- planned_model_table(cfg)
          append_app_stage(state, "Validation", "model plan ready", paste(nrow(state$model_table), "model(s)"))
          append_app_message(state, if (all(state$validation$ok)) "Validation passed." else "Validation failed.")
          shiny::incProgress(1)
          })
        })
      })

      shiny::observeEvent(input$load_results, {
        run_app_action(state, {
          shiny::withProgress(message = "Loading existing results", value = 0, {
          append_app_stage(state, "Load existing results", "started", current_output_dir())
          result <- load_existing_workflow_result(current_output_dir())
          state$result <- result
          state$validation <- result$validation
          state$model_table <- result$model_run_status
          state$manifest <- result$workflow_manifest
          state$report <- report_preview_path(state)
          refresh_shiny_result_exports(session, state)
          append_app_stage(state, "Load existing results", "ready", result$project_paths$root)
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
            config = current_config_path(),
            output_dir = current_output_dir(),
            dry_run = isTRUE(input$dry_run),
            require_biogeobears = isTRUE(input$require_biogeobears),
            force = isTRUE(input$force)
          )
          state$result <- result
          state$validation <- result$validation
          state$model_table <- result$model_run_status
          state$manifest <- result$workflow_manifest
          append_app_stage(state, "Workflow", "validation complete", workflow_validation_label(result$validation))
          append_app_stage(state, "Workflow", "model status ready", workflow_model_status_label(result$model_run_status))
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
          append_app_stage(state, "Report", "render started", input$report_format)
          state$report <- render_report(state$result, format = input$report_format)
          refresh_shiny_result_exports(session, state)
          append_app_message(state, paste("Report ready:", state$report))
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

      shiny::observeEvent(input$open_output, {
        run_app_action(state, {
          require_workflow_result(state$result)
          output_dir <- normalizePath(state$result$project_paths$root, winslash = "/", mustWork = TRUE)
          utils::browseURL(output_dir)
          append_app_message(state, paste("Output directory:", output_dir))
        })
      })

      output$status <- shiny::renderUI({
        shiny::tags$div(class = paste("ibgb-status", state$status_type), state$message)
      })

      output$summary_table <- shiny::renderTable({
        shiny_summary_table(state)
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
        state$validation
      }, striped = TRUE, bordered = TRUE, na = "")

      output$model_table <- shiny::renderTable({
        table_head(state$model_table, 20L)
      }, striped = TRUE, bordered = TRUE, na = "")

      output$model_fit_summary_table <- shiny::renderTable({
        shiny_model_fit_summary_table(state)
      }, striped = TRUE, bordered = TRUE, na = "")

      output$model_comparison_table <- shiny::renderTable({
        table_head(shiny_model_comparison_table(state), 30L)
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

      output$manifest_table <- shiny::renderTable({
        table_head(state$manifest, 30L)
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

      output$figure_root_states <- shiny::renderImage({
        shiny_named_figure_image(state, "root_state_probabilities")
      }, deleteFile = FALSE)

      output$figure_node_best <- shiny::renderImage({
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
  figure_manifest <- read_existing_figure_manifest(project_paths)

  standardized_tables <- list(
    geographic_states = read_existing_output_table(project_paths, "geographic_states.csv") %||% data.frame(),
    tree_nodes = read_existing_output_table(project_paths, "tree_nodes.csv") %||% data.frame(),
    parameter_table = read_existing_output_table(project_paths, "model_parameters.csv") %||% data.frame(),
    ancestral_state_probabilities = read_existing_output_table(project_paths, "ancestral_state_probabilities.csv") %||% data.frame(),
    root_state_probabilities = read_existing_output_table(project_paths, "root_state_probabilities.csv") %||% data.frame(),
    node_state_summary = read_existing_output_table(project_paths, "node_state_summary.csv") %||% data.frame(),
    node_state_sensitivity = node_state_sensitivity %||% data.frame()
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
    if ("warning_count" %in% names(state$model_table)) {
      warning_count <- as.character(sum(state$model_table$warning_count, na.rm = TRUE))
    }
  }

  data.frame(
    item = c("Validation", "Run mode", "Run status", "Completed models", "Warning count", "Report", "Bundle"),
    value = c(
      validation_status,
      run_mode,
      run_status,
      completed_models,
      warning_count,
      if (!is.null(report_preview_path(state))) "available" else "not available",
      if (!is.null(state$bundle) && file.exists(state$bundle)) "available" else "not available"
    ),
    stringsAsFactors = FALSE
  )
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
  output_dir <- if (!is.null(state$result)) state$result$project_paths$root %||% "not available" else "not available"

  data.frame(
    item = c(
      "Fitted models",
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
  if (identical(item, "Report")) {
    return("good")
  }
  "info"
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
      "+J sensitivity CSV",
      "Workflow manifest CSV",
      "Report",
      "Result bundle"
    ),
    relative_path = c(
      "tables/shiny_run_summary.csv",
      "tables/model_comparison.csv",
      "tables/model_sensitivity.csv",
      "tables/workflow_manifest.csv",
      "reports/summary_report.html",
      NA_character_
    ),
    missing_action = c(
      "Run or load workflow results, then refresh key files.",
      "Run or load workflow results.",
      "Run or load workflow results.",
      "Run or load workflow results, then refresh key files.",
      "Click Render report.",
      "Click Create bundle if missing."
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

  if (is.na(relative_path)) {
    bundle <- state$bundle %||% NULL
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
  rows <- warnings[!is.na(warnings$model) & warnings$model != "No captured warnings", , drop = FALSE]
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

warning_next_step <- function(total) {
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

  data.frame(
    item = c("Captured warnings", "Affected models", "Highest warning count", "Recommended next step"),
    value = c(
      total,
      affected,
      max_warning_count_label(warnings),
      warning_next_step(total)
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
  rows <- table[!is.na(table$warning_count) & table$warning_count > 0L, , drop = FALSE]
  if (nrow(rows) == 0L) {
    return(data.frame(model = "No captured warnings", warning_count = 0L, warning_messages = ""))
  }
  cols <- c("model", "status", "warning_count", "warning_messages", "log_file")
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
      "node_state_sensitivity"
    ),
    display_label = c(
      "Model Comparison",
      "Root State Probabilities",
      "Best Model Node States",
      "Best Non-+J Node States",
      "Best +J Node States",
      "Node-State Sensitivity"
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
