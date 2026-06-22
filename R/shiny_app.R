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
          ".ibgb-downloads{margin:10px 0 16px 0} .ibgb-downloads .btn{margin-right:6px}"
        ))
      ),
      shiny::titlePanel("iBiogeobears"),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::textInput("config_path", "analysis.yml", value = default_config),
          shiny::textInput("output_dir", "Output directory", value = default_output),
          shiny::checkboxInput("dry_run", "Dry run", value = TRUE),
          shiny::checkboxInput("require_biogeobears", "Require BioGeoBEARS", value = FALSE),
          shiny::checkboxInput("force", "Force execution after validation failure", value = FALSE),
          shiny::selectInput("report_format", "Report format", choices = c("source", "html", "pdf"), selected = "html"),
          shiny::actionButton("validate", "Validate"),
          shiny::actionButton("run", "Run workflow"),
          shiny::actionButton("render_report", "Render report"),
          shiny::actionButton("bundle", "Bundle results"),
          shiny::actionButton("open_output", "Open output directory"),
          shiny::tags$div(
            class = "ibgb-downloads",
            shiny::downloadButton("download_report", "Download report"),
            shiny::downloadButton("download_bundle", "Download bundle")
          )
        ),
        shiny::mainPanel(
          shiny::uiOutput("status"),
          shiny::tabsetPanel(
            shiny::tabPanel("Validation", shiny::tableOutput("validation_table")),
            shiny::tabPanel("Models", shiny::tableOutput("model_table")),
            shiny::tabPanel("Manifest", shiny::tableOutput("manifest_table")),
            shiny::tabPanel("Paths", shiny::verbatimTextOutput("paths_text")),
            shiny::tabPanel("Messages", shiny::verbatimTextOutput("messages_text"))
          )
        )
      )
    ),
    server = iBGB_shiny_server
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

      current_output_dir <- shiny::reactive({
        value <- trimws(input$output_dir %||% "")
        if (nzchar(value)) value else NULL
      })

      shiny::observeEvent(input$validate, {
        run_app_action(state, {
          shiny::withProgress(message = "Validating", value = 0, {
          cfg <- read_config(input$config_path)
          if (!is.null(current_output_dir())) {
            cfg$project$output_dir <- current_output_dir()
          }
          state$validation <- validate_inputs(cfg)
          state$model_table <- planned_model_table(cfg)
          append_app_message(state, if (all(state$validation$ok)) "Validation passed." else "Validation failed.")
          shiny::incProgress(1)
          })
        })
      })

      shiny::observeEvent(input$run, {
        run_app_action(state, {
          shiny::withProgress(message = "Running workflow", value = 0, {
          result <- run_workflow(
            config = input$config_path,
            output_dir = current_output_dir(),
            dry_run = isTRUE(input$dry_run),
            require_biogeobears = isTRUE(input$require_biogeobears),
            force = isTRUE(input$force)
          )
          state$result <- result
          state$validation <- result$validation
          state$model_table <- result$model_run_status
          state$manifest <- result$workflow_manifest
          append_app_message(state, if (isTRUE(result$dry_run)) "Dry run completed." else "Workflow completed.")
          shiny::incProgress(1)
          })
        })
      })

      shiny::observeEvent(input$render_report, {
        run_app_action(state, {
          require_workflow_result(state$result)
          shiny::withProgress(message = "Rendering report", value = 0, {
          state$report <- render_report(state$result, format = input$report_format)
          state$manifest <- create_workflow_manifest(state$result, write = TRUE)
          append_app_message(state, paste("Report:", state$report))
          shiny::incProgress(1)
          })
        })
      })

      shiny::observeEvent(input$bundle, {
        run_app_action(state, {
          require_workflow_result(state$result)
          shiny::withProgress(message = "Bundling results", value = 0, {
          state$bundle <- bundle_results(state$result, overwrite = TRUE)
          state$manifest <- create_workflow_manifest(state$result, write = TRUE)
          append_app_message(state, paste("Bundle:", state$bundle))
          shiny::incProgress(1)
          })
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

      output$validation_table <- shiny::renderTable({
        state$validation
      }, striped = TRUE, bordered = TRUE, na = "")

      output$model_table <- shiny::renderTable({
        table_head(state$model_table, 20L)
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

      output$download_report <- shiny::downloadHandler(
        filename = function() {
          basename(resolve_report_file(state))
        },
        content = function(file) {
          src <- resolve_report_file(state)
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

resolve_report_file <- function(state) {
  path <- state$report
  if (is.null(path) && !is.null(state$result)) {
    candidates <- file.path(state$result$project_paths$reports, c("summary_report.html", "summary_report.pdf", "summary_report.qmd"))
    candidates <- candidates[file.exists(candidates)]
    path <- candidates[1L] %||% NULL
  }
  require_existing_file(path, "Render a report before downloading it.")
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

check_shiny_available <- function() {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("The shiny package is required to launch the GUI. Install it with install.packages('shiny').", call. = FALSE)
  }
  invisible(TRUE)
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
