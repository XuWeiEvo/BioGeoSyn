#' Render a Quarto report
#'
#' @param result Workflow result from [run_workflow()].
#' @param template Optional path to a Quarto template.
#' @param format Quarto output format, usually `"html"` or `"pdf"`.
#' @return Path to the report source or rendered output.
#' @export
render_report <- function(result, template = NULL, format = "html") {
  template <- template %||% system.file("templates", "report_template.qmd", package = "BioGeoSyn")
  if (!file.exists(template)) {
    stop("Report template not found: ", template, call. = FALSE)
  }

  report_src <- file.path(result$project_paths$reports, "summary_report.qmd")
  file.copy(template, report_src, overwrite = TRUE)

  if (format %in% c("source", "qmd")) {
    create_workflow_manifest(result, write = TRUE)
    return(report_src)
  }

  env <- check_report_environment(format)
  if (!isTRUE(env$available[[1L]])) {
    message(env$next_step[[1L]], " Wrote report source only: ", report_src)
    return(report_src)
  }

  quarto::quarto_render(report_src, output_format = format)
  output_ext <- if (identical(format, "pdf")) "pdf" else if (identical(format, "html")) "html" else NULL
  if (!is.null(output_ext)) {
    rendered <- file.path(result$project_paths$reports, paste0("summary_report.", output_ext))
    if (file.exists(rendered)) {
      create_workflow_manifest(result, write = TRUE)
      return(rendered)
    }
  }
  create_workflow_manifest(result, write = TRUE)
  report_src
}

#' Check report rendering environment
#'
#' @param formats Character vector of report formats to check. Supported values
#'   are `"source"`, `"html"`, and `"pdf"`.
#' @param required Logical. If `TRUE`, stop when any requested format is not
#'   available.
#' @return A data frame with report-rendering availability and next-step
#'   guidance.
#' @export
check_report_environment <- function(formats = c("source", "html", "pdf"), required = FALSE) {
  formats <- unique(as.character(formats %||% c("source", "html", "pdf")))
  unsupported <- setdiff(formats, c("source", "html", "pdf"))
  if (length(unsupported) > 0L) {
    stop("Unsupported report format(s): ", paste(unsupported, collapse = ", "), call. = FALSE)
  }

  quarto_package <- requireNamespace("quarto", quietly = TRUE)
  quarto_cli <- FALSE
  quarto_version <- NA_character_
  if (quarto_package) {
    quarto_cli <- tryCatch(isTRUE(quarto::quarto_available()), error = function(e) FALSE)
    if (quarto_cli) {
      quarto_version <- tryCatch(as.character(quarto::quarto_version()), error = function(e) NA_character_)
    }
  }
  latex_engines <- Sys.which(c("pdflatex", "xelatex", "lualatex"))
  latex_available <- any(nzchar(latex_engines))
  latex_engine_names <- paste(names(latex_engines)[nzchar(latex_engines)], collapse = ", ")
  if (!nzchar(latex_engine_names)) {
    latex_engine_names <- NA_character_
  }

  out <- do.call(rbind, lapply(formats, function(format) {
    available <- switch(
      format,
      source = TRUE,
      html = quarto_package && quarto_cli,
      pdf = quarto_package && quarto_cli && latex_available
    )
    data.frame(
      format = format,
      available = available,
      quarto_package = quarto_package,
      quarto_cli = quarto_cli,
      quarto_version = quarto_version,
      latex_available = latex_available,
      latex_engines = latex_engine_names,
      next_step = report_environment_next_step(format, available, quarto_package, quarto_cli, latex_available),
      stringsAsFactors = FALSE
    )
  }))
  row.names(out) <- NULL

  if (isTRUE(required) && any(!out$available)) {
    missing <- out[!out$available, , drop = FALSE]
    stop(
      "Report environment is not ready for: ",
      paste(missing$format, collapse = ", "),
      ". ",
      paste(unique(missing$next_step), collapse = " "),
      call. = FALSE
    )
  }

  out
}

report_environment_next_step <- function(format, available, quarto_package, quarto_cli, latex_available) {
  if (isTRUE(available)) {
    return("Ready.")
  }
  if (identical(format, "source")) {
    return("Ready.")
  }
  if (!isTRUE(quarto_package)) {
    return("Install the quarto R package or render source reports only.")
  }
  if (!isTRUE(quarto_cli)) {
    return("Install the Quarto command-line tool, or render source reports only.")
  }
  if (identical(format, "pdf") && !isTRUE(latex_available)) {
    return("Install a LaTeX engine such as TinyTeX, MiKTeX, or TeX Live for PDF reports.")
  }
  "Render source reports only until the report environment is ready."
}
