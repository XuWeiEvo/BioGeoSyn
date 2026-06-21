#' Render a Quarto report
#'
#' @param result Workflow result from [run_workflow()].
#' @param template Optional path to a Quarto template.
#' @param format Quarto output format, usually `"html"` or `"pdf"`.
#' @return Path to the report source or rendered output.
#' @export
render_report <- function(result, template = NULL, format = "html") {
  template <- template %||% system.file("templates", "report_template.qmd", package = "iBiogeobears")
  if (!file.exists(template)) {
    stop("Report template not found: ", template, call. = FALSE)
  }

  report_src <- file.path(result$project_paths$reports, "summary_report.qmd")
  file.copy(template, report_src, overwrite = TRUE)

  if (format %in% c("source", "qmd")) {
    return(report_src)
  }

  if (!requireNamespace("quarto", quietly = TRUE)) {
    message("The quarto R package is not installed. Wrote report source only: ", report_src)
    return(report_src)
  }

  quarto::quarto_render(report_src, output_format = format)
  output_ext <- if (identical(format, "pdf")) "pdf" else if (identical(format, "html")) "html" else NULL
  if (!is.null(output_ext)) {
    rendered <- file.path(result$project_paths$reports, paste0("summary_report.", output_ext))
    if (file.exists(rendered)) {
      return(rendered)
    }
  }
  report_src
}
