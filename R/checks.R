#' Check for an external BioGeoBEARS installation
#'
#' BioGeoBEARS is intentionally not bundled with iBiogeobears. This function
#' checks whether it is available and provides installation and citation helper
#' messages.
#'
#' @param required Logical. If `TRUE`, stop when BioGeoBEARS is missing.
#' @return A list with availability, version, package path, citation, and helper
#'   text.
#' @export
check_biogeobears <- function(required = TRUE) {
  available <- requireNamespace("BioGeoBEARS", quietly = TRUE)

  install_help <- paste(
    "BioGeoBEARS is required for running analyses but is not bundled with",
    "iBiogeobears. Install it separately, for example:",
    "install.packages(c('devtools', 'rexpokit', 'cladoRcpp'));",
    "devtools::install_github('nmatzke/BioGeoBEARS', dependencies = FALSE)"
  )

  if (!available) {
    if (isTRUE(required)) {
      stop(install_help, call. = FALSE)
    }
    return(list(
      available = FALSE,
      version = NA_character_,
      path = NA_character_,
      citation = NA_character_,
      install_help = install_help
    ))
  }

  citation_text <- tryCatch(
    paste(utils::capture.output(utils::citation("BioGeoBEARS")), collapse = "\n"),
    error = function(e) "Run citation('BioGeoBEARS') for citation details."
  )

  list(
    available = TRUE,
    version = as.character(utils::packageVersion("BioGeoBEARS")),
    path = find.package("BioGeoBEARS"),
    citation = citation_text,
    install_help = install_help
  )
}

#' Check whether iBiogeobears is ready for common user workflows
#'
#' Summarizes the local R, package, BioGeoBEARS, Shiny, and report-rendering
#' environment in one user-facing table. Missing optional PDF support does not
#' prevent model execution or HTML reporting.
#'
#' @param include_pdf Logical. Include the optional PDF-report check.
#' @return A data frame with component status, purpose, version, and a
#'   recommended next step.
#' @export
check_installation <- function(include_pdf = TRUE) {
  core_packages <- c("yaml", "ggplot2", "igraph", "ggraph", "ape")
  core_available <- vapply(core_packages, requireNamespace, logical(1), quietly = TRUE)
  missing_core <- core_packages[!core_available]
  core_versions <- vapply(
    core_packages[core_available],
    function(package) as.character(utils::packageVersion(package)),
    character(1)
  )

  bgb <- check_biogeobears(required = FALSE)
  shiny_available <- requireNamespace("shiny", quietly = TRUE)
  html <- check_report_environment("html")

  rows <- list(
    installation_check_row(
      "R",
      "All workflows",
      TRUE,
      getRversion() >= "4.1",
      as.character(getRversion()),
      "Install R 4.1 or newer."
    ),
    installation_check_row(
      "Core R packages",
      "All workflows",
      TRUE,
      all(core_available),
      if (length(core_versions) > 0L) {
        paste(paste(names(core_versions), core_versions, sep = " "), collapse = "; ")
      } else {
        NA_character_
      },
      if (length(missing_core) > 0L) {
        paste0(
          "Install missing packages: install.packages(c(",
          paste(sprintf("'%s'", missing_core), collapse = ", "),
          "))."
        )
      } else {
        "Ready."
      }
    ),
    installation_check_row(
      "Shiny",
      "Graphical interface",
      TRUE,
      shiny_available,
      if (shiny_available) as.character(utils::packageVersion("shiny")) else NA_character_,
      "Install the graphical interface dependency with install.packages('shiny')."
    ),
    installation_check_row(
      "BioGeoBEARS",
      "Real model execution",
      TRUE,
      isTRUE(bgb$available),
      bgb$version,
      bgb$install_help
    ),
    installation_check_row(
      "Quarto HTML",
      "HTML reports",
      FALSE,
      isTRUE(html$available[[1L]]),
      html$quarto_version[[1L]],
      html$next_step[[1L]]
    )
  )

  if (isTRUE(include_pdf)) {
    pdf <- check_report_environment("pdf")
    rows[[length(rows) + 1L]] <- installation_check_row(
      "Quarto PDF",
      "PDF reports",
      FALSE,
      isTRUE(pdf$available[[1L]]),
      pdf$quarto_version[[1L]],
      pdf$next_step[[1L]]
    )
  }

  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

installation_check_row <- function(component, required_for, required, available, version, next_step) {
  data.frame(
    component = component,
    required_for = required_for,
    required = if (isTRUE(required)) "yes" else "no",
    status = if (isTRUE(available)) "Ready" else "Action needed",
    version = if (is.null(version) || length(version) == 0L || is.na(version[[1L]])) {
      NA_character_
    } else {
      as.character(version[[1L]])
    },
    next_step = if (isTRUE(available)) "Ready." else as.character(next_step %||% "Install the missing component."),
    stringsAsFactors = FALSE
  )
}
