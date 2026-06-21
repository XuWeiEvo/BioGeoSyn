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
