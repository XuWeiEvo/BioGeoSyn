#' Open the ordinary-user quick-start guide
#'
#' Opens or returns the installed Markdown guide for first-time users. The guide
#' covers installation checks, the bundled example project, the Shiny app,
#' multi-clade synthesis, result bundles, diagnostic bundles, and common
#' recovery steps.
#'
#' @param browse Logical. If `TRUE`, open the guide with [utils::browseURL()].
#'   Defaults to `interactive()`.
#' @return The absolute path to the installed guide.
#' @export
open_user_guide <- function(browse = interactive()) {
  guide <- system.file(
    "docs",
    "ordinary-user-quick-start.md",
    package = "BioGeoSyn",
    mustWork = FALSE
  )
  if (!nzchar(guide) || !file.exists(guide)) {
    stop(
      "The BioGeoSyn ordinary-user guide was not found in the installed package.",
      call. = FALSE
    )
  }

  guide <- as_path(guide)
  if (isTRUE(browse)) {
    utils::browseURL(guide)
  }
  guide
}
