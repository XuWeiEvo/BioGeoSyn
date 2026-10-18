# ---- Progress reporting ----
#
# Long runs (model fitting, stochastic mapping) report what they are doing:
# a timestamped line on the console and, inside the Shiny app, the progress
# bar and the run log. Set options(BioGeoSyn.progress = FALSE) to silence the
# console lines.

#' Report workflow progress
#'
#' @param ... Message parts, pasted together.
#' @param value Optional progress fraction in `[0, 1]` for the app's bar.
#' @return Invisibly, the message.
#' @keywords internal
bgs_progress <- function(..., value = NULL) {
  msg <- paste0(...)
  if (!isFALSE(getOption("BioGeoSyn.progress", TRUE))) {
    # Written to stderr directly so it is not captured by the per-model log
    # sinks and always reaches the console.
    cat(sprintf("[BioGeoSyn %s] %s\n", format(Sys.time(), "%H:%M:%S"), msg), file = stderr())
  }
  cb <- getOption("BioGeoSyn.progress_callback")
  if (is.function(cb)) try(cb(msg, value), silent = TRUE)
  invisible(msg)
}

format_elapsed <- function(t0) {
  secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  if (secs < 90) sprintf("%.0f s", secs) else sprintf("%.1f min", secs / 60)
}
