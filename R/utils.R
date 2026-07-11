`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "best_state", "best_state_label", "comparison", "delta_aicc",
    "event_count", "event_label", "event_time", "event_type", "frequency",
    "internal_node_label", "model",
    "name", "node_display", "node_label", "parent_x", "parent_y",
    "plot_probability", "plus_j", "probability", "probability_difference_abs",
    "state", "state_change", "tip_display", "x", "xend", "y", "yend"
  ))
}

as_path <- function(path) {
  if (is.null(path) || length(path) == 0L || identical(path, "")) {
    return(NULL)
  }
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

write_csv_base <- function(x, path) {
  utils::write.csv(x, file = path, row.names = FALSE, na = "")
  invisible(path)
}

valid_models <- function() {
  c("DEC", "DEC+J", "DIVALIKE", "DIVALIKE+J", "BAYAREALIKE", "BAYAREALIKE+J")
}

is_j_model <- function(model) {
  grepl("\\+J$", model)
}

model_family <- function(model) {
  sub("\\+J$", "", model)
}
