`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "delta_aicc", "event_time", "event_type", "frequency", "model",
    "name", "plus_j", "probability", "state"
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
