#' Read an iBiogeobears YAML configuration file
#'
#' @param config Path to a YAML configuration file.
#' @return A normalized configuration list with defaults filled in.
#' @export
read_config <- function(config) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("The yaml package is required to read iBiogeobears config files.", call. = FALSE)
  }
  if (!file.exists(config)) {
    stop("Config file does not exist: ", config, call. = FALSE)
  }

  cfg <- yaml::read_yaml(config)
  cfg <- fill_config_defaults(cfg)
  cfg$.config_file <- as_path(config)
  cfg
}

fill_config_defaults <- function(cfg) {
  cfg$project <- cfg$project %||% list()
  cfg$inputs <- cfg$inputs %||% list()
  cfg$models <- cfg$models %||% list()
  cfg$analysis <- cfg$analysis %||% list()
  cfg$figures <- cfg$figures %||% list()
  cfg$report <- cfg$report %||% list()
  cfg$methodology <- cfg$methodology %||% list()
  cfg$advanced <- cfg$advanced %||% list()

  cfg$project$name <- cfg$project$name %||% "iBiogeobears_project"
  cfg$project$output_dir <- cfg$project$output_dir %||% file.path("results", cfg$project$name)
  cfg$inputs$max_range_size <- cfg$inputs$max_range_size %||% 3L
  cfg$models$run <- cfg$models$run %||% valid_models()
  cfg$analysis$run_stochastic_mapping <- cfg$analysis$run_stochastic_mapping %||% FALSE
  cfg$analysis$time_bins <- cfg$analysis$time_bins %||% NULL
  cfg$figures$output_formats <- cfg$figures$output_formats %||% c("pdf", "png", "svg")
  cfg$report$formats <- cfg$report$formats %||% c("html", "pdf")

  cfg$methodology$show_decj_caution <- cfg$methodology$show_decj_caution %||% TRUE
  cfg$methodology$report_model_uncertainty <- cfg$methodology$report_model_uncertainty %||% TRUE
  cfg$methodology$separate_j_and_no_j_comparisons <- cfg$methodology$separate_j_and_no_j_comparisons %||% TRUE
  cfg$methodology$auto_declare_best_model <- cfg$methodology$auto_declare_best_model %||% FALSE
  cfg$methodology$require_sensitivity_summary <- cfg$methodology$require_sensitivity_summary %||% TRUE

  cfg
}
