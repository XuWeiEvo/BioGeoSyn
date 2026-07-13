#' Run selected BioGeoBEARS models
#'
#' Builds BioGeoBEARS run objects from an iBiogeobears configuration and,
#' when requested, executes the selected models. BioGeoBEARS is not bundled
#' with this package and is checked only at runtime.
#'
#' @param config Configuration list.
#' @param project_paths Paths returned by [create_project()].
#' @param execute Logical. If `FALSE`, return a run plan without executing.
#' @return A data frame describing planned or executed model runs. Executed
#'   runs also write raw `.rds` outputs, logs, `model_fit_raw.csv`,
#'   `model_comparison.csv`, `model_sensitivity.csv`,
#'   `model_sensitivity.rds`, `node_state_sensitivity.csv`,
#'   `best_fit_events.csv`, optional BSM stochastic mapping tables, and warning
#'   summaries in `model_run_status.csv`.
#' @export
run_models <- function(config, project_paths, execute = FALSE) {
  models <- config$models$run %||% valid_models()
  retry_failed_only <- isTRUE(config$analysis$retry_failed_only %||% FALSE)
  reuse_completed <- isTRUE(config$analysis$resume_completed_models %||% TRUE) ||
    retry_failed_only
  unknown_models <- setdiff(models, valid_models())
  if (length(unknown_models) > 0L) {
    stop("Unsupported BioGeoBEARS model(s): ", paste(unknown_models, collapse = ", "), call. = FALSE)
  }

  raw_output_dirs <- file.path(project_paths$raw_biogeobears, models)
  run_plan <- data.frame(
    model = models,
    status = "planned",
    run_action = "planned",
    raw_output_dir = raw_output_dirs,
    stringsAsFactors = FALSE
  )

  if (!isTRUE(execute)) {
    return(run_plan)
  }

  check_biogeobears(required = TRUE)
  prepare_biogeobears_runtime()
  invisible(lapply(raw_output_dirs, dir.create, recursive = TRUE, showWarnings = FALSE))

  prepared_inputs <- prepare_biogeobears_inputs(config, project_paths)
  model_results <- vector("list", length(models))
  names(model_results) <- models
  previous_results <- list()
  previous_status <- read_previous_model_status(project_paths)
  if (retry_failed_only && nrow(previous_status) == 0L) {
    stop(
      "Retry-failed-only mode requires an existing tables/model_run_status.csv file.",
      call. = FALSE
    )
  }

  for (model in models) {
    family <- model_family(model)
    raw_dir <- file.path(project_paths$raw_biogeobears, model)
    run_signature <- model_run_signature(config, prepared_inputs, model)
    no_j_seed <- if (is_j_model(model)) previous_results[[family]] else NULL

    reusable <- if (reuse_completed) {
      load_reusable_model_result(model, raw_dir, run_signature)
    } else {
      NULL
    }

    if (!is.null(reusable)) {
      model_results[[model]] <- reusable
    } else if (retry_failed_only && !model_was_previously_failed(model, previous_status)) {
      model_results[[model]] <- skipped_model_result(
        model,
        raw_dir,
        run_signature,
        "Model was not marked as failed in the previous run."
      )
    } else {
      model_results[[model]] <- run_one_biogeobears_model(
        config = config,
        prepared_inputs = prepared_inputs,
        model = model,
        raw_dir = raw_dir,
        no_j_seed = no_j_seed,
        run_signature = run_signature
      )
    }

    if (!is_j_model(model) && identical(model_results[[model]]$status, "completed")) {
      previous_results[[family]] <- model_results[[model]]$result
    }
  }

  raw_table <- do.call(rbind, lapply(model_results, `[[`, "summary"))
  row.names(raw_table) <- NULL
  write_csv_base(raw_table, file.path(project_paths$tables, "model_fit_raw.csv"))
  write_csv_base(raw_table, file.path(project_paths$tables, "model_run_status.csv"))

  completed <- raw_table[raw_table$status == "completed", , drop = FALSE]
  if (nrow(completed) == 0L) {
    attr(raw_table, "run_status") <- raw_table
    return(raw_table)
  }

  standardized_tables <- standardize_biogeobears_outputs(
    model_results = model_results,
    prepared_inputs = prepared_inputs,
    project_paths = project_paths
  )

  comparison_input <- completed[, c("model", "logLik", "num_params"), drop = FALSE]
  comparison <- compare_models(comparison_input, n = prepared_inputs$n_taxa)
  comparison <- merge(
    comparison,
    completed[, c("model", "status", "raw_output_dir", "result_file", "log_file"), drop = FALSE],
    by = "model",
    all.x = TRUE,
    sort = FALSE
  )
  comparison <- comparison[order(comparison$AICc), , drop = FALSE]
  row.names(comparison) <- NULL

  sensitivity <- assess_model_sensitivity(comparison)
  sensitivity_table <- model_sensitivity_summary_table(comparison, sensitivity)
  node_state_sensitivity <- compare_node_state_sensitivity(
    node_state_summary = standardized_tables$node_state_summary,
    comparison = comparison
  )
  best_fit_events <- summarize_best_fit_events(
    range_change_events = standardized_tables$range_change_events,
    comparison = comparison
  )
  standardized_tables$node_state_sensitivity <- node_state_sensitivity
  standardized_tables$best_fit_events <- best_fit_events
  saveRDS(sensitivity, file.path(project_paths$tables, "model_sensitivity.rds"))
  write_csv_base(sensitivity_table, file.path(project_paths$tables, "model_sensitivity.csv"))
  write_csv_base(node_state_sensitivity, file.path(project_paths$tables, "node_state_sensitivity.csv"))
  write_csv_base(best_fit_events, file.path(project_paths$tables, "best_fit_events.csv"))
  write_csv_base(comparison, file.path(project_paths$tables, "model_comparison.csv"))

  bsm_tables <- run_bsm_stochastic_mapping(
    config = config,
    project_paths = project_paths,
    model_results = model_results,
    comparison = comparison,
    prepared_inputs = prepared_inputs
  )
  standardized_tables <- c(standardized_tables, bsm_tables)

  attr(comparison, "sensitivity") <- sensitivity
  attr(comparison, "sensitivity_table") <- sensitivity_table
  attr(comparison, "node_state_sensitivity") <- node_state_sensitivity
  attr(comparison, "best_fit_events") <- best_fit_events
  attr(comparison, "bsm_tables") <- bsm_tables
  attr(comparison, "run_status") <- raw_table
  attr(comparison, "standardized_tables") <- standardized_tables
  comparison
}

prepare_biogeobears_inputs <- function(config, project_paths) {
  base_dir <- dirname(config$.config_file %||% ".")
  tree_file <- resolve_config_path(config$inputs$tree_file, base_dir)
  geography_file <- resolve_config_path(config$inputs$geography_file, base_dir)
  regions_file <- resolve_config_path(config$inputs$regions_file, base_dir)

  if (is.null(tree_file) || !file.exists(tree_file)) {
    stop("Tree file does not exist: ", tree_file %||% "missing", call. = FALSE)
  }
  if (is.null(geography_file) || !file.exists(geography_file)) {
    stop("Geography file does not exist: ", geography_file %||% "missing", call. = FALSE)
  }

  input_dir <- project_paths$inputs
  dir.create(input_dir, recursive = TRUE, showWarnings = FALSE)

  tree_out <- file.path(input_dir, basename(tree_file))
  file.copy(tree_file, tree_out, overwrite = TRUE)
  original_geography_out <- file.path(input_dir, basename(geography_file))
  file.copy(geography_file, original_geography_out, overwrite = TRUE)

  geography <- read_range_matrix(geography_file)
  geog_out <- file.path(input_dir, "geography.data")
  write_biogeobears_geography(geography, geog_out)

  list(
    tree_file = as_path(tree_out),
    geography_file = as_path(geog_out),
    original_geography_file = as_path(original_geography_out),
    n_taxa = nrow(geography$matrix),
    areas = colnames(geography$matrix),
    max_range_size = as.integer(config$inputs$max_range_size),
    region_metadata = read_region_metadata(regions_file)
  )
}

read_region_metadata <- function(path) {
  if (is.null(path) || !file.exists(path)) {
    return(data.frame())
  }
  regions <- tryCatch(
    utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE),
    error = function(e) data.frame()
  )
  if (nrow(regions) == 0L || !"region" %in% names(regions)) {
    return(data.frame())
  }
  if (!"label" %in% names(regions)) {
    regions$label <- regions$region
  }
  regions[, intersect(c("region", "label", "color"), names(regions)), drop = FALSE]
}

read_range_matrix <- function(path) {
  ranges <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  if (ncol(ranges) < 2L) {
    stop("Geography CSV must contain a taxon column and at least one area column.", call. = FALSE)
  }

  taxon_col <- if ("species" %in% names(ranges)) "species" else if ("taxon" %in% names(ranges)) "taxon" else names(ranges)[1L]
  taxa <- ranges[[taxon_col]]
  if (any(is.na(taxa) | taxa == "")) {
    stop("Geography CSV contains missing taxon names.", call. = FALSE)
  }

  area_data <- ranges[setdiff(names(ranges), taxon_col)]
  area_matrix <- as.matrix(area_data)
  mode(area_matrix) <- "numeric"
  if (any(is.na(area_matrix)) || any(!area_matrix %in% c(0, 1))) {
    stop("Geography CSV area columns must contain only 0/1 values.", call. = FALSE)
  }
  if (any(rowSums(area_matrix) == 0L)) {
    stop("Every taxon must be present in at least one area.", call. = FALSE)
  }

  row.names(area_matrix) <- taxa
  list(taxa = taxa, matrix = area_matrix)
}

write_biogeobears_geography <- function(geography, path) {
  area_names <- colnames(geography$matrix)
  header <- paste0(
    nrow(geography$matrix),
    "\t",
    ncol(geography$matrix),
    " (",
    paste(area_names, collapse = " "),
    ")"
  )
  range_strings <- apply(geography$matrix, 1L, paste0, collapse = "")
  lines <- c(header, paste(row.names(geography$matrix), range_strings, sep = "\t"))
  writeLines(lines, path, useBytes = TRUE)
  invisible(path)
}

run_one_biogeobears_model <- function(
    config,
    prepared_inputs,
    model,
    raw_dir,
    no_j_seed = NULL,
    run_signature = NA_character_) {
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  result_file <- file.path(raw_dir, paste0(safe_model_name(model), "_result.rds"))
  run_object_file <- file.path(raw_dir, paste0(safe_model_name(model), "_run_object.rds"))
  log_file <- file.path(raw_dir, paste0(safe_model_name(model), ".log"))
  metadata_file <- model_run_metadata_file(raw_dir, model)

  archive_previous_model_log(log_file)
  log_connection <- file(log_file, open = "wt")
  sink(log_connection, type = "output")
  sink(log_connection, type = "message")
  on.exit({
    sink(type = "message")
    sink(type = "output")
    close(log_connection)
  }, add = TRUE)

  status <- "completed"
  error_message <- NA_character_
  warning_messages <- character()
  result <- NULL

  withCallingHandlers(
    tryCatch({
      run_object <- build_biogeobears_run_object(config, prepared_inputs, raw_dir)
      run_object <- configure_biogeobears_model(run_object, model, no_j_seed = no_j_seed)
      get_biogeobears_function("check_BioGeoBEARS_run")(run_object)
      saveRDS(run_object, run_object_file)
      result <- get_biogeobears_function("bears_optim_run")(run_object)
      saveRDS(result, result_file)
    }, error = function(e) {
      status <<- "failed"
      error_message <<- conditionMessage(e)
      message(error_message)
    }),
    warning = function(w) {
      warning_message <- conditionMessage(w)
      warning_messages <<- c(warning_messages, warning_message)
      message("Warning: ", warning_message)
      invokeRestart("muffleWarning")
    }
  )

  summary <- tryCatch(
    summarize_biogeobears_result(
      model = model,
      status = status,
      result = result,
      raw_dir = raw_dir,
      result_file = result_file,
      log_file = log_file,
      error_message = error_message,
      warning_messages = warning_messages,
      run_action = "executed",
      run_signature = run_signature
    ),
    error = function(e) {
      status <<- "failed"
      summarize_biogeobears_result(
        model = model,
        status = status,
        result = NULL,
        raw_dir = raw_dir,
        result_file = result_file,
        log_file = log_file,
        error_message = conditionMessage(e),
        warning_messages = warning_messages,
        run_action = "executed",
        run_signature = run_signature
      )
    }
  )

  saveRDS(
    list(
      schema_version = 1L,
      model = model,
      signature = run_signature,
      status = status,
      summary = summary
    ),
    metadata_file
  )

  list(summary = summary, result = result, status = status)
}

build_biogeobears_run_object <- function(config, prepared_inputs, raw_dir) {
  run_object <- get_biogeobears_function("define_BioGeoBEARS_run")()
  run_object$trfn <- prepared_inputs$tree_file
  run_object$geogfn <- prepared_inputs$geography_file
  run_object$max_range_size <- as.integer(config$inputs$max_range_size)
  run_object$min_branchlength <- 0.000001
  run_object$include_null_range <- TRUE
  run_object$on_NaN_error <- -1e50
  run_object$speedup <- TRUE
  run_object$use_optimx <- TRUE
  run_object$num_cores_to_use <- 1L
  run_object$force_sparse <- FALSE
  run_object$return_condlikes_table <- TRUE
  run_object$calc_TTL_loglike_from_condlikes_table <- TRUE
  run_object$calc_ancprobs <- TRUE
  run_object$wd <- raw_dir

  run_object <- apply_biogeobears_advanced_settings(run_object, config, raw_dir)
  get_biogeobears_function("readfiles_BioGeoBEARS_run")(run_object)
}

configure_biogeobears_model <- function(run_object, model, no_j_seed = NULL) {
  family <- model_family(model)
  has_j <- is_j_model(model)

  run_object <- configure_no_jump_model(run_object)
  if (!is.null(no_j_seed)) {
    run_object <- seed_params_from_result(run_object, no_j_seed, c("d", "e", "x"))
  }

  if (identical(family, "DIVALIKE")) {
    run_object <- configure_divalike_model(run_object)
  } else if (identical(family, "BAYAREALIKE")) {
    run_object <- configure_bayarealike_model(run_object)
  }

  if (isTRUE(has_j)) {
    run_object <- configure_jump_parameter(run_object, family)
  }

  run_object
}

configure_no_jump_model <- function(run_object) {
  run_object <- set_bgb_param(run_object, "j", "type", "fixed")
  run_object <- set_bgb_param(run_object, "j", "init", 0)
  set_bgb_param(run_object, "j", "est", 0)
}

configure_divalike_model <- function(run_object) {
  run_object <- set_bgb_param(run_object, "s", "type", "fixed")
  run_object <- set_bgb_param(run_object, "s", "init", 0)
  run_object <- set_bgb_param(run_object, "s", "est", 0)
  run_object <- set_bgb_param(run_object, "ysv", "type", "2-j")
  run_object <- set_bgb_param(run_object, "ys", "type", "ysv*1/2")
  run_object <- set_bgb_param(run_object, "y", "type", "ysv*1/2")
  run_object <- set_bgb_param(run_object, "v", "type", "ysv*1/2")
  run_object <- set_bgb_param(run_object, "mx01v", "type", "fixed")
  run_object <- set_bgb_param(run_object, "mx01v", "init", 0.5)
  set_bgb_param(run_object, "mx01v", "est", 0.5)
}

configure_bayarealike_model <- function(run_object) {
  run_object <- set_bgb_param(run_object, "s", "type", "fixed")
  run_object <- set_bgb_param(run_object, "s", "init", 0)
  run_object <- set_bgb_param(run_object, "s", "est", 0)
  run_object <- set_bgb_param(run_object, "v", "type", "fixed")
  run_object <- set_bgb_param(run_object, "v", "init", 0)
  run_object <- set_bgb_param(run_object, "v", "est", 0)
  run_object <- set_bgb_param(run_object, "ysv", "type", "1-j")
  run_object <- set_bgb_param(run_object, "ys", "type", "ysv*1/1")
  run_object <- set_bgb_param(run_object, "y", "type", "1-j")
  run_object <- set_bgb_param(run_object, "mx01y", "type", "fixed")
  run_object <- set_bgb_param(run_object, "mx01y", "init", 0.9999)
  set_bgb_param(run_object, "mx01y", "est", 0.9999)
}

configure_jump_parameter <- function(run_object, family) {
  run_object <- set_bgb_param(run_object, "j", "type", "free")
  run_object <- set_bgb_param(run_object, "j", "init", 0.0001)
  run_object <- set_bgb_param(run_object, "j", "est", 0.0001)
  if (identical(family, "DIVALIKE")) {
    run_object <- set_bgb_param(run_object, "j", "min", 0.00001)
    run_object <- set_bgb_param(run_object, "j", "max", 1.99999)
  } else if (identical(family, "BAYAREALIKE")) {
    run_object <- set_bgb_param(run_object, "j", "min", 0.00001)
    run_object <- set_bgb_param(run_object, "j", "max", 0.99999)
    run_object <- set_bgb_param(run_object, "d", "min", 0.0000001)
    run_object <- set_bgb_param(run_object, "d", "max", 4.9999999)
    run_object <- set_bgb_param(run_object, "e", "min", 0.0000001)
    run_object <- set_bgb_param(run_object, "e", "max", 4.9999999)
  }
  run_object
}

apply_biogeobears_advanced_settings <- function(run_object, config, raw_dir) {
  advanced <- config$advanced %||% list()
  base_dir <- dirname(config$.config_file %||% ".")

  run_object <- apply_named_run_overrides(run_object, advanced$BioGeoBEARS_run_object)
  run_object <- apply_named_run_overrides(run_object, advanced$optimizer_settings)
  run_object <- apply_constraint_files(run_object, advanced$constraints, base_dir)

  if (is_config_path_set(run_object$distsfn)) {
    run_object <- set_bgb_param(run_object, "x", "type", "free")
    run_object <- set_bgb_param(run_object, "x", "init", 0)
    run_object <- set_bgb_param(run_object, "x", "est", 0)
  }

  run_object$wd <- raw_dir
  run_object
}

apply_named_run_overrides <- function(run_object, overrides) {
  if (is.null(overrides) || !is.list(overrides)) {
    return(run_object)
  }
  for (name in names(overrides)) {
    if (!identical(overrides[[name]], NULL)) {
      run_object[[name]] <- overrides[[name]]
    }
  }
  run_object
}

apply_constraint_files <- function(run_object, constraints, base_dir) {
  if (is.null(constraints) || !is.list(constraints)) {
    return(run_object)
  }

  mapping <- c(
    times_file = "timesfn",
    dists_file = "distsfn",
    distance_file = "distsfn",
    dispersal_multipliers_file = "dispersal_multipliers_fn",
    areas_allowed_file = "areas_allowed_fn",
    areas_adjacency_file = "areas_adjacency_fn",
    area_of_areas_file = "area_of_areas_fn"
  )

  for (input_name in names(mapping)) {
    if (!is.null(constraints[[input_name]])) {
      run_object[[mapping[[input_name]]]] <- resolve_config_path(constraints[[input_name]], base_dir)
    }
  }
  run_object
}

seed_params_from_result <- function(run_object, result, params) {
  params_table <- tryCatch(result$outputs@params_table, error = function(e) NULL)
  if (is.null(params_table)) {
    return(run_object)
  }

  for (param in params) {
    if (param %in% row.names(params_table) && "est" %in% colnames(params_table)) {
      estimate <- params_table[param, "est"]
      if (is.numeric(estimate) && is.finite(estimate)) {
        run_object <- set_bgb_param(run_object, param, "init", estimate)
        run_object <- set_bgb_param(run_object, param, "est", estimate)
      }
    }
  }
  run_object
}

set_bgb_param <- function(run_object, param, column, value) {
  params_table <- run_object$BioGeoBEARS_model_object@params_table
  if (param %in% row.names(params_table) && column %in% colnames(params_table)) {
    run_object$BioGeoBEARS_model_object@params_table[param, column] <- value
  }
  run_object
}

summarize_biogeobears_result <- function(
    model,
    status,
    result,
    raw_dir,
    result_file,
    log_file,
    error_message = NA_character_,
    warning_messages = character(),
    run_action = "executed",
    run_signature = NA_character_) {
  warnings <- summarize_run_warnings(warning_messages)

  if (!identical(status, "completed") || is.null(result)) {
    return(data.frame(
      model = model,
      status = status,
      run_action = run_action,
      run_signature = run_signature,
      logLik = NA_real_,
      num_params = NA_integer_,
      raw_output_dir = raw_dir,
      result_file = result_file,
      log_file = log_file,
      error_message = error_message,
      warning_count = warnings$count,
      warning_messages = warnings$messages,
      stringsAsFactors = FALSE
    ))
  }

  params <- extract_biogeobears_fit_table(result)
  data.frame(
    model = model,
    status = status,
    run_action = run_action,
    run_signature = run_signature,
    logLik = params$logLik,
    num_params = params$num_params,
    raw_output_dir = raw_dir,
    result_file = result_file,
    log_file = log_file,
    error_message = NA_character_,
    warning_count = warnings$count,
    warning_messages = warnings$messages,
    stringsAsFactors = FALSE
  )
}

model_run_signature <- function(config, prepared_inputs, model) {
  analysis <- config$analysis %||% list()
  payload <- list(
    schema_version = 1L,
    model = model,
    inputs = list(
      tree = file_md5(prepared_inputs$tree_file),
      geography = file_md5(prepared_inputs$geography_file),
      max_range_size = prepared_inputs$max_range_size
    ),
    analysis = analysis[c(
      "time_bins",
      "run_stochastic_mapping",
      "stochastic_mapping_model",
      "stochastic_mapping_models",
      "stochastic_mapping_replicates",
      "stochastic_mapping_max_maps_to_try",
      "stochastic_mapping_maxtries_per_branch",
      "stochastic_mapping_max_tries_per_branch",
      "stochastic_mapping_seed"
    )],
    advanced = config$advanced %||% list(),
    constraint_files = constraint_file_hashes(config),
    biogeobears_version = installed_package_version("BioGeoBEARS")
  )
  serialized_object_md5(payload)
}

constraint_file_hashes <- function(config) {
  constraints <- config$advanced$constraints %||% list()
  if (!is.list(constraints) || length(constraints) == 0L) {
    return(character())
  }
  base_dir <- dirname(config$.config_file %||% ".")
  values <- vapply(names(constraints), function(name) {
    value <- constraints[[name]]
    if (is.null(value) || length(value) != 1L || is.na(value) || !nzchar(value)) {
      return(NA_character_)
    }
    file_md5(resolve_config_path(value, base_dir))
  }, character(1))
  values
}

file_md5 <- function(path) {
  if (is.null(path) || length(path) != 1L || is.na(path) || !file.exists(path)) {
    return(NA_character_)
  }
  unname(tools::md5sum(path)[[1L]])
}

serialized_object_md5 <- function(object) {
  path <- tempfile("ibgb-signature-", fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(object, path, version = 2)
  file_md5(path)
}

model_run_metadata_file <- function(raw_dir, model) {
  file.path(raw_dir, paste0(safe_model_name(model), "_run_metadata.rds"))
}

load_reusable_model_result <- function(model, raw_dir, run_signature) {
  metadata_file <- model_run_metadata_file(raw_dir, model)
  result_file <- file.path(raw_dir, paste0(safe_model_name(model), "_result.rds"))
  log_file <- file.path(raw_dir, paste0(safe_model_name(model), ".log"))
  if (!file.exists(metadata_file) || !file.exists(result_file)) {
    return(NULL)
  }

  metadata <- tryCatch(readRDS(metadata_file), error = function(e) NULL)
  if (is.null(metadata) ||
      !identical(metadata$signature, run_signature) ||
      !identical(metadata$status, "completed")) {
    return(NULL)
  }
  result <- tryCatch(readRDS(result_file), error = function(e) NULL)
  if (is.null(result)) {
    return(NULL)
  }

  summary <- as.data.frame(metadata$summary, stringsAsFactors = FALSE)
  summary$model <- model
  summary$status <- "completed"
  summary$run_action <- "reused"
  summary$run_signature <- run_signature
  summary$raw_output_dir <- raw_dir
  summary$result_file <- result_file
  summary$log_file <- log_file
  summary$error_message <- NA_character_
  list(summary = summary, result = result, status = "completed")
}

skipped_model_result <- function(model, raw_dir, run_signature, reason) {
  result_file <- file.path(raw_dir, paste0(safe_model_name(model), "_result.rds"))
  log_file <- file.path(raw_dir, paste0(safe_model_name(model), ".log"))
  summary <- summarize_biogeobears_result(
    model = model,
    status = "skipped",
    result = NULL,
    raw_dir = raw_dir,
    result_file = result_file,
    log_file = log_file,
    error_message = reason,
    run_action = "not_failed",
    run_signature = run_signature
  )
  list(summary = summary, result = NULL, status = "skipped")
}

read_previous_model_status <- function(project_paths) {
  path <- file.path(project_paths$tables, "model_run_status.csv")
  if (!file.exists(path)) {
    return(data.frame())
  }
  tryCatch(
    utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE),
    error = function(e) data.frame()
  )
}

model_was_previously_failed <- function(model, previous_status) {
  if (is.null(previous_status) || nrow(previous_status) == 0L ||
      !all(c("model", "status") %in% names(previous_status))) {
    return(FALSE)
  }
  rows <- previous_status[previous_status$model == model, , drop = FALSE]
  nrow(rows) > 0L && identical(tolower(rows$status[[nrow(rows)]]), "failed")
}

archive_previous_model_log <- function(log_file) {
  if (!file.exists(log_file)) {
    return(NULL)
  }
  index <- 1L
  repeat {
    archived <- sub("[.]log$", paste0(".retry-", index, ".log"), log_file)
    if (!file.exists(archived)) {
      break
    }
    index <- index + 1L
  }
  if (!file.copy(log_file, archived, overwrite = FALSE)) {
    return(NULL)
  }
  archived
}

summarize_run_warnings <- function(warning_messages) {
  warning_messages <- gsub("\\s+", " ", trimws(as.character(warning_messages)))
  warning_messages <- unique(stats::na.omit(warning_messages))
  warning_messages <- warning_messages[nzchar(warning_messages)]
  list(
    count = length(warning_messages),
    messages = if (length(warning_messages) > 0L) paste(warning_messages, collapse = " | ") else NA_character_
  )
}

extract_biogeobears_fit_table <- function(result) {
  params <- get_biogeobears_function("extract_params_from_BioGeoBEARS_results_object")(
    results_object = result,
    returnwhat = "table",
    addl_params = c("j")
  )

  log_likelihood <- if ("LnL" %in% names(params)) params$LnL[[1L]] else NA_real_
  num_params <- if ("numparams" %in% names(params)) params$numparams[[1L]] else NA_integer_
  if (is.na(log_likelihood)) {
    log_likelihood <- get_biogeobears_function("get_LnL_from_BioGeoBEARS_results_object")(result)
  }

  list(logLik = as.numeric(log_likelihood), num_params = as.integer(num_params))
}

get_biogeobears_function <- function(name) {
  if (exists(name, envir = asNamespace("BioGeoBEARS"), inherits = FALSE)) {
    return(get(name, envir = asNamespace("BioGeoBEARS"), inherits = FALSE))
  }
  getExportedValue("BioGeoBEARS", name)
}

prepare_biogeobears_runtime <- function() {
  if (!requireNamespace("ape", quietly = TRUE)) {
    stop("The ape package is required by BioGeoBEARS runtime functions.", call. = FALSE)
  }
  if (!"package:ape" %in% search()) {
    suppressPackageStartupMessages(base::attachNamespace("ape"))
  }
  invisible(TRUE)
}

safe_model_name <- function(model) {
  gsub("[^A-Za-z0-9_-]+", "_", model)
}

is_config_path_set <- function(path) {
  !is.null(path) && length(path) == 1L && !is.na(path) && nzchar(path)
}
