run_bsm_stochastic_mapping <- function(config, project_paths, model_results, comparison, prepared_inputs) {
  if (!isTRUE(config$analysis$run_stochastic_mapping %||% FALSE)) {
    return(empty_bsm_standardized_tables())
  }

  selected_models <- select_bsm_models(config, comparison, model_results)
  if (length(selected_models) == 0L) {
    status <- empty_bsm_run_status_table()
    write_bsm_standardized_tables(empty_bsm_standardized_tables(status), project_paths)
    return(empty_bsm_standardized_tables(status))
  }

  options <- bsm_run_options(config)
  outputs <- lapply(seq_along(selected_models), function(i) {
    run_one_bsm_model(
      model = selected_models[[i]],
      model_result = model_results[[selected_models[[i]]]],
      project_paths = project_paths,
      prepared_inputs = prepared_inputs,
      options = options,
      model_index = i
    )
  })

  tables <- combine_bsm_outputs(outputs)
  write_bsm_standardized_tables(tables, project_paths)
  tables
}

select_bsm_models <- function(config, comparison, model_results) {
  completed_models <- names(model_results)[
    vapply(model_results, function(x) identical(x$status, "completed") && !is.null(x$result), logical(1))
  ]
  if (length(completed_models) == 0L || is.null(comparison) || nrow(comparison) == 0L) {
    return(character())
  }

  selection <- config$analysis$stochastic_mapping_models %||%
    config$analysis$stochastic_mapping_model %||%
    "best"
  selection <- unique(as.character(selection))
  selection <- selection[!is.na(selection) & nzchar(selection)]
  if (length(selection) == 0L) {
    selection <- "best"
  }

  ordered_comparison <- comparison
  if ("AICc" %in% names(ordered_comparison)) {
    ordered_comparison <- ordered_comparison[order(ordered_comparison$AICc), , drop = FALSE]
  } else if ("delta_aicc" %in% names(ordered_comparison)) {
    ordered_comparison <- ordered_comparison[order(ordered_comparison$delta_aicc), , drop = FALSE]
  }
  ordered_models <- ordered_comparison$model[ordered_comparison$model %in% completed_models]
  if (length(ordered_models) == 0L) {
    return(character())
  }

  selected <- character()
  for (item in selection) {
    key <- tolower(item)
    if (key %in% c("best", "best_statistical", "best_model")) {
      selected <- c(selected, ordered_models[[1L]])
    } else if (key %in% c("all", "completed", "all_completed")) {
      selected <- c(selected, ordered_models)
    } else if (key %in% c("best_non_j", "best_no_j", "best_non_plus_j")) {
      rows <- ordered_comparison[ordered_comparison$model %in% completed_models & !is_j_model(ordered_comparison$model), , drop = FALSE]
      if (nrow(rows) > 0L) {
        selected <- c(selected, rows$model[[1L]])
      }
    } else if (key %in% c("best_plus_j", "best_j")) {
      rows <- ordered_comparison[ordered_comparison$model %in% completed_models & is_j_model(ordered_comparison$model), , drop = FALSE]
      if (nrow(rows) > 0L) {
        selected <- c(selected, rows$model[[1L]])
      }
    } else if (item %in% completed_models) {
      selected <- c(selected, item)
    }
  }

  unique(selected[selected %in% completed_models])
}

bsm_run_options <- function(config) {
  analysis <- config$analysis %||% list()
  replicates <- as.integer(analysis$stochastic_mapping_replicates %||% 100L)
  if (is.na(replicates) || replicates < 1L) {
    replicates <- 1L
  }

  max_maps <- as.integer(analysis$stochastic_mapping_max_maps_to_try %||% max(replicates, ceiling(replicates * 2)))
  if (is.na(max_maps) || max_maps < replicates) {
    max_maps <- replicates
  }

  maxtries <- as.integer(analysis$stochastic_mapping_maxtries_per_branch %||%
    analysis$stochastic_mapping_max_tries_per_branch %||%
    40000L)
  if (is.na(maxtries) || maxtries < 1L) {
    maxtries <- 40000L
  }

  seed <- as.integer(analysis$stochastic_mapping_seed %||% 1L)
  if (is.na(seed)) {
    seed <- 1L
  }

  list(
    replicates = replicates,
    max_maps_to_try = max_maps,
    maxtries_per_branch = maxtries,
    seed = seed,
    save_after_every_try = isTRUE(analysis$stochastic_mapping_save_after_every_try %||% FALSE)
  )
}

run_one_bsm_model <- function(model, model_result, project_paths, prepared_inputs, options, model_index = 1L) {
  raw_dir <- file.path(project_paths$raw_biogeobears, model)
  bsm_dir <- file.path(raw_dir, "bsm")
  dir.create(bsm_dir, recursive = TRUE, showWarnings = FALSE)

  result_file <- file.path(bsm_dir, paste0(safe_model_name(model), "_bsm_result.rds"))
  counts_file <- file.path(bsm_dir, paste0(safe_model_name(model), "_bsm_counts.rds"))
  log_file <- file.path(bsm_dir, paste0(safe_model_name(model), "_bsm.log"))

  output_sink_count <- sink.number(type = "output")
  message_sink_count <- sink.number(type = "message")
  log_connection <- file(log_file, open = "wt")
  sink(log_connection, type = "output")
  sink(log_connection, type = "message")
  on.exit({
    while (sink.number(type = "message") > message_sink_count) {
      sink(type = "message")
    }
    while (sink.number(type = "output") > output_sink_count) {
      sink(type = "output")
    }
    close(log_connection)
  }, add = TRUE)

  status <- "completed"
  error_message <- NA_character_
  warning_messages <- character()
  raw_result <- NULL
  standardized <- empty_bsm_standardized_tables()

  withCallingHandlers(
    tryCatch({
      validate_bsm_area_codes(prepared_inputs$areas)
      result <- model_result$result
      result$inputs$num_cores_to_use <- 1L
      result$cluster_already_open <- FALSE
      seed <- options$seed + as.integer(model_index) * 10000L

      stochastic_inputs <- get_biogeobears_function("get_inputs_for_stochastic_mapping")(
        res = result,
        cluster_already_open = FALSE,
        printlevel = 0
      )
      bsm_output <- get_biogeobears_function("runBSM")(
        res = result,
        stochastic_mapping_inputs_list = stochastic_inputs,
        maxnum_maps_to_try = options$max_maps_to_try,
        nummaps_goal = options$replicates,
        maxtries_per_branch = options$maxtries_per_branch,
        save_after_every_try = options$save_after_every_try,
        savedir = bsm_dir,
        seedval = seed,
        wait_before_save = 0,
        master_nodenum_toPrint = 0
      )
      source_output <- get_biogeobears_function("simulate_source_areas_ana_clado")(
        res = result,
        clado_events_tables = bsm_output$RES_clado_events_tables,
        ana_events_tables = bsm_output$RES_ana_events_tables,
        areanames = prepared_inputs$areas
      )
      area_labels <- bsm_area_labels(prepared_inputs)
      counts <- get_biogeobears_function("count_ana_clado_events")(
        clado_events_tables = source_output$clado_events_tables,
        ana_events_tables = source_output$ana_events_tables,
        areanames = prepared_inputs$areas,
        actual_names = unname(area_labels[prepared_inputs$areas])
      )

      raw_result <- list(
        schema_version = 1L,
        model = model,
        options = options,
        stochastic_mapping_inputs = stochastic_inputs,
        bsm_output = bsm_output,
        source_area_output = source_output,
        counts = counts
      )
      saveRDS(raw_result, result_file)
      saveRDS(counts, counts_file)
      standardized <- standardize_bsm_result(
        model = model,
        raw_result = raw_result,
        prepared_inputs = prepared_inputs
      )
      completed_maps <- bsm_completed_map_count(raw_result)
      if (completed_maps < options$replicates) {
        status <- "partial"
        warning_messages <- c(
          warning_messages,
          paste0("Only ", completed_maps, " of ", options$replicates, " requested stochastic maps completed.")
        )
      }
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

  completed_maps <- bsm_completed_map_count(raw_result)
  summary <- bsm_run_status_row(
    model = model,
    status = status,
    options = options,
    completed_maps = completed_maps,
    bsm_dir = bsm_dir,
    result_file = result_file,
    counts_file = counts_file,
    log_file = log_file,
    error_message = error_message,
    warning_messages = warning_messages
  )
  standardized$bsm_run_status <- summary
  list(summary = summary, standardized = standardized, raw_result = raw_result)
}

validate_bsm_area_codes <- function(areas) {
  bad <- areas[nchar(areas) != 1L]
  if (length(bad) > 0L) {
    stop(
      "BioGeoBEARS stochastic mapping requires one-character area codes. ",
      "Use short area codes such as A, B, C in geography.csv and put full names in regions.csv. ",
      "Invalid area code(s): ",
      paste(bad, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

bsm_area_labels <- function(prepared_inputs) {
  areas <- prepared_inputs$areas
  labels <- areas
  names(labels) <- areas
  regions <- prepared_inputs$region_metadata %||% data.frame()
  if (nrow(regions) > 0L && all(c("region", "label") %in% names(regions))) {
    matched <- match(areas, regions$region)
    use <- !is.na(matched) & !is.na(regions$label[matched]) & nzchar(regions$label[matched])
    labels[use] <- regions$label[matched[use]]
  }
  labels
}

standardize_bsm_result <- function(model, raw_result, prepared_inputs) {
  counts <- raw_result$counts
  source_output <- raw_result$source_area_output
  area_labels <- bsm_area_labels(prepared_inputs)
  replicate_count <- bsm_completed_map_count(raw_result)

  events <- standardize_bsm_event_tables(
    model = model,
    clado_events_tables = source_output$clado_events_tables,
    ana_events_tables = source_output$ana_events_tables,
    area_labels = area_labels
  )

  list(
    bsm_run_status = empty_bsm_run_status_table(),
    bsm_event_summary = standardize_bsm_count_summary(model, counts, replicate_count),
    bsm_replicate_counts = standardize_bsm_replicate_counts(model, counts),
    bsm_dispersal_routes = standardize_bsm_dispersal_routes(model, counts),
    bsm_events = events,
    bsm_event_times = standardize_bsm_event_times(events)
  )
}

standardize_bsm_count_summary <- function(model, counts, replicate_count) {
  empty <- empty_bsm_event_summary_table()
  summary <- counts$summary_counts_BSMs
  if (is.null(summary) || ncol(summary) == 0L) {
    return(empty)
  }
  summary <- as.data.frame(summary, stringsAsFactors = FALSE)
  event_types <- names(summary)

  row_value <- function(row_name) {
    if (!row_name %in% row.names(summary)) {
      return(rep(NA_real_, length(event_types)))
    }
    suppressWarnings(as.numeric(summary[row_name, event_types]))
  }

  out <- data.frame(
    model = model,
    event_type = event_types,
    event_label = bsm_event_labels(event_types),
    mean_count = row_value("means"),
    sd_count = row_value("stdevs"),
    sum_count = row_value("sums"),
    replicate_count = as.integer(replicate_count),
    interpretation_note = "Formal BioGeoBEARS biogeographical stochastic mapping event-count summary.",
    stringsAsFactors = FALSE
  )
  out[order(-out$mean_count, out$event_type), , drop = FALSE]
}

standardize_bsm_replicate_counts <- function(model, counts) {
  mapping <- c(
    founder = "founder_totals_list",
    a = "a_totals_list",
    d = "d_totals_list",
    e = "e_totals_list",
    subset = "subsetSymp_totals_list",
    vicariance = "vicariance_totals_list",
    sympatry = "sympatry_totals_list",
    ALL_disp = "all_dispersals_totals_list",
    ana_disp = "anagenetic_dispersals_totals_list",
    all_ana = "ana_totals_list",
    all_clado = "clado_totals_list",
    total_events = "all_totals_list"
  )
  lengths <- vapply(mapping, function(name) length(counts[[name]] %||% numeric()), integer(1))
  replicate_count <- max(lengths, 0L)
  if (replicate_count == 0L) {
    return(empty_bsm_replicate_counts_table())
  }

  out <- data.frame(model = model, replicate = seq_len(replicate_count), stringsAsFactors = FALSE)
  for (event_type in names(mapping)) {
    values <- suppressWarnings(as.numeric(counts[[mapping[[event_type]]]] %||% rep(NA_real_, replicate_count)))
    length(values) <- replicate_count
    out[[event_type]] <- values
  }
  out
}

standardize_bsm_dispersal_routes <- function(model, counts) {
  route_specs <- data.frame(
    route_type = c(
      "all_dispersal",
      "anagenetic_dispersal",
      "founder_event",
      "range_switching",
      "range_expansion"
    ),
    means = c(
      "all_dispersals_counts_fromto_means",
      "ana_dispersals_counts_fromto_means",
      "founder_counts_fromto_means",
      "a_counts_fromto_means",
      "d_counts_fromto_means"
    ),
    sds = c(
      "all_dispersals_counts_fromto_sds",
      "ana_dispersals_counts_fromto_sds",
      "founder_counts_fromto_sds",
      "a_counts_fromto_sds",
      "d_counts_fromto_sds"
    ),
    stringsAsFactors = FALSE
  )

  routes <- lapply(seq_len(nrow(route_specs)), function(i) {
    bsm_route_matrix_to_long(
      model = model,
      route_type = route_specs$route_type[[i]],
      means = counts[[route_specs$means[[i]]]],
      sds = counts[[route_specs$sds[[i]]]]
    )
  })
  out <- do.call(rbind, routes)
  if (is.null(out) || nrow(out) == 0L) {
    return(empty_bsm_dispersal_routes_table())
  }
  out[order(out$model, out$route_type, -out$mean_count, out$source_region, out$target_region), , drop = FALSE]
}

bsm_route_matrix_to_long <- function(model, route_type, means, sds = NULL) {
  if (is.null(means) || length(means) == 0L) {
    return(empty_bsm_dispersal_routes_table())
  }
  means <- as.matrix(means)
  if (nrow(means) == 0L || ncol(means) == 0L) {
    return(empty_bsm_dispersal_routes_table())
  }
  if (is.null(row.names(means))) {
    row.names(means) <- paste0("source_", seq_len(nrow(means)))
  }
  if (is.null(colnames(means))) {
    colnames(means) <- paste0("target_", seq_len(ncol(means)))
  }
  if (!is.null(sds) && length(sds) > 0L) {
    sds <- as.matrix(sds)
  } else {
    sds <- matrix(NA_real_, nrow = nrow(means), ncol = ncol(means))
  }

  grid <- expand.grid(
    source_region = row.names(means),
    target_region = colnames(means),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  out <- data.frame(
    model = model,
    route_type = route_type,
    source_region = grid$source_region,
    target_region = grid$target_region,
    direction_label = paste(grid$source_region, grid$target_region, sep = " -> "),
    mean_count = as.numeric(means),
    sd_count = as.numeric(sds),
    frequency = as.numeric(means),
    interpretation_note = "Mean BioGeoBEARS stochastic mapping route count across completed maps.",
    stringsAsFactors = FALSE
  )
  out
}

standardize_bsm_event_tables <- function(model, clado_events_tables, ana_events_tables, area_labels) {
  clado <- do.call(rbind, lapply(seq_along(clado_events_tables), function(i) {
    standardize_bsm_clado_events(model, i, clado_events_tables[[i]], area_labels)
  }))
  ana <- do.call(rbind, lapply(seq_along(ana_events_tables), function(i) {
    standardize_bsm_ana_events(model, i, ana_events_tables[[i]], area_labels)
  }))
  out <- rbind_with_empty(empty_bsm_events_table(), clado, ana)
  if (nrow(out) == 0L) {
    return(empty_bsm_events_table())
  }
  out$event_index <- seq_len(nrow(out))
  out <- out[order(out$model, out$replicate, -out$event_time_before_present, out$event_index), , drop = FALSE]
  row.names(out) <- NULL
  out
}

standardize_bsm_clado_events <- function(model, replicate, table, area_labels) {
  if (is.null(table) || length(table) == 0L || all(is.na(table))) {
    return(empty_bsm_events_table())
  }
  table <- as.data.frame(table, stringsAsFactors = FALSE)
  if (!"clado_event_type" %in% names(table)) {
    return(empty_bsm_events_table())
  }
  event_type_raw <- trimws(as.character(table$clado_event_type))
  keep <- !is.na(event_type_raw) & nzchar(event_type_raw)
  table <- table[keep, , drop = FALSE]
  event_type_raw <- event_type_raw[keep]
  if (nrow(table) == 0L) {
    return(empty_bsm_events_table())
  }
  source_code <- bsm_blank_to_na(table$clado_dispersal_from %||% NA_character_)
  target_code <- bsm_blank_to_na(table$clado_dispersal_to %||% NA_character_)
  parsed <- parse_bsm_event_text(table$clado_event_txt %||% NA_character_)
  out <- empty_bsm_events_table()
  out <- data.frame(
    event_index = seq_len(nrow(table)),
    model = model,
    replicate = as.integer(replicate),
    event_class = "cladogenetic",
    event_type = normalize_bsm_clado_event_type(event_type_raw),
    event_label = bsm_event_labels(normalize_bsm_clado_event_type(event_type_raw)),
    raw_event_type = event_type_raw,
    raw_event_text = table$clado_event_txt %||% NA_character_,
    event_time_before_present = suppressWarnings(as.numeric(table$time_bp %||% NA_real_)),
    event_time_relative = NA_real_,
    node_index = suppressWarnings(as.integer(table$node %||% NA_integer_)),
    node_label = table$label %||% NA_character_,
    parent_state = parsed$parent_state,
    child_state = parsed$child_state,
    source_region_code = source_code,
    target_region_code = target_code,
    source_region = bsm_label_area(source_code, area_labels),
    target_region = bsm_label_area(target_code, area_labels),
    extirpation_region_code = NA_character_,
    extirpation_region = NA_character_,
    direction = bsm_direction(source_code, target_code),
    direction_label = bsm_direction(
      bsm_label_area(source_code, area_labels),
      bsm_label_area(target_code, area_labels)
    ),
    interpretation_note = "Cladogenetic event sampled by BioGeoBEARS stochastic mapping.",
    stringsAsFactors = FALSE
  )
  out
}

standardize_bsm_ana_events <- function(model, replicate, table, area_labels) {
  if (is.null(table) || length(table) == 0L || all(is.na(table))) {
    return(empty_bsm_events_table())
  }
  table <- as.data.frame(table, stringsAsFactors = FALSE)
  if (!"event_type" %in% names(table)) {
    return(empty_bsm_events_table())
  }
  event_type_raw <- trimws(as.character(table$event_type))
  keep <- !is.na(event_type_raw) & nzchar(event_type_raw)
  table <- table[keep, , drop = FALSE]
  event_type_raw <- event_type_raw[keep]
  if (nrow(table) == 0L) {
    return(empty_bsm_events_table())
  }
  source_code <- bsm_blank_to_na(table$ana_dispersal_from %||% NA_character_)
  target_code <- bsm_blank_to_na(table$dispersal_to %||% NA_character_)
  ext_code <- bsm_blank_to_na(table$extirpation_from %||% NA_character_)
  parsed <- parse_bsm_event_text(table$event_txt %||% NA_character_)
  event_type <- normalize_bsm_ana_event_type(event_type_raw)
  out <- data.frame(
    event_index = seq_len(nrow(table)),
    model = model,
    replicate = as.integer(replicate),
    event_class = "anagenetic",
    event_type = event_type,
    event_label = bsm_event_labels(event_type),
    raw_event_type = event_type_raw,
    raw_event_text = table$event_txt %||% NA_character_,
    event_time_before_present = suppressWarnings(as.numeric(table$abs_event_time %||% NA_real_)),
    event_time_relative = suppressWarnings(as.numeric(table$event_time %||% NA_real_)),
    node_index = suppressWarnings(as.integer(table$node %||% table$nodenum_at_top_of_branch %||% NA_integer_)),
    node_label = table$label %||% NA_character_,
    parent_state = parsed$parent_state,
    child_state = parsed$child_state,
    source_region_code = source_code,
    target_region_code = target_code,
    source_region = bsm_label_area(source_code, area_labels),
    target_region = bsm_label_area(target_code, area_labels),
    extirpation_region_code = ext_code,
    extirpation_region = bsm_label_area(ext_code, area_labels),
    direction = bsm_ana_direction(event_type, source_code, target_code, ext_code),
    direction_label = bsm_ana_direction(
      event_type,
      bsm_label_area(source_code, area_labels),
      bsm_label_area(target_code, area_labels),
      bsm_label_area(ext_code, area_labels)
    ),
    interpretation_note = "Anagenetic event sampled by BioGeoBEARS stochastic mapping.",
    stringsAsFactors = FALSE
  )
  out
}

standardize_bsm_event_times <- function(events) {
  if (is.null(events) || nrow(events) == 0L) {
    return(empty_bsm_event_times_table())
  }
  rows <- events[!is.na(events$event_time_before_present), , drop = FALSE]
  if (nrow(rows) == 0L) {
    return(empty_bsm_event_times_table())
  }
  cols <- c(
    "event_index", "model", "replicate", "event_class", "event_label",
    "event_time_before_present", "direction_label", "direction",
    "source_region", "target_region", "extirpation_region",
    "parent_state", "child_state", "node_index", "node_label"
  )
  rows <- rows[order(rows$model, rows$replicate, -rows$event_time_before_present), intersect(cols, names(rows)), drop = FALSE]
  row.names(rows) <- NULL
  rows
}

combine_bsm_outputs <- function(outputs) {
  if (length(outputs) == 0L) {
    return(empty_bsm_standardized_tables())
  }
  combine <- function(name, empty) {
    tables <- lapply(outputs, function(x) x$standardized[[name]] %||% empty)
    rbind_with_empty(empty, tables)
  }
  list(
    bsm_run_status = combine("bsm_run_status", empty_bsm_run_status_table()),
    bsm_event_summary = combine("bsm_event_summary", empty_bsm_event_summary_table()),
    bsm_replicate_counts = combine("bsm_replicate_counts", empty_bsm_replicate_counts_table()),
    bsm_dispersal_routes = combine("bsm_dispersal_routes", empty_bsm_dispersal_routes_table()),
    bsm_events = combine("bsm_events", empty_bsm_events_table()),
    bsm_event_times = combine("bsm_event_times", empty_bsm_event_times_table())
  )
}

write_bsm_standardized_tables <- function(tables, project_paths) {
  write_csv_base(tables$bsm_run_status %||% empty_bsm_run_status_table(), file.path(project_paths$tables, "bsm_run_status.csv"))
  write_csv_base(tables$bsm_event_summary %||% empty_bsm_event_summary_table(), file.path(project_paths$tables, "bsm_event_summary.csv"))
  write_csv_base(tables$bsm_replicate_counts %||% empty_bsm_replicate_counts_table(), file.path(project_paths$tables, "bsm_replicate_counts.csv"))
  write_csv_base(tables$bsm_dispersal_routes %||% empty_bsm_dispersal_routes_table(), file.path(project_paths$tables, "bsm_dispersal_routes.csv"))
  write_csv_base(tables$bsm_events %||% empty_bsm_events_table(), file.path(project_paths$tables, "bsm_events.csv"))
  write_csv_base(tables$bsm_event_times %||% empty_bsm_event_times_table(), file.path(project_paths$tables, "bsm_event_times.csv"))
  invisible(tables)
}

bsm_completed_map_count <- function(raw_result) {
  if (is.null(raw_result) || is.null(raw_result$bsm_output$RES_clado_events_tables)) {
    return(0L)
  }
  length(raw_result$bsm_output$RES_clado_events_tables)
}

bsm_run_status_row <- function(
    model,
    status,
    options,
    completed_maps,
    bsm_dir,
    result_file,
    counts_file,
    log_file,
    error_message = NA_character_,
    warning_messages = character()) {
  warnings <- summarize_run_warnings(warning_messages)
  data.frame(
    model = model,
    status = status,
    requested_maps = as.integer(options$replicates),
    completed_maps = as.integer(completed_maps),
    max_maps_to_try = as.integer(options$max_maps_to_try),
    maxtries_per_branch = as.integer(options$maxtries_per_branch),
    seed = as.integer(options$seed),
    bsm_output_dir = as_path(bsm_dir),
    result_file = as_path(result_file),
    counts_file = as_path(counts_file),
    log_file = as_path(log_file),
    error_message = error_message,
    warning_count = warnings$count,
    warning_messages = warnings$messages,
    stringsAsFactors = FALSE
  )
}

normalize_bsm_clado_event_type <- function(x) {
  out <- tolower(x)
  out[grepl("founder|jump|\\(j\\)", out)] <- "founder"
  out[grepl("subset|\\(s\\)", out)] <- "subset"
  out[grepl("vicariance|\\(v\\)", out)] <- "vicariance"
  out[grepl("sympatry|\\(y\\)", out)] <- "sympatry"
  out
}

normalize_bsm_ana_event_type <- function(x) {
  out <- tolower(x)
  out[out %in% c("d", "expansion", "range-expansion dispersal")] <- "d"
  out[out %in% c("e", "contraction", "extinction")] <- "e"
  out[out %in% c("a", "range-switching")] <- "a"
  out
}

bsm_event_labels <- function(event_type) {
  lookup <- c(
    founder = "Founder-event jump dispersal",
    a = "Range-switching dispersal",
    d = "Range-expansion dispersal",
    e = "Local extinction",
    subset = "Subset sympatry",
    vicariance = "Vicariance",
    sympatry = "Sympatry",
    ALL_disp = "All dispersal",
    ana_disp = "Anagenetic dispersal",
    all_ana = "All anagenetic events",
    all_clado = "All cladogenetic events",
    total_events = "Total events",
    all_dispersal = "All dispersal routes",
    anagenetic_dispersal = "Anagenetic dispersal routes",
    founder_event = "Founder-event routes",
    range_switching = "Range-switching dispersal",
    range_expansion = "Range-expansion dispersal"
  )
  out <- lookup[event_type]
  missing <- is.na(out)
  out[missing] <- event_type[missing]
  unname(out)
}

bsm_blank_to_na <- function(x) {
  x <- as.character(x)
  x[is.na(x) | trimws(x) %in% c("", "-", "NA", "none")] <- NA_character_
  x
}

bsm_label_area <- function(area_codes, area_labels) {
  out <- as.character(area_codes)
  matched <- match(out, names(area_labels))
  use <- !is.na(matched)
  out[use] <- unname(area_labels[matched[use]])
  out[is.na(area_codes)] <- NA_character_
  out
}

bsm_direction <- function(source, target) {
  source <- bsm_blank_to_na(source)
  target <- bsm_blank_to_na(target)
  out <- rep(NA_character_, max(length(source), length(target)))
  length(source) <- length(out)
  length(target) <- length(out)
  use <- !is.na(source) & !is.na(target)
  out[use] <- paste(source[use], target[use], sep = " -> ")
  target_only <- is.na(source) & !is.na(target)
  out[target_only] <- paste("to", target[target_only])
  out
}

bsm_ana_direction <- function(event_type, source, target, extirpation) {
  out <- bsm_direction(source, target)
  event_type <- as.character(event_type)
  length(event_type) <- length(out)
  extirpation <- bsm_blank_to_na(extirpation)
  length(extirpation) <- length(out)
  extinct <- event_type == "e" & !is.na(extirpation)
  out[extinct] <- paste("local extinction from", extirpation[extinct])
  out
}

parse_bsm_event_text <- function(x) {
  x <- as.character(x)
  parent <- rep(NA_character_, length(x))
  child <- rep(NA_character_, length(x))
  has_arrow <- !is.na(x) & grepl("->", x, fixed = TRUE)
  parent[has_arrow] <- sub("->.*$", "", x[has_arrow])
  child[has_arrow] <- sub("^.*?->", "", x[has_arrow])
  list(parent_state = parent, child_state = child)
}

rbind_with_empty <- function(empty, ...) {
  pieces <- list(...)
  if (length(pieces) == 1L && is.list(pieces[[1L]]) && !is.data.frame(pieces[[1L]])) {
    pieces <- pieces[[1L]]
  }
  pieces <- pieces[!vapply(pieces, is.null, logical(1))]
  pieces <- lapply(pieces, function(x) {
    x <- as.data.frame(x, stringsAsFactors = FALSE)
    if (nrow(x) == 0L) {
      return(empty[0L, , drop = FALSE])
    }
    missing <- setdiff(names(empty), names(x))
    for (name in missing) {
      x[[name]] <- empty[[name]][NA_integer_]
    }
    x[, names(empty), drop = FALSE]
  })
  pieces <- pieces[vapply(pieces, nrow, integer(1)) > 0L]
  if (length(pieces) == 0L) {
    return(empty[0L, , drop = FALSE])
  }
  out <- do.call(rbind, pieces)
  row.names(out) <- NULL
  out
}

empty_bsm_standardized_tables <- function(status = empty_bsm_run_status_table()) {
  list(
    bsm_run_status = status,
    bsm_event_summary = empty_bsm_event_summary_table(),
    bsm_replicate_counts = empty_bsm_replicate_counts_table(),
    bsm_dispersal_routes = empty_bsm_dispersal_routes_table(),
    bsm_events = empty_bsm_events_table(),
    bsm_event_times = empty_bsm_event_times_table()
  )
}

empty_bsm_run_status_table <- function() {
  data.frame(
    model = character(),
    status = character(),
    requested_maps = integer(),
    completed_maps = integer(),
    max_maps_to_try = integer(),
    maxtries_per_branch = integer(),
    seed = integer(),
    bsm_output_dir = character(),
    result_file = character(),
    counts_file = character(),
    log_file = character(),
    error_message = character(),
    warning_count = integer(),
    warning_messages = character(),
    stringsAsFactors = FALSE
  )
}

empty_bsm_event_summary_table <- function() {
  data.frame(
    model = character(),
    event_type = character(),
    event_label = character(),
    mean_count = numeric(),
    sd_count = numeric(),
    sum_count = numeric(),
    replicate_count = integer(),
    interpretation_note = character(),
    stringsAsFactors = FALSE
  )
}

empty_bsm_replicate_counts_table <- function() {
  data.frame(
    model = character(),
    replicate = integer(),
    founder = numeric(),
    a = numeric(),
    d = numeric(),
    e = numeric(),
    subset = numeric(),
    vicariance = numeric(),
    sympatry = numeric(),
    ALL_disp = numeric(),
    ana_disp = numeric(),
    all_ana = numeric(),
    all_clado = numeric(),
    total_events = numeric(),
    stringsAsFactors = FALSE
  )
}

empty_bsm_dispersal_routes_table <- function() {
  data.frame(
    model = character(),
    route_type = character(),
    source_region = character(),
    target_region = character(),
    direction_label = character(),
    mean_count = numeric(),
    sd_count = numeric(),
    frequency = numeric(),
    interpretation_note = character(),
    stringsAsFactors = FALSE
  )
}

empty_bsm_events_table <- function() {
  data.frame(
    event_index = integer(),
    model = character(),
    replicate = integer(),
    event_class = character(),
    event_type = character(),
    event_label = character(),
    raw_event_type = character(),
    raw_event_text = character(),
    event_time_before_present = numeric(),
    event_time_relative = numeric(),
    node_index = integer(),
    node_label = character(),
    parent_state = character(),
    child_state = character(),
    source_region_code = character(),
    target_region_code = character(),
    source_region = character(),
    target_region = character(),
    extirpation_region_code = character(),
    extirpation_region = character(),
    direction = character(),
    direction_label = character(),
    interpretation_note = character(),
    stringsAsFactors = FALSE
  )
}

empty_bsm_event_times_table <- function() {
  data.frame(
    event_index = integer(),
    model = character(),
    replicate = integer(),
    event_class = character(),
    event_label = character(),
    event_time_before_present = numeric(),
    direction_label = character(),
    direction = character(),
    source_region = character(),
    target_region = character(),
    extirpation_region = character(),
    parent_state = character(),
    child_state = character(),
    node_index = integer(),
    node_label = character(),
    stringsAsFactors = FALSE
  )
}
