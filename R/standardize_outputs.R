standardize_biogeobears_outputs <- function(model_results, prepared_inputs, project_paths) {
  completed <- model_results[
    vapply(model_results, function(x) identical(x$status, "completed") && !is.null(x$result), logical(1))
  ]

  empty_outputs <- list(
    geographic_states = data.frame(),
    tree_nodes = data.frame(),
    parameter_table = data.frame(),
    ancestral_state_probabilities = data.frame(),
    root_state_probabilities = data.frame(),
    node_state_summary = data.frame(),
    range_change_events = data.frame(),
    best_fit_events = data.frame(),
    event_summary = data.frame()
  )
  if (length(completed) == 0L) {
    return(empty_outputs)
  }

  parameter_table <- do.call(rbind, lapply(names(completed), function(model) {
    extract_parameter_table(completed[[model]]$result, model)
  }))
  row.names(parameter_table) <- NULL

  geographic_states <- make_state_table(
    areas = prepared_inputs$areas,
    max_range_size = prepared_inputs$max_range_size,
    include_null_range = TRUE
  )
  state_labels <- geographic_states$state
  node_lookup <- make_node_lookup(prepared_inputs$tree_file)

  ancestral_state_probabilities <- do.call(rbind, lapply(names(completed), function(model) {
    extract_ancestral_state_probabilities(
      result = completed[[model]]$result,
      model = model,
      state_labels = state_labels,
      node_lookup = node_lookup
    )
  }))
  row.names(ancestral_state_probabilities) <- NULL

  root_state_probabilities <- do.call(rbind, lapply(names(completed), function(model) {
    extract_root_state_probabilities(
      result = completed[[model]]$result,
      model = model,
      state_labels = state_labels
    )
  }))
  row.names(root_state_probabilities) <- NULL

  node_state_summary <- summarize_top_node_states(ancestral_state_probabilities)
  range_change_events <- summarize_range_change_events(
    node_state_summary = node_state_summary,
    tree_nodes = node_lookup,
    geographic_states = geographic_states,
    region_metadata = prepared_inputs$region_metadata %||% data.frame()
  )
  event_summary <- summarize_range_change_event_counts(range_change_events)

  write_csv_base(geographic_states, file.path(project_paths$tables, "geographic_states.csv"))
  write_csv_base(node_lookup, file.path(project_paths$tables, "tree_nodes.csv"))
  write_csv_base(parameter_table, file.path(project_paths$tables, "model_parameters.csv"))
  write_csv_base(ancestral_state_probabilities, file.path(project_paths$tables, "ancestral_state_probabilities.csv"))
  write_csv_base(root_state_probabilities, file.path(project_paths$tables, "root_state_probabilities.csv"))
  write_csv_base(node_state_summary, file.path(project_paths$tables, "node_state_summary.csv"))
  write_csv_base(range_change_events, file.path(project_paths$tables, "range_change_events.csv"))
  write_csv_base(event_summary, file.path(project_paths$tables, "event_summary.csv"))

  list(
    geographic_states = geographic_states,
    tree_nodes = node_lookup,
    parameter_table = parameter_table,
    ancestral_state_probabilities = ancestral_state_probabilities,
    root_state_probabilities = root_state_probabilities,
    node_state_summary = node_state_summary,
    range_change_events = range_change_events,
    best_fit_events = data.frame(),
    event_summary = event_summary
  )
}

extract_parameter_table <- function(result, model) {
  params <- as.data.frame(result$outputs@params_table, stringsAsFactors = FALSE)
  params$parameter <- row.names(params)
  params$model <- model
  params$is_free <- identical_or_na(params$type, "free")

  params <- params[, c(
    "model", "parameter", "type", "is_free", "init", "min", "max",
    "est", "note", "desc"
  )]
  row.names(params) <- NULL
  params
}

extract_ancestral_state_probabilities <- function(result, model, state_labels, node_lookup) {
  matrices <- list(
    branch_top_at_node = result$ML_marginal_prob_each_state_at_branch_top_AT_node,
    branch_bottom_below_node = result$ML_marginal_prob_each_state_at_branch_bottom_below_node
  )

  out <- do.call(rbind, lapply(names(matrices), function(location) {
    probability_matrix_to_long(
      matrix = matrices[[location]],
      model = model,
      location = location,
      state_labels = state_labels,
      node_lookup = node_lookup
    )
  }))
  row.names(out) <- NULL
  out
}

extract_root_state_probabilities <- function(result, model, state_labels) {
  probs <- result$relative_probs_of_each_state_at_bottom_of_root_branch
  if (is.null(probs) || length(probs) == 0L) {
    return(data.frame())
  }

  state_labels <- align_state_labels(state_labels, length(probs))
  out <- data.frame(
    model = model,
    location = "bottom_of_root_branch",
    state_index = seq_along(probs),
    state = state_labels,
    probability = as.numeric(probs),
    stringsAsFactors = FALSE
  )
  out[!is.na(out$probability), , drop = FALSE]
}

probability_matrix_to_long <- function(matrix, model, location, state_labels, node_lookup) {
  if (is.null(matrix) || length(matrix) == 0L) {
    return(data.frame())
  }

  probability_matrix <- as.matrix(matrix)
  state_labels <- align_state_labels(state_labels, ncol(probability_matrix))
  node_indices <- seq_len(nrow(probability_matrix))
  node_rows <- node_lookup[match(node_indices, node_lookup$node_index), , drop = FALSE]

  grid <- expand.grid(
    node_index = node_indices,
    state_index = seq_len(ncol(probability_matrix)),
    KEEP.OUT.ATTRS = FALSE
  )
  probs <- as.vector(probability_matrix)

  out <- data.frame(
    model = model,
    location = location,
    node_index = grid$node_index,
    node_type = node_rows$node_type[match(grid$node_index, node_rows$node_index)],
    node_label = node_rows$node_label[match(grid$node_index, node_rows$node_index)],
    state_index = grid$state_index,
    state = state_labels[grid$state_index],
    probability = as.numeric(probs),
    stringsAsFactors = FALSE
  )
  out[!is.na(out$probability), , drop = FALSE]
}

make_state_labels <- function(areas, max_range_size, include_null_range = TRUE) {
  make_state_table(areas, max_range_size, include_null_range)$state
}

make_state_table <- function(areas, max_range_size, include_null_range = TRUE) {
  max_range_size <- min(as.integer(max_range_size), length(areas))
  states <- data.frame(
    state = character(),
    areas = character(),
    area_count = integer(),
    is_null_range = logical(),
    stringsAsFactors = FALSE
  )

  if (isTRUE(include_null_range)) {
    states <- rbind(states, data.frame(
      state = "null",
      areas = NA_character_,
      area_count = 0L,
      is_null_range = TRUE,
      stringsAsFactors = FALSE
    ))
  }

  for (size in seq_len(max_range_size)) {
    combos <- utils::combn(areas, size, simplify = FALSE)
    states <- rbind(states, do.call(rbind, lapply(combos, function(combo) {
      data.frame(
        state = paste0(combo, collapse = ""),
        areas = paste(combo, collapse = ";"),
        area_count = length(combo),
        is_null_range = FALSE,
        stringsAsFactors = FALSE
      )
    })))
  }

  states$state_index <- seq_len(nrow(states))
  states <- states[, c("state_index", "state", "areas", "area_count", "is_null_range")]
  row.names(states) <- NULL
  states
}

align_state_labels <- function(state_labels, n_states) {
  if (length(state_labels) == n_states) {
    return(state_labels)
  }
  paste0("state_", seq_len(n_states))
}

make_node_lookup <- function(tree_file) {
  tree <- ape::read.tree(tree_file)
  n_tips <- length(tree$tip.label)
  n_nodes <- tree$Nnode
  node_index <- seq_len(n_tips + n_nodes)
  node_type <- ifelse(node_index <= n_tips, "tip", "internal")
  node_label <- paste0("node_", node_index)
  node_label[seq_len(n_tips)] <- tree$tip.label
  root_index <- n_tips + 1L

  parent_node_index <- rep(NA_integer_, length(node_index))
  edge_length <- rep(NA_real_, length(node_index))
  if (!is.null(tree$edge) && nrow(tree$edge) > 0L) {
    child <- tree$edge[, 2L]
    parent_node_index[child] <- tree$edge[, 1L]
    if (!is.null(tree$edge.length)) {
      edge_length[child] <- tree$edge.length
    }
  }
  distance_from_root <- compute_node_distances(parent_node_index, edge_length, root_index)
  tree_height <- infer_tree_height(distance_from_root, node_type)
  time_before_present <- ifelse(is.finite(distance_from_root), tree_height - distance_from_root, NA_real_)

  data.frame(
    node_index = node_index,
    node_type = node_type,
    node_label = node_label,
    parent_node_index = parent_node_index,
    edge_length = edge_length,
    distance_from_root = distance_from_root,
    time_before_present = time_before_present,
    tree_height = tree_height,
    is_root = node_index == root_index,
    stringsAsFactors = FALSE
  )
}

compute_node_distances <- function(parent_node_index, edge_length, root_index) {
  n <- length(parent_node_index)
  distances <- rep(NA_real_, n)
  distances[[root_index]] <- 0
  branch_lengths <- edge_length
  branch_lengths[is.na(branch_lengths) & !is.na(parent_node_index)] <- 1

  unresolved <- is.na(distances)
  guard <- 0L
  while (any(unresolved) && guard < n + 1L) {
    guard <- guard + 1L
    changed <- FALSE
    for (i in which(unresolved)) {
      parent <- parent_node_index[[i]]
      if (!is.na(parent) && !is.na(distances[[parent]])) {
        distances[[i]] <- distances[[parent]] + branch_lengths[[i]]
        changed <- TRUE
      }
    }
    if (!changed) {
      break
    }
    unresolved <- is.na(distances)
  }
  distances
}

infer_tree_height <- function(distance_from_root, node_type) {
  tip_distances <- distance_from_root[node_type == "tip"]
  height <- suppressWarnings(max(tip_distances, na.rm = TRUE))
  if (!is.finite(height)) {
    height <- suppressWarnings(max(distance_from_root, na.rm = TRUE))
  }
  if (is.finite(height)) height else 0
}

summarize_top_node_states <- function(ancestral_state_probabilities) {
  if (is.null(ancestral_state_probabilities) || nrow(ancestral_state_probabilities) == 0L) {
    return(data.frame())
  }

  split_keys <- paste(
    ancestral_state_probabilities$model,
    ancestral_state_probabilities$location,
    ancestral_state_probabilities$node_index,
    sep = "\r"
  )
  rows <- lapply(split(ancestral_state_probabilities, split_keys), function(x) {
    x <- x[order(-x$probability, x$state_index), , drop = FALSE]
    best <- x[1L, , drop = FALSE]
    data.frame(
      model = best$model,
      location = best$location,
      node_index = best$node_index,
      node_type = best$node_type,
      node_label = best$node_label,
      best_state_index = best$state_index,
      best_state = best$state,
      best_probability = best$probability,
      state_count = nrow(x),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out[order(out$model, out$location, out$node_index), , drop = FALSE]
}

compare_node_state_sensitivity <- function(node_state_summary, comparison) {
  empty <- empty_node_state_sensitivity_table()
  if (is.null(node_state_summary) || nrow(node_state_summary) == 0L ||
      is.null(comparison) || nrow(comparison) == 0L) {
    return(empty)
  }
  if (!"has_j" %in% names(comparison)) {
    comparison$has_j <- is_j_model(comparison$model)
  }
  if (!"AICc" %in% names(comparison)) {
    return(empty)
  }

  non_j <- comparison[!comparison$has_j, , drop = FALSE]
  plus_j <- comparison[comparison$has_j, , drop = FALSE]
  if (nrow(non_j) == 0L || nrow(plus_j) == 0L) {
    return(empty)
  }

  best_non_j <- non_j$model[which.min(non_j$AICc)]
  best_plus_j <- plus_j$model[which.min(plus_j$AICc)]
  if (identical(best_non_j, best_plus_j)) {
    return(empty)
  }

  non_j_rows <- node_state_summary[node_state_summary$model == best_non_j, , drop = FALSE]
  plus_j_rows <- node_state_summary[node_state_summary$model == best_plus_j, , drop = FALSE]
  if (nrow(non_j_rows) == 0L || nrow(plus_j_rows) == 0L) {
    return(empty)
  }

  key_cols <- c("location", "node_index")
  merged <- merge(
    non_j_rows,
    plus_j_rows,
    by = key_cols,
    suffixes = c("_non_j", "_plus_j"),
    all = FALSE,
    sort = FALSE
  )
  if (nrow(merged) == 0L) {
    return(empty)
  }

  out <- data.frame(
    location = merged$location,
    node_index = merged$node_index,
    node_type = merged$node_type_non_j,
    node_label = merged$node_label_non_j,
    non_j_model = best_non_j,
    non_j_state = merged$best_state_non_j,
    non_j_probability = merged$best_probability_non_j,
    plus_j_model = best_plus_j,
    plus_j_state = merged$best_state_plus_j,
    plus_j_probability = merged$best_probability_plus_j,
    state_differs = merged$best_state_non_j != merged$best_state_plus_j,
    probability_difference = merged$best_probability_plus_j - merged$best_probability_non_j,
    probability_difference_abs = abs(merged$best_probability_plus_j - merged$best_probability_non_j),
    stringsAsFactors = FALSE
  )
  out <- out[order(out$location, out$node_index), , drop = FALSE]
  row.names(out) <- NULL
  out
}

empty_node_state_sensitivity_table <- function() {
  data.frame(
    location = character(),
    node_index = integer(),
    node_type = character(),
    node_label = character(),
    non_j_model = character(),
    non_j_state = character(),
    non_j_probability = numeric(),
    plus_j_model = character(),
    plus_j_state = character(),
    plus_j_probability = numeric(),
    state_differs = logical(),
    probability_difference = numeric(),
    probability_difference_abs = numeric(),
    stringsAsFactors = FALSE
  )
}

summarize_range_change_events <- function(node_state_summary, tree_nodes, geographic_states = NULL, region_metadata = NULL) {
  empty <- empty_range_change_events_table()
  if (is.null(node_state_summary) || nrow(node_state_summary) == 0L ||
      is.null(tree_nodes) || nrow(tree_nodes) == 0L) {
    return(empty)
  }

  required_summary <- c("model", "location", "node_index", "best_state", "best_probability")
  required_nodes <- c("node_index", "node_type", "node_label", "parent_node_index", "edge_length")
  if (length(setdiff(required_summary, names(node_state_summary))) > 0L ||
      length(setdiff(required_nodes, names(tree_nodes))) > 0L) {
    return(empty)
  }

  node_cols <- intersect(
    c(
      "node_index", "node_type", "node_label", "parent_node_index", "edge_length",
      "distance_from_root", "time_before_present", "tree_height"
    ),
    names(tree_nodes)
  )
  rows <- merge(
    node_state_summary[, required_summary, drop = FALSE],
    tree_nodes[, node_cols, drop = FALSE],
    by = "node_index",
    all.x = TRUE,
    sort = FALSE
  )
  for (col in c("distance_from_root", "time_before_present", "tree_height")) {
    if (!col %in% names(rows)) {
      rows[[col]] <- NA_real_
    }
  }
  rows <- rows[!is.na(rows$parent_node_index), , drop = FALSE]
  if (nrow(rows) == 0L) {
    return(empty)
  }

  parent_rows <- node_state_summary[, required_summary, drop = FALSE]
  names(parent_rows) <- c(
    "model", "location", "parent_node_index",
    "parent_state", "parent_probability"
  )
  rows <- merge(
    rows,
    parent_rows,
    by = c("model", "location", "parent_node_index"),
    all.x = TRUE,
    sort = FALSE
  )
  parent_time_cols <- intersect(c("node_index", "distance_from_root", "time_before_present"), names(tree_nodes))
  if (all(c("node_index", "distance_from_root", "time_before_present") %in% parent_time_cols)) {
    parent_times <- tree_nodes[, parent_time_cols, drop = FALSE]
    names(parent_times) <- c("parent_node_index", "parent_distance_from_root", "parent_time_before_present")
    rows <- merge(rows, parent_times, by = "parent_node_index", all.x = TRUE, sort = FALSE)
  } else {
    rows$parent_distance_from_root <- NA_real_
    rows$parent_time_before_present <- NA_real_
  }
  rows <- rows[!is.na(rows$parent_state) & !is.na(rows$best_state), , drop = FALSE]
  if (nrow(rows) == 0L) {
    return(empty)
  }

  lookup <- state_area_lookup(geographic_states)
  label_lookup <- region_label_lookup(region_metadata)
  event_rows <- lapply(seq_len(nrow(rows)), function(i) {
    parent_areas <- state_areas(rows$parent_state[[i]], lookup)
    child_areas <- state_areas(rows$best_state[[i]], lookup)
    gained <- setdiff(child_areas, parent_areas)
    lost <- setdiff(parent_areas, child_areas)
    event <- classify_range_change_event(parent_areas, child_areas, gained, lost)
    direction <- describe_range_change_direction(
      parent_areas = parent_areas,
      child_areas = child_areas,
      gained = gained,
      lost = lost,
      event_type = event$type,
      label_lookup = label_lookup
    )
    time_summary <- branch_event_time_summary(
      parent_time_before_present = rows$parent_time_before_present[[i]],
      child_time_before_present = rows$time_before_present[[i]]
    )

    data.frame(
      model = rows$model[[i]],
      location = rows$location[[i]],
      parent_node_index = rows$parent_node_index[[i]],
      node_index = rows$node_index[[i]],
      node_type = rows$node_type[[i]],
      node_label = rows$node_label[[i]],
      edge_length = rows$edge_length[[i]],
      parent_distance_from_root = rows$parent_distance_from_root[[i]],
      child_distance_from_root = rows$distance_from_root[[i]],
      parent_time_before_present = rows$parent_time_before_present[[i]],
      child_time_before_present = rows$time_before_present[[i]],
      event_time_min = time_summary$event_time_min,
      event_time_max = time_summary$event_time_max,
      event_time_midpoint = time_summary$event_time_midpoint,
      time_unit = "tree branch length units before present",
      event_time_note = "Approximate branch midpoint time; deterministic best-state change, not stochastic mapping.",
      parent_state = rows$parent_state[[i]],
      child_state = rows$best_state[[i]],
      parent_probability = rows$parent_probability[[i]],
      child_probability = rows$best_probability[[i]],
      event_type = event$type,
      event_label = event$label,
      state_changed = !identical(rows$parent_state[[i]], rows$best_state[[i]]),
      gained_areas = paste(gained, collapse = ";"),
      lost_areas = paste(lost, collapse = ";"),
      source_region = direction$source_region,
      target_region = direction$target_region,
      direction = direction$direction,
      source_region_label = direction$source_region_label,
      target_region_label = direction$target_region_label,
      direction_label = direction$direction_label,
      gained_count = length(gained),
      lost_count = length(lost),
      interpretation_note = "Deterministic summary from highest-probability ancestral states; not stochastic mapping event counts.",
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, event_rows)
  row.names(out) <- NULL
  out[order(out$model, out$location, out$node_index), , drop = FALSE]
}

branch_event_time_summary <- function(parent_time_before_present, child_time_before_present) {
  values <- c(parent_time_before_present, child_time_before_present)
  values <- values[is.finite(values)]
  if (length(values) == 0L) {
    return(list(event_time_min = NA_real_, event_time_max = NA_real_, event_time_midpoint = NA_real_))
  }
  list(
    event_time_min = min(values),
    event_time_max = max(values),
    event_time_midpoint = mean(values)
  )
}

region_label_lookup <- function(region_metadata) {
  if (is.null(region_metadata) || nrow(region_metadata) == 0L || !"region" %in% names(region_metadata)) {
    return(character())
  }
  labels <- if ("label" %in% names(region_metadata)) region_metadata$label else region_metadata$region
  labels[is.na(labels) | !nzchar(labels)] <- region_metadata$region[is.na(labels) | !nzchar(labels)]
  stats::setNames(as.character(labels), as.character(region_metadata$region))
}

describe_range_change_direction <- function(parent_areas, child_areas, gained, lost, event_type, label_lookup = character()) {
  source <- parent_areas
  target <- child_areas
  if (identical(event_type, "range_expansion")) {
    source <- parent_areas
    target <- gained
  } else if (identical(event_type, "local_extinction")) {
    source <- lost
    target <- child_areas
  } else if (identical(event_type, "range_shift")) {
    source <- if (length(lost) > 0L) lost else parent_areas
    target <- if (length(gained) > 0L) gained else child_areas
  }

  source_region <- paste_area_values(source)
  target_region <- paste_area_values(target)
  source_region_label <- paste_area_values(label_area_values(source, label_lookup))
  target_region_label <- paste_area_values(label_area_values(target, label_lookup))
  list(
    source_region = source_region,
    target_region = target_region,
    direction = paste(source_region, target_region, sep = " -> "),
    source_region_label = source_region_label,
    target_region_label = target_region_label,
    direction_label = paste(source_region_label, target_region_label, sep = " -> ")
  )
}

paste_area_values <- function(areas) {
  areas <- as.character(areas %||% character())
  areas <- areas[nzchar(areas) & !is.na(areas)]
  if (length(areas) == 0L) {
    return("null")
  }
  paste(areas, collapse = ";")
}

label_area_values <- function(areas, label_lookup) {
  areas <- as.character(areas %||% character())
  if (length(areas) == 0L || length(label_lookup) == 0L) {
    return(areas)
  }
  labels <- label_lookup[areas]
  labels[is.na(labels) | !nzchar(labels)] <- areas[is.na(labels) | !nzchar(labels)]
  unname(labels)
}

summarize_best_fit_events <- function(range_change_events, comparison, location = "branch_top_at_node") {
  empty <- empty_best_fit_events_table()
  if (is.null(range_change_events) || nrow(range_change_events) == 0L ||
      is.null(comparison) || nrow(comparison) == 0L || !"model" %in% names(comparison)) {
    return(empty)
  }
  best_model <- best_fit_model_from_comparison(comparison)
  if (is.null(best_model)) {
    return(empty)
  }

  events <- range_change_events[range_change_events$model == best_model, , drop = FALSE]
  if (nrow(events) == 0L) {
    return(empty)
  }
  if ("location" %in% names(events) && location %in% events$location) {
    events <- events[events$location == location, , drop = FALSE]
  }
  if ("state_changed" %in% names(events)) {
    changed <- events[!is.na(events$state_changed) & events$state_changed, , drop = FALSE]
    if (nrow(changed) > 0L) {
      events <- changed
    }
  }
  if (nrow(events) == 0L) {
    return(empty)
  }

  if ("event_time_midpoint" %in% names(events)) {
    events <- events[order(-events$event_time_midpoint, events$node_index), , drop = FALSE]
  } else {
    events <- events[order(events$node_index), , drop = FALSE]
  }
  events$event_index <- seq_len(nrow(events))
  cols <- c(
    "event_index", "model", "location",
    "event_time_midpoint", "event_time_min", "event_time_max", "time_unit",
    "event_time_note", "direction", "direction_label", "source_region",
    "target_region", "source_region_label", "target_region_label",
    "event_type", "event_label", "parent_state", "child_state",
    "gained_areas", "lost_areas", "parent_node_index", "node_index",
    "node_label", "edge_length", "parent_probability", "child_probability",
    "interpretation_note"
  )
  events[, intersect(cols, names(events)), drop = FALSE]
}

best_fit_model_from_comparison <- function(comparison) {
  if (is.null(comparison) || nrow(comparison) == 0L || !"model" %in% names(comparison)) {
    return(NULL)
  }
  if ("delta_aicc" %in% names(comparison) && any(!is.na(comparison$delta_aicc))) {
    return(comparison$model[[which.min(comparison$delta_aicc)]])
  }
  if ("AICc" %in% names(comparison) && any(!is.na(comparison$AICc))) {
    return(comparison$model[[which.min(comparison$AICc)]])
  }
  comparison$model[[1L]]
}

summarize_range_change_event_counts <- function(range_change_events) {
  empty <- empty_event_summary_table()
  if (is.null(range_change_events) || nrow(range_change_events) == 0L) {
    return(empty)
  }
  required <- c("model", "location", "event_type", "event_label", "state_changed")
  if (length(setdiff(required, names(range_change_events))) > 0L) {
    return(empty)
  }

  range_change_events$edge_count <- 1L
  range_change_events$changed_edge <- as.integer(range_change_events$state_changed)
  summary <- stats::aggregate(
    cbind(edge_count, changed_edge) ~
      model + location + event_type + event_label,
    data = range_change_events,
    FUN = sum
  )
  names(summary)[names(summary) == "edge_count"] <- "event_count"
  names(summary)[names(summary) == "changed_edge"] <- "changed_edges"
  summary$interpretation_note <- "Summary is derived from best ancestral state changes along branches; use stochastic mapping for formal event counts."
  summary <- summary[order(summary$model, summary$location, -summary$event_count, summary$event_type), , drop = FALSE]
  row.names(summary) <- NULL
  summary
}

state_area_lookup <- function(geographic_states) {
  if (is.null(geographic_states) || nrow(geographic_states) == 0L ||
      !all(c("state", "areas") %in% names(geographic_states))) {
    return(list())
  }
  lookup <- lapply(geographic_states$areas, function(x) {
    if (is.na(x) || !nzchar(as.character(x))) {
      character()
    } else {
      strsplit(as.character(x), ";", fixed = TRUE)[[1L]]
    }
  })
  names(lookup) <- geographic_states$state
  lookup
}

state_areas <- function(state, lookup) {
  state <- as.character(state %||% "")
  if (!nzchar(state) || identical(tolower(state), "null")) {
    return(character())
  }
  if (length(lookup) > 0L && state %in% names(lookup)) {
    return(lookup[[state]])
  }
  strsplit(state, "", fixed = TRUE)[[1L]]
}

classify_range_change_event <- function(parent_areas, child_areas, gained, lost) {
  if (length(parent_areas) == 0L && length(child_areas) > 0L) {
    return(list(type = "range_origin", label = "Range origin from null"))
  }
  if (length(parent_areas) > 0L && length(child_areas) == 0L) {
    return(list(type = "range_loss_to_null", label = "Range loss to null"))
  }
  if (length(gained) == 0L && length(lost) == 0L) {
    return(list(type = "no_change", label = "No range change"))
  }
  if (length(gained) > 0L && length(lost) == 0L) {
    return(list(type = "range_expansion", label = "Range expansion"))
  }
  if (length(gained) == 0L && length(lost) > 0L) {
    return(list(type = "local_extinction", label = "Local extinction"))
  }
  list(type = "range_shift", label = "Range shift")
}

empty_range_change_events_table <- function() {
  data.frame(
    model = character(),
    location = character(),
    parent_node_index = integer(),
    node_index = integer(),
    node_type = character(),
    node_label = character(),
    edge_length = numeric(),
    parent_distance_from_root = numeric(),
    child_distance_from_root = numeric(),
    parent_time_before_present = numeric(),
    child_time_before_present = numeric(),
    event_time_min = numeric(),
    event_time_max = numeric(),
    event_time_midpoint = numeric(),
    time_unit = character(),
    event_time_note = character(),
    parent_state = character(),
    child_state = character(),
    parent_probability = numeric(),
    child_probability = numeric(),
    event_type = character(),
    event_label = character(),
    state_changed = logical(),
    gained_areas = character(),
    lost_areas = character(),
    source_region = character(),
    target_region = character(),
    direction = character(),
    source_region_label = character(),
    target_region_label = character(),
    direction_label = character(),
    gained_count = integer(),
    lost_count = integer(),
    interpretation_note = character(),
    stringsAsFactors = FALSE
  )
}

empty_best_fit_events_table <- function() {
  data.frame(
    event_index = integer(),
    model = character(),
    location = character(),
    event_time_midpoint = numeric(),
    event_time_min = numeric(),
    event_time_max = numeric(),
    time_unit = character(),
    event_time_note = character(),
    direction = character(),
    direction_label = character(),
    source_region = character(),
    target_region = character(),
    source_region_label = character(),
    target_region_label = character(),
    event_type = character(),
    event_label = character(),
    parent_state = character(),
    child_state = character(),
    gained_areas = character(),
    lost_areas = character(),
    parent_node_index = integer(),
    node_index = integer(),
    node_label = character(),
    edge_length = numeric(),
    parent_probability = numeric(),
    child_probability = numeric(),
    interpretation_note = character(),
    stringsAsFactors = FALSE
  )
}

empty_event_summary_table <- function() {
  data.frame(
    model = character(),
    location = character(),
    event_type = character(),
    event_label = character(),
    event_count = integer(),
    changed_edges = integer(),
    interpretation_note = character(),
    stringsAsFactors = FALSE
  )
}

identical_or_na <- function(x, value) {
  out <- x == value
  out[is.na(out)] <- FALSE
  out
}
