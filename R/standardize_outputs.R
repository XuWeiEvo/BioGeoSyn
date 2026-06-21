standardize_biogeobears_outputs <- function(model_results, prepared_inputs, project_paths) {
  completed <- model_results[
    vapply(model_results, function(x) identical(x$status, "completed") && !is.null(x$result), logical(1))
  ]

  empty_outputs <- list(
    parameter_table = data.frame(),
    ancestral_state_probabilities = data.frame(),
    root_state_probabilities = data.frame()
  )
  if (length(completed) == 0L) {
    return(empty_outputs)
  }

  parameter_table <- do.call(rbind, lapply(names(completed), function(model) {
    extract_parameter_table(completed[[model]]$result, model)
  }))
  row.names(parameter_table) <- NULL

  state_labels <- make_state_labels(
    areas = prepared_inputs$areas,
    max_range_size = prepared_inputs$max_range_size,
    include_null_range = TRUE
  )
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

  write_csv_base(parameter_table, file.path(project_paths$tables, "model_parameters.csv"))
  write_csv_base(ancestral_state_probabilities, file.path(project_paths$tables, "ancestral_state_probabilities.csv"))
  write_csv_base(root_state_probabilities, file.path(project_paths$tables, "root_state_probabilities.csv"))

  list(
    parameter_table = parameter_table,
    ancestral_state_probabilities = ancestral_state_probabilities,
    root_state_probabilities = root_state_probabilities
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
  max_range_size <- min(as.integer(max_range_size), length(areas))
  labels <- character()
  if (isTRUE(include_null_range)) {
    labels <- c(labels, "null")
  }
  for (size in seq_len(max_range_size)) {
    combos <- utils::combn(areas, size, FUN = paste0, collapse = "")
    labels <- c(labels, combos)
  }
  labels
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
  data.frame(
    node_index = node_index,
    node_type = node_type,
    node_label = node_label,
    stringsAsFactors = FALSE
  )
}

identical_or_na <- function(x, value) {
  out <- x == value
  out[is.na(out)] <- FALSE
  out
}
