#' Validate an iBiogeobears configuration
#'
#' @param config A configuration list returned by [read_config()].
#' @param base_dir Directory used to resolve relative input paths.
#' @return A data frame of validation checks.
#' @export
validate_inputs <- function(config, base_dir = dirname(config$.config_file %||% ".")) {
  checks <- list()
  add_check <- function(name, ok, detail = "") {
    checks[[length(checks) + 1L]] <<- data.frame(
      check = name,
      ok = isTRUE(ok),
      detail = as.character(detail),
      stringsAsFactors = FALSE
    )
  }

  tree_file <- resolve_config_path(config$inputs$tree_file, base_dir)
  geography_file <- resolve_config_path(config$inputs$geography_file, base_dir)
  regions_file <- resolve_config_path(config$inputs$regions_file, base_dir)
  tree <- NULL
  geography <- NULL

  add_check("tree_file_exists", !is.null(tree_file) && file.exists(tree_file), tree_file %||% "missing")
  add_check("geography_file_exists", !is.null(geography_file) && file.exists(geography_file), geography_file %||% "missing")
  if (!is.null(regions_file)) {
    add_check("regions_file_exists", file.exists(regions_file), regions_file)
  }
  if (!is.null(tree_file) && file.exists(tree_file)) {
    tree <- tryCatch(ape::read.tree(tree_file), error = function(e) e)
    add_check("tree_file_parseable", !inherits(tree, "error"), if (inherits(tree, "error")) conditionMessage(tree) else tree_file)
    if (inherits(tree, "error")) {
      tree <- NULL
    }
  }
  if (!is.null(geography_file) && file.exists(geography_file)) {
    geography <- tryCatch(read_geography_for_validation(geography_file), error = function(e) e)
    add_check(
      "geography_csv_parseable",
      !inherits(geography, "error"),
      if (inherits(geography, "error")) conditionMessage(geography) else geography_file
    )
    if (inherits(geography, "error")) {
      geography <- NULL
    }
  }
  if (!is.null(geography)) {
    duplicated_taxa <- unique(geography$taxa[duplicated(geography$taxa)])
    add_check(
      "geography_species_unique",
      length(duplicated_taxa) == 0L,
      if (length(duplicated_taxa) == 0L) "unique" else paste(duplicated_taxa, collapse = ", ")
    )
    add_check(
      "geography_area_values_binary",
      all(!is.na(geography$matrix)) && all(geography$matrix %in% c(0, 1)),
      paste(colnames(geography$matrix), collapse = ", ")
    )
    empty_ranges <- geography$taxa[rowSums(geography$matrix, na.rm = TRUE) == 0L]
    add_check(
      "geography_each_species_has_area",
      length(empty_ranges) == 0L,
      if (length(empty_ranges) == 0L) "all species have at least one area" else paste(empty_ranges, collapse = ", ")
    )
    add_check(
      "max_range_size_within_area_count",
      is.numeric(config$inputs$max_range_size) && config$inputs$max_range_size <= ncol(geography$matrix),
      paste0("max_range_size=", config$inputs$max_range_size, "; area_count=", ncol(geography$matrix))
    )
  }
  if (!is.null(tree) && !is.null(geography)) {
    missing_from_geography <- setdiff(tree$tip.label, geography$taxa)
    missing_from_tree <- setdiff(geography$taxa, tree$tip.label)
    add_check(
      "tree_geography_species_match",
      length(missing_from_geography) == 0L && length(missing_from_tree) == 0L,
      format_species_mismatch(missing_from_geography, missing_from_tree)
    )
  }
  if (!is.null(regions_file) && file.exists(regions_file) && !is.null(geography)) {
    regions <- tryCatch(utils::read.csv(regions_file, stringsAsFactors = FALSE), error = function(e) NULL)
    region_ids <- if (!is.null(regions) && "region" %in% names(regions)) regions$region else character()
    missing_regions <- setdiff(colnames(geography$matrix), region_ids)
    add_check(
      "regions_cover_geography_areas",
      length(missing_regions) == 0L,
      if (length(missing_regions) == 0L) "all geography areas have region metadata" else paste(missing_regions, collapse = ", ")
    )
  }

  requested_models <- config$models$run %||% character()
  unknown_models <- setdiff(requested_models, valid_models())
  add_check(
    "models_supported",
    length(requested_models) > 0L && length(unknown_models) == 0L,
    if (length(unknown_models) == 0L) paste(requested_models, collapse = ", ") else paste(unknown_models, collapse = ", ")
  )
  duplicated_models <- unique(requested_models[duplicated(requested_models)])
  add_check(
    "models_not_duplicated",
    length(duplicated_models) == 0L,
    if (length(duplicated_models) == 0L) "no duplicates" else paste(duplicated_models, collapse = ", ")
  )

  add_check(
    "max_range_size_positive",
    is.numeric(config$inputs$max_range_size) && config$inputs$max_range_size >= 1,
    config$inputs$max_range_size
  )
  output_parent <- nearest_existing_parent(as_path(config$project$output_dir %||% "."))
  add_check(
    "output_parent_writable",
    !is.null(output_parent) && file.access(output_parent, mode = 2) == 0L,
    output_parent %||% "missing"
  )

  constraint_checks <- validate_constraint_files(config$advanced$constraints, base_dir)
  if (length(constraint_checks)) {
    checks <- c(checks, constraint_checks)
  }

  validation <- do.call(rbind, checks)
  row.names(validation) <- NULL
  validation
}

read_geography_for_validation <- function(path) {
  ranges <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  if (ncol(ranges) < 2L) {
    stop("Geography CSV must contain a taxon column and at least one area column.", call. = FALSE)
  }
  taxon_col <- if ("species" %in% names(ranges)) "species" else if ("taxon" %in% names(ranges)) "taxon" else names(ranges)[1L]
  taxa <- ranges[[taxon_col]]
  area_data <- ranges[setdiff(names(ranges), taxon_col)]
  area_matrix <- as.matrix(area_data)
  suppressWarnings(mode(area_matrix) <- "numeric")
  row.names(area_matrix) <- taxa
  list(taxa = taxa, matrix = area_matrix)
}

format_species_mismatch <- function(missing_from_geography, missing_from_tree) {
  if (length(missing_from_geography) == 0L && length(missing_from_tree) == 0L) {
    return("tree tips and geography species match")
  }
  paste(
    "missing_from_geography:",
    if (length(missing_from_geography)) paste(missing_from_geography, collapse = ", ") else "none",
    "| missing_from_tree:",
    if (length(missing_from_tree)) paste(missing_from_tree, collapse = ", ") else "none"
  )
}

nearest_existing_parent <- function(path) {
  if (is.null(path) || identical(path, "")) {
    return(NULL)
  }
  current <- if (dir.exists(path)) path else dirname(path)
  repeat {
    if (dir.exists(current)) {
      return(current)
    }
    parent <- dirname(current)
    if (identical(parent, current)) {
      return(NULL)
    }
    current <- parent
  }
}

validate_constraint_files <- function(constraints, base_dir) {
  checks <- list()
  add_constraint_check <- function(name, ok, detail = "") {
    checks[[length(checks) + 1L]] <<- data.frame(
      check = name,
      ok = isTRUE(ok),
      detail = as.character(detail),
      stringsAsFactors = FALSE
    )
  }
  if (is.null(constraints)) {
    return(checks)
  }
  if (!is.list(constraints)) {
    add_constraint_check("advanced_constraints_list", FALSE, "advanced$constraints must be a named list")
    return(checks)
  }

  known <- c(
    "times_file", "dists_file", "distance_file", "dispersal_multipliers_file",
    "areas_allowed_file", "areas_adjacency_file", "area_of_areas_file"
  )
  unknown <- setdiff(names(constraints), known)
  add_constraint_check(
    "advanced_constraint_fields_known",
    length(unknown) == 0L,
    if (length(unknown) == 0L) "all constraint fields are recognized" else paste(unknown, collapse = ", ")
  )

  file_fields <- intersect(names(constraints), known)
  missing_files <- character()
  for (field in file_fields) {
    if (is.null(constraints[[field]]) || identical(constraints[[field]], "")) {
      next
    }
    path <- resolve_config_path(constraints[[field]], base_dir)
    if (is.null(path) || !file.exists(path)) {
      missing_files <- c(missing_files, paste0(field, "=", path %||% "missing"))
    }
  }
  add_constraint_check(
    "advanced_constraint_files_exist",
    length(missing_files) == 0L,
    if (length(missing_files) == 0L) "all configured constraint files exist" else paste(missing_files, collapse = "; ")
  )
  checks
}

resolve_config_path <- function(path, base_dir) {
  if (is.null(path) || identical(path, "")) {
    return(NULL)
  }
  if (grepl("^[A-Za-z]:[/\\\\]|^/", path)) {
    return(as_path(path))
  }
  as_path(file.path(base_dir, path))
}
