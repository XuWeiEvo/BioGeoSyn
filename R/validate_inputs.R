#' Validate a BioGeoSyn configuration
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

  constraint_checks <- validate_constraint_files(config$advanced$constraints, base_dir, config)
  if (length(constraint_checks)) {
    checks <- c(checks, constraint_checks)
  }

  validation <- do.call(rbind, checks)
  row.names(validation) <- NULL
  format_validation_results(validation)
}

#' Add user-facing labels and repair guidance to validation results
#'
#' @param validation A validation data frame containing `check`, `ok`, and
#'   `detail` columns.
#' @return The validation data frame with `label`, `status`, and `next_step`
#'   columns added while preserving the machine-readable fields.
#' @export
format_validation_results <- function(validation) {
  required <- c("check", "ok", "detail")
  missing <- setdiff(required, names(validation))
  if (length(missing) > 0L) {
    stop(
      "Validation results are missing column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  catalog <- validation_check_catalog()
  matched <- match(validation$check, catalog$check)
  fallback_label <- gsub("_", " ", validation$check, fixed = TRUE)
  fallback_label <- paste0(toupper(substr(fallback_label, 1L, 1L)), substr(fallback_label, 2L, nchar(fallback_label)))
  labels <- ifelse(is.na(matched), fallback_label, catalog$label[matched])
  failed_steps <- ifelse(
    is.na(matched),
    "Review the technical detail, correct the input, and validate again.",
    catalog$next_step[matched]
  )

  data.frame(
    check = validation$check,
    label = labels,
    status = ifelse(validation$ok, "Passed", "Needs attention"),
    ok = validation$ok,
    detail = validation$detail,
    next_step = ifelse(validation$ok, "No action needed.", failed_steps),
    stringsAsFactors = FALSE
  )
}

validation_check_catalog <- function() {
  data.frame(
    check = c(
      "tree_file_exists",
      "geography_file_exists",
      "regions_file_exists",
      "tree_file_parseable",
      "geography_csv_parseable",
      "geography_species_unique",
      "geography_area_values_binary",
      "geography_each_species_has_area",
      "max_range_size_within_area_count",
      "tree_geography_species_match",
      "regions_cover_geography_areas",
      "models_supported",
      "models_not_duplicated",
      "max_range_size_positive",
      "output_parent_writable",
      "advanced_constraints_list",
      "advanced_constraint_fields_known",
      "advanced_constraint_files_exist",
      "advanced_constraint_layout",
      "advanced_constraint_areas_match_geography",
      "advanced_constraint_times_readable",
      "advanced_constraint_strata_cover_times",
      "advanced_constraint_times_cover_root",
      "advanced_constraint_times_avoid_nodes"
    ),
    label = c(
      "Tree file is available",
      "Geography CSV is available",
      "Regions CSV is available",
      "Tree file can be read",
      "Geography CSV can be read",
      "Taxon names in geography CSV are unique",
      "Geography area values are binary",
      "Every taxon occurs in at least one area",
      "Maximum range size fits the area count",
      "Tree and geography taxon names match",
      "Regions CSV covers every geography area",
      "Selected models are supported",
      "Selected models are not duplicated",
      "Maximum range size is positive",
      "Output directory is writable",
      "Advanced constraints use a valid structure",
      "Advanced constraint names are recognized",
      "Advanced constraint files are available",
      "Advanced constraint files use the BioGeoBEARS layout",
      "Advanced constraint areas match the geography",
      "Times file lists numeric time-bin bottoms",
      "Each constraint has a block per time bin",
      "Oldest time bin is older than the tree root",
      "No tree node sits on a time-bin boundary"
    ),
    next_step = c(
      "Select an existing Newick tree file.",
      "Select an existing geography CSV file.",
      "Select an existing regions CSV file, or remove it from the configuration.",
      "Export the tree in valid Newick format and upload it again.",
      "Use a CSV with one taxon column and at least one area column.",
      "Keep one geography row per taxon and remove or merge duplicate names.",
      "Use only 0 and 1 in all geography area columns.",
      "Assign at least one area with value 1 to every taxon.",
      "Set maximum range size no higher than the number of geography areas.",
      "Make tree tip names and geography taxon names identical, including capitalization.",
      "Add one regions CSV row for every geography area column.",
      "Select at least one model from the supported model list.",
      "Remove duplicate model selections.",
      "Set maximum range size to a whole number of 1 or greater.",
      "Choose an output location where the current user can create files.",
      "Define advanced constraints as named YAML fields.",
      "Remove unknown advanced constraint fields or use a documented field name.",
      "Correct or remove advanced constraint file paths that do not exist.",
      "Write each constraint block as an area-name header followed by rows of plain numbers, with no row labels.",
      "Use the same area codes, in the geography's areas, in every constraint file.",
      "List one numeric time-bin bottom per line in the times file.",
      "Give every constraint file at least one block per time bin in the times file.",
      "Add a time-bin bottom older than the tree root age.",
      "Shift the time-bin bottoms so that no tree node falls exactly on a boundary."
    ),
    stringsAsFactors = FALSE
  )
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

validate_constraint_files <- function(constraints, base_dir, config = list()) {
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
  if (length(missing_files) > 0L) {
    return(checks)
  }

  present <- function(field) {
    value <- constraints[[field]]
    if (is.null(value) || identical(value, "")) {
      return(NULL)
    }
    resolve_config_path(value, base_dir)
  }
  matrix_fields <- c(
    "dists_file", "distance_file", "dispersal_multipliers_file",
    "areas_allowed_file", "areas_adjacency_file"
  )

  # Layout: BioGeoBEARS wants an area-name header then rows of plain numbers.
  # Row labels ("A 1 1 1") make read_distances_fn() and friends abort mid-run.
  layout_problems <- character()
  parsed <- list()
  for (field in intersect(names(constraints), c(matrix_fields, "area_of_areas_file"))) {
    path <- present(field)
    if (is.null(path)) next
    blocks <- parse_constraint_blocks(path)
    parsed[[field]] <- blocks
    if (is.null(blocks)) {
      layout_problems <- c(layout_problems, paste0(field, ": file is empty"))
      next
    }
    n_areas <- length(blocks$areas)
    expected_rows <- if (identical(field, "area_of_areas_file")) 1L else n_areas
    for (i in seq_along(blocks$blocks)) {
      rows <- blocks$blocks[[i]]
      bad <- vapply(rows, function(r) {
        length(r) != n_areas || any(is.na(suppressWarnings(as.numeric(r))))
      }, logical(1))
      if (any(bad)) {
        layout_problems <- c(layout_problems, paste0(
          field, ": block ", i, " has rows that are not ", n_areas,
          " plain numbers (row labels are not allowed)"
        ))
      } else if (length(rows) != expected_rows) {
        layout_problems <- c(layout_problems, paste0(
          field, ": block ", i, " has ", length(rows), " rows, expected ", expected_rows
        ))
      }
    }
  }
  if (length(parsed) > 0L) {
    add_constraint_check(
      "advanced_constraint_layout",
      length(layout_problems) == 0L,
      if (length(layout_problems) == 0L) "constraint files use the BioGeoBEARS layout" else paste(layout_problems, collapse = "; ")
    )
  }

  # Every constraint must describe the same areas as the geography matrix.
  geography_file <- resolve_config_path(config$inputs$geography_file, base_dir)
  geo_areas <- NULL
  if (!is.null(geography_file) && file.exists(geography_file)) {
    geo_areas <- tryCatch(colnames(read_range_matrix(geography_file)$matrix), error = function(e) NULL)
  }
  if (!is.null(geo_areas) && length(parsed) > 0L) {
    area_problems <- character()
    for (field in names(parsed)) {
      blocks <- parsed[[field]]
      if (is.null(blocks)) next
      if (!setequal(blocks$areas, geo_areas)) {
        area_problems <- c(area_problems, paste0(
          field, ": areas [", paste(blocks$areas, collapse = " "),
          "] do not match the geography areas [", paste(geo_areas, collapse = " "), "]"
        ))
      }
    }
    add_constraint_check(
      "advanced_constraint_areas_match_geography",
      length(area_problems) == 0L,
      if (length(area_problems) == 0L) "constraint areas match the geography" else paste(area_problems, collapse = "; ")
    )
  }

  times_path <- present("times_file")
  if (is.null(times_path)) {
    return(checks)
  }
  times <- suppressWarnings(as.numeric(trimws(readLines(times_path, warn = FALSE))))
  times <- times[!is.na(times)]
  add_constraint_check(
    "advanced_constraint_times_readable",
    length(times) > 0L,
    if (length(times) > 0L) paste(times, collapse = ", ") else "times file has no numeric time-bin bottoms"
  )
  if (length(times) == 0L) {
    return(checks)
  }

  # One block per stratum: fewer blocks than times aborts the run (extra blocks
  # are tolerated by BioGeoBEARS).
  if (length(parsed) > 0L) {
    strata_problems <- character()
    for (field in names(parsed)) {
      blocks <- parsed[[field]]
      if (is.null(blocks)) next
      if (length(blocks$blocks) < length(times)) {
        strata_problems <- c(strata_problems, paste0(
          field, ": ", length(blocks$blocks), " block(s) for ", length(times), " time bin(s)"
        ))
      }
    }
    add_constraint_check(
      "advanced_constraint_strata_cover_times",
      length(strata_problems) == 0L,
      if (length(strata_problems) == 0L) "each constraint has a block per time bin" else paste(strata_problems, collapse = "; ")
    )
  }

  # BioGeoBEARS requires the oldest time-bin bottom to sit above the root, and
  # refuses to section a tree whose nodes land exactly on a bin boundary.
  tree_file <- resolve_config_path(config$inputs$tree_file, base_dir)
  tree <- if (!is.null(tree_file) && file.exists(tree_file)) {
    tryCatch(ape::read.tree(tree_file), error = function(e) NULL)
  } else {
    NULL
  }
  if (!is.null(tree) && !is.null(tree$edge.length)) {
    depths <- ape::node.depth.edgelength(tree)
    root_age <- max(depths, na.rm = TRUE)
    add_constraint_check(
      "advanced_constraint_times_cover_root",
      max(times) > root_age,
      paste0("oldest time = ", max(times), "; tree root age = ", signif(root_age, 6))
    )

    internal <- seq(from = length(tree$tip.label) + 1L, length.out = tree$Nnode)
    node_ages <- root_age - depths[internal]
    on_boundary <- vapply(times, function(t) any(abs(node_ages - t) < 1e-6), logical(1))
    add_constraint_check(
      "advanced_constraint_times_avoid_nodes",
      !any(on_boundary),
      if (!any(on_boundary)) {
        "no node sits on a time-bin boundary"
      } else {
        paste0("nodes sit exactly on time(s): ", paste(times[on_boundary], collapse = ", "))
      }
    )
  }
  checks
}

#' Parse a BioGeoBEARS constraint file into per-stratum blocks
#'
#' Constraint files are one or more blocks of "area-name header followed by rows
#' of plain numbers", one block per time bin, optionally terminated by `END`.
#' A line identical to the header starts a new block.
#'
#' @param path Path to a constraint file.
#' @return A list with `areas` (the header) and `blocks` (a list of blocks, each
#'   a list of split data rows), or `NULL` when the file has no content.
#' @noRd
parse_constraint_blocks <- function(path) {
  lines <- trimws(readLines(path, warn = FALSE))
  lines <- lines[nzchar(lines) & toupper(lines) != "END"]
  if (length(lines) == 0L) {
    return(NULL)
  }
  split_fields <- function(x) strsplit(x, "[[:space:]]+")[[1L]]
  header <- split_fields(lines[[1L]])
  blocks <- list()
  rows <- NULL
  for (line in lines) {
    fields <- split_fields(line)
    if (identical(fields, header)) {
      if (!is.null(rows)) {
        blocks[[length(blocks) + 1L]] <- rows
      }
      rows <- list()
    } else {
      rows[[length(rows) + 1L]] <- fields
    }
  }
  if (!is.null(rows)) {
    blocks[[length(blocks) + 1L]] <- rows
  }
  list(areas = header, blocks = blocks)
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
