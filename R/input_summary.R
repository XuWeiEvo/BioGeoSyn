#' Summarize input data for the analysis overview
#'
#' Produce a compact, RASP-style overview of a BioGeoSyn input dataset: how
#' many tips the tree has, how many areas and species the geography defines, how
#' many species occupy each region, and how species range sizes are distributed.
#' This backs the data-overview step of the Shiny wizard, but it is usable on its
#' own for a quick sanity check of a dataset before fitting models.
#'
#' @param config A configuration list from [read_config()].
#' @param base_dir Directory used to resolve relative input paths.
#' @return A list of class `bgs_input_summary` with elements `tree`,
#'   `geography`, `region_occupancy`, `range_size_distribution`, `taxon_match`,
#'   and `overview` (a tidy item/value table for display). An element is `NULL`
#'   when the input file it needs is missing or unreadable.
#' @export
summarize_input_data <- function(config, base_dir = dirname(config$.config_file %||% ".")) {
  config <- config %||% list()
  inputs <- config$inputs %||% list()

  tree_file <- resolve_config_path(inputs$tree_file, base_dir)
  geography_file <- resolve_config_path(inputs$geography_file, base_dir)
  regions_file <- resolve_config_path(inputs$regions_file, base_dir)

  tree <- NULL
  if (!is.null(tree_file) && file.exists(tree_file)) {
    tree <- tryCatch(ape::read.tree(tree_file), error = function(e) NULL)
  }
  geography <- NULL
  if (!is.null(geography_file) && file.exists(geography_file)) {
    geography <- tryCatch(read_geography_for_validation(geography_file), error = function(e) NULL)
  }
  region_meta <- read_region_metadata(regions_file)

  tree_summary <- summarize_input_tree(tree)
  geography_parts <- summarize_input_geography(geography, region_meta, inputs$max_range_size)
  taxon_match <- summarize_taxon_match(tree, geography)

  overview <- build_input_overview(
    tree_summary,
    geography_parts$geography,
    taxon_match
  )

  structure(
    list(
      tree = tree_summary,
      geography = geography_parts$geography,
      region_occupancy = geography_parts$region_occupancy,
      range_size_distribution = geography_parts$range_size_distribution,
      taxon_match = taxon_match,
      overview = overview
    ),
    class = "bgs_input_summary"
  )
}

summarize_input_tree <- function(tree) {
  if (is.null(tree) || is.null(tree$tip.label)) {
    return(NULL)
  }
  has_branch_lengths <- !is.null(tree$edge.length) && length(tree$edge.length) > 0L
  root_age <- NA_real_
  if (has_branch_lengths) {
    root_age <- tryCatch(max(ape::node.depth.edgelength(tree)), error = function(e) NA_real_)
  }
  list(
    n_tips = length(tree$tip.label),
    n_internal_nodes = tree$Nnode %||% NA_integer_,
    has_branch_lengths = has_branch_lengths,
    is_binary = tryCatch(isTRUE(ape::is.binary(tree)), error = function(e) NA),
    is_ultrametric = if (has_branch_lengths) {
      tryCatch(isTRUE(ape::is.ultrametric(tree)), error = function(e) NA)
    } else {
      NA
    },
    root_age = root_age
  )
}

summarize_input_geography <- function(geography, region_meta, max_range_size_setting) {
  empty <- list(geography = NULL, region_occupancy = NULL, range_size_distribution = NULL)
  if (is.null(geography) || is.null(geography$matrix) || nrow(geography$matrix) == 0L) {
    return(empty)
  }
  mat <- geography$matrix
  present <- !is.na(mat) & mat > 0
  range_sizes <- rowSums(present)
  area_ids <- colnames(mat)
  if (is.null(area_ids)) {
    area_ids <- paste0("area", seq_len(ncol(mat)))
  }

  setting <- suppressWarnings(as.integer(max_range_size_setting))
  geography_summary <- list(
    n_species = nrow(mat),
    n_areas = ncol(mat),
    area_ids = area_ids,
    max_range_size_setting = if (length(setting) == 1L) setting else NA_integer_,
    max_range_size_observed = if (length(range_sizes)) as.integer(max(range_sizes)) else NA_integer_,
    mean_range_size = if (length(range_sizes)) mean(range_sizes) else NA_real_,
    single_area_species = sum(range_sizes == 1L),
    widespread_species = sum(range_sizes > 1L),
    empty_range_species = sum(range_sizes == 0L)
  )

  list(
    geography = geography_summary,
    region_occupancy = summarize_region_occupancy(present, area_ids, region_meta),
    range_size_distribution = summarize_range_size_distribution(range_sizes)
  )
}

summarize_region_occupancy <- function(present, area_ids, region_meta) {
  n_area <- colSums(present)
  range_sizes <- rowSums(present)
  endemic <- colSums(present & (range_sizes == 1L))
  total <- nrow(present)

  labels <- area_ids
  if (!is.null(region_meta) && nrow(region_meta) > 0L && "label" %in% names(region_meta)) {
    idx <- match(area_ids, region_meta$region)
    matched <- !is.na(idx)
    labels[matched] <- as.character(region_meta$label[idx[matched]])
  }

  out <- data.frame(
    region = area_ids,
    label = labels,
    n_species = as.integer(n_area),
    n_endemic = as.integer(endemic),
    proportion = if (total > 0L) as.numeric(n_area) / total else NA_real_,
    stringsAsFactors = FALSE
  )
  out <- out[order(-out$n_species, out$region), , drop = FALSE]
  row.names(out) <- NULL
  out
}

summarize_range_size_distribution <- function(range_sizes) {
  positive <- range_sizes[range_sizes > 0L]
  if (length(positive) == 0L) {
    return(data.frame(
      range_size = integer(),
      n_species = integer(),
      proportion = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  max_size <- max(positive)
  counts <- tabulate(positive, nbins = max_size)
  total <- length(positive)
  data.frame(
    range_size = seq_len(max_size),
    n_species = as.integer(counts),
    proportion = counts / total,
    stringsAsFactors = FALSE
  )
}

summarize_taxon_match <- function(tree, geography) {
  if (is.null(tree) || is.null(tree$tip.label) || is.null(geography) || is.null(geography$taxa)) {
    return(NULL)
  }
  tips <- tree$tip.label
  taxa <- geography$taxa
  missing_from_geography <- setdiff(tips, taxa)
  missing_from_tree <- setdiff(taxa, tips)
  list(
    n_tree_tips = length(tips),
    n_geography_species = length(taxa),
    n_shared = length(intersect(tips, taxa)),
    missing_from_geography = missing_from_geography,
    missing_from_tree = missing_from_tree,
    all_match = length(missing_from_geography) == 0L && length(missing_from_tree) == 0L
  )
}

build_input_overview <- function(tree_summary, geography_summary, taxon_match) {
  rows <- list()
  add <- function(item, value) {
    rows[[length(rows) + 1L]] <<- data.frame(
      item = item,
      value = as.character(value),
      stringsAsFactors = FALSE
    )
  }

  if (!is.null(tree_summary)) {
    add("Tree tips", tree_summary$n_tips)
    if (isTRUE(tree_summary$has_branch_lengths) && !is.na(tree_summary$root_age)) {
      add("Root age (tree height)", format_overview_number(tree_summary$root_age))
    }
    if (!is.na(tree_summary$is_ultrametric)) {
      add("Ultrametric tree", if (isTRUE(tree_summary$is_ultrametric)) "Yes" else "No")
    }
  }
  if (!is.null(geography_summary)) {
    add("Areas (regions)", geography_summary$n_areas)
    add("Species (geography)", geography_summary$n_species)
    if (!is.na(geography_summary$max_range_size_setting)) {
      add("Max range size (setting)", geography_summary$max_range_size_setting)
    }
    add("Largest observed range", geography_summary$max_range_size_observed)
    add("Mean range size", format_overview_number(geography_summary$mean_range_size))
    add("Widespread species (>1 area)", geography_summary$widespread_species)
  }
  if (!is.null(taxon_match)) {
    add(
      "Tree/geography names match",
      if (isTRUE(taxon_match$all_match)) {
        "Yes"
      } else {
        paste0(
          "No (", length(taxon_match$missing_from_geography),
          " missing from geography, ", length(taxon_match$missing_from_tree),
          " missing from tree)"
        )
      }
    )
  }

  if (length(rows) == 0L) {
    return(data.frame(item = character(), value = character(), stringsAsFactors = FALSE))
  }
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

format_overview_number <- function(x) {
  if (length(x) != 1L || is.na(x)) {
    return(NA_character_)
  }
  formatC(x, format = "fg", digits = 3)
}

#' @export
print.bgs_input_summary <- function(x, ...) {
  cat("BioGeoSyn input data overview\n")
  if (!is.null(x$overview) && nrow(x$overview) > 0L) {
    for (i in seq_len(nrow(x$overview))) {
      cat(sprintf("  %-30s %s\n", x$overview$item[i], x$overview$value[i]))
    }
  }
  if (!is.null(x$region_occupancy) && nrow(x$region_occupancy) > 0L) {
    cat("\nSpecies per region:\n")
    print(x$region_occupancy, row.names = FALSE)
  }
  invisible(x)
}
