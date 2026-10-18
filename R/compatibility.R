# ---- Compatibility of clades combined in the cross-clade synthesis ----
#
# Standardising event labels does not standardise inference. Clades fitted
# under different models, area codings, range-size limits or constraints can
# differ in their inferred processes for methodological rather than biological
# reasons. check_bundle_compatibility() reads the settings each result bundle
# records and reports where the clades being combined differ.

#' Biogeographic processes possible under a BioGeoBEARS model
#'
#' Under the standard parameterisations of the six models fitted by BioGeoSyn:
#' DEC allows narrow sympatry, subset sympatry and narrow vicariance at nodes;
#' DIVALIKE allows sympatry and vicariance of any size but not subset sympatry;
#' BAYAREALIKE allows only widespread sympatry (range copying) at nodes; `+J`
#' adds founder-event (jump) speciation; all allow range expansion and local
#' extinction along branches. Range switching requires the `a` parameter, which
#' none of the six standard models estimates.
#'
#' @param model Model name, e.g. `"DEC"` or `"BAYAREALIKE+J"`.
#' @return Character vector of `process_key` values (see
#'   [biogeographic_process_taxonomy()]) that the model can produce.
#' @export
processes_possible_under_model <- function(model) {
  model <- toupper(as.character(model))
  family <- sub("\\+J$", "", model)
  clado <- switch(family,
    DEC = c("in_situ_speciation", "subset_sympatry", "vicariance"),
    DIVALIKE = c("in_situ_speciation", "vicariance"),
    BAYAREALIKE = c("in_situ_speciation"),
    c("in_situ_speciation", "subset_sympatry", "vicariance")
  )
  if (grepl("\\+J$", model)) clado <- c(clado, "founder_event_speciation")
  c(clado, "range_expansion", "local_extinction")
}

#' Check that result bundles can be combined across clades
#'
#' Read the settings recorded in each single-clade result bundle and report
#' where the clades differ in ways that affect comparability: the model used
#' for stochastic mapping (and the processes it allows), the area coding and
#' the regions occupied, the maximum range size, time stratification and other
#' constraints, the number of stochastic maps, crown ages, likely time units,
#' and software versions. Bundles named `<clade>__treeNNN` are treated as
#' trees of one clade.
#'
#' @param zip_paths Paths to result bundles (`.zip`).
#' @param clade_names Optional labels, one per bundle.
#' @return An object of class `bgs_compatibility`: a list with `clades` (one
#'   row per bundle) and `checks` (one row per check, with `level` `"ok"`,
#'   `"info"` or `"warning"` and a `message`).
#' @export
check_bundle_compatibility <- function(zip_paths, clade_names = NULL) {
  zip_paths <- as.character(zip_paths %||% character())
  clade_names <- as.character(clade_names %||% rep(NA_character_, length(zip_paths)))
  labels <- vapply(seq_along(zip_paths), function(i) {
    if (length(clade_names) >= i && !is.na(clade_names[[i]]) && nzchar(clade_names[[i]])) clade_names[[i]] else derive_clade_name(zip_paths[[i]])
  }, character(1))
  labels <- make.unique(labels, sep = " ")
  rows <- lapply(seq_along(zip_paths), function(i) bundle_settings(zip_paths[[i]], labels[[i]]))
  clades <- do.call(rbind, rows)
  checks <- if (is.null(clades) || nrow(clades) == 0L) {
    data.frame(check = "bundles", level = "warning", message = "No readable result bundles.", stringsAsFactors = FALSE)
  } else {
    compatibility_checks(clades, zip_paths)
  }
  structure(list(clades = clades, checks = checks), class = "bgs_compatibility")
}

#' @export
print.bgs_compatibility <- function(x, ...) {
  n_clades <- if (is.null(x$clades)) 0L else length(unique(x$clades$clade))
  cat("BioGeoSyn cross-clade compatibility:", n_clades, "clades,", NROW(x$clades), "bundles\n")
  for (i in seq_len(nrow(x$checks))) {
    cat(sprintf("  [%s] %s\n", x$checks$level[[i]], x$checks$message[[i]]))
  }
  invisible(x)
}

# Read a non-table file (by base name) from a bundle as text lines.
read_bundle_lines <- function(zip_path, file) {
  entries <- tryCatch(utils::unzip(zip_path, list = TRUE)$Name, error = function(e) character())
  cand <- entries[basename(entries) == file]
  if (length(cand) == 0L) return(NULL)
  cand <- cand[order(nchar(cand))][[1L]]
  tmp <- tempfile("bgs-bundle-file-")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  ok <- tryCatch({ utils::unzip(zip_path, files = cand, exdir = tmp); TRUE }, error = function(e) FALSE)
  if (!isTRUE(ok)) return(NULL)
  tryCatch(readLines(file.path(tmp, cand), warn = FALSE), error = function(e) NULL)
}

bundle_settings <- function(zip_path, label) {
  events <- keep_first_model(read_bundle_table(zip_path, "bsm_events"))
  status <- read_bundle_table(zip_path, "bsm_run_status")
  tree <- read_bundle_table(zip_path, "tree_nodes")
  esum <- read_bundle_table(zip_path, "bsm_event_summary")
  cfg_lines <- read_bundle_lines(zip_path, "config_used.yml")
  cfg <- if (is.null(cfg_lines)) list() else tryCatch(yaml::yaml.load(paste(cfg_lines, collapse = "\n")), error = function(e) list())
  sess <- read_bundle_lines(zip_path, "session_info.txt") %||% character()

  model <- if (!is.null(events) && nrow(events) > 0L) as.character(events$model[[1L]]) else
    if (!is.null(status) && nrow(status) > 0L) as.character(status$model[[1L]]) else NA_character_
  n_maps <- if (!is.null(events) && nrow(events) > 0L) region_n_maps(esum, events, model) else NA_integer_
  codes <- character()
  if (!is.null(events) && nrow(events) > 0L) {
    st <- unique(c(events$parent_state, unlist(strsplit(as.character(events$child_state), ",", fixed = TRUE))))
    codes <- sort(unique(unlist(strsplit(gsub("[^A-Za-z]", "", st), ""))))
  }
  code_map <- if (!is.null(events) && nrow(events) > 0L) region_code_name_map(events) else character()
  constraints <- (cfg$advanced %||% list())$constraints %||% list()
  used <- names(constraints)[vapply(constraints, function(v) !is.null(v) && length(v) > 0L && nzchar(as.character(v[[1L]])), logical(1))]
  tbp <- if (!is.null(tree)) suppressWarnings(as.numeric(tree$time_before_present)) else numeric()
  version_of <- function(pkg) {
    hit <- regmatches(sess, regexpr(paste0(pkg, "_[0-9][0-9.\\-]*"), sess))
    if (length(hit) == 0L) NA_character_ else sub(paste0(pkg, "_"), "", hit[[1L]])
  }
  data.frame(
    bundle = label,
    clade = clade_group_name(label),
    bsm_model = model,
    model_family = sub("\\+J$", "", toupper(model %||% NA_character_)),
    has_j = grepl("\\+J$", toupper(model %||% "")),
    n_maps = as.integer(n_maps),
    area_codes = paste(codes, collapse = ""),
    n_areas = length(codes),
    area_names = paste(sprintf("%s=%s", names(code_map), code_map)[order(names(code_map))], collapse = "; "),
    max_range_size = suppressWarnings(as.integer((cfg$inputs %||% list())$max_range_size %||% NA)),
    constraints = if (length(used)) paste(sort(used), collapse = ", ") else "none",
    crown_age = if (length(tbp)) max(tbp, na.rm = TRUE) else NA_real_,
    n_tips = if (!is.null(tree) && "node_type" %in% names(tree)) sum(tree$node_type == "tip") else NA_integer_,
    biogeobears_version = version_of("BioGeoBEARS"),
    biogeosyn_version = version_of("BioGeoSyn"),
    has_events = !is.null(events) && nrow(events) > 0L,
    stringsAsFactors = FALSE
  )
}

compatibility_checks <- function(cl, zip_paths) {
  out <- list()
  add <- function(check, level, message) out[[length(out) + 1L]] <<- data.frame(check = check, level = level, message = message, stringsAsFactors = FALSE)
  per_clade <- cl[!duplicated(cl$clade), , drop = FALSE]
  n_cl <- nrow(per_clade)

  no_bsm <- cl$bundle[!cl$has_events]
  if (length(no_bsm)) add("bsm", "warning", paste0("No stochastic-mapping events in: ", paste(no_bsm, collapse = ", "), ". These bundles cannot contribute to rates through time."))

  # Model family and +J.
  fams <- unique(stats::na.omit(cl$model_family))
  jmix <- length(unique(cl$has_j[cl$has_events])) > 1L
  taxonomy <- biogeographic_process_taxonomy()
  if (length(fams) > 1L || jmix) {
    poss <- lapply(unique(stats::na.omit(cl$bsm_model)), processes_possible_under_model)
    not_all <- setdiff(Reduce(union, poss), Reduce(intersect, poss))
    labs <- taxonomy$process_label[match(not_all, taxonomy$process_key)]
    add("model", "warning", paste0(
      "Clades were mapped under different models (", paste(sort(unique(stats::na.omit(cl$bsm_model))), collapse = ", "), "). ",
      "Some processes are impossible under some of these models (", paste(labs, collapse = ", "), "), ",
      "so differences in those processes partly reflect model choice. Compare like with like, or interpret them per model."))
  } else if (length(fams) == 1L) {
    allowed <- processes_possible_under_model(stats::na.omit(cl$bsm_model)[[1L]])
    missing <- setdiff(taxonomy$process_key, allowed)
    add("model", "ok", paste0("All clades were mapped under ", stats::na.omit(cl$bsm_model)[[1L]], ". Processes impossible under this model and therefore absent: ",
                             paste(taxonomy$process_label[match(missing, taxonomy$process_key)], collapse = ", "), "."))
  }
  # Trees of one clade mapped under different models.
  within <- tapply(cl$bsm_model, cl$clade, function(v) length(unique(stats::na.omit(v))))
  if (any(within > 1L)) add("model_within_clade", "warning", paste0("Trees of the same clade were mapped under different models: ", paste(names(within)[within > 1L], collapse = ", "), ". Fix the mapping model across trees (bsm_model in run_workflow_over_trees())."))

  # Area coding.
  pairs <- unlist(strsplit(cl$area_names[nzchar(cl$area_names)], "; ", fixed = TRUE))
  if (length(pairs)) {
    code <- sub("=.*$", "", pairs)
    name <- sub("^[^=]*=", "", pairs)
    conflict <- tapply(name, code, function(v) length(unique(v)) > 1L)
    name_conflict <- tapply(code, name, function(v) length(unique(v)) > 1L)
    if (any(conflict) || any(name_conflict)) {
      add("areas", "warning", paste0("Area codes are not used consistently across clades (",
        paste(c(names(conflict)[conflict], names(name_conflict)[name_conflict]), collapse = ", "),
        "). Region-resolved results assume one shared area coding."))
    } else {
      add("areas", "ok", "Area codes map to the same region names in every clade.")
    }
  }
  sets <- unique(cl$area_codes[nzchar(cl$area_codes)])
  if (length(sets) > 1L) {
    add("area_sets", "info", paste0("Clades occupy different sets of areas (", length(sets), " distinct sets). ",
      "A region's pooled curves include only the clades that occur there."))
  }
  # Max range size and constraints.
  mrs <- unique(stats::na.omit(cl$max_range_size))
  if (length(mrs) > 1L) add("max_range_size", "warning", paste0("Maximum range size differs among clades (", paste(sort(mrs), collapse = ", "), "). It limits widespread ranges and therefore the frequency of vicariance, subset sympatry and dispersal."))
  else if (length(mrs) == 1L) add("max_range_size", "ok", paste0("All clades used a maximum range size of ", mrs, "."))
  cons <- unique(cl$constraints)
  if (length(cons) > 1L) add("constraints", "warning", paste0("Clades differ in time stratification or dispersal constraints (", paste(cons, collapse = " | "), "). Constrained and unconstrained analyses imply different dispersal opportunities."))
  else if (!identical(cons, "none")) add("constraints", "info", paste0("All clades used the same kinds of constraints (", cons, "); check that the constraint files themselves are comparable."))
  # Maps.
  nm <- cl$n_maps[cl$has_events]
  if (length(nm) && min(nm, na.rm = TRUE) < 50L) add("maps", "info", paste0("Some clades have few stochastic maps (minimum ", min(nm, na.rm = TRUE), "). Intervals from fewer than about 50 maps are coarse."))
  # Ages and units.
  ages <- tapply(cl$crown_age, cl$clade, stats::median, na.rm = TRUE)
  if (any(ages > 200, na.rm = TRUE)) add("time_units", "warning", paste0("Some trees have crown ages above 200 (", paste(names(ages)[ages > 200], collapse = ", "), "). Check that all trees are in millions of years."))
  if (length(ages) > 1L && all(is.finite(ages))) {
    add("ages", if (max(ages) / max(min(ages), 1e-9) > 3) "info" else "ok", sprintf(
      "Crown ages range from %.1f to %.1f. Older bins draw on fewer clades; the number of contributing clades is reported for every bin of the per-lineage rates.", min(ages), max(ages)))
  }
  # Software versions.
  v <- unique(stats::na.omit(cl$biogeobears_version))
  if (length(v) > 1L) add("versions", "info", paste0("Bundles were produced with different BioGeoBEARS versions (", paste(sort(v), collapse = ", "), ")."))
  # Trees.
  nt <- table(cl$clade)
  if (any(nt > 1L)) add("trees", "info", paste0(sum(nt > 1L), " of ", n_cl, " clades were analysed on several trees; their trees are resampled jointly."))
  if (all(nt == 1L)) add("trees", "info", "Each clade was analysed on a single tree, so intervals do not include topological or age uncertainty (see run_workflow_over_trees()).")
  do.call(rbind, out)
}

#' Assumptions inherited by the cross-clade synthesis
#'
#' A short statement of what the synthesis takes from the analyses it combines,
#' shown in the interface and the cross-clade report.
#'
#' @return A character vector, one assumption per element.
#' @export
inherited_assumptions <- function() {
  c(
    "The synthesis re-presents each clade's BioGeoBEARS stochastic maps. It does not re-estimate anything and inherits every assumption of those analyses.",
    "Area definitions and coding determine which events are possible and where they are attributed; clades must share one area coding.",
    "The maximum range size, time stratification and dispersal constraints of each analysis shape the inferred dispersal and vicariance.",
    "Incomplete taxon sampling removes nodes and therefore cladogenetic events; rates near the present are also affected by missing extinct lineages.",
    "The fitted model limits which processes can appear (for example, DEC has no founder-event speciation); a good fit does not guarantee an adequate model.",
    "Stochastic maps on a single tree ignore uncertainty in topology and node ages; use several trees per clade where possible.",
    "Coincident events across clades describe concordance; they are not, by themselves, evidence of a shared cause."
  )
}
