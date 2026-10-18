# ---- Topological and age uncertainty: one analysis per tree ----
#
# A stochastic map is a draw of the biogeographic history given one tree, so
# intervals from maps alone ignore uncertainty in topology and node ages
# (Magalhaes et al. 2021, Diversity 13: 418). run_workflow_over_trees() runs
# the usual single-clade workflow on each of a set of trees (for example a
# sample from a Bayesian posterior), writing one output directory and one
# result bundle per tree, named <clade>__treeNNN. The cross-clade synthesis
# recognises the suffix and resamples a clade's trees and maps jointly (tree
# first, then map), so pooled intervals carry tree uncertainty too.

#' Run the workflow on several trees of one clade
#'
#' @param config Path to the clade's YAML configuration file. Its tree is
#'   replaced in turn by each tree in `trees`; every other setting is kept.
#' @param trees Trees to analyse: a path to a file holding several trees
#'   (Newick or NEXUS), a directory of tree files, a character vector of tree
#'   files, or an `ape` `multiPhylo` object.
#' @param n_trees Optional number of trees to sample at random (without
#'   replacement) from `trees`. All trees are used when `NULL`.
#' @param seed Seed for sampling the trees.
#' @param output_dir Directory that receives one sub-directory per tree.
#'   Defaults to `<config output_dir>_trees`.
#' @param bsm_model Model used for stochastic mapping on every tree. Fixing
#'   it keeps the events comparable across trees; with `"best"` each tree may
#'   map under a different model. Defaults to the config setting.
#' @param bundle Logical. Write a result bundle (`.zip`) for each tree.
#' @param include_raw Logical. Include the raw BioGeoBEARS objects in each
#'   bundle (large); the cross-clade synthesis needs only the tables.
#' @param dry_run,force,require_biogeobears Passed to [run_workflow()].
#' @param progress Logical. Print one line per tree.
#' @return Invisibly, a data frame with one row per tree (`tree_id`,
#'   `source`, `output_dir`, `bundle`, `status`, `message`), also written to
#'   `tree_runs.csv` in `output_dir`.
#' @export
run_workflow_over_trees <- function(config, trees, n_trees = NULL, seed = 1L, output_dir = NULL,
                                    bsm_model = NULL, bundle = TRUE, include_raw = TRUE, dry_run = TRUE,
                                    force = FALSE, require_biogeobears = !dry_run,
                                    progress = TRUE) {
  cfg <- read_config(config)
  base_dir <- dirname(normalizePath(config, winslash = "/", mustWork = TRUE))
  tree_list <- read_tree_set(trees)
  if (length(tree_list) == 0L) {
    stop("No trees could be read from `trees`.", call. = FALSE)
  }
  ids <- seq_along(tree_list)
  if (!is.null(n_trees)) {
    n_trees <- min(as.integer(n_trees), length(tree_list))
    old <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv()) else NULL
    on.exit(if (is.null(old)) suppressWarnings(rm(".Random.seed", envir = globalenv())) else assign(".Random.seed", old, envir = globalenv()), add = TRUE)
    set.seed(as.integer(seed))
    ids <- sort(sample(ids, n_trees))
  }

  clade <- cfg$project$name %||% "clade"
  out_root <- output_dir %||% paste0(resolve_config_path(cfg$project$output_dir, base_dir), "_trees")
  dir.create(file.path(out_root, "trees"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(out_root, "configs"), recursive = TRUE, showWarnings = FALSE)

  # Inputs other than the tree keep pointing at the original files.
  for (key in c("geography_file", "regions_file")) {
    if (!is.null(cfg$inputs[[key]]) && nzchar(cfg$inputs[[key]])) {
      cfg$inputs[[key]] <- resolve_config_path(cfg$inputs[[key]], base_dir)
    }
  }
  constraints <- (cfg$advanced %||% list())$constraints
  if (is.list(constraints)) {
    for (key in names(constraints)) {
      if (is.character(constraints[[key]]) && nzchar(constraints[[key]])) {
        cfg$advanced$constraints[[key]] <- resolve_config_path(constraints[[key]], base_dir)
      }
    }
  }
  if (!is.null(bsm_model)) cfg$analysis$stochastic_mapping_model <- bsm_model
  width <- max(3L, nchar(as.character(length(tree_list))))

  rows <- lapply(ids, function(k) {
    tag <- sprintf(paste0("%s__tree%0", width, "d"), clade, k)
    tree_path <- file.path(out_root, "trees", paste0(tag, ".nwk"))
    ape::write.tree(tree_list[[k]], tree_path)
    run_cfg <- cfg
    run_cfg$.config_file <- NULL
    run_cfg$project$name <- tag
    run_cfg$project$output_dir <- as_path(file.path(out_root, tag))
    run_cfg$inputs$tree_file <- as_path(tree_path)
    cfg_path <- file.path(out_root, "configs", paste0(tag, ".yml"))
    yaml::write_yaml(run_cfg, cfg_path)
    if (isTRUE(progress)) message(sprintf("[%s] tree %d of %d (source tree %d)", clade, match(k, ids), length(ids), k))
    res <- tryCatch(
      run_workflow(cfg_path, dry_run = dry_run, force = force, require_biogeobears = require_biogeobears),
      error = function(e) e
    )
    status <- if (inherits(res, "error")) "failed" else if (isTRUE(dry_run)) "planned" else "completed"
    msg <- if (inherits(res, "error")) conditionMessage(res) else ""
    zip <- NA_character_
    if (isTRUE(bundle) && !isTRUE(dry_run) && !inherits(res, "error")) {
      zip <- tryCatch(
        bundle_results(res, bundle_file = file.path(out_root, paste0(tag, "_results.zip")),
                       include_raw = include_raw, overwrite = TRUE),
        error = function(e) {
          msg <<- paste("bundle failed:", conditionMessage(e))
          NA_character_
        }
      )
    }
    data.frame(tree_id = k, source = names(tree_list)[[k]] %||% as.character(k),
               output_dir = as_path(file.path(out_root, tag)), bundle = as_path(zip),
               status = status, message = msg, stringsAsFactors = FALSE)
  })
  runs <- do.call(rbind, rows)
  utils::write.csv(runs, file.path(out_root, "tree_runs.csv"), row.names = FALSE)
  invisible(runs)
}

# Read a set of trees from a multi-tree file, a directory, a vector of files,
# or a multiPhylo object, returning a named list of phylo objects.
read_tree_set <- function(trees) {
  if (inherits(trees, "multiPhylo")) {
    out <- unclass(trees)
    if (is.null(names(out))) names(out) <- paste0("tree", seq_along(out))
    return(out)
  }
  if (inherits(trees, "phylo")) {
    return(list(tree1 = trees))
  }
  trees <- as.character(trees)
  if (length(trees) == 1L && dir.exists(trees)) {
    files <- list.files(trees, pattern = "[.](nwk|newick|tre|tree|trees|nex|nexus|t)$", full.names = TRUE, ignore.case = TRUE)
    files <- files[order(as.integer(gsub("\\D", "", basename(files))), basename(files))]
    trees <- files
  }
  out <- list()
  for (f in trees) {
    tr <- read_tree_file(f)
    if (inherits(tr, "phylo")) tr <- list(tr)
    tr <- unclass(tr)
    nm <- if (length(tr) == 1L) tools::file_path_sans_ext(basename(f)) else paste0(tools::file_path_sans_ext(basename(f)), "_", seq_along(tr))
    names(tr) <- nm
    out <- c(out, tr)
  }
  out
}

read_tree_file <- function(f) {
  first <- tryCatch(readLines(f, n = 1L, warn = FALSE), error = function(e) "")
  tr <- if (length(first) && grepl("^#NEXUS", first, ignore.case = TRUE)) {
    tryCatch(ape::read.nexus(f), error = function(e) NULL)
  } else {
    tryCatch(ape::read.tree(f), error = function(e) NULL)
  }
  if (is.null(tr)) stop("Could not read trees from ", f, call. = FALSE)
  tr
}
