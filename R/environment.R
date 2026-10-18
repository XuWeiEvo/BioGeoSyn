#' Record the computational environment of a run
#'
#' Writes one row per package loaded in the session, plus R itself, with the
#' installed version and, for packages installed from GitHub, the repository
#' and commit. BioGeoBEARS is always included when it is installed, because it
#' is installed separately from GitHub and its version is the one most likely
#' to differ between machines. The table is what [check_environment()] compares
#' against when a bundle is re-run.
#'
#' @param path File to write (CSV). `NULL` returns the table without writing.
#' @param packages Packages to record in addition to those loaded.
#' @return A data frame, invisibly when `path` is given.
#' @export
record_environment <- function(path = NULL, packages = c("BioGeoSyn", "BioGeoBEARS")) {
  pkgs <- unique(c(packages, loadedNamespaces()))
  pkgs <- setdiff(pkgs, c("base", "compiler", "datasets", "graphics", "grDevices", "grid",
                          "methods", "parallel", "splines", "stats", "stats4", "tcltk", "tools", "utils"))
  rows <- lapply(sort(pkgs), package_provenance)
  rows <- rows[!vapply(rows, is.null, logical(1))]
  r <- data.frame(package = "R", version = paste(R.version$major, R.version$minor, sep = "."),
                  source = "R", remote_repo = NA_character_, remote_sha = NA_character_,
                  stringsAsFactors = FALSE)
  out <- do.call(rbind, c(list(r), rows))
  rownames(out) <- NULL
  if (is.null(path)) return(out)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(out, path, row.names = FALSE)
  invisible(out)
}

package_provenance <- function(pkg) {
  d <- suppressWarnings(tryCatch(utils::packageDescription(pkg), error = function(e) NA))
  if (!is.list(d)) return(NULL)
  field <- function(x) if (is.null(d[[x]]) || !nzchar(d[[x]])) NA_character_ else as.character(d[[x]])
  remote_type <- field("RemoteType")
  github <- identical(tolower(remote_type), "github")
  source <- if (github) "GitHub" else if (!is.na(field("Repository"))) field("Repository") else remote_type
  data.frame(
    package = pkg,
    version = field("Version"),
    source = if (is.na(source)) "local" else source,
    remote_repo = if (github) paste0(field("RemoteUsername"), "/", field("RemoteRepo")) else NA_character_,
    remote_sha = if (github) field("RemoteSha") else NA_character_,
    stringsAsFactors = FALSE
  )
}

#' Compare the current environment with the one a run recorded
#'
#' Reads the environment table written with a run (`logs/environment.csv`) and
#' reports, for each recorded package, whether the installed version matches.
#' For every difference it gives the command that installs the recorded
#' version: `remotes::install_github("owner/repo@commit")` for GitHub packages
#' such as BioGeoBEARS and `remotes::install_version()` for CRAN packages. The
#' check does not install anything. Recreating an environment exactly may also
#' require the recorded R version and system libraries, which a container or a
#' lockfile (for example with renv) can provide.
#'
#' @param path The recorded environment table.
#' @param packages Packages to check. By default R, BioGeoSyn, BioGeoBEARS and
#'   the packages BioGeoBEARS depends on most directly.
#' @param quiet If `FALSE`, print a summary.
#' @return A data frame with columns `package`, `recorded`, `installed`,
#'   `status` (`"match"`, `"differs"` or `"missing"`) and `install_command`,
#'   invisibly.
#' @export
check_environment <- function(path = "logs/environment.csv",
                              packages = c("R", "BioGeoSyn", "BioGeoBEARS", "ape", "GenSA",
                                           "optimx", "rexpokit", "cladoRcpp", "FD"),
                              quiet = FALSE) {
  if (!file.exists(path)) {
    stop("No recorded environment at ", path, ". Runs made with BioGeoSyn 1.2.0.9000 or later record one.",
         call. = FALSE)
  }
  rec <- utils::read.csv(path, stringsAsFactors = FALSE, na.strings = c("NA", ""))
  if (!is.null(packages)) rec <- rec[rec$package %in% packages, , drop = FALSE]
  now <- record_environment(NULL, packages = setdiff(rec$package, "R"))
  installed <- now$version[match(rec$package, now$package)]
  installed_sha <- now$remote_sha[match(rec$package, now$package)]
  same_sha <- is.na(rec$remote_sha) | (!is.na(installed_sha) & installed_sha == rec$remote_sha)
  status <- ifelse(is.na(installed), "missing",
                   ifelse(installed == rec$version & same_sha, "match", "differs"))
  cmd <- ifelse(rec$package == "R", paste0("install R ", rec$version),
                ifelse(!is.na(rec$remote_repo) & !is.na(rec$remote_sha),
                       paste0('remotes::install_github("', rec$remote_repo, "@", rec$remote_sha, '")'),
                       paste0('remotes::install_version("', rec$package, '", "', rec$version, '")')))
  out <- data.frame(package = rec$package, recorded = rec$version, installed = installed,
                    status = status, install_command = ifelse(status == "match", NA_character_, cmd),
                    stringsAsFactors = FALSE)
  if (!quiet) {
    bad <- out[out$status != "match", , drop = FALSE]
    if (nrow(bad) == 0L) {
      message("The installed versions match those recorded for this run.")
    } else {
      message("These packages differ from the versions recorded for this run; results may differ:")
      for (i in seq_len(nrow(bad))) {
        message("  ", bad$package[[i]], ": recorded ", bad$recorded[[i]], ", installed ",
                bad$installed[[i]] %|NA|% "none", ". To install the recorded version: ",
                bad$install_command[[i]])
      }
    }
  }
  invisible(out)
}

`%|NA|%` <- function(x, y) if (is.na(x)) y else x
