#' Check for an external BioGeoBEARS installation
#'
#' BioGeoBEARS is intentionally not bundled with iBiogeobears. This function
#' checks whether it is available and provides installation and citation helper
#' messages.
#'
#' @param required Logical. If `TRUE`, stop when BioGeoBEARS is missing.
#' @return A list with availability, version, package path, citation, and helper
#'   text.
#' @export
check_biogeobears <- function(required = TRUE) {
  available <- requireNamespace("BioGeoBEARS", quietly = TRUE)

  install_help <- paste(
    "BioGeoBEARS is required for running analyses but is not bundled with",
    "iBiogeobears. Inspect the installation plan with",
    "iBiogeobears::biogeobears_install_plan(), then explicitly install with",
    "iBiogeobears::install_biogeobears(execute = TRUE)."
  )

  if (!available) {
    if (isTRUE(required)) {
      stop(install_help, call. = FALSE)
    }
    return(list(
      available = FALSE,
      version = NA_character_,
      path = NA_character_,
      citation = NA_character_,
      install_help = install_help
    ))
  }

  citation_text <- tryCatch(
    paste(utils::capture.output(utils::citation("BioGeoBEARS")), collapse = "\n"),
    error = function(e) "Run citation('BioGeoBEARS') for citation details."
  )

  list(
    available = TRUE,
    version = as.character(utils::packageVersion("BioGeoBEARS")),
    path = find.package("BioGeoBEARS"),
    citation = citation_text,
    install_help = install_help
  )
}

#' Check whether iBiogeobears is ready for common user workflows
#'
#' Summarizes the local R, package, BioGeoBEARS, Shiny, and report-rendering
#' environment in one user-facing table. Missing optional PDF support does not
#' prevent model execution or HTML reporting.
#'
#' @param include_pdf Logical. Include the optional PDF-report check.
#' @return A data frame with component status, purpose, version, and a
#'   recommended next step.
#' @export
check_installation <- function(include_pdf = TRUE) {
  core_packages <- c("yaml", "ggplot2", "igraph", "ggraph", "ape", "zip")
  core_status <- do.call(rbind, lapply(core_packages, package_namespace_status))
  core_available <- core_status$available
  core_versions <- stats::setNames(
    core_status$version[core_available],
    core_status$package[core_available]
  )

  bgb <- check_biogeobears(required = FALSE)
  shiny_available <- requireNamespace("shiny", quietly = TRUE)
  html <- check_report_environment("html")

  rows <- list(
    installation_check_row(
      "R",
      "All workflows",
      TRUE,
      getRversion() >= "4.1",
      as.character(getRversion()),
      "Install R 4.1 or newer."
    ),
    installation_check_row(
      "Core R packages",
      "All workflows",
      TRUE,
      all(core_available),
      if (length(core_versions) > 0L) {
        paste(paste(names(core_versions), core_versions, sep = " "), collapse = "; ")
      } else {
        NA_character_
      },
      core_package_next_step(core_status)
    ),
    installation_check_row(
      "Shiny",
      "Graphical interface",
      TRUE,
      shiny_available,
      if (shiny_available) as.character(utils::packageVersion("shiny")) else NA_character_,
      "Install the graphical interface dependency with install.packages('shiny')."
    ),
    installation_check_row(
      "BioGeoBEARS",
      "Real model execution",
      TRUE,
      isTRUE(bgb$available),
      bgb$version,
      bgb$install_help
    ),
    installation_check_row(
      "Quarto HTML",
      "HTML reports",
      FALSE,
      isTRUE(html$available[[1L]]),
      html$quarto_version[[1L]],
      html$next_step[[1L]]
    )
  )

  if (isTRUE(include_pdf)) {
    pdf <- check_report_environment("pdf")
    rows[[length(rows) + 1L]] <- installation_check_row(
      "Quarto PDF",
      "PDF reports",
      FALSE,
      isTRUE(pdf$available[[1L]]),
      pdf$quarto_version[[1L]],
      pdf$next_step[[1L]]
    )
  }

  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

package_namespace_status <- function(package) {
  error_message <- NA_character_
  available <- tryCatch(
    {
      loadNamespace(package)
      TRUE
    },
    error = function(e) {
      error_message <<- conditionMessage(e)
      FALSE
    }
  )
  installed <- package %in% row.names(utils::installed.packages())
  version <- if (available) {
    as.character(utils::packageVersion(package))
  } else {
    NA_character_
  }
  data.frame(
    package = package,
    installed = installed,
    available = available,
    version = version,
    error_message = error_message,
    stringsAsFactors = FALSE
  )
}

core_package_next_step <- function(status) {
  unavailable <- status[!status$available, , drop = FALSE]
  if (nrow(unavailable) == 0L) {
    return("Ready.")
  }

  missing <- unavailable$package[!unavailable$installed]
  unloadable <- unavailable[unavailable$installed, , drop = FALSE]
  guidance <- character()
  if (length(missing) > 0L) {
    guidance <- c(
      guidance,
      paste0(
        "Install missing packages: install.packages(c(",
        paste(sprintf("'%s'", missing), collapse = ", "),
        "))."
      )
    )
  }
  if (nrow(unloadable) > 0L) {
    errors <- paste0(
      unloadable$package,
      ": ",
      unloadable$error_message
    )
    guidance <- c(
      guidance,
      paste0(
        "Installed package(s) could not be loaded: ",
        paste(unloadable$package, collapse = ", "),
        ". Load errors: ",
        paste(errors, collapse = "; "),
        ". On Debian/Ubuntu, install libglpk-dev and libxml2-dev before ",
        "reinstalling igraph and ggraph."
      )
    )
  }
  paste(guidance, collapse = " ")
}

installation_check_row <- function(component, required_for, required, available, version, next_step) {
  data.frame(
    component = component,
    required_for = required_for,
    required = if (isTRUE(required)) "yes" else "no",
    status = if (isTRUE(available)) "Ready" else "Action needed",
    version = if (is.null(version) || length(version) == 0L || is.na(version[[1L]])) {
      NA_character_
    } else {
      as.character(version[[1L]])
    },
    next_step = if (isTRUE(available)) "Ready." else as.character(next_step %||% "Install the missing component."),
    stringsAsFactors = FALSE
  )
}

#' Plan a BioGeoBEARS installation
#'
#' Checks the complete BioGeoBEARS 1.1.x import set plus BioGeoBEARS itself.
#' This function never installs or updates packages.
#'
#' @return A data frame listing package source, availability, version, and the
#'   next installation step.
#' @export
biogeobears_install_plan <- function() {
  dependencies <- biogeobears_cran_dependencies()
  archived <- biogeobears_archived_dependencies()
  packages <- c(dependencies, names(archived), "BioGeoBEARS")
  available <- vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  versions <- vapply(packages, installed_package_version, character(1))
  sources <- c(
    rep("CRAN", length(dependencies)),
    rep("CRAN Archive", length(archived)),
    "GitHub"
  )

  plan <- data.frame(
    package = packages,
    source = sources,
    status = ifelse(available, "Ready", "Action needed"),
    version = versions,
    next_step = ifelse(
      available,
      "No action needed.",
      ifelse(
        sources == "CRAN",
        paste0("Install from CRAN: install.packages('", packages, "')."),
        ifelse(
          sources == "CRAN Archive",
          "Install the archived MultinomialCI 1.2 source package from CRAN.",
          "Install from the official nmatzke/BioGeoBEARS GitHub repository."
        )
      )
    ),
    stringsAsFactors = FALSE
  )
  row.names(plan) <- NULL
  plan
}

#' Install BioGeoBEARS and its R package dependencies
#'
#' By default this function only returns an installation plan. Set
#' `execute = TRUE` to install missing CRAN dependencies and then install
#' BioGeoBEARS from its official GitHub repository.
#'
#' @param execute Logical. Perform installation only when explicitly `TRUE`.
#' @param lib Optional R library directory. Defaults to the first writable
#'   library in `.libPaths()`.
#' @param repos CRAN repository passed to [utils::install.packages()].
#' @param github_repo GitHub repository used for BioGeoBEARS.
#' @param force Logical. Reinstall BioGeoBEARS even when it is already
#'   available.
#' @return When `execute = FALSE`, an installation-plan data frame. Otherwise,
#'   a list containing the library path, final plan, and BioGeoBEARS check.
#' @export
install_biogeobears <- function(
    execute = FALSE,
    lib = NULL,
    repos = NULL,
    github_repo = "nmatzke/BioGeoBEARS",
    force = FALSE) {
  plan <- biogeobears_install_plan()
  if (!isTRUE(execute)) {
    return(plan)
  }

  lib <- resolve_install_library(lib)
  repos <- resolve_cran_repositories(repos)
  .libPaths(unique(c(lib, .libPaths())))

  dependency_rows <- plan$source == "CRAN" & plan$status != "Ready"
  missing_dependencies <- plan$package[dependency_rows]
  if (length(missing_dependencies) > 0L) {
    utils::install.packages(
      missing_dependencies,
      lib = lib,
      repos = repos,
      dependencies = c("Depends", "Imports", "LinkingTo")
    )
  }

  archived <- biogeobears_archived_dependencies()
  missing_archived <- names(archived)[
    !vapply(names(archived), requireNamespace, logical(1), quietly = TRUE)
  ]
  for (package in missing_archived) {
    utils::install.packages(
      archived[[package]],
      lib = lib,
      repos = NULL,
      type = "source"
    )
  }

  required_dependencies <- c(biogeobears_cran_dependencies(), names(archived))
  remaining <- required_dependencies[
    !vapply(required_dependencies, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(remaining) > 0L) {
    stop(
      "Could not install required BioGeoBEARS dependencies: ",
      paste(remaining, collapse = ", "),
      ". Review the installation messages and try these packages individually.",
      call. = FALSE
    )
  }

  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop("The devtools package is required to install BioGeoBEARS from GitHub.", call. = FALSE)
  }
  if (isTRUE(force) || !requireNamespace("BioGeoBEARS", quietly = TRUE)) {
    devtools::install_github(
      repo = github_repo,
      dependencies = FALSE,
      upgrade = "never",
      force = isTRUE(force),
      lib = lib
    )
  }

  final_check <- check_biogeobears(required = TRUE)
  list(
    library = as_path(lib),
    plan = biogeobears_install_plan(),
    biogeobears = final_check
  )
}

biogeobears_cran_dependencies <- function() {
  c(
    "optimx", "plotrix", "gdata", "GenSA", "rexpokit", "cladoRcpp", "ape",
    "phylobase", "phytools", "FD", "minqa", "expm", "devtools", "fdrtool",
    "httr", "statmod", "SparseM", "spam", "stringr"
  )
}

biogeobears_archived_dependencies <- function() {
  c(
    MultinomialCI = paste0(
      "https://cran.r-project.org/src/contrib/Archive/MultinomialCI/",
      "MultinomialCI_1.2.tar.gz"
    )
  )
}

installed_package_version <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    return(NA_character_)
  }
  as.character(utils::packageVersion(package))
}

resolve_install_library <- function(lib = NULL) {
  if (!is.null(lib)) {
    lib <- as_path(lib)
    dir.create(lib, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(lib) || file.access(lib, mode = 2) != 0L) {
      stop("R library is not writable: ", lib, call. = FALSE)
    }
    return(lib)
  }

  candidates <- .libPaths()
  writable <- candidates[file.access(candidates, mode = 2) == 0L]
  if (length(writable) == 0L) {
    stop(
      "No writable R library was found. Create a user library and pass it with the lib argument.",
      call. = FALSE
    )
  }
  as_path(writable[[1L]])
}

resolve_cran_repositories <- function(repos = NULL) {
  repos <- repos %||% getOption("repos")
  if (is.null(repos) || length(repos) == 0L || all(is.na(repos) | repos == "@CRAN@")) {
    repos <- c(CRAN = "https://cloud.r-project.org")
  } else {
    repos[is.na(repos) | repos == "@CRAN@"] <- "https://cloud.r-project.org"
  }
  repos
}
