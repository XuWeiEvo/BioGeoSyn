args <- commandArgs(trailingOnly = TRUE)
repo <- normalizePath(if (length(args) >= 1L) args[[1L]] else ".", winslash = "/", mustWork = TRUE)
mode <- if (length(args) >= 2L) args[[2L]] else "quick"

lib <- tempfile("bgs-acceptance-lib-")
dir.create(lib, recursive = TRUE, showWarnings = FALSE)
install.packages(repo, lib = lib, repos = NULL, type = "source")
.libPaths(c(lib, .libPaths()))

library(BioGeoSyn)

output <- tempfile("bgs-acceptance-")
result <- run_acceptance_check(output, mode = mode)
print(result)

if (!isTRUE(result$passed)) {
  stop("BioGeoSyn acceptance check failed. Results: ", result$results_file, call. = FALSE)
}
