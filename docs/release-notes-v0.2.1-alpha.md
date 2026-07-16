# BioGeoSyn 0.2.1 alpha

Third alpha release of the single-clade BioGeoBEARS workflow MVP.

This release focuses on ordinary-user readiness: clearer setup checks, safer
first-run behavior, Shiny guidance, reproducible smoke tests, and a stricter
full acceptance gate for real BioGeoBEARS execution.

## Highlights

- Added `run_acceptance_check()` with quick and full modes.
- Added a full six-model release gate covering real BioGeoBEARS execution,
  model reuse, HTML reporting, result bundling, and diagnostic bundling.
- Expanded GitHub Actions checks across Linux, Windows, and macOS.
- Added cross-platform result and diagnostic bundling through the R `zip`
  package.
- Added `open_user_guide()` and an installed ordinary-user quick-start and
  troubleshooting guide.
- Added a Shiny `Start Here` readiness checklist for setup, validation,
  workflow execution, report rendering, and exports.
- Added Shiny setup checks and BioGeoBEARS installation guidance with explicit
  user confirmation before installation.
- Added a no-YAML Shiny project wizard for tree, geography, and regions files.
- Added `create_analysis_project()` for building validated user projects from
  local input files.
- Added user-facing validation labels, pass/fail status, and repair guidance.
- Added safe model-result reuse using run signatures, plus failed-model-only
  retry mode.
- Added a clean GitHub-install smoke script for release readiness checks.

## BioGeoBEARS Dependency

BioGeoBEARS is not bundled with `BioGeoSyn`. Users must install BioGeoBEARS
separately for real model execution.

Run this in R to inspect local availability:

```r
check_biogeobears(required = FALSE)
check_installation()
```

Users should cite BioGeoBEARS directly:

```r
citation("BioGeoBEARS")
```

## Install

```r
install.packages("remotes")
remotes::install_github("XuWeiEvo/BioGeoSyn@v0.2.1-alpha")
```

For the ordinary-user path:

```r
library(BioGeoSyn)
open_user_guide()
launch_app()
```

## Verification

Before tagging this release, the following checks passed locally:

- complete `testthat`;
- local `R CMD check --no-manual`;
- clean GitHub install smoke test;
- real full acceptance check with all six supported BioGeoBEARS models;
- HTML report generation;
- result bundle creation with raw BioGeoBEARS outputs;
- diagnostic bundle creation;
- model reuse verification on the second run.

GitHub Actions should pass on Linux, Windows, and macOS before publishing the
release page.
