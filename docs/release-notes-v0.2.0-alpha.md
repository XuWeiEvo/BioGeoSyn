# BioGeoSyn 0.2.0 alpha

Second alpha release of the single-clade BioGeoBEARS workflow MVP.

## Highlights

- Added a real `run_models()` bridge for DEC, DEC+J, DIVALIKE, DIVALIKE+J,
  BAYAREALIKE, and BAYAREALIKE+J.
- Added standardized model status, model comparison, parameter, root-state,
  node-state, and `+J` sensitivity tables.
- Added report summaries that separate best-fitting statistical models from
  biological interpretation, with explicit `+J` caution handling.
- Added publication-oriented model, root-state, node-state, and node-sensitivity
  figures.
- Added a Shiny workflow runner for validation, dry-run execution, report
  rendering, table/figure preview, result triage, and bundling.
- Added result bundles and lightweight diagnostic bundles.
- Added report environment checks for Quarto and PDF/LaTeX readiness.
- Added user-workflow smoke testing and GitHub Actions CI coverage.

## BioGeoBEARS Dependency

BioGeoBEARS is not bundled with `BioGeoSyn`. Users must install BioGeoBEARS
separately for real model execution.

Run this in R to inspect local availability:

```r
check_biogeobears(required = FALSE)
```

Users should cite BioGeoBEARS directly:

```r
citation("BioGeoBEARS")
```

## Install

```r
install.packages("remotes")
remotes::install_github("XuWeiEvo/BioGeoSyn@v0.2.0-alpha")
```

## Verification

Before tagging this release, the following checks should pass:

- local user-workflow smoke test;
- local complete `testthat`;
- local installed-package smoke test;
- local `R CMD check --no-manual`;
- GitHub Actions `R-CMD-check`;
- real BioGeoBEARS bundled example on a machine where BioGeoBEARS is installed.
