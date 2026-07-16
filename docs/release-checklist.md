# BioGeoSyn Release Checklist

Use this checklist before publishing the next alpha release.

## Version Decision

- Choose the release tag:
  - `v0.1.1-alpha` for an incremental alpha after `v0.1.0-alpha`.
  - `v0.2.0-alpha` if the release should signal a larger feature step.
  - `v0.2.1-alpha` for the ordinary-user readiness alpha after `v0.2.0-alpha`.
- Update `DESCRIPTION` from the development version to the chosen release
  version.
- Update `README.md` installation text if the recommended alpha tag changes.
- Update `NEWS.md` so the release section matches the chosen version.
- Confirm `open_user_guide()` points to the current ordinary-user quick-start
  and troubleshooting guide.

## Required Local Checks

Run these from the repository root:

```r
source("tools/smoke-user-workflow.R")
```

```r
library(BioGeoSyn)
open_user_guide(browse = FALSE)
```

```powershell
& 'C:\Program Files\R\R-4.3.1\bin\Rscript.exe' tools\smoke-clean-github-install.R
```

```powershell
& 'C:\Program Files\R\R-4.3.1\bin\Rscript.exe' -e ".libPaths(c('C:/Users/xuwei/AppData/Local/R/win-library/4.3', .libPaths())); pkgload::load_all('.', quiet=TRUE); testthat::test_dir('tests/testthat', reporter='summary')"
```

```powershell
& 'C:\Program Files\R\R-4.3.1\bin\Rscript.exe' tools\smoke-installed-package.R .
```

```powershell
& 'C:\Program Files\R\R-4.3.1\bin\Rscript.exe' tools\smoke-shiny-browser.R .
```

Build/check from a clean source copy that excludes `.git`, because Windows path
lengths inside `.git/refs/codex/` can break `R CMD build .`:

```powershell
$src = (Resolve-Path -LiteralPath '.').Path
$tmp = Join-Path $env:TEMP ('bgs-build-src-' + [guid]::NewGuid().ToString())
New-Item -ItemType Directory -Path $tmp | Out-Null
Get-ChildItem -LiteralPath $src -Force |
  Where-Object { $_.Name -notin @('.git','BioGeoSyn.Rcheck','BioGeoSyn_*.tar.gz','BioGeoSyn-scaffold.zip') } |
  Copy-Item -Destination $tmp -Recurse -Force
& 'C:\Program Files\R\R-4.3.1\bin\R.exe' CMD build $tmp
& 'C:\Program Files\R\R-4.3.1\bin\R.exe' CMD check --no-manual BioGeoSyn_*.tar.gz
```

Expected result:

```text
Status: OK
```

## GitHub Actions Gate

The latest `main` push must pass:

- `R CMD check --no-manual`
- installed-package smoke test
- user-workflow smoke test

The CI workflow intentionally does not install or execute BioGeoBEARS.

## BioGeoBEARS Local Gate

On a machine with BioGeoBEARS installed, run a real bundled example:

```r
library(BioGeoSyn)

example <- create_example_project(tempfile("bgs-release-real-"))
result <- run_workflow(example$config, dry_run = FALSE)
render_report(result, format = "source")
bundle_results(result, overwrite = TRUE)
bundle_diagnostics(result, overwrite = TRUE)
```

Confirm:

- `tables/model_run_status.csv` has expected completed models or clearly
  reported failures.
- `tables/model_comparison.csv` exists after real model execution.
- `tables/model_sensitivity.csv` exists.
- `logs/biogeobears_citation.txt` exists.
- `raw_biogeobears/<model>/` directories are present.

## Release Notes

The release notes should highlight:

- real `run_models()` bridge for DEC, DEC+J, DIVALIKE, DIVALIKE+J,
  BAYAREALIKE, and BAYAREALIKE+J;
- standardized model/status/sensitivity/root/node tables;
- report executive summary and methodological `+J` caution;
- Shiny workflow runner and result views;
- result and diagnostic bundles;
- CI and smoke-test coverage;
- BioGeoBEARS is external and must be installed separately.

## Tag And Release

After checks pass:

```powershell
git status --short
git tag vX.Y.Z-alpha
git push origin main
git push origin vX.Y.Z-alpha
```

Then create the GitHub release from the tag and mark it as pre-release if it is
not intended as a stable production release.

## Post-Release

- Confirm the release page installation command works:

```r
remotes::install_github("XuWeiEvo/BioGeoSyn@vX.Y.Z-alpha")
```

- Return `DESCRIPTION` to the next development version if needed.
