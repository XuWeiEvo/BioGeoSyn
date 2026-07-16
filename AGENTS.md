# BioGeoSyn Development Context

This project is the beginning of `BioGeoSyn`, a reproducible R workflow,
synthesis, visualization, and reporting package for BioGeoBEARS.

## Product Direction

`BioGeoSyn` is not only a plotting package. It should become a higher-level
workflow layer that can:

- accept a user tree, geographic range matrix, region definitions, and YAML
  parameters;
- call the BioGeoBEARS R package in the background;
- save raw BioGeoBEARS outputs;
- standardize outputs into clean tables;
- generate high-level synthesis results;
- produce publication-ready figures;
- render Quarto HTML/PDF reports;
- later expose the same backend through a Shiny GUI.

The architecture should remain:

1. Reproducible R package backend.
2. Optional Shiny GUI wrapper later.

The GUI must call package functions. Scientific logic should stay in the R
backend, not inside Shiny server code.

## Package Name

Package name: `BioGeoSyn`

Desktop project folder requested by the user: `BioGeoSyn`

## Core MVP Scope

Version 0.1 should run one clade at a time.

Required model support:

- DEC
- DEC+J
- DIVALIKE
- DIVALIKE+J
- BAYAREALIKE
- BAYAREALIKE+J

Stochastic mapping is optional and should be implemented after the main model
run workflow is stable.

Reports should be generated with Quarto, targeting HTML and PDF.

The MVP should include a small built-in example dataset.

## Input Design

Use a simplified YAML config with an optional `advanced:` section.

The current template lives at:

```text
inst/templates/analysis.yml
```

The simplified config should expose approachable fields for most users, while
`advanced:` can later pass expert settings into the BioGeoBEARS run object.

## Output Design

The workflow output directory should eventually look like:

```text
results/example_clade/
  inputs/
  raw_biogeobears/
  tables/
  figures/
  reports/
  logs/
  config_used.yml
```

Raw BioGeoBEARS output must remain separate from derived tables, figures, and
reports.

## Licensing and Citation

BioGeoBEARS is GPL (>= 2), authored by Nicholas J. Matzke. `BioGeoSyn`
should remain GPL compatible:

```text
License: GPL (>= 2)
```

Do not bundle BioGeoBEARS source code. Users must install BioGeoBEARS
separately. Runtime checks should use:

```r
requireNamespace("BioGeoBEARS", quietly = TRUE)
```

Reports, README, and GUI About pages should visibly acknowledge BioGeoBEARS and
tell users to run:

```r
citation("BioGeoBEARS")
```

Every workflow should save session information and BioGeoBEARS version/citation
metadata where possible.

## Methodological Guardrails

The package should not encourage users to blindly pick the lowest-AIC model,
especially for DEC+J and other `+J` models.

Keep these defaults:

```yaml
methodology:
  show_decj_caution: true
  report_model_uncertainty: true
  separate_j_and_no_j_comparisons: true
  auto_declare_best_model: false
  require_sensitivity_summary: true
```

Model comparison outputs should include at least:

```text
model
model_family
has_j
logLik
num_params
AIC
AICc
delta_aicc
aicc_weight
caution_flag
interpretation_note
```

The report should separate "best-fitting statistical model" from biological
interpretation.

If a `+J` model is best or near-best, the software should flag sensitivity and
interpretation cautions rather than declaring a simple answer.

## Current Scaffold

The current scaffold includes:

- `DESCRIPTION`
- `NAMESPACE`
- `README.md`
- `inst/CITATION`
- `inst/templates/analysis.yml`
- `inst/templates/report_template.qmd`
- `inst/example_data/`
- `R/checks.R`
- `R/config.R`
- `R/project.R`
- `R/validate_inputs.R`
- `R/model_comparison.R`
- `R/run_models.R`
- `R/workflow.R`
- `R/plots.R`
- `R/report.R`
- `R/shiny_app.R`
- `tests/testthat/`
- `man/BioGeoSyn-api.Rd`

Verification already performed:

- All R files parse successfully.
- All R source files load successfully in dependency-light checks.
- Smoke check passed for `compare_models()` and `create_project()`.
- `R CMD check` stops at missing local R dependencies:
  `yaml`, `ggplot2`, `igraph`, `ggraph`, `ape`, `testthat`, `quarto`,
  `knitr`, and `rmarkdown`.

This is an environment limitation, not a known syntax failure.

## Next Engineering Step

Implement the real BioGeoBEARS bridge in `run_models()`:

1. Translate YAML config into a BioGeoBEARS run object.
2. Set model parameters for DEC, DEC+J, DIVALIKE, DIVALIKE+J, BAYAREALIKE,
   BAYAREALIKE+J.
3. Run each model into its own `raw_biogeobears/<model>/` directory.
4. Save raw `.rds` outputs and logs.
5. Extract a model comparison table.
6. Feed that table through `compare_models()` and
   `assess_model_sensitivity()`.

Keep implementation small and testable. Prefer adding narrow functions instead
of growing one large workflow function.
