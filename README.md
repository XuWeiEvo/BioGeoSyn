# iBiogeobears

`iBiogeobears` is a reproducible workflow, synthesis, visualization, and
reporting layer for single-clade BioGeoBEARS analyses.

It currently supports an end-to-end MVP workflow:

- read a simple YAML configuration file;
- validate tree, geography, region metadata, model settings, constraint files,
  and output paths;
- build BioGeoBEARS run objects from the YAML configuration;
- run DEC, DEC+J, DIVALIKE, DIVALIKE+J, BAYAREALIKE, and BAYAREALIKE+J;
- save raw BioGeoBEARS `.rds` outputs and per-model logs;
- standardize raw outputs into reusable tables;
- compare models with methodological cautions around `+J`;
- generate publication-oriented figures;
- render Quarto reports;
- keep the R package backend separate from any future Shiny GUI wrapper.

## BioGeoBEARS Dependency

`iBiogeobears` does not bundle BioGeoBEARS. Users must install BioGeoBEARS
separately.

Typical installation:

```r
install.packages(c("devtools", "rexpokit", "cladoRcpp"))
devtools::install_github("nmatzke/BioGeoBEARS", dependencies = FALSE)
```

The workflow checks BioGeoBEARS at runtime with:

```r
requireNamespace("BioGeoBEARS", quietly = TRUE)
```

You can inspect the local BioGeoBEARS status with:

```r
check_biogeobears(required = FALSE)
```

## Minimal Workflow

Create a runnable example project:

```r
library(iBiogeobears)

example <- create_example_project("example_run")
config <- example$config
```

Validate and plan without running BioGeoBEARS:

```r
result <- run_workflow(config, dry_run = TRUE)
```

Run the example:

```r
result <- run_workflow(config, dry_run = FALSE)
```

Render a report:

```r
report <- render_report(result, format = "html")
```

If validation fails, real execution is blocked by default. Review:

```text
tables/input_validation.csv
```

Only override this after reviewing the failure:

```r
result <- run_workflow(config, dry_run = FALSE, force = TRUE)
```

## YAML Configuration

The template lives at:

```text
inst/templates/analysis.yml
```

For a user-editable copy, run:

```r
create_example_project("example_run")
```

Core fields:

```yaml
project:
  name: example_clade
  output_dir: results/example_clade

inputs:
  tree_file: ../example_data/tree.nwk
  geography_file: ../example_data/geography.csv
  regions_file: ../example_data/regions.csv
  max_range_size: 3

models:
  run:
    - DEC
    - DEC+J
    - DIVALIKE
    - DIVALIKE+J
    - BAYAREALIKE
    - BAYAREALIKE+J
```

Advanced BioGeoBEARS file constraints can be supplied under:

```yaml
advanced:
  constraints:
    times_file: null
    dists_file: null
    dispersal_multipliers_file: null
    areas_allowed_file: null
    areas_adjacency_file: null
    area_of_areas_file: null
```

## Outputs

A completed workflow writes a directory like:

```text
results/example_clade/
  inputs/
    tree.nwk
    geography.csv
    geography.data
  raw_biogeobears/
    DEC/
    DEC+J/
    DIVALIKE/
    DIVALIKE+J/
    BAYAREALIKE/
    BAYAREALIKE+J/
  tables/
    input_validation.csv
    model_run_status.csv
    model_fit_raw.csv
    model_comparison.csv
    model_parameters.csv
    ancestral_state_probabilities.csv
    root_state_probabilities.csv
    model_sensitivity.rds
  figures/
    figure_manifest.csv
    model_comparison.png
    model_comparison.pdf
    model_comparison.svg
    root_state_probabilities.png
    root_state_probabilities.pdf
    root_state_probabilities.svg
  reports/
    summary_report.qmd
    summary_report.html
  logs/
    session_info.txt
    biogeobears_citation.txt
  config_used.yml
```

Raw BioGeoBEARS outputs remain separate from derived tables, figures, and
reports.

## Standard Tables

The main derived tables are:

- `model_run_status.csv`: per-model completion status, raw result path, log path,
  and error messages when present.
- `model_comparison.csv`: model family, `+J` status, log-likelihood, parameter
  count, AIC/AICc, weights, caution flags, and interpretation notes.
- `model_parameters.csv`: standardized BioGeoBEARS parameter table per model.
- `ancestral_state_probabilities.csv`: long-format node/state probabilities.
- `root_state_probabilities.csv`: root state probability table per model.

## Figures

Workflow execution generates:

- `model_comparison`: Delta AICc by model, with `+J` models marked separately.
- `root_state_probabilities`: highest-probability root range states by model.

Figures are written in the formats configured by:

```yaml
figures:
  output_formats:
    - pdf
    - png
    - svg
```

## Methodological Position

`iBiogeobears` is intentionally not a "pick the lowest AIC and stop" tool.
Historical biogeography model choice, especially comparisons involving `+J`
founder-event speciation models, requires interpretation.

Defaults keep these guardrails active:

```yaml
methodology:
  show_decj_caution: true
  report_model_uncertainty: true
  separate_j_and_no_j_comparisons: true
  auto_declare_best_model: false
  require_sensitivity_summary: true
```

The report separates statistical fit from biological interpretation. If a `+J`
model is best or near-best, the software flags sensitivity and interpretation
cautions instead of declaring a simple biological conclusion.

## Citation and License

BioGeoBEARS is authored by Nicholas J. Matzke and is licensed under GPL
compatible terms. `iBiogeobears` is released under:

```text
GPL (>= 2)
```

Users should cite BioGeoBEARS directly:

```r
citation("BioGeoBEARS")
```

Workflow reports include a software citation section and write BioGeoBEARS
citation metadata to:

```text
logs/biogeobears_citation.txt
```
