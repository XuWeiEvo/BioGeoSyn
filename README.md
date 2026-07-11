# iBiogeobears

[![R-CMD-check](https://github.com/XuWeiEvo/iBioGeoBEARS/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/XuWeiEvo/iBioGeoBEARS/actions/workflows/R-CMD-check.yaml)

`iBiogeobears` is a reproducible workflow, synthesis, visualization, and
reporting layer for single-clade BioGeoBEARS analyses.

Current status: `0.2.1-alpha`, an alpha release candidate for small-scope
ordinary-user testing of single-clade BioGeoBEARS workflows.

It currently supports:

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
- launch a thin Shiny workflow runner;
- keep the R package backend separate from any future Shiny GUI wrapper.

## Installation

Install the current development version from GitHub:

```r
install.packages("remotes")
remotes::install_github("XuWeiEvo/iBioGeoBEARS")
```

Install the 0.2.1 alpha release:

```r
install.packages("remotes")
remotes::install_github("XuWeiEvo/iBioGeoBEARS@v0.2.1-alpha")
```

BioGeoBEARS must be installed separately; it is not bundled with
`iBiogeobears`. Quarto is required only when rendering reports.

For the ordinary-user path, open the installed guide after loading the package:

```r
library(iBiogeobears)
open_user_guide()
```

For the simplified Chinese guide:

```r
open_user_guide(language = "zh-CN")
```

The same guide is available in the repository at
[`inst/docs/ordinary-user-quick-start.md`](inst/docs/ordinary-user-quick-start.md).
The simplified Chinese version is available at
[`inst/docs/ordinary-user-quick-start.zh-CN.md`](inst/docs/ordinary-user-quick-start.zh-CN.md).

For Chinese ordinary-user testing, share the fuller user tutorial:
[`docs/user-tutorial.zh-CN.md`](docs/user-tutorial.zh-CN.md). A standalone
HTML version is also included at
[`docs/user-tutorial.zh-CN.html`](docs/user-tutorial.zh-CN.html).

## Continuous Integration

GitHub Actions runs `R CMD check --no-manual`, an installed-package smoke test,
the user-workflow smoke test, and the quick acceptance matrix on Linux, Windows,
and macOS for every push and pull request to `main`.

The CI workflow intentionally does not install or execute BioGeoBEARS. It
verifies package installation, exported APIs, bundled templates, bundled example
data, GUI-style config overrides, advanced constraint paths, report-source
rendering, cross-platform result bundling, diagnostic bundling, and the dry-run
workflow. Real BioGeoBEARS model execution should be validated on a local
machine where BioGeoBEARS is installed.

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

For one combined setup check covering the graphical interface, real model
execution, and report rendering, run:

```r
check_installation()
```

For a single end-to-end readiness result, run:

```r
acceptance <- run_acceptance_check(
  tempfile("iBiogeobears-acceptance-"),
  mode = "quick"
)
acceptance
```

Quick mode validates project creation, bundled inputs, the six-model dry-run
plan, report source generation, and result/diagnostic bundles without requiring
BioGeoBEARS, Shiny, or Quarto. Maintainers preparing a stable release should
run the stricter gate:

```r
acceptance <- run_acceptance_check(
  "stable-release-acceptance",
  mode = "full"
)
```

Full mode requires the ordinary-user environment, executes all six supported
models, verifies that the second run reuses every completed result, renders an
HTML report, and creates both bundles. Detailed results are saved in
`acceptance_results.csv`; failed steps include a recommended next action.

Inspect the complete BioGeoBEARS dependency plan without changing the R
installation:

```r
biogeobears_install_plan()
install_biogeobears()
```

To explicitly install missing dependencies and BioGeoBEARS from its official
GitHub repository:

```r
install_biogeobears(execute = TRUE)
```

The Shiny Setup section exposes the same plan and requires a confirmation
dialog before installation starts. The helper installs `MultinomialCI` 1.2
from the official CRAN Archive because it is no longer in the active CRAN
package index.

## Quick Start

This creates a self-contained example project, runs the bundled example data,
and renders an HTML report:

```r
library(iBiogeobears)

example <- tempfile("ibgb-example-")
project <- create_example_project(example)

result <- run_workflow(project$config, dry_run = FALSE)
render_report(result, format = "html")
```

For a no-BioGeoBEARS smoke test that only validates inputs and creates the
output scaffold:

```r
result <- run_workflow(project$config, dry_run = TRUE, require_biogeobears = FALSE)
```

For a broader local smoke test of the user workflow, including GUI-style config
overrides, advanced constraint paths, report-source rendering, result bundling,
and diagnostic bundling, run:

```r
source("tools/smoke-user-workflow.R")
```

## Command-Line Workflow

Create a runnable example project:

```r
library(iBiogeobears)

example <- create_example_project("example_run")
config <- example$config
```

Validate and plan without running BioGeoBEARS. This is the safest first check
on a new machine or after editing `analysis.yml`:

```r
dry <- run_workflow(config, dry_run = TRUE, require_biogeobears = FALSE)
```

Run the example with real BioGeoBEARS execution:

```r
result <- run_workflow(config, dry_run = FALSE)
```

Completed models are reused by default only when their saved run signature
matches the current input files, constraints, model, and BioGeoBEARS version.
To rerun every selected model:

```r
result <- run_workflow(
  config,
  dry_run = FALSE,
  resume_completed_models = FALSE
)
```

To retain valid completed results and execute only models marked failed in the
previous `model_run_status.csv`:

```r
result <- run_workflow(
  config,
  dry_run = FALSE,
  retry_failed_only = TRUE
)
```

Render a report and create a portable result archive:

```r
report <- render_report(result, format = "html")
bundle <- bundle_results(result)
```

Reload an existing result directory without rerunning BioGeoBEARS:

```r
result_dir <- result$project_paths$root
manifest <- create_workflow_manifest(result_dir)
```

## Shiny Workflow Runner

The Shiny entrypoint is a thin wrapper around the package backend. It validates
the YAML config, runs `run_workflow()`, renders reports, and bundles results
without moving scientific logic into Shiny server code. The default interface
now starts with a simplified Chinese `首页` tab and a focused `结果` tab. The main
results shown first are the best-model ancestral reconstruction, the model
comparison table, a range-change event summary, and the best-fit model event
timing/direction table. Advanced diagnostics, full tables, figure dashboards,
paths, and logs remain available under
`高级结果` and `排错`.

```r
install.packages("shiny")
launch_app()
```

Calling `launch_app()` without arguments prepares and loads a complete example
project automatically. The `环境检查` tab reports missing requirements and the
exact next action before a real analysis is started.

For a new analysis, use `使用自己的数据` in the sidebar:

1. Enter a project name.
2. Upload a Newick tree, geography CSV, and regions CSV.
3. Check the lightweight upload preview table.
4. Choose the maximum range size and models.
5. Click `创建自己的分析项目`.

The wizard copies the inputs into a portable project, writes `analysis.yml`,
loads it into the app, and immediately displays input validation results.
Template buttons provide working tree, geography, and regions files that can be
edited for a new analysis.

Recommended GUI flow:

1. Create a project with `使用自己的数据`, click `1 创建示例项目`,
   or provide an existing `analysis.yml`.
2. Click `2 检查输入`.
3. Keep `Dry run` checked for a first pass, then click `3 运行流程`.
4. Uncheck `Dry run` when BioGeoBEARS is installed and a real analysis is
   intended.
5. Click `4 生成报告`.
6. Review `结果` first: ancestral reconstruction, model comparison, and
   event summary.
7. Use `高级结果` only when you need the full tables and figure dashboard.
8. Use `排错` for warnings, failed models, logs, and output paths.

The Shiny result views are designed for triage:

- `首页`: current readiness checklist and the next action for setup,
  validation, workflow execution, report rendering, and exports.
- `结果`: the primary interpretation view with best-model ancestral
  reconstruction, model comparison, and event summary.
- `Model Comparison`: compact fit summary plus the full model-comparison table.
- `Event Details`: branch-level range-change events inferred from
  highest-probability ancestral states. These are quick summaries, not
  stochastic mapping event counts.
- `+J Sensitivity`: direct answers about whether `+J` is best or near-best,
  plus the detailed sensitivity table.
- `Warnings`: captured optimizer/BioGeoBEARS warnings and recommended review
  steps, including failed-model error messages and log paths.
- `Figure Dashboard`: expected workflow figures, preview status, missing
  reasons, and next steps.
- `Tables`: key CSV availability, row and column counts, missing reasons, next
  steps, and CSV previews.
- `About/Citation`: package version, GPL license, BioGeoBEARS availability,
  report rendering environment, citation guidance, and workflow log paths.
- `Messages`: timestamped progress messages for validation, workflow
  execution, output refresh, report rendering, and result bundling.

Use `check_report_environment()` or the Shiny `About/Citation` tab to confirm
report readiness. HTML reports require the Quarto R package and Quarto command
line tool. PDF reports also require a LaTeX engine such as TinyTeX, MiKTeX, or
TeX Live. If those tools are unavailable, `render_report()` writes the
`summary_report.qmd` source and returns that path.

For a browser-level GUI smoke test, install `shinytest2` and run:

```r
install.packages("shinytest2")
source("tools/smoke-shiny-browser.R")
```

Before publishing a new alpha release, use the release checklist:

```text
docs/release-checklist.md
```

If validation fails, real execution is blocked by default. Review:

```text
tables/input_validation.csv
```

Validation output includes a readable check name, `Passed` or
`Needs attention`, the technical detail, and a concrete repair step. The
machine-readable `check` and `ok` columns remain available for reproducibility.

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

The Shiny config editor exposes these same advanced constraint file fields.
Relative paths are resolved against the selected `analysis.yml` location when
the workflow writes its temporary GUI-edited config.

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
    workflow_manifest.csv
    model_run_status.csv
    model_fit_raw.csv
    model_comparison.csv
    model_sensitivity.csv
    geographic_states.csv
    tree_nodes.csv
    model_parameters.csv
    ancestral_state_probabilities.csv
    root_state_probabilities.csv
    node_state_summary.csv
    node_state_sensitivity.csv
    range_change_events.csv
    best_fit_events.csv
    event_summary.csv
    model_sensitivity.rds
  figures/
    figure_manifest.csv
    model_comparison.png
    model_comparison.pdf
    model_comparison.svg
    root_state_probabilities.png
    root_state_probabilities.pdf
    root_state_probabilities.svg
    node_state_summary_best_model.png
    node_state_summary_best_model.pdf
    node_state_summary_best_model.svg
    node_state_summary_best_non_j.png
    node_state_summary_best_plus_j.png
    node_state_sensitivity.png
    node_state_sensitivity.pdf
    node_state_sensitivity.svg
    event_summary.png
    event_summary.pdf
    event_summary.svg
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

## Reading Results

Start with these files:

```text
reports/summary_report.html
tables/shiny_run_summary.csv
tables/model_comparison.csv
tables/event_summary.csv
tables/best_fit_events.csv
tables/model_sensitivity.csv
tables/model_run_status.csv
figures/figure_manifest.csv
```

Use `model_comparison.csv` to inspect statistical fit. Use
`event_summary.csv` for a quick range-change overview inferred from
highest-probability ancestral states. Use `best_fit_events.csv` for approximate
best-fit event timing and source -> target direction. Use
`model_sensitivity.csv` to decide how to report `+J` sensitivity. Use
`model_run_status.csv` before interpretation to check failed models, warnings,
and log paths.

Do not treat the lowest AICc model as an automatic biological conclusion. The
report and Shiny app separate "best-fitting statistical model" from
interpretation, especially when a `+J` model is best or near-best.

Create a portable zip archive of a completed run with:

```r
bundle <- bundle_results(result)
```

To share derived outputs without raw BioGeoBEARS objects:

```r
bundle <- bundle_results(result, include_raw = FALSE)
```

To share only troubleshooting files after failures or warnings:

```r
diagnostics <- bundle_diagnostics(result)
```

The diagnostics archive contains configuration, model status tables, workflow
manifest, session metadata, BioGeoBEARS citation metadata, and log files. It
does not include full raw BioGeoBEARS result objects.

## Standard Tables

The main derived tables are:

- `model_run_status.csv`: per-model completion status, raw result path, log path,
  and error messages when present.
- `workflow_manifest.csv`: inventory of output files by category, relative path,
  extension, size, and modification time.
- `model_comparison.csv`: model family, `+J` status, log-likelihood, parameter
  count, AIC/AICc, weights, caution flags, and interpretation notes.
- `model_sensitivity.csv`: user-readable `+J` sensitivity summary and
  methodological guardrail checks.
- `geographic_states.csv`: state-space definition with state index, area
  composition, area count, and null-range flag.
- `tree_nodes.csv`: node metadata with node index, tip/internal status, labels,
  parent node, edge length, and root flag.
- `model_parameters.csv`: standardized BioGeoBEARS parameter table per model.
- `ancestral_state_probabilities.csv`: long-format node/state probabilities.
- `root_state_probabilities.csv`: root state probability table per model.
- `node_state_summary.csv`: highest-probability state per model, node, and
  probability location.
- `node_state_sensitivity.csv`: node-by-node comparison between the best
  non-`+J` and best `+J` models, including best-state changes and probability
  differences.
- `range_change_events.csv`: branch-level range-change details inferred from
  highest-probability ancestral states.
- `best_fit_events.csv`: changed branches under the best-fitting statistical
  model, including approximate branch-midpoint time and source -> target
  direction. This is a deterministic summary, not stochastic mapping output.
- `event_summary.csv`: counts of range expansion, local extinction, range
  shift, range origin, and no-change categories by model and probability
  location. This is not a stochastic mapping event-count table.

## Figures

Workflow execution generates:

- `model_comparison`: Delta AICc by model, with `+J` models marked separately.
- `root_state_probabilities`: highest-probability root range states by model.
- `node_state_summary_best_model`: best ancestral range state and probability
  on the tree for the top-ranked statistical model.
- `node_state_summary_best_non_j` and `node_state_summary_best_plus_j`: paired
  node-state plots for +J sensitivity interpretation when those model classes
  are available.
- `node_state_sensitivity`: ranked node-level differences between the best
  non-`+J` and best `+J` model.
- `event_summary`: branch-count summary of deterministic range-change
  categories inferred from ancestral states.

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
