# iBiogeobears 0.1.0.9000

Development version after the 0.1.0 alpha release.

- Added standardized `geographic_states.csv`, `tree_nodes.csv`, and
  `node_state_summary.csv` tables for downstream plotting and future Shiny
  views.
- Added `plot_node_state_summary()` and automatic `node_state_summary` workflow
  figures.
- Added automatic best-overall, best-non-+J, and best-+J node-state figures
  where those model classes are available.
- Added `node_state_sensitivity.csv` to compare best non-+J and best +J
  node-state summaries.
- Added `plot_node_state_sensitivity()` and automatic node-state sensitivity
  figures for report-level +J interpretation.
- Added `workflow_manifest.csv` plus `create_workflow_manifest()` and
  `bundle_results()` for portable result sharing.
- Added a minimal Shiny workflow runner through `launch_app()` for validation,
  workflow execution, report rendering, and result bundling.
- Added Shiny status-message error handling plus report and result-bundle
  downloads.
- Added Shiny server-level tests for validation, dry-run workflow execution,
  report rendering, and result bundling.
- Updated GitHub Actions to treat optional `Suggests` packages as optional
  during dry-run CI checks.

# iBiogeobears 0.1.0

Initial alpha release of the single-clade BioGeoBEARS workflow MVP.

## Workflow

- Added YAML-driven project configuration and validation.
- Added `create_example_project()` for a runnable bundled example.
- Added `run_workflow()` to coordinate validation, project output creation,
  BioGeoBEARS execution, standardized tables, figures, logs, and reports.

## BioGeoBEARS integration

- Added `run_models()` support for DEC, DEC+J, DIVALIKE, DIVALIKE+J,
  BAYAREALIKE, and BAYAREALIKE+J.
- Added per-model raw output directories, `.rds` result files, logs, run status
  tables, and warning summaries.
- Added advanced YAML constraint fields for BioGeoBEARS input files.

## Outputs and reporting

- Added standardized model comparison, parameter, ancestral-state, root-state,
  and sensitivity tables.
- Added model comparison and root-state probability figures.
- Added Quarto report rendering with executive summary, model status, warnings,
  fit metrics, +J sensitivity summary, methodological cautions, and citation
  guidance.

## Methodological guardrails

- Added model comparison cautions around +J models.
- Added readable `model_sensitivity.csv` output.
- Kept automatic biological best-model declaration disabled by default.

## Infrastructure

- Added roxygen-generated documentation and NAMESPACE.
- Added installed-package smoke testing.
- Added GitHub Actions CI for dry-run package checks.
