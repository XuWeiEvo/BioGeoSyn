# iBiogeobears 0.2.0.9000

Development toward a stable, ordinary-user release.

- Added `run_acceptance_check()` with a CI-friendly quick mode and a full
  six-model stability gate covering model reuse, HTML reporting, result
  bundling, and diagnostic bundling.
- Added a saved acceptance matrix with platform, R version, package version,
  elapsed time, failure details, and actionable next steps.
- Added the quick acceptance matrix to GitHub Actions.
- Expanded GitHub Actions quick checks across Linux, Windows, and macOS.
- Switched result and diagnostic bundling to the R `zip` package when available
  for more reliable cross-platform archive creation.
- Added Linux GLPK/libxml2 CI dependencies and actionable diagnostics for R
  packages that are installed but cannot load because a system library is
  missing.
- Added a unified installation-readiness check for R, package dependencies,
  Shiny, BioGeoBEARS, and report rendering.
- Made the no-argument Shiny launch open with a complete, valid example project
  instead of a template with unresolved relative input paths.
- Added a Shiny Setup page with refreshable environment checks and actionable
  installation guidance.
- Added a no-YAML Shiny project wizard for tree, geography, and region uploads.
- Added `create_analysis_project()` to build, configure, and validate a
  portable project from user input files.
- Added user-facing validation labels, pass/fail status, and specific repair
  guidance while preserving technical check identifiers.
- Added downloadable tree, geography, and regions templates to the Shiny
  project wizard.
- Added a complete BioGeoBEARS dependency plan and an explicitly enabled
  installation API.
- Added a Shiny BioGeoBEARS installation plan and confirmation dialog; package
  installation never starts from the GUI without a second user action.
- Added model-level run signatures and safe reuse of matching completed model
  results.
- Added failed-model-only retry mode, archived retry logs, and explicit
  `run_action` status values for executed, reused, and skipped models.

# iBiogeobears 0.2.0

Second alpha release of the single-clade BioGeoBEARS workflow MVP.

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
- Added Shiny config upload support and GUI-driven example project creation.
- Added Shiny PNG figure previews from the workflow manifest or figures
  directory.
- Added Shiny CSV table previews from the workflow manifest or tables
  directory.
- Added Shiny report path display and an open-report action for rendered
  reports.
- Added a compact Shiny workflow status summary for validation, run status,
  completed models, warnings, report, and bundle availability.
- Grouped Shiny sidebar controls into project, run-option, workflow, and
  report/export sections.
- Added dedicated Shiny result panels for model comparison, `+J` sensitivity,
  and captured model warnings.
- Added dedicated Shiny result panels for node-state summaries and node-state
  `+J` sensitivity comparisons.
- Added a Shiny figure dashboard for standard model-comparison, root-state,
  node-state, and node-sensitivity PNG outputs.
- Added Shiny support for loading existing workflow output directories without
  rerunning the analysis.
- Added a Shiny run-summary result tab for best-model, `+J` caution, warning,
  report, and output-path triage.
- Added Shiny export support for `tables/shiny_run_summary.csv` and a run
  summary download button.
- Added Shiny run-summary status cards while keeping the CSV/table summary as
  the shared data source.
- Added a Shiny key-files table for common result CSVs, reports, and bundles.
- Added Shiny controls to refresh key files and create result bundles only when
  missing.
- Clarified Shiny key-file status wording with next-step hints for missing
  reports, bundles, and result tables.
- Standardized Shiny key-file action feedback with `Report ready:`,
  `Bundle ready:`, and `Key files refreshed:` messages.
- Added compact Shiny summaries for model fit, +J sensitivity, and captured
  warnings before the detailed result tables.
- Expanded the Shiny figure dashboard with preview status, missing reasons,
  and recommended next steps for missing or failed figures.
- Added a Shiny table-status view for key CSV outputs with row and column
  counts, missing reasons, and recommended next steps.
- Added a Shiny About/Citation view for BioGeoBEARS availability, citation
  guidance, license, package version, and workflow log paths.
- Added report-environment checks for source, HTML, and PDF output, including
  Shiny visibility and fallback guidance when Quarto or LaTeX is unavailable.
- Added staged Shiny progress messages for validation, workflow execution,
  output refresh, report rendering, and result bundling.
- Added Shiny failed-model diagnostics in run summaries, warning summaries,
  run-status details, and staged workflow messages.
- Added `bundle_diagnostics()` and Shiny diagnostic-bundle export for lightweight
  troubleshooting archives containing config, status tables, manifests, session
  metadata, and log files.
- Added a first-pass Shiny config editor for common project, input,
  `max_range_size`, and model-selection overrides.
- Added Shiny config-editor fields for advanced BioGeoBEARS constraint files,
  including times, distances, dispersal multipliers, area-allowed, adjacency,
  and area-of-areas files.
- Added a user-workflow smoke script covering GUI-style config overrides,
  advanced constraint paths, report-source rendering, result bundles, and
  diagnostic bundles.
- Added the user-workflow smoke script to GitHub Actions CI.
- Updated the user-workflow smoke script to run against an installed package
  instead of a source-loaded package for CI parity.
- Added a release checklist for local checks, CI gates, real BioGeoBEARS
  validation, release notes, and GitHub release steps.
- Updated README workflow documentation for command-line runs, Shiny GUI use,
  result triage, key outputs, report rendering, and result bundling.
- Added a `shinytest2` browser smoke script for the Shiny workflow runner.
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
