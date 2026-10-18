# BioGeoSyn (development version, MER revision)

## Compatibility checks and inherited assumptions

Responds to Reviewer 3 (major comments 5 and 7) and Editor point 5.

- Added `check_bundle_compatibility()`, which reads the settings recorded in
  each result bundle (mapping model, maps, area coding, regions occupied,
  maximum range size, constraints, crown age, tips, software versions) and
  reports, as ok / info / warning, where the combined clades differ: mixed
  model families or `+J`, processes impossible under some of the models,
  conflicting area codes, different maximum range sizes or constraints, few
  maps, wide crown-age spread, implausible time units, mixed BioGeoBEARS
  versions, and whether tree uncertainty is included.
- Added `processes_possible_under_model()` and `inherited_assumptions()`.
- The cross-clade tab shows the checks as soon as bundles are uploaded,
  together with the settings table and the inherited assumptions; the
  cross-clade report and results bundle include them.
- The quick-start guide replaces "comparable time units and area codes" with
  an explicit list of comparability criteria.

## Simulation validation and bin-width sensitivity

Responds to Reviewer 3 (major comment 4; minor comment 5).

- Added `simulate_event_histories()` (events placed along branches with a known
  per-lineage rate, one realisation per "stochastic map") and
  `expected_event_counts()` (the truth on a shared grid), so the cross-clade
  synthesis can be validated where the answer is known.
- Added `bin_width_sensitivity()` and `plot_bin_width_sensitivity()`, which
  recompute the pooled rates for several bin widths and report where each
  peaks, so apparent pulses can be checked for stability.
- Added the validation study `inst/validation/cross_clade_simulation.R`
  (null, synchronous, asynchronous and single-clade pulses; interval coverage
  against held-out maps; pulse recovery under each weighting; bin width).

## Topological and age uncertainty

Responds to Reviewer 2 (comment 2 of the suggested features) and the second
half of Editor point 3. Intervals from stochastic maps on one tree ignore
uncertainty in topology and node ages.

- Added `run_workflow_over_trees()`, which runs the single-clade workflow once
  per tree from a multi-tree file, a directory of tree files, a vector of
  files or a `multiPhylo` object (optionally a random sample of `n_trees`),
  with the stochastic-mapping model fixed across trees (`bsm_model`). Each tree
  gets its own output directory and result bundle named `<clade>__treeNNN`,
  and the runs are listed in `tree_runs.csv`.
- The cross-clade synthesis treats bundles named `<clade>__treeNNN` as one
  clade (`clade_group_name()`). Pooled counts and per-lineage rates resample a
  tree first and then one of its maps, so intervals include tree uncertainty;
  every tree carries equal weight whatever its number of maps, and per-lineage
  rates use each tree's own lineage-Myr. Summed summaries (process synthesis,
  event statistics, routes, budgets, period networks, exchange matrix) average
  a clade's trees before summing clades.
- Fixed `bundle_results()` failing when given a relative `bundle_file` (the
  path was resolved after changing into the result directory).

## Per-lineage rates and clade weighting

Responds to Editor point 2, Reviewer 1 and Reviewer 3 (major comment 2, minor
comments 3 and 4). Counts of events rise with the number of lineages alive, and
pooled counts are dominated by species-rich clades.

- Added `pool_per_lineage_rates()` and `pool_per_lineage_rates_from_bundles()`.
  Event counts are divided by lineage-Myr, the branch time inside each bin
  (events per lineage per Myr), the continuous-time analogue of the per-capita
  rates of Xing & Ree (2017) and Ding et al. (2025). Whole-tree processes use
  the tree's lineage-Myr (`tree_lineage_myr()`). Region-resolved in-situ
  speciation and emigration use the lineage-Myr spent in the region and
  immigration the lineage-Myr spent outside it, reconstructed for every
  stochastic map from the sampled node states and anagenetic events
  (`map_region_occupancy()`).
- Two pooled versions are reported: lineage-weighted (summed events over summed
  lineage-Myr) and clade-weighted (mean of the clades' own rates, so every
  clade counts equally). Intervals use the stochastic-map resampling, with
  numerator and denominator taken from the same map. Bins with less than
  `min_lineage_myr` (default: the bin width) are not reported.
- `plot_per_lineage_rates_across_clades()`; the Shiny cross-clade tab gains a
  "Show" selector (event counts, lineage-weighted rate, clade-weighted rate)
  for the overall and region panels; the cross-clade report and bundle include
  the per-lineage tables and figures.
- Area codes that a clade never disperses into or out of are now named from
  the codes seen across all uploaded bundles (with a warning if clades
  disagree), instead of appearing as bare codes.
- The single-clade `rate` column is now documented as expected events per Myr
  for the whole clade, not per lineage.

Corrects how uncertainty is combined across clades in the cross-clade synthesis
(Molecular Ecology Resources review 7209990, Reviewer 3 major comment 3).

- The submitted version summed each clade's 2.5% and 97.5% limits to draw the
  band around a pooled curve. That assumes every clade sits at its own extreme
  in the same draw, so the band is wider than a 95% interval for the sum and
  widens linearly with the number of clades.
- Added `pool_rates_by_map_resampling()` and `pool_process_rates_from_bundles()`.
  Each clade's individually timed BSM events (`bsm_events.csv`, already in every
  result bundle) are binned directly onto the shared time grid. In each of
  `n_draws` draws (default 1000) one stochastic map is sampled at random from
  every clade and the per-bin counts are added; the band is the 2.5% and 97.5%
  quantile of these pooled counts. The pooled mean is the exact sum of the
  clades' per-map means. Maps without events count as zeros, the seed is
  fixed for reproducibility and the caller's random-number stream is restored.
- `plot_process_rates_across_clades()` and
  `plot_region_process_rates_across_clades()` gain `pooled_data`; the Shiny
  cross-clade tab, the cross-clade report and the cross-clade results bundle
  now use the resampled interval and write the pooled tables
  (`process_rates_through_time_pooled.csv`,
  `region_process_rates_through_time_pooled.csv`). Without per-event tables the
  old summed band is still drawn, but its caption now states that it is wider
  than a 95% interval for the sum.
- New tests check the pooled mean against the per-clade means, zero-event maps,
  degenerate (identical) maps, a 21-clade known-answer case (pooled Poisson(105),
  95% interval [85, 126]), recovery of a shared pulse among clades of different
  ages, reproducibility, region pooling and reading from bundles.

# BioGeoSyn 1.2.0

Makes every result bundle self-reproducing from the command line, so that an
analysis driven through the graphical interface can be re-run and audited
without it.

- Added `write_reproducibility_script()`. Every `run_workflow()` call now writes
  an executable `reproduce.R` into the output directory: it reads the saved
  `config_used.yml`, repoints the input paths at the copies stored in `inputs/`,
  and re-runs the analysis end to end. The script is inventoried in the manifest
  under a new `script` category and therefore travels inside the result bundle.
- `prepare_biogeobears_inputs()` now also copies the regions table into
  `inputs/`, so a bundle carries every input needed to reproduce it.

# BioGeoSyn 1.1.0

Adds the biogeographic process synthesis: the interpretive layer that turns
raw BioGeoBEARS stochastic mapping into named biogeographic processes, intended
as the analytical centerpiece for reporting and publication.

- Added `biogeographic_process_taxonomy()`, a canonical mapping from
  BioGeoBEARS event codes (`y`, `s`, `v`, `j`, `d`, `e`, `a`) to interpretable
  biogeographic processes (in-situ/sympatric speciation, subset sympatry,
  vicariance, founder-event jump speciation, range expansion, local extinction,
  range switching), grouped into cladogenetic speciation modes and anagenetic
  range changes.
- Added `summarize_biogeographic_processes()`, which aggregates the BSM event
  summary into a per-process table with mean counts and within-group and
  overall proportions, written to `biogeographic_process_summary.csv`.
- Added `plot_biogeographic_process_synthesis()` and a
  `biogeographic_process_synthesis` workflow figure that displays mean event
  counts per process, coloured by process class.
- Surfaced the process synthesis as the headline of the report's stochastic
  mapping section and documented the new table and figure.
- The log-scaled cross-clade rate axes now read in log units. They previously
  used a log-spaced axis labelled with the raw counts (so a summed value near
  2000 appeared as "2000" on a "log" axis); the plots now show log10(count + 1)
  directly, so the axis carries small log-unit numbers as in the source
  literature, with the zero bins staying finite at 0.
- Further refined the cross-clade tab. The overall rates figure now shows all
  processes in one panel on a log axis; the region-resolved figure is log-scaled
  by default; both carry a summed 95% interval band (the confidence bounds are
  re-binned onto the shared grid alongside the mean). Panels 3 and 4 have separate
  time-bin-width controls. The dispersal network now draws only its strongest
  routes (default 25) with larger labels, so dense all-to-all graphs no longer
  make different geological periods look alike, and the per-area budget beside it
  is derived from the same period- and region-filtered routes, so the network and
  budget stay in sync; the two are stacked vertically for legibility.
- Reworked the cross-clade tab. The overall process-rates figure now pools all
  clades onto a shared time grid (one curve per process) instead of drawing one
  curve per clade; `plot_process_rates_across_clades(pooled = TRUE)` exposes this,
  and the per-clade view remains the default of that function. The region-resolved
  figure gained an optional log y-axis (`log_y`), the dispersal network gained a
  region selector and is shown beside the per-area budget, and the exchange matrix
  and event-statistics tables moved out of the tab into the report and bundle
  (where they were already written) to keep the tab focused on figures.
- Fixed the cross-clade region-resolved rates figure, which collapsed into
  vertical spikes instead of curves. Each clade's own time bins differ (trees
  have different root ages), so pooling by raw bin midpoint mixed non-aligned
  points. Clades are now re-binned onto a shared time grid (overlap-weighted, so
  totals are conserved) before pooling, and the figure faces one panel per
  region with a curve per process (in-situ / immigration / emigration), mirroring
  the region-through-time figures in the multi-clade literature. The cross-clade
  tab gained a time-bin-width control and a region selector to focus the figure.
- Added dispersal networks sliced by geological period (Paleogene / Neogene /
  Quaternary / Total), built from the per-event times already in each bundle.
  The cross-clade tab has a period selector, and the report and results bundle
  include one network per period.
- Fixed the single-clade result bundle download always returning the first
  clade's archive. The cached bundle, report and diagnostics paths were not
  invalidated when a new clade was run or loaded, so every download served the
  stale archive; they are now cleared whenever the workflow result changes.
- Made the cross-clade tab explain why an upload yields nothing. The integrated
  results are built from BSM stochastic-mapping tables, so bundles from runs
  without BSM produced an empty result and a misleading "upload clade bundles
  first" error. The upload status now names bundles that lack BSM outputs, and
  the report error tells the user to re-run those clades with BSM enabled.
- Raised the Shiny upload limit from its 5 MB default to 1 GB per file
  (`options(biogeosyn.maxUploadSizeMB=)` to change), so uploading result bundles
  - especially several at once to the cross-clade tab - no longer fails with
  "Maximum upload size exceeded".
- Added a single-most-likely-range view of the ancestral reconstruction
  alongside the probability pies. `plot_node_state_summary(style = "single")`
  draws one solid, area-code-labelled disc per internal node (text colour is
  chosen for contrast against each fill), which stays legible on large trees
  where the pies blur together. The single-clade results tab now shows both
  views and the workflow writes `node_state_summary_*_single` figures.
- Added a `max_range_size_covers_observed_ranges` validation check. BioGeoBEARS
  rejects an entire run when any taxon occupies more areas than
  `max_range_size`, so the check now catches this before any model is fitted and
  names the taxa that exceed the limit.
- Fixed a run where every model failed reporting `comparison is missing required
  columns: delta_aicc, has_j` from the figure code. The raw status table was
  returned as if it were a model comparison, which discarded the error
  BioGeoBEARS had actually reported; the run now stops with that error instead.

# BioGeoSyn 1.0.0

First beta release (beta 1.0): a publishable, feature-complete reproducible
workflow, GUI, and reporting layer for single-clade BioGeoBEARS analyses.

- Added BioGeoBEARS BSM stochastic mapping for completed model results,
  including `bsm_run_status.csv`, `bsm_event_summary.csv`,
  `bsm_replicate_counts.csv`, `bsm_dispersal_routes.csv`, `bsm_events.csv`,
  `bsm_event_times.csv`, raw per-model BSM RDS outputs, report sections, Shiny
  views, and BSM figures.
- Completed the simplified Chinese localization of the Shiny interface,
  translating the advanced config editor, run options, BSM controls, advanced
  constraint fields, all advanced-results and troubleshooting sub-tabs, figure
  dashboard labels, and the initial status message so the guided flow and
  advanced views share one consistent language.
- Fixed a ggplot2 3.5 deprecation warning in the best-model node-state figure by
  replacing the deprecated `label.size` argument with `linewidth`.
- Fixed an R CMD check NOTE by declaring `sd_count` as a known global variable
  used in the BSM event-summary figure.
- Made package metadata publication-ready: set a real maintainer email,
  consolidated authorship into a single `Authors@R` entry, and rewrote
  `inst/CITATION` to use `bibentry()` with an auto-synced package version.
- Achieved a clean `R CMD check --no-manual` with no errors, warnings, or notes.

# BioGeoSyn 0.2.1

Third alpha release focused on ordinary-user stability and release readiness.

- Added `run_acceptance_check()` with a CI-friendly quick mode and a full
  six-model stability gate covering model reuse, HTML reporting, result
  bundling, and diagnostic bundling.
- Added a saved acceptance matrix with platform, R version, package version,
  elapsed time, failure details, and actionable next steps.
- Added the quick acceptance matrix to GitHub Actions.
- Expanded GitHub Actions quick checks across Linux, Windows, and macOS.
- Switched result and diagnostic bundling to the R `zip` package when available
  for more reliable cross-platform archive creation.
- Added an installed ordinary-user quick-start and troubleshooting guide,
  reachable with `open_user_guide()`.
- Added a simplified Chinese ordinary-user quick-start guide, reachable with
  `open_user_guide(language = "zh-CN")`.
- Added a fuller simplified Chinese ordinary-user testing tutorial in Markdown
  and standalone HTML for sharing installation, Shiny, example-data, user-data,
  result-reading, and feedback instructions.
- Added `create_windows_launcher()` and a packaged Windows `.bat` launcher so
  ordinary users can start the GUI by double-clicking after installation.
- Added a Shiny Start Here readiness checklist and user-guide action for the
  ordinary-user workflow path.
- Simplified the Shiny first-run interface around `Home`, focused `Results`,
  `Setup`, `Advanced`, and `Troubleshooting` views.
- Added a guided Shiny Home workflow with a single next-action prompt for
  example data, user data, existing results, validation, dry run, real run,
  result review, and export.
- Localized the main Shiny first-run path into simplified Chinese and added
  lightweight upload previews for user tree, geography, and regions files.
- Added a short Chinese tester-share message for forwarding installation,
  Shiny startup, example-data testing, user-data import, and feedback steps.
- Added a primary Shiny results view focused on best-model ancestral
  reconstruction, model comparison, and event summary.
- Added deterministic range-change event summaries from highest-probability
  ancestral states, written to `range_change_events.csv`,
  `event_summary.csv`, and `event_summary` figures.
- Added a clean GitHub-install smoke script for release readiness checks.
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

# BioGeoSyn 0.2.0

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

# BioGeoSyn 0.1.0

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
