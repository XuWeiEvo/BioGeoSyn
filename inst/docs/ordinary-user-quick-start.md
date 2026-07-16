# BioGeoSyn Ordinary-User Quick Start

This guide is for first-time users who want to run a BioGeoBEARS analysis
through `BioGeoSyn` — one clade, and then several clades integrated
together.

`BioGeoSyn` does not bundle BioGeoBEARS. BioGeoBEARS must be installed
separately before real model execution. You can still run dry-run checks and
prepare projects before BioGeoBEARS is available.

## 1. Install BioGeoSyn

Run this in R:

```r
install.packages("remotes")
remotes::install_github("XuWeiEvo/BioGeoSyn")
```

Load the package and check the local setup:

```r
library(BioGeoSyn)
check_installation()
```

The `check_installation()` table tells you what is ready and what to install
next. HTML reports require Quarto. PDF reports also require a LaTeX engine.

## 2. Install BioGeoBEARS For Real Runs

Inspect the installation plan first:

```r
biogeobears_install_plan()
```

Then explicitly install BioGeoBEARS and its known helper packages:

```r
install_biogeobears(execute = TRUE)
```

Check again:

```r
check_installation()
check_biogeobears(required = FALSE)
```

## 3. Run The Built-In Example

Create a self-contained example project:

```r
project <- create_example_project("bgs_example")
```

Run a dry check first. This does not require BioGeoBEARS:

```r
dry <- run_workflow(
  project$config,
  dry_run = TRUE,
  require_biogeobears = FALSE
)
```

When BioGeoBEARS is installed, run the real six-model example:

```r
result <- run_workflow(project$config, dry_run = FALSE)
```

Render an HTML report and create shareable archives:

```r
report <- render_report(result, format = "html")
results_zip <- bundle_results(result)
diagnostics_zip <- bundle_diagnostics(result)
```

Start interpretation from these files:

```text
reports/summary_report.html
tables/shiny_run_summary.csv
tables/model_comparison.csv
tables/model_sensitivity.csv
tables/model_run_status.csv
```

## 4. Use The Shiny App

Install Shiny if needed:

```r
install.packages("shiny")
```

Launch the app:

```r
launch_app()
```

On Windows, you can create a double-click launcher:

```r
create_windows_launcher()
```

After that, double-click `start-BioGeoSyn.bat` on the Desktop instead of
opening RStudio and typing `launch_app()`.

The app is a five-step wizard. The interface is English; so is everything the
run writes.

### 1 · Data

1. Upload your tree (Newick), geography CSV, and regions CSV. The **Template**
   button beside each upload downloads a working example to edit.
2. Set **Maximum range size**. It must be at least the largest range any taxon
   actually occupies, which the data overview reports.
3. Choose the **Models to fit**. All six are selected by default; fitting fewer
   is much faster (`DEC` and `DEC+J` are the usual pair).
4. Set **CPU cores**. Extra cores speed up model fitting only.
5. Optionally open **Advanced constraints** for time stratification, dispersal
   multipliers, distances, allowed areas, adjacency or area sizes.
6. Click **Check inputs** and read the data overview and validation table.

Watch the state-space note under **Maximum range size**. Runtime is driven by
the state count, `sum(choose(n_areas, 0:max_range))`, not by the number of
tips — eleven areas at `max_range = 4` is 562 states, and each likelihood
evaluation scales with the square of that. If the note warns that the space is
large, raise **CPU cores** and fit fewer models.

### 2 · Analysis

1. Keep **Dry run** checked for a check-only pass; it does not need
   BioGeoBEARS.
2. Uncheck **Dry run** for a real run.
3. Enable **Run BSM stochastic mapping** if you want the event and process
   outputs — the process synthesis, event statistics, rates through time, and
   the tables the multi-clade synthesis needs. Without it the run only fits
   models and estimates ancestral ranges.
4. Click **Start the analysis**.

A large run prints no progress bar. If it seems stuck, check the state-space
note and the number of models first — it is almost always compute, not a hang.

### 3. Single clade

The ancestral-range reconstruction and the model comparison appear here. Click
**Download result bundle** for every table, figure and log for this clade. That
bundle is also what the next step consumes.

### 4. Multi-clade synthesis

See section 6.

## 5. Use Your Own Data

Required files:

- Newick tree file.
- Geography CSV with one row per taxon and one column per area. Area codes must
  be single characters (`A`, `B`, `C`, …) for BSM.
- Regions CSV giving the full name of each area, used to label the output.

Tip labels and geography taxon names must match exactly, including
capitalisation.

From the console:

```r
project <- create_analysis_project(
  path = "my_bgs_project",
  project_name = "my_clade",
  tree_file = "my_tree.nwk",
  geography_file = "my_geography.csv",
  regions_file = "my_regions.csv"
)

dry <- run_workflow(
  project$config,
  dry_run = TRUE,
  require_biogeobears = FALSE
)
```

## 6. Integrate Several Clades

This is what `BioGeoSyn` adds over running BioGeoBEARS directly.

1. Analyse each clade separately **with BSM enabled**.
2. Download each clade's result bundle from **3. Single clade** and rename it
   to the clade name (e.g. `Muridae.zip`).
3. Open **4. Multi-clade synthesis** and upload all the bundles at once.

The tab reads every clade's standardized tables and builds the integrated
results: the process synthesis summed across clades; process rates through
time, overall and resolved by region (in-situ speciation / immigration /
emigration); a source-to-recipient exchange matrix; the dispersal network and
each area's immigration/emigration budget; and event statistics. Export every
integrated table and figure, or build a shareable HTML report, from the bottom
of the same tab.

Clades must use comparable time units and the same area codes. Each clade's own
inference is untouched: the synthesis re-presents the stochastic maps you
already ran, it never re-estimates anything.

## 7. If Something Fails

First inspect:

```text
tables/input_validation.csv
tables/model_run_status.csv
logs/session_info.txt
```

If the workflow returned a `result` object, create a small diagnostics archive:

```r
diagnostics_zip <- bundle_diagnostics(result, overwrite = TRUE)
```

If the run stopped before returning `result`, send these files manually if they
exist:

```text
analysis.yml
results/<project>/tables/input_validation.csv
results/<project>/tables/model_run_status.csv
results/<project>/logs/session_info.txt
results/<project>/logs/
```

Common fixes:

- BioGeoBEARS missing: run `biogeobears_install_plan()` and then
  `install_biogeobears(execute = TRUE)`.
- Shiny missing: run `install.packages("shiny")`.
- HTML report missing: install Quarto, then run `check_report_environment()`.
- PDF report missing: install TinyTeX, MiKTeX, or TeX Live.
- Validation failed: open `tables/input_validation.csv` and follow the
  `repair_step` column.
- One model failed: open `tables/model_run_status.csv`, inspect the failed
  model log path, then rerun with `retry_failed_only = TRUE`.
- The run takes far longer than expected: this is normally the state space, not
  a hang. Check the note under **Maximum range size**, fit fewer models, and
  raise **CPU cores**.
- A constrained run fails: constraint matrix files are an area-name header
  followed by rows of plain numbers, with no row labels, and one block per time
  bin. Times are time-bin bottoms — the oldest must be older than the tree root,
  and no boundary may sit exactly on a node date. The data step checks all of
  this up front.
- `+J` caution appears: this is not an execution error. It means the best or
  near-best statistical model includes founder-event jump dispersal and should
  be interpreted cautiously.

## 8. Stable-Release Readiness Check

Before sharing the package with ordinary users, maintainers should run:

```r
acceptance <- run_acceptance_check(
  "stable-release-acceptance",
  mode = "full"
)
acceptance
```

Full mode requires Shiny, BioGeoBEARS, and report rendering. It runs the
bundled example with all six supported models, checks model reuse, renders an
HTML report, and creates result and diagnostic bundles.
