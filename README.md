# BioGeoSyn

[![R-CMD-check](https://github.com/XuWeiEvo/BioGeoSyn/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/XuWeiEvo/BioGeoSyn/actions/workflows/R-CMD-check.yaml)

`BioGeoSyn` wraps [BioGeoBEARS](http://phylo.wikidot.com/biogeobears) in a
reproducible, step-by-step GUI with standardized output tables, publication
figures, and — its main novelty — **cross-clade integration of biogeographic
event statistics** (range expansion, in-situ/sympatric speciation, vicariance,
subset sympatry, founder-event jump speciation, local extinction, range
switching). Beta.

## Installation

```r
install.packages("remotes")
remotes::install_github("XuWeiEvo/BioGeoSyn")
```

BioGeoBEARS is not bundled and must be installed separately (Quarto is only
needed to render reports):

```r
install.packages(c("devtools", "rexpokit", "cladoRcpp"))
devtools::install_github("nmatzke/BioGeoBEARS", dependencies = FALSE)
```

The **Environment and installation** panel at the top of the app checks
BioGeoBEARS and can install it for you.

## Quick start (GUI)

```r
install.packages("shiny")   # once
library(BioGeoSyn)
launch_app()
```

The app is a five-step wizard:

1. **1 · Data** — upload your tree (Newick), geography CSV, and regions CSV.
   Each upload has an inline **Template** button that downloads a working
   example to edit; optional advanced constraint files sit in a collapsed
   section. Set **Maximum range size**, the **Models to fit**, and **CPU cores**
   here. A data overview (tips, species per area, range sizes) and input
   validation appear below. Your uploads drive the analysis directly — there is
   no separate "create project" step.
2. **2 · Analysis** — click **Start the analysis**. Keep **Dry run** on for a
   check-only pass; uncheck it for a real BioGeoBEARS run. Enable **Run BSM
   stochastic mapping** to also produce the event/process outputs (and the
   inputs the cross-clade synthesis needs).
3. **3. Single clade** — the ancestral-range reconstruction and the model
   comparison, plus **Download result bundle** (every table, figure and log for
   this clade).
4. **4. Multi-clade synthesis** — see below.
5. **About and citation** — version, citation, and environment.

### Watch the state space

Runtime is dominated by the state space, which is
`sum(choose(n_areas, 0:max_range))` — not by the number of tips. Eleven areas at
`max_range = 4` is 562 states and each likelihood evaluation scales with the
square of that, so fitting all six models on one core can take hours. The data
step prints the state count and warns when it gets large; raising **CPU cores**
and fitting fewer models are the two effective levers.

## Quick start (script)

```r
library(BioGeoSyn)
project <- create_example_project(tempfile("bgs-example-"))

# Validate and plan without running BioGeoBEARS:
run_workflow(project$config, dry_run = TRUE, require_biogeobears = FALSE)

# Real run (needs BioGeoBEARS). Enable BSM in analysis.yml for event outputs:
result <- run_workflow(project$config, dry_run = FALSE)
render_report(result, format = "html")
```

## Cross-clade integration (headline feature)

Analyse several clades **with BSM enabled**, download each clade's result
bundle from the **3. Single clade** step, then integrate them.

In the GUI, the **4. Multi-clade synthesis** tab takes a batch (multi-file)
upload of one **result bundle (`.zip`) per clade** — rename each to its clade
name (e.g. `Muridae.zip`). It reads every clade's standardized tables and builds
the integrated results:

- biogeographic process synthesis, summed across clades;
- process rates through time, overall (one curve per clade) and resolved by
  region (in-situ speciation / immigration / emigration, pooled across clades);
- a source-to-recipient **exchange matrix** (diagonal = in-situ speciation,
  off-diagonal = dispersal, with per-area immigration/emigration totals);
- the inter-area dispersal network and each area's immigration/emigration
  budget;
- event statistics, plus a one-click export of every integrated table and
  figure and a shareable HTML report.

Clades must use comparable time units and area codes. Every clade's own
inference is left untouched — the synthesis re-presents each clade's stochastic
maps in a shared vocabulary, it never re-estimates anything.

From the console:

```r
combined <- combine_process_rates_across_clades(
  c("results/anolis/tables/process_rates_through_time.csv",
    "results/phelsuma/tables/process_rates_through_time.csv"),
  clade_names = c("Anolis", "Phelsuma")
)
plot_process_rates_across_clades(combined)

# Region-resolved, from each clade's region_process_rates_through_time.csv:
combine_region_process_rates_across_clades(files, clade_names)
plot_region_process_rates_across_clades(combined_region)
```

## Outputs and BSM

A run writes `tables/`, `figures/`, `reports/`, and `logs/` under the chosen
output directory, plus a portable archive via `bundle_results(result)`. All
output — tables, figures, reports and bundles — is English regardless of
anything else. The biogeographic **process** outputs (per-process synthesis,
region budgets, rates through time, dispersal routes, and a `bsm_qc.csv`
reliability check) are BioGeoBEARS' own stochastic-mapping event counts,
relabelled into named processes — so they are produced only when BSM is
enabled:

```yaml
analysis:
  run_stochastic_mapping: true
  stochastic_mapping_replicates: 100
```

BSM needs one-character area codes (`A`, `B`, `C`, …) in the geography matrix;
put full region names in `regions.csv`, which are used as labels in the output.

Model selection is not a "lowest AICc wins" tool: the report and app separate
statistical fit from biological interpretation, especially when a `+J` model is
best or near-best.

## Advanced constraints

Time stratification, dispersal multipliers, distances, allowed areas, area
adjacency and area-of-areas are optional uploads on the data step (templates
included). Each matrix file is an area-name header followed by rows of **plain
numbers** — no row labels — with one block per time bin. Supplying a distances
file frees the distance-decay exponent `x`; supplying a times file makes the run
time-stratified. Times are time-bin **bottoms**: the oldest must be older than
the tree root, and no bin boundary may fall exactly on a node date. The data
step checks all of this before the run rather than letting BioGeoBEARS abort
mid-way.

## Reports (HTML / PDF)

Generate the multi-clade report on demand — click **Build report** on the
**4. Multi-clade synthesis** step, or call `render_report(result, format =
"html")` (or `"pdf"`, or `"source"`) for a single clade. Report rendering is
kept separate from the run so it never blocks the analysis; and if the tools are
missing, `render_report()` still writes the `.qmd` source and returns its path,
so **a missing renderer never blocks the run or loses results** — you just get
the source instead of a rendered HTML/PDF.

**HTML reports** need the Quarto command-line tool plus the `quarto` R package:

```r
install.packages("quarto")
```

and the Quarto CLI from <https://quarto.org/docs/get-started/> (install it, then
restart R so it is found on the `PATH`). Many machines that render R Markdown
already have it.

**PDF reports** additionally need a LaTeX engine. The simplest is TinyTeX:

```r
install.packages("tinytex")
tinytex::install_tinytex()
```

Check what is available (and the exact next step for anything missing) with:

```r
check_report_environment(c("source", "html", "pdf"))
```

The same status appears in the app's top **Environment and installation** panel.
If you only need the numbers and figures, the source report plus the
downloadable result bundle are enough — HTML/PDF are for a formatted, shareable
write-up.

## Citation and license

Released under GPL (>= 2). BioGeoBEARS is by Nicholas J. Matzke — cite it
directly with `citation("BioGeoBEARS")`.
