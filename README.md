# iBiogeobears

[![R-CMD-check](https://github.com/XuWeiEvo/iBioGeoBEARS/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/XuWeiEvo/iBioGeoBEARS/actions/workflows/R-CMD-check.yaml)

`iBiogeobears` wraps [BioGeoBEARS](http://phylo.wikidot.com/biogeobears) in a
reproducible, step-by-step GUI with standardized output tables, publication
figures, and — its main novelty — **cross-clade integration of biogeographic
event statistics** (range expansion, in-situ/sympatric speciation, vicariance,
subset sympatry, founder-event jump speciation, local extinction, range
switching). Beta.

## Installation

```r
install.packages("remotes")
remotes::install_github("XuWeiEvo/iBioGeoBEARS")
```

BioGeoBEARS is not bundled and must be installed separately (Quarto is only
needed to render reports):

```r
install.packages(c("devtools", "rexpokit", "cladoRcpp"))
devtools::install_github("nmatzke/BioGeoBEARS", dependencies = FALSE)
```

The **“环境与安装”** panel at the top of the app checks BioGeoBEARS and can
install it for you.

## Quick start (GUI)

```r
install.packages("shiny")   # once
library(iBiogeobears)
launch_app()
```

The app is a five-step wizard:

1. **Data (数据)** — upload your tree (Newick), geography CSV, and regions CSV.
   Each upload has an inline **模板** button that downloads a working example to
   edit; optional advanced constraint files sit in a collapsed section. A data
   overview (tips, species per region, range sizes) and input validation appear
   below. Your uploads drive the analysis directly — there is no separate
   "create project" step.
2. **Analysis (分析)** — click **点击开始分析**. Keep **试运行 (dry run)** on for a
   check-only pass; uncheck it for a real BioGeoBEARS run. Enable **运行 BSM
   随机映射** to also produce the event/process outputs (and the cross-clade
   inputs). A report is generated automatically after a real run.
3. **Single-clade results (单一类群结果)** — previews of the ancestral-range
   reconstruction, model comparison, the biogeographic process synthesis, event
   statistics, and the dispersal arrow-network + heatmap. Download the full
   result bundle or the report here.
4. **Cross-clade (跨类群)** — see below.
5. **About (关于与引用)** — version, citation, and environment.

## Quick start (script)

```r
library(iBiogeobears)
project <- create_example_project(tempfile("ibgb-example-"))

# Validate and plan without running BioGeoBEARS:
run_workflow(project$config, dry_run = TRUE, require_biogeobears = FALSE)

# Real run (needs BioGeoBEARS). Enable BSM in analysis.yml for event outputs:
result <- run_workflow(project$config, dry_run = FALSE)
render_report(result, format = "html")
```

## Cross-clade integration (headline feature)

Analyse several clades **with BSM enabled**, then integrate their biogeographic
event rates through time. Each clade writes
`tables/process_rates_through_time.csv` (and a per-region companion) only when
stochastic mapping was run.

In the GUI, the **跨类群** tab takes a batch (multi-file) upload of each clade's
`process_rates_through_time.csv`. It plots one panel per biogeographic process
with a curve per clade and a **95% CI** band (the 2.5–97.5% percentiles across
stochastic maps), and exports the combined table. Per-region rates are uploaded
and compared the same way. Rename each file to the clade name (e.g.
`Anolis.csv`); clades must use comparable time units.

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
output directory, plus a portable archive via `bundle_results(result)`. The
biogeographic **process** outputs (per-process synthesis, region budgets, rates
through time, dispersal routes, and a `bsm_qc.csv` reliability check) are
BioGeoBEARS' own stochastic-mapping event counts, relabelled into named
processes — so they are produced only when BSM is enabled:

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

## Citation and license

Released under GPL (>= 2). BioGeoBEARS is by Nicholas J. Matzke — cite it
directly with `citation("BioGeoBEARS")`.
