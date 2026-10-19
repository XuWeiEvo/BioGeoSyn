# Figure 5 of the BioGeoSyn manuscript: uncertainty and validation of the cross-clade synthesis.
# (a) Four mammal clades of Feijo et al. (2022) on one tree vs five posterior trees per clade.
# (b) One simulated replicate per scenario of the validation study (inst/validation).
# Usage: Rscript figure5.R <folder with clade*__treeNNN_results.zip bundles> [output stem]
suppressPackageStartupMessages({
  if (nzchar(Sys.getenv("BGS_DEV"))) pkgload::load_all(Sys.getenv("BGS_DEV"), quiet = TRUE) else library(BioGeoSyn)
})
bgs <- asNamespace("BioGeoSyn")
args <- commandArgs(trailingOnly = TRUE)
tree_dir <- args[[1L]]
out <- if (length(args) >= 2L) args[[2L]] else "figure5_uncertainty_validation"

# (a) Tree uncertainty.
z <- sort(list.files(tree_dir, pattern = "__tree[0-9]+_results[.]zip$", recursive = TRUE, full.names = TRUE))
nm <- tools::file_path_sans_ext(basename(z))
first <- !duplicated(clade_group_name(nm))
one <- pool_process_rates_from_bundles(z[first], nm[first], bin_width = 5, n_draws = 4000, seed = 1)
all <- pool_process_rates_from_bundles(z, nm, bin_width = 5, n_draws = 4000, seed = 1)
dd <- rbind(transform(one, set = "One tree per clade (20 maps)"),
            transform(all[, names(one)], set = "Five posterior trees per clade (5 x 20 maps)"))
dd$set <- factor(dd$set, levels = unique(dd$set))
dd <- dd[dd$process_label %in% c("Range expansion", "In-situ (sympatric) speciation", "Vicariance"), ]
pa <- ggplot2::ggplot(dd, ggplot2::aes(bin_midpoint, mean_count, colour = process_label, fill = process_label)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.18, colour = NA) +
  ggplot2::geom_line(linewidth = 0.8) + ggplot2::geom_point(size = 1.1) +
  ggplot2::scale_x_reverse() + ggplot2::facet_wrap(~ set, nrow = 1) +
  scale_colour_bgs() + scale_fill_bgs() + ggplot2::guides(fill = "none") +
  ggplot2::labs(x = "Time before present (Ma)", y = "Mean events per map,\nsummed across 4 clades", colour = "Process",
                title = "Adding topological and age uncertainty",
                subtitle = "Four mammal clades of Feij\u00f3 et al. (2022), DEC, 5-Ma bins; band = 95% interval over 4,000 draws") +
  theme_bgs() + ggplot2::theme(legend.position = "bottom")

# (b) Simulated histories (same settings as inst/validation/cross_clade_simulation.R).
N_CLADES <- 10L
N_MAPS <- 30L
BASE <- 0.05
PULSE <- 0.40
PULSE_FROM <- 10
PULSE_TO <- 12
BIN <- 2
N_DRAWS <- 1000L

pulse_rate <- function(from, to) function(t) ifelse(t >= from & t < to, PULSE, BASE)
const_rate <- function(t) rep(BASE, length(t))

sim_tree <- function(g, age) {
  # Preserve the generator used for the reported validation run. rlineage()
  # starts with two crown lineages, so expected tip count is 2 * g; the
  # realized count is stochastic. Do not switch generators by package availability.
  tr <- ape::rlineage(log(g) / age, 0, Tmax = age)
  tr$edge.length <- tr$edge.length * age / max(ape::node.depth.edgelength(tr))
  tr
}

tree_nodes_of <- function(tr) {
  f <- tempfile(fileext = ".tre")
  on.exit(unlink(f))
  ape::write.tree(tr, f)
  bgs$make_node_lookup(f)
}

example <- do.call(rbind, lapply(c("null", "sync", "async", "single"), function(sc) {
  set.seed(1000L * match(sc, c("null", "sync", "async", "single")) + 1L)
  ages <- stats::runif(N_CLADES, 15, 60)
  growth_factors <- round(exp(stats::runif(N_CLADES, log(10), log(300))))
  rf <- switch(sc,
    null = rep(list(const_rate), N_CLADES),
    sync = rep(list(pulse_rate(PULSE_FROM, PULSE_TO)), N_CLADES),
    async = lapply(ages, function(a) { s <- stats::runif(1, 2, a - 2); pulse_rate(s, s + 2) }),
    single = { f <- rep(list(const_rate), N_CLADES); f[[which.max(growth_factors)]] <- pulse_rate(PULSE_FROM, PULSE_TO); f })
  cd <- lapply(seq_len(N_CLADES), function(i) simulate_event_histories(tree_nodes_of(sim_tree(growth_factors[[i]], ages[[i]])), rf[[i]], n_maps = N_MAPS))
  names(cd) <- paste0("c", seq_len(N_CLADES))
  co <- pool_rates_by_map_resampling(cd, bin_width = BIN, n_draws = N_DRAWS, seed = 1)
  ra <- pool_per_lineage_rates(cd, bin_width = BIN, n_draws = N_DRAWS, seed = 1, min_lineage_myr = BIN)
  rbind(
    data.frame(scenario = sc, measure = "Event count (summed)", x = co$bin_midpoint, y = co$mean_count, lo = co$ci_lower, hi = co$ci_upper),
    data.frame(scenario = sc, measure = "Rate, lineage-weighted", x = ra$bin_midpoint, y = ra$rate, lo = ra$rate_ci_lower, hi = ra$rate_ci_upper),
    data.frame(scenario = sc, measure = "Rate, clades weighted equally", x = ra$bin_midpoint, y = ra$clade_rate, lo = ra$clade_rate_ci_lower, hi = ra$clade_rate_ci_upper)
  )
}))
labels <- c(null = "No pulse", sync = "Synchronous pulse (10-12 Ma)", async = "Asynchronous pulses", single = "Pulse in one clade")
example$scenario <- factor(labels[example$scenario], levels = labels)
example$measure <- factor(example$measure, levels = c("Event count (summed)", "Rate, lineage-weighted", "Rate, clades weighted equally"))
example <- example[!is.na(example$y) & example$x <= 40, ]
pb <- ggplot2::ggplot(example, ggplot2::aes(x, y)) +
  ggplot2::annotate("rect", xmin = PULSE_FROM, xmax = PULSE_TO, ymin = -Inf, ymax = Inf, alpha = 0.12, fill = "grey40") +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lo, ymax = hi), alpha = 0.25, fill = "#2a6f97") +
  ggplot2::geom_line(colour = "#2a6f97", linewidth = 0.7) +
  ggplot2::scale_x_reverse() +
  ggplot2::facet_grid(measure ~ scenario, scales = "free_y") +
  ggplot2::labs(x = "Time before present (Ma)", y = NULL,
                title = "Simulated histories with known per-lineage rates",
                subtitle = "One replicate per scenario; 10 clades, 30 maps each, 2-Ma bins; grey = pulse window; band = 95% interval") +
  theme_bgs() +
  ggplot2::theme(strip.text = ggplot2::element_text(size = 8))

fig <- patchwork::wrap_plots(pa, pb, ncol = 1, heights = c(0.8, 1.25)) +
  patchwork::plot_annotation(tag_levels = "a") &
  ggplot2::theme(plot.tag = ggplot2::element_text(face = "bold", size = 13))
ggplot2::ggsave(paste0(out, ".pdf"), fig, width = 11, height = 12, device = grDevices::cairo_pdf)
ggplot2::ggsave(paste0(out, ".png"), fig, width = 11, height = 12, dpi = 300, bg = "white")
utils::write.csv(dd, paste0(out, "_panel_a_data.csv"), row.names = FALSE)
utils::write.csv(example, paste0(out, "_panel_b_data.csv"), row.names = FALSE)
writeLines(utils::capture.output(utils::sessionInfo()), paste0(out, "_session.txt"))
