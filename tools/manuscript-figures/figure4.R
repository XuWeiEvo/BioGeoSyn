# Figure 4 of the BioGeoSyn manuscript: cross-clade worked example (Feijo et al. 2022).
# Usage: Rscript figure4.R <folder with clade1.zip ... clade21.zip> <output prefix>
# Every panel is drawn by BioGeoSyn functions from the 21 result bundles; nothing is re-estimated.
suppressPackageStartupMessages({
  if (nzchar(Sys.getenv("BGS_DEV"))) pkgload::load_all(Sys.getenv("BGS_DEV"), quiet = TRUE) else library(BioGeoSyn)
})
args <- commandArgs(trailingOnly = TRUE)
dir <- args[[1L]]
out <- if (length(args) >= 2L) args[[2L]] else "figure4_cross_clade_results"
zips <- file.path(dir, paste0("clade", 1:21, ".zip"))
labs <- paste0("clade", 1:21)

# (a) Pooled process counts through time, 5-Ma bins, 95% intervals from 2,000 draws of
#     one stochastic map per clade.
rates <- combine_process_rates_from_bundles(zips, labs)
pooled <- pool_process_rates_from_bundles(zips, labs, bin_width = 5, n_draws = 2000L, seed = 1L)
pa <- plot_process_rates_across_clades(rates, pooled = TRUE, pooled_data = pooled) +
  ggplot2::labs(title = "Cross-clade process counts through time",
                y = expression(log[10](mean~events + 1)),
                subtitle = "All clades pooled on 5-Ma bins; log y-axis; band = 95% interval over resampled stochastic maps") +
  ggplot2::theme(legend.position = "none", axis.text = ggplot2::element_text(size = 11))

# (b) The same events as per-lineage rates (events per lineage-Myr), lineage-weighted, with
#     intervals from the same resampling. Counts grow with the number of lineages; rates do not.
#     Bins with fewer than 2 lineage-Myr per Myr (10 lineage-Myr in a 5-Ma bin) are not shown.
per_lineage <- pool_per_lineage_rates_from_bundles(zips, labs, bin_width = 5, n_draws = 2000L, seed = 1L,
                                                   min_lineage_myr = 10)
pr <- plot_per_lineage_rates_across_clades(per_lineage, weighting = "lineage") +
  ggplot2::labs(title = "Cross-clade per-lineage rates through time",
                subtitle = "Summed events / summed lineage-Myr; 5-Ma bins; band = 95% interval over resampled stochastic maps") +
  ggplot2::scale_x_reverse(limits = c(60, 0)) +
  ggplot2::theme(legend.position = "bottom", axis.text = ggplot2::element_text(size = 11))

# (c, d) Directed dispersal networks for the Paleogene and the Neogene, 20 strongest routes.
et <- BioGeoSyn:::combine_event_times_across_clades(zips, labs)
net <- function(period) {
  w <- BioGeoSyn:::bgs_period_window(period)
  routes <- BioGeoSyn:::dispersal_routes_from_event_times(et, w$from, w$to)
  plot_bsm_dispersal_network(routes, max_edges = 20L) +
    ggplot2::labs(title = BioGeoSyn:::period_network_subtitle(period), subtitle = "Dispersal network, 20 strongest routes") +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 13, face = "bold"), plot.subtitle = ggplot2::element_text(size = 10))
}
pb <- net("Paleogene")
pc <- net("Neogene")

# (e) Lineages through space and time (Skeels 2019), 1-Ma bins, from the ranges sampled in every map.
ltst <- lineages_through_space_time_from_bundles(zips, labs, bin_width = 1, n_draws = 1000L, seed = 1L)
pe <- plot_lineages_through_space_time(ltst) +
  ggplot2::labs(subtitle = "Mean lineages per region summed across clades; 1-Ma bins; band = 95% interval over resampled stochastic maps") +
  ggplot2::theme(legend.position = "bottom", axis.text = ggplot2::element_text(size = 11)) +
  ggplot2::guides(colour = ggplot2::guide_legend(nrow = 3, byrow = TRUE))

fig <- patchwork::wrap_plots(pa, pr, patchwork::wrap_plots(pb, pc, ncol = 2), pe, ncol = 1, heights = c(0.65, 0.65, 1.3, 0.85)) +
  patchwork::plot_annotation(tag_levels = "a") &
  ggplot2::theme(plot.tag = ggplot2::element_text(face = "bold", size = 13),
                 legend.text = ggplot2::element_text(size = 11))
ggplot2::ggsave(paste0(out, ".pdf"), fig, width = 10, height = 14, device = grDevices::cairo_pdf)
ggplot2::ggsave(paste0(out, ".png"), fig, width = 10, height = 14, dpi = 300, bg = "white")
utils::write.csv(pooled, paste0(out, "_panel_a_data.csv"), row.names = FALSE)
utils::write.csv(per_lineage, paste0(out, "_panel_b_data.csv"), row.names = FALSE)
utils::write.csv(ltst, paste0(out, "_panel_e_data.csv"), row.names = FALSE)
writeLines(utils::capture.output(utils::sessionInfo()), paste0(out, "_session.txt"))
