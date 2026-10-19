# Simulation validation of the BioGeoSyn cross-clade synthesis
#
# Run with:  Rscript cross_clade_simulation.R <output_dir> [n_reps] [n_cores]
#
# Each replicate simulates 10 clades (pure-birth trees; crown ages 15-60 Ma;
# growth factors 10-300, log-uniform and rounded; stochastic tip counts)
# and places range-expansion events along their
# branches with a known per-lineage rate lambda(t), in 30 "stochastic maps"
# per clade plus one held-out map. Scenarios:
#   null    lambda = 0.05 everywhere
#   sync    lambda = 0.05, rising to 0.40 in every clade at 10-12 Ma
#   async   as sync, but each clade's 2-Myr pulse starts at its own random time
#   single  only the clade with the largest growth factor has the 10-12 Ma pulse
# The script reports interval coverage, pulse recovery, the effect of weighting,
# and sensitivity to bin width, and writes tables and figures to <output_dir>.

suppressPackageStartupMessages({
  if (requireNamespace("BioGeoSyn", quietly = TRUE) && !nzchar(Sys.getenv("BGS_DEV"))) {
    library(BioGeoSyn)
  } else {
    pkgload::load_all(Sys.getenv("BGS_DEV", "."), quiet = TRUE)
  }
})
bgs <- asNamespace("BioGeoSyn")

args <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args) >= 1L) args[[1L]] else "simulation_results"
n_reps <- if (length(args) >= 2L) as.integer(args[[2L]]) else 100L
n_cores <- if (length(args) >= 3L) as.integer(args[[3L]]) else 1L
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

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

one_replicate <- function(rep_id, scenario) {
  set.seed(1000L * match(scenario, c("null", "sync", "async", "single")) + rep_id)
  ages <- stats::runif(N_CLADES, 15, 60)
  growth_factors <- round(exp(stats::runif(N_CLADES, log(10), log(300))))
  rate_funs <- switch(scenario,
    null = rep(list(const_rate), N_CLADES),
    sync = rep(list(pulse_rate(PULSE_FROM, PULSE_TO)), N_CLADES),
    async = lapply(ages, function(a) {
      s <- stats::runif(1, 2, a - 2)
      pulse_rate(s, s + 2)
    }),
    single = {
      f <- rep(list(const_rate), N_CLADES)
      f[[which.max(growth_factors)]] <- pulse_rate(PULSE_FROM, PULSE_TO)
      f
    }
  )
  clades <- lapply(seq_len(N_CLADES), function(i) {
    tn <- tree_nodes_of(sim_tree(growth_factors[[i]], ages[[i]]))
    x <- simulate_event_histories(tn, rate_funs[[i]], n_maps = N_MAPS + 1L)
    held <- x$events[x$events$replicate == N_MAPS + 1L, , drop = FALSE]
    x$events <- x$events[x$events$replicate <= N_MAPS, , drop = FALSE]
    x$n_maps <- N_MAPS
    x$replicates <- seq_len(N_MAPS)
    list(x = x, held = held, rate = rate_funs[[i]])
  })
  names(clades) <- paste0("c", seq_len(N_CLADES))
  cd <- lapply(clades, `[[`, "x")
  n_bins <- ceiling((max(ages) + 1e-9) / BIN)

  # Truth on the shared grid.
  truth <- lapply(seq_len(N_CLADES), function(i) expected_event_counts(cd[[i]]$tree_nodes, clades[[i]]$rate, n_bins, BIN))
  E <- Reduce(`+`, lapply(truth, `[[`, "expected_count"))
  LM <- Reduce(`+`, lapply(truth, `[[`, "lineage_myr"))
  true_lin <- ifelse(LM >= BIN, E / LM, NA)
  cw <- do.call(cbind, lapply(truth, function(t) ifelse(t$lineage_myr >= BIN, t$expected_count / t$lineage_myr, NA)))
  true_cw <- ifelse(rowSums(!is.na(cw)) > 0, rowMeans(cw, na.rm = TRUE), NA)

  # Align pooled tables on the full grid (bins without events are zeros).
  align <- function(df, cols, fill) {
    out <- as.data.frame(stats::setNames(rep(list(rep(fill, n_bins)), length(cols)), cols))
    keep <- df$grid_bin <= n_bins
    for (cc in cols) out[[cc]][df$grid_bin[keep]] <- df[[cc]][keep]
    out
  }
  counts <- align(pool_rates_by_map_resampling(cd, bin_width = BIN, n_draws = N_DRAWS, seed = rep_id),
                  c("mean_count", "ci_lower", "ci_upper"), 0)
  rates <- align(pool_per_lineage_rates(cd, bin_width = BIN, n_draws = N_DRAWS, seed = rep_id, min_lineage_myr = BIN),
                 c("rate", "rate_ci_lower", "rate_ci_upper", "clade_rate", "clade_rate_ci_lower", "clade_rate_ci_upper"), NA_real_)

  # Held-out map: does the pooled interval contain an independent realisation?
  held_count <- tabulate(pmin(n_bins, floor(unlist(lapply(clades, function(c) c$held$event_time_before_present)) / BIN) + 1L), n_bins)
  # The submitted method: per-clade 2.5/97.5% limits added across clades.
  per_clade_limits <- lapply(cd, function(x) {
    m <- bgs$clade_map_count_matrix(x, "process_label", "Range expansion", n_bins, BIN)
    apply(m, 2L, stats::quantile, c(0.025, 0.975), names = FALSE)
  })
  sum_lo <- Reduce(`+`, lapply(per_clade_limits, function(q) q[1L, ]))
  sum_hi <- Reduce(`+`, lapply(per_clade_limits, function(q) q[2L, ]))

  live <- which(E > 1)
  # Held-out map expressed as rates (same denominators as the pooled rates).
  held_c <- lapply(clades, function(c) tabulate(pmin(n_bins, floor(c$held$event_time_before_present / BIN) + 1L), n_bins))
  lm_c <- lapply(truth, `[[`, "lineage_myr")
  held_lin <- ifelse(LM >= BIN, Reduce(`+`, held_c) / LM, NA)
  hc <- do.call(cbind, Map(function(h, l) ifelse(l >= BIN, h / l, NA), held_c, lm_c))
  held_cw <- ifelse(rowSums(!is.na(hc)) > 0, rowMeans(hc, na.rm = TRUE), NA)
  inside <- function(v, lo, hi) mean(v >= lo & v <= hi, na.rm = TRUE)
  relerr <- function(est, tru) stats::median(abs(est - tru) / tru, na.rm = TRUE)
  peak_bin <- function(v) if (all(is.na(v))) NA_integer_ else which.max(v)
  pulse_bin <- floor(PULSE_FROM / BIN) + 1L
  ptm <- function(v, b) if (is.na(b)) NA_real_ else v[[b]] / stats::median(v, na.rm = TRUE)

  data.frame(
    scenario = scenario, rep = rep_id,
    cover_resampled = mean(held_count[live] >= counts$ci_lower[live] & held_count[live] <= counts$ci_upper[live]),
    cover_summed = mean(held_count[live] >= sum_lo[live] & held_count[live] <= sum_hi[live]),
    width_ratio = stats::median((counts$ci_upper - counts$ci_lower)[live] / pmax(1e-9, (sum_hi - sum_lo)[live])),
    cover_rate_lin = inside(held_lin, rates$rate_ci_lower, rates$rate_ci_upper),
    cover_rate_cw = inside(held_cw, rates$clade_rate_ci_lower, rates$clade_rate_ci_upper),
    relerr_rate_lin = relerr(rates$rate, true_lin),
    relerr_rate_cw = relerr(rates$clade_rate, true_cw),
    peak_count_in_pulse = isTRUE(peak_bin(counts$mean_count) == pulse_bin),
    peak_lin_in_pulse = isTRUE(peak_bin(rates$rate) == pulse_bin),
    peak_cw_in_pulse = isTRUE(peak_bin(rates$clade_rate) == pulse_bin),
    contrast_lin = ptm(rates$rate, pulse_bin),
    contrast_cw = ptm(rates$clade_rate, pulse_bin),
    max_contrast_cw = max(rates$clade_rate, na.rm = TRUE) / stats::median(rates$clade_rate, na.rm = TRUE),
    recent_over_old_count = counts$mean_count[[1L]] / max(1e-9, counts$mean_count[[floor(20 / BIN) + 1L]]),
    recent_over_old_rate = rates$clade_rate[[1L]] / rates$clade_rate[[floor(20 / BIN) + 1L]],
    stringsAsFactors = FALSE
  )
}

bin_width_replicate <- function(rep_id, widths = c(1, 2, 5, 10)) {
  set.seed(5000L + rep_id)
  ages <- stats::runif(N_CLADES, 15, 60)
  growth_factors <- round(exp(stats::runif(N_CLADES, log(10), log(300))))
  cd <- lapply(seq_len(N_CLADES), function(i) {
    simulate_event_histories(tree_nodes_of(sim_tree(growth_factors[[i]], ages[[i]])), pulse_rate(PULSE_FROM, PULSE_TO), n_maps = N_MAPS)
  })
  names(cd) <- paste0("c", seq_len(N_CLADES))
  s <- bin_width_sensitivity(cd, widths, weighting = "clade", n_draws = 200L, seed = rep_id)
  p <- s$peaks
  p$rep <- rep_id
  p$peak_overlaps_pulse <- p$peak_start < PULSE_TO & p$peak_end > PULSE_FROM
  p
}

apply_fun <- if (n_cores > 1L && .Platform$OS.type != "windows") {
  function(X, FUN) parallel::mclapply(X, FUN, mc.cores = n_cores)
} else {
  lapply
}

t0 <- Sys.time()
res <- do.call(rbind, unlist(lapply(c("null", "sync", "async", "single"), function(sc) {
  apply_fun(seq_len(n_reps), function(r) one_replicate(r, sc))
}), recursive = FALSE))
bw <- do.call(rbind, apply_fun(seq_len(n_reps), bin_width_replicate))
utils::write.csv(res, file.path(out_dir, "simulation_replicates.csv"), row.names = FALSE)
utils::write.csv(bw, file.path(out_dir, "bin_width_replicates.csv"), row.names = FALSE)

summ <- do.call(rbind, lapply(split(res, res$scenario), function(d) data.frame(
  scenario = d$scenario[[1L]], n = nrow(d),
  coverage_resampled = mean(d$cover_resampled), coverage_summed_limits = mean(d$cover_summed),
  median_width_ratio = stats::median(d$width_ratio),
  coverage_rate_lineage = mean(d$cover_rate_lin), coverage_rate_clade = mean(d$cover_rate_cw),
  median_relerr_rate_lineage = stats::median(d$relerr_rate_lin), median_relerr_rate_clade = stats::median(d$relerr_rate_cw),
  pulse_found_counts = mean(d$peak_count_in_pulse), pulse_found_rate_lineage = mean(d$peak_lin_in_pulse),
  pulse_found_rate_clade = mean(d$peak_cw_in_pulse),
  median_contrast_lineage = stats::median(d$contrast_lin, na.rm = TRUE),
  median_contrast_clade = stats::median(d$contrast_cw, na.rm = TRUE),
  median_max_contrast_clade = stats::median(d$max_contrast_cw, na.rm = TRUE),
  median_recent_over_old_count = stats::median(d$recent_over_old_count),
  median_recent_over_old_rate = stats::median(d$recent_over_old_rate, na.rm = TRUE)
)))
bw_summ <- do.call(rbind, lapply(split(bw, bw$bin_width), function(d) data.frame(
  bin_width = d$bin_width[[1L]], n = nrow(d),
  peak_overlaps_pulse = mean(d$peak_overlaps_pulse),
  median_peak_rate = stats::median(d$peak_rate),
  median_peak_to_median = stats::median(d$peak_to_median)
)))
utils::write.csv(summ, file.path(out_dir, "simulation_summary.csv"), row.names = FALSE)
utils::write.csv(bw_summ, file.path(out_dir, "bin_width_summary.csv"), row.names = FALSE)
writeLines(c(sprintf("replicates per scenario: %d; clades: %d; maps: %d; bin: %g Ma; draws: %d",
                     n_reps, N_CLADES, N_MAPS, BIN, N_DRAWS),
             sprintf("run time: %s", format(Sys.time() - t0)),
             utils::capture.output(utils::sessionInfo())),
           file.path(out_dir, "simulation_session.txt"))
print(summ)
print(bw_summ)

# Figure: one example replicate per scenario (counts and clade-weighted rates).
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
p <- ggplot2::ggplot(example, ggplot2::aes(x, y)) +
  ggplot2::annotate("rect", xmin = PULSE_FROM, xmax = PULSE_TO, ymin = -Inf, ymax = Inf, alpha = 0.12, fill = "grey40") +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lo, ymax = hi), alpha = 0.25, fill = "#2a6f97") +
  ggplot2::geom_line(colour = "#2a6f97", linewidth = 0.7) +
  ggplot2::scale_x_reverse() +
  ggplot2::facet_grid(measure ~ scenario, scales = "free_y") +
  ggplot2::labs(x = "Time before present (Ma)", y = NULL,
                title = "Cross-clade synthesis on simulated histories with known rates",
                subtitle = "10 clades, 30 maps each, 2-Ma bins; grey band = simulated pulse window; blue band = 95% interval") +
  ggplot2::theme_bw(base_size = 10) +
  ggplot2::theme(strip.text = ggplot2::element_text(size = 8))
ggplot2::ggsave(file.path(out_dir, "simulation_examples.png"), p, width = 11, height = 7, dpi = 200, bg = "white")
