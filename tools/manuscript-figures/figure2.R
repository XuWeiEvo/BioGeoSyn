# Figure 2. Cross-clade synthesis, drawn entirely with grid.
#
# Usage: Rscript tools/manuscript-figures/figure2.R [output_dir]
# Writes figure2_cross_clade.pdf and figure2_cross_clade.png (600 dpi), 180 mm wide.
# The curves are illustrative functions, not data.

args <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args) >= 1) args[[1]] else "manuscript-figures-output"
here <- local({
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if (length(f)) dirname(normalizePath(f)) else "tools/manuscript-figures"
})
source(file.path(here, "schematic_helpers.R"))

W <- 180; H <- 104

chip <- function(x0, y0, w, h, label, fill, col, size = 5.4) {
  box(x0, y0, x0 + w, y0 + h, fill = fill, col = col, lwd = 0.7, r = 1.2)
  txt(label, x0 + w / 2, y0 + h / 2, size = size, col = COL$ink)
}

# Draw a curve given in [0, 1] coordinates inside the plot rectangle (x0, y0, x1, y1).
plot_line <- function(t, v, x0, y0, x1, y1, col, lwd = 1.3) {
  grid.lines(nat(x0 + t * (x1 - x0)), nat(y0 + v * (y1 - y0)), gp = gpar(col = col, lwd = lwd))
}
plot_band <- function(t, lo, hi, x0, y0, x1, y1, fill) {
  grid.polygon(nat(x0 + c(t, rev(t)) * (x1 - x0)), nat(y0 + c(lo, rev(hi)) * (y1 - y0)),
               gp = gpar(fill = fill, col = NA))
}
pulse <- function(t, base, height, centre = 0.62, width = 0.13, trend = 0) {
  base + trend * t + height * exp(-((t - centre) / width)^2)
}

draw_figure2 <- function() {
  open_page(W, H)
  head <- function(letter, label, x) {
    txt(letter, x, 98, size = 8, face = "bold", hjust = 0)
    txt(label, x + 4, 98, size = 7, face = "bold", hjust = 0)
  }
  head("a", "Per-clade analyses", 3)
  head("b", "Common vocabulary", 62)
  head("c", "Integrated synthesis", 116)

  # (a) clades, each analysed separately with the same model and settings.
  clades <- list(
    list(name = "Clade A", col = COL$blue, tips = c(COL$sky, COL$pink, COL$sky, COL$pink, COL$pink)),
    list(name = "Clade B", col = COL$gold, tips = c(COL$pink, COL$sky, COL$sky, COL$pink, COL$sky)),
    list(name = "Clade C", col = COL$green, tips = c(COL$sky, COL$sky, COL$pink, COL$sky, COL$pink))
  )
  ys <- c(70, 46, 22); ch <- 21
  for (k in 1:3) {
    y0 <- ys[[k]]; cl <- clades[[k]]
    box(3, y0, 52, y0 + ch, fill = "#FAFBFC", col = cl$col, lwd = 1, r = 2)
    mini_tree(6, y0 + 2.5, 14, ch - 5, tips = cl$tips, col = COL$ink, lwd = 0.8, dot = 0.95)
    txt(cl$name, 26, y0 + ch - 5.5, size = 7, face = "bold", col = cl$col, hjust = 0)
    txt("DEC \u00b7 BSM", 26, y0 + ch - 10.5, size = 5.8, col = COL$muted, hjust = 0)
    txt("\u2192 result bundle", 26, y0 + ch - 15, size = 5.8, col = COL$muted, hjust = 0)
  }
  txt("\u2026 for each of N clades, on one or several trees", 27.5, 18.4, size = 5.2, col = COL$muted, face = "italic")

  # (b) the seven processes, and the compatibility check run on upload.
  box(62, 32, 108, 84, fill = "#EEF2F6", col = "#3E4C59", lwd = 0.9, r = 2.5)
  txt("Cladogenetic (at nodes)", 64.5, 76.5, size = 5.9, face = "bold", col = COL$blue, hjust = 0)
  chip(64.5, 66.5, 20, 6.5, "In-situ spec.", COL$blue_fill, COL$blue)
  chip(85.5, 66.5, 20, 6.5, "Subset symp.", COL$blue_fill, COL$blue)
  chip(64.5, 58.8, 20, 6.5, "Vicariance", COL$blue_fill, COL$blue)
  chip(85.5, 58.8, 20, 6.5, "Founder (+J)", COL$blue_fill, COL$blue)
  txt("Anagenetic (on branches)", 64.5, 51.5, size = 5.9, face = "bold", col = COL$orange, hjust = 0)
  chip(64.5, 41.5, 13.2, 6.5, "Range exp.", COL$orange_fill, COL$orange, size = 5)
  chip(78.5, 41.5, 13.2, 6.5, "Local ext.", COL$orange_fill, COL$orange, size = 5)
  chip(92.5, 41.5, 13.2, 6.5, "Range sw.", COL$orange_fill, COL$orange, size = 5)
  txt("Labels standardize events;\nthe model still decides\nwhich processes can occur", 85, 36.2, size = 4.8,
      col = COL$muted, face = "italic", lineheight = 1.05)
  box(62, 17, 108, 28, fill = "white", col = COL$faint, lwd = 0.7, r = 1.5, lty = 2)
  txt("Compatibility check on upload", 85, 25, size = 5.6, face = "bold", col = COL$ink)
  txt("area coding \u00b7 model \u00b7 range size\nconstraints \u00b7 time unit \u00b7 maps", 85, 20.6, size = 5,
      col = COL$muted, lineheight = 1.1)
  seg(85, 28, 85, 31.6, col = COL$faint, lwd = 0.8, arrow_len = 1.3)

  for (k in 1:3) curve_arrow(52.5, ys[[k]] + ch / 2, 61.5, c(70, 58, 46)[[k]],
                             curvature = c(-0.15, 0, 0.15)[[k]])

  # (c) pooled curves through time.
  px0 <- 122; px1 <- 177; py0 <- 56; py1 <- 82
  txt("Pooled across clades through time", 116, 91.5, size = 6.4, face = "bold", hjust = 0)
  lg <- list(c("In-situ speciation", COL$blue), c("Vicariance", COL$gold), c("Range expansion", COL$green))
  lx <- c(116, 138.5, 155)
  for (k in 1:3) {
    seg(lx[[k]], 87.4, lx[[k]] + 4, 87.4, col = lg[[k]][[2]], lwd = 1.4)
    txt(lg[[k]][[1]], lx[[k]] + 5, 87.4, size = 5.2, col = COL$muted, hjust = 0)
  }
  box(116.5, 50.5, 178, 84.5, fill = "white", col = "#E1E6EC", lwd = 0.6, r = 1.5)
  seg(px0, py0, px0, py1, col = COL$faint, lwd = 0.6)
  seg(px0, py0, px1, py0, col = COL$faint, lwd = 0.6)
  t <- seq(0, 1, length.out = 120)
  insitu <- pulse(t, 0.12, 0.55, trend = 0.08)
  plot_band(t, insitu - 0.07 - 0.05 * t, insitu + 0.07 + 0.05 * t, px0, py0, px1, py1, "#D6E6F3")
  plot_line(t, insitu, px0, py0, px1, py1, COL$blue)
  plot_line(t, pulse(t, 0.08, 0.32, trend = 0.06), px0, py0, px1, py1, COL$gold)
  plot_line(t, pulse(t, 0.05, 0.2, trend = 0.1), px0, py0, px1, py1, COL$green)
  xp <- px0 + 0.62 * (px1 - px0)
  seg(xp, py0, xp, py1 - 1, col = COL$orange, lwd = 0.9, lty = 2)
  txt("candidate shared pulse", xp, py1 + 0.4, size = 5, col = COL$orange, face = "bold")
  grid.text("events or per-lineage rate", nat(119.2), nat((py0 + py1) / 2), rot = 90,
            gp = gpar(fontsize = 4.9, col = COL$muted, fontfamily = FIG_FONT))
  txt("time before present", (px0 + px1) / 2, 53.2, size = 4.9, col = COL$muted)
  txt("older", px0, 53.2, size = 4.6, col = COL$faint, hjust = 0)
  txt("present", px1, 53.2, size = 4.6, col = COL$faint, hjust = 1)

  # Region-resolved panels.
  txt("Region-resolved", 116, 46, size = 6.4, face = "bold", hjust = 0)
  seg(141, 46, 145, 46, col = COL$blue, lwd = 1.3)
  txt("In-situ speciation", 146, 46, size = 5.2, col = COL$muted, hjust = 0)
  seg(161, 46, 165, 46, col = COL$pink, lwd = 1.3)
  txt("Immigration", 166, 46, size = 5.2, col = COL$muted, hjust = 0)
  for (k in 1:2) {
    x0 <- 116.5 + (k - 1) * 31; x1 <- x0 + 30
    box(x0, 23, x1, 42.5, fill = "white", col = "#E1E6EC", lwd = 0.6, r = 1.5)
    rx0 <- x0 + 2.5; rx1 <- x1 - 2.5; ry0 <- 25.5; ry1 <- 40.5
    if (k == 1) {
      plot_line(t, pulse(t, 0.1, 0.65, centre = 0.6, width = 0.17), rx0, ry0, rx1, ry1, COL$blue, lwd = 1.1)
      plot_line(t, pulse(t, 0.05, 0.2, centre = 0.75, width = 0.2, trend = 0.15), rx0, ry0, rx1, ry1, COL$pink, lwd = 1.1)
    } else {
      plot_line(t, pulse(t, 0.08, 0.2, centre = 0.4, width = 0.15), rx0, ry0, rx1, ry1, COL$blue, lwd = 1.1)
      plot_line(t, pulse(t, 0.06, 0.7, centre = 0.63, width = 0.13, trend = 0.1), rx0, ry0, rx1, ry1, COL$pink, lwd = 1.1)
    }
    txt(paste("Region", k), (x0 + x1) / 2, 20.6, size = 5.6, col = COL$muted)
  }
  txt("and routes, budgets, exchange matrix, networks by period", 147, 16.8, size = 5, col = COL$muted, face = "italic")

  curve_arrow(108.5, 72, 115.8, 78, curvature = -0.1)
  curve_arrow(108.5, 50, 115.8, 34, curvature = 0.15)

  # Bottom message.
  box(3, 1.5, 177, 12.5, fill = COL$orange_fill, col = COL$orange, lwd = 0.9, r = 2)
  txt("Shared process labels on a common time axis let the concordance of events be described across clades.",
      90, 8.9, size = 6.4, face = "bold", col = COL$orange)
  txt("Intervals come from resampling one stochastic map per clade (and one tree, when several are analysed); testing synchrony needs a null model.",
      90, 4.9, size = 5.4, col = COL$orange)
}

files <- save_figure(draw_figure2, "figure2_cross_clade", W, H, out_dir)
message("Wrote ", paste(files, collapse = ", "))
