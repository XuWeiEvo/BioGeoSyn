# Figure 1. The BioGeoSyn single-clade workflow, drawn entirely with grid.
#
# Usage: Rscript tools/manuscript-figures/figure1.R [output_dir]
# Writes figure1_workflow.pdf and figure1_workflow.png (600 dpi), 180 mm wide.

args <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args) >= 1) args[[1]] else "manuscript-figures-output"
here <- local({
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if (length(f)) dirname(normalizePath(f)) else "tools/manuscript-figures"
})
source(file.path(here, "schematic_helpers.R"))

W <- 180; H <- 68

draw_figure1 <- function() {
  open_page(W, H)
  box(2, 19, 178, 66, fill = COL$panel, col = "#E1E6EC", r = 2.5)
  txt("SHARED R BACKEND FOR GRAPHICAL AND SCRIPTED WORKFLOWS", 5, 62.6,
      size = 5.6, col = COL$faint, face = "bold", hjust = 0)

  cw <- 32.4; gap <- 2.5; y0 <- 22; y1 <- 59
  titles <- c("Inputs and\nchecks", "Fit and\ncompare", "Stochastic\nmapping", "Biogeographic\nprocesses", "Outputs")
  descr <- c(
    "Tree, geography (CSV or\nPHYLIP) and regions;\ninput validation",
    "Six range-evolution\nmodels; AICc weights;\n+J interpretation caution",
    "BSM samples event\nhistories (optional);\none or several trees",
    "Seven named processes;\nrates, regions, routes;\ncount reconciliation",
    "Tables and figures;\noptional report and bundle;\nmanifest, script, versions"
  )
  for (i in 1:5) {
    x0 <- 4 + (i - 1) * (cw + gap); x1 <- x0 + cw
    focal <- i == 4
    box(x0, y0, x1, y1, fill = if (focal) COL$blue_fill else COL$card,
        col = if (focal) COL$blue else COL$line, lwd = if (focal) 1.1 else 0.7)
    badge(x0 + 4.2, y1 - 5, as.character(i), fill = if (i %in% 2:4) COL$blue else COL$muted, r = 2.4, size = 6.5)
    txt(titles[[i]], x0 + 7.8, y1 - 5, size = 6.6, face = "bold", hjust = 0,
        col = if (i %in% 2:4) COL$blue else COL$ink, lineheight = 1)
    txt(descr[[i]], x0 + 2.6, y0 + 2.6, size = 5.3, col = COL$muted, hjust = 0, vjust = 0, lineheight = 1.15)
    if (i < 5) chevron(x1 + 0.2, (y0 + y1) / 2, w = 2, h = 2.8)
    cx <- (x0 + x1) / 2; cy <- 42
    if (i == 1) {
      for (k in -1:1) document_icon(cx + k * 9 - 3.2, cy - 4.5, w = 6.4, h = 8.5)
    } else if (i == 2) {
      hts <- c(3.2, 6.4, 4.6, 7.6, 5.3, 3.6)
      for (k in seq_along(hts)) {
        bx <- cx - 10.5 + (k - 1) * 3.6
        grid.rect(nat(bx), nat(cy - 4.6), nat(2.6), nat(hts[[k]]), just = c("left", "bottom"),
                  gp = gpar(fill = if (k == 4) COL$blue else COL$blue_light, col = NA))
      }
      seg(cx - 11.5, cy - 4.6, cx + 11.5, cy - 4.6, col = COL$faint, lwd = 0.6)
      txt("lowest AICc", cx - 10.5 + 3 * 3.6 + 1.3, cy + 4.3, size = 4.8, col = COL$blue, face = "bold")
    } else if (i == 3) {
      mini_tree(cx - 9, cy - 5, 16, 10, tips = rep(COL$ink, 5), dot = 0.01, lwd = 0.8)
      tx <- cx - 9; ty <- cy - 5; tw <- 16; th <- 10
      grid.circle(nat(tx + tw * c(0.5, 0.72)), nat(ty + th * c(0.725, 0.18)),
                  r = unit(0.9, "mm"), gp = gpar(fill = COL$blue, col = NA))
      grid.circle(nat(tx + tw * c(0.86, 0.62, 0.9)), nat(ty + th * c(1, 0.56, 0.06)),
                  r = unit(0.9, "mm"), gp = gpar(fill = COL$orange, col = NA))
      txt("node events", cx - 6.8, cy - 7.4, size = 4.6, col = COL$blue, face = "bold")
      txt("branch events", cx + 7.2, cy - 7.4, size = 4.6, col = COL$orange, face = "bold")
    } else if (i == 4) {
      cl <- c(7.6, 3.6, 5.0, 2.6); an <- c(6.0, 3.8, 2.8)
      for (k in 1:4) grid.rect(nat(cx - 11.6 + (k - 1) * 2.9), nat(cy - 4.2), nat(2.3), nat(cl[[k]]),
                               just = c("left", "bottom"), gp = gpar(fill = COL$blue, col = NA))
      seg(cx + 0.6, cy - 4.4, cx + 0.6, cy + 4.6, col = COL$faint, lwd = 0.5, lty = 2)
      for (k in 1:3) grid.rect(nat(cx + 2.4 + (k - 1) * 2.9), nat(cy - 4.2), nat(2.3), nat(an[[k]]),
                               just = c("left", "bottom"), gp = gpar(fill = COL$orange, col = NA))
      txt("cladogenetic", cx - 6.2, cy - 6.1, size = 4.6, col = COL$blue, face = "bold")
      txt("anagenetic", cx + 6.1, cy - 6.1, size = 4.6, col = COL$orange, face = "bold")
    } else {
      box(cx - 12, cy - 4.4, cx - 4.4, cy + 4.4, fill = "white", col = COL$faint, lwd = 0.6, r = 0.8)
      hts <- c(2.4, 4.0, 5.6)
      for (k in 1:3) grid.rect(nat(cx - 11 + (k - 1) * 2.2), nat(cy - 3.2), nat(1.6), nat(hts[[k]]),
                               just = c("left", "bottom"), gp = gpar(fill = c(COL$blue, COL$sky, COL$orange)[[k]], col = NA))
      document_icon(cx - 3, cy - 4.4, w = 6.2, h = 8.8)
      box(cx + 4.8, cy - 4.4, cx + 12, cy + 4.4, fill = "#EEF1F5", col = COL$faint, lwd = 0.6, r = 0.8)
      seg(cx + 8.4, cy - 4.4, cx + 8.4, cy + 4.4, col = COL$faint, lwd = 0.6)
      grid.rect(nat(cx + 8.4), nat(cy + 1.4), nat(1.6), nat(1.6), gp = gpar(fill = "#D5DBE2", col = COL$faint, lwd = 0.4))
    }
  }

  x4 <- 4 + 3 * (cw + gap) + cw / 2
  seg(x4, 19, x4, 14.6, col = COL$orange, lwd = 1, arrow_len = 1.6)
  box(46, 3, 134, 14, fill = COL$orange_fill, col = COL$orange, lwd = 0.9, r = 2)
  txt("Run each clade the same way,", 90, 10.2, size = 6.8, col = COL$orange, face = "bold")
  txt("then integrate the standardized events across clades (Figure 2)", 90, 6.3, size = 6.2, col = COL$orange)
}

files <- save_figure(draw_figure1, "figure1_workflow", W, H, out_dir)
message("Wrote ", paste(files, collapse = ", "))
