# Supplementary figure: DEC+J and DEC reconstructions of Amolops side by side,
# with the node-state sensitivity plot. All three panels are unmodified BioGeoSyn
# output read from the Amolops result bundle; this script only arranges them.
#
# Usage: Rscript figureS_amolops_dec_vs_decj.R <amolops.zip> [output stem]
args <- commandArgs(trailingOnly = TRUE)
zip <- args[[1L]]
out <- if (length(args) >= 2L) args[[2L]] else "figureS_amolops_dec_vs_decj"
tmp <- tempfile("amolops-"); dir.create(tmp)
files <- c("figures/node_state_summary_best_model.png", "figures/node_state_summary_best_non_j.png",
           "figures/node_state_sensitivity.png")
utils::unzip(zip, files = files, exdir = tmp, junkpaths = TRUE)
img <- lapply(basename(files), function(f) png::readPNG(file.path(tmp, f)))
labs <- c("a  DEC+J (best by AICc)", "b  DEC (best model without +J)", "c  Most probable range at each node, DEC vs DEC+J")
draw <- function() {
  grid::grid.newpage()
  lay <- grid::grid.layout(2, 2, heights = grid::unit(c(1.05, 0.75), "null"))
  grid::pushViewport(grid::viewport(layout = lay))
  cell <- function(i, row, col) {
    grid::pushViewport(grid::viewport(layout.pos.row = row, layout.pos.col = col))
    grid::grid.text(labs[[i]], x = grid::unit(2, "mm"), y = grid::unit(1, "npc") - grid::unit(2, "mm"),
                    just = c("left", "top"), gp = grid::gpar(fontsize = 10, fontface = "bold"))
    grid::pushViewport(grid::viewport(y = grid::unit(0.47, "npc"), height = grid::unit(0.9, "npc")))
    grid::grid.raster(img[[i]])
    grid::popViewport(2)
  }
  cell(1, 1, 1); cell(2, 1, 2)
  grid::pushViewport(grid::viewport(layout.pos.row = 2, layout.pos.col = 1:2))
  grid::grid.text(labs[[3]], x = grid::unit(2, "mm"), y = grid::unit(1, "npc") - grid::unit(2, "mm"),
                  just = c("left", "top"), gp = grid::gpar(fontsize = 10, fontface = "bold"))
  grid::pushViewport(grid::viewport(y = grid::unit(0.46, "npc"), height = grid::unit(0.88, "npc")))
  grid::grid.raster(img[[3]])
  grid::popViewport(2)
}
grDevices::png(paste0(out, ".png"), width = 12, height = 10.5, units = "in", res = 300, type = "cairo")
draw(); grDevices::dev.off()
grDevices::cairo_pdf(paste0(out, ".pdf"), width = 12, height = 10.5)
draw(); grDevices::dev.off()
