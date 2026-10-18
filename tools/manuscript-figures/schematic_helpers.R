# Drawing helpers shared by the schematic manuscript figures (Figures 1 and 2).
#
# Everything is drawn with the grid package from coordinates in millimetres,
# so the figures are fully reproducible from code and contain no raster or
# generated imagery. Coordinates are native units of a viewport whose scales
# equal the page size in mm.

library(grid)

FIG_FONT <- Sys.getenv("BGS_FIG_FONT", if (.Platform$OS.type == "windows") "Arial" else "Liberation Sans")
FIG_MONO <- Sys.getenv("BGS_FIG_MONO", if (.Platform$OS.type == "windows") "Consolas" else "DejaVu Sans Mono")

COL <- list(
  ink = "#1F2933", muted = "#5B6672", faint = "#9AA5B1", line = "#CBD2D9",
  panel = "#F5F7FA", card = "#FFFFFF",
  blue = "#0B6AA2", blue_light = "#9CC3DE", blue_fill = "#E8F2FA",
  orange = "#C8540C", orange_fill = "#FCEFE6",
  green = "#138A62", gold = "#C08A00", pink = "#C26BA0", sky = "#4FA9DD"
)

nat <- function(v) unit(v, "native")

open_page <- function(width_mm, height_mm) {
  grid.newpage()
  pushViewport(viewport(xscale = c(0, width_mm), yscale = c(0, height_mm)))
}

txt <- function(label, x, y, size = 7, col = COL$ink, face = "plain", hjust = 0.5, vjust = 0.5,
                family = FIG_FONT, lineheight = 1.1) {
  grid.text(label, nat(x), nat(y), hjust = hjust, vjust = vjust,
            gp = gpar(fontsize = size, col = col, fontface = face, fontfamily = family,
                      lineheight = lineheight))
}

box <- function(x0, y0, x1, y1, fill = COL$card, col = COL$line, lwd = 0.8, r = 2, lty = 1) {
  grid.roundrect(nat((x0 + x1) / 2), nat((y0 + y1) / 2), nat(x1 - x0), nat(y1 - y0),
                 r = unit(r, "mm"), gp = gpar(fill = fill, col = col, lwd = lwd, lty = lty))
}

seg <- function(x0, y0, x1, y1, col = COL$ink, lwd = 0.8, lty = 1, arrow_len = NULL, lineend = "butt") {
  arr <- if (is.null(arrow_len)) NULL else arrow(length = unit(arrow_len, "mm"), type = "closed")
  grid.segments(nat(x0), nat(y0), nat(x1), nat(y1), arrow = arr,
                gp = gpar(col = col, fill = col, lwd = lwd, lty = lty, lineend = lineend))
}

curve_arrow <- function(x0, y0, x1, y1, curvature = 0.2, col = COL$faint, lwd = 1) {
  grid.curve(nat(x0), nat(y0), nat(x1), nat(y1), curvature = curvature, ncp = 8, square = FALSE,
             arrow = arrow(length = unit(1.6, "mm"), type = "closed"),
             gp = gpar(col = col, fill = col, lwd = lwd))
}

chevron <- function(x, y, w = 2.2, h = 3, col = COL$faint) {
  grid.polygon(nat(c(x, x + w, x)), nat(c(y - h / 2, y, y + h / 2)), gp = gpar(fill = col, col = NA))
}

badge <- function(x, y, label, fill = COL$muted, r = 2.3, size = 7) {
  grid.circle(nat(x), nat(y), r = unit(r, "mm"), gp = gpar(fill = fill, col = NA))
  txt(label, x, y, size = size, col = "white", face = "bold")
}

document_icon <- function(x, y, w = 7, h = 9, col = COL$faint, fill = "#EEF1F5") {
  f <- 2
  grid.polygon(nat(c(x, x + w, x + w, x + w - f, x)), nat(c(y, y, y + h - f, y + h, y + h)),
               gp = gpar(fill = fill, col = col, lwd = 0.6))
  grid.polygon(nat(c(x + w - f, x + w - f, x + w)), nat(c(y + h, y + h - f, y + h - f)),
               gp = gpar(fill = "white", col = col, lwd = 0.6))
  for (k in 1:3) seg(x + 1.3, y + h - 2.6 - 1.5 * k, x + w - 1.6, y + h - 2.6 - 1.5 * k, col = col, lwd = 0.6)
}

# A small rooted tree in the cladogram style used in the figures. tips gives the
# colours of the tip dots from top to bottom; the topology is ((1,2),3),((4,5)).
mini_tree <- function(x, y, w, h, tips, col = COL$ink, lwd = 0.9, dot = 1) {
  ty <- y + h * c(1, 0.78, 0.56, 0.3, 0.06)[seq_along(tips)]
  xr <- x
  x1 <- x + w * 0.22
  x2 <- x + w * 0.5
  x3 <- x + w * 0.72
  xe <- x + w
  n12 <- mean(ty[1:2]); n123 <- mean(c(n12, ty[3])); n45 <- mean(ty[4:5]); root <- mean(c(n123, n45))
  s <- function(...) seg(..., col = col, lwd = lwd, lineend = "square")
  s(xr, root, x1, root)
  s(x1, n123, x1, n45)
  s(x1, n123, x2, n123); s(x2, n12, x2, ty[3])
  s(x2, n12, x3, n12); s(x3, ty[1], x3, ty[2])
  s(x3, ty[1], xe, ty[1]); s(x3, ty[2], xe, ty[2]); s(x2, ty[3], xe, ty[3])
  s(x1, n45, x3, n45); s(x3, ty[4], x3, ty[5]); s(x3, ty[4], xe, ty[4]); s(x3, ty[5], xe, ty[5])
  for (k in seq_along(tips)) grid.circle(nat(xe + dot * 0.6), nat(ty[k]), r = unit(dot, "mm"),
                                          gp = gpar(fill = tips[[k]], col = NA))
}

save_figure <- function(draw, stem, width_mm, height_mm, out_dir) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  w <- width_mm / 25.4; h <- height_mm / 25.4
  grDevices::cairo_pdf(file.path(out_dir, paste0(stem, ".pdf")), width = w, height = h)
  draw(); grDevices::dev.off()
  grDevices::png(file.path(out_dir, paste0(stem, ".png")), width = w, height = h, units = "in",
                 res = 600, type = "cairo")
  draw(); grDevices::dev.off()
  invisible(file.path(out_dir, paste0(stem, c(".pdf", ".png"))))
}
