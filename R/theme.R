# Shared publication plotting style for iBiogeobears figures.
#
# Colours use the Okabe-Ito colourblind-safe palette. The categorical order is
# fixed (never cycled); the two-colour process palette and the sequential
# heatmap ramp are validated for CVD separation and lightness monotonicity.

ibgb_palette <- function() {
  list(
    process = c(
      "Cladogenetic (speciation mode)" = "#0072B2",
      "Anagenetic (range evolution)" = "#D55E00"
    ),
    plus_j = c("No +J" = "#0072B2", "+J" = "#D55E00"),
    qualitative = c(
      "#0072B2", "#E69F00", "#009E73", "#CC79A7",
      "#56B4E9", "#D55E00", "#000000"
    ),
    sequential_low = "#eaf2fb",
    sequential_high = "#08519c",
    ink = "#1f2937",
    muted = "#6b7280",
    grid = "#e5e7eb",
    outline = "#374151"
  )
}

# Return n categorical colours in fixed order, extending by interpolation only
# when a plot genuinely needs more than the seven base hues.
ibgb_qual_colors <- function(n) {
  base <- ibgb_palette()$qualitative
  if (n <= length(base)) {
    return(unname(base[seq_len(n)]))
  }
  grDevices::colorRampPalette(base)(n)
}

#' iBiogeobears ggplot theme
#'
#' A clean, publication-oriented ggplot2 theme used by the package figures.
#'
#' @param base_size Base font size in points.
#' @param base_family Base font family; empty string uses the device default.
#' @return A ggplot2 theme object.
#' @export
theme_ibgb <- function(base_size = 12, base_family = "") {
  pal <- ibgb_palette()
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold", size = ggplot2::rel(1.15), colour = pal$ink,
        margin = ggplot2::margin(b = 3)
      ),
      plot.subtitle = ggplot2::element_text(
        size = ggplot2::rel(0.9), colour = pal$muted,
        margin = ggplot2::margin(b = 10)
      ),
      plot.caption = ggplot2::element_text(size = ggplot2::rel(0.75), colour = pal$muted),
      axis.title = ggplot2::element_text(colour = pal$muted, size = ggplot2::rel(0.95)),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 6)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 6)),
      axis.text = ggplot2::element_text(colour = pal$ink),
      panel.grid.major = ggplot2::element_line(colour = pal$grid, linewidth = 0.35),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      plot.background = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(colour = pal$muted, size = ggplot2::rel(0.9)),
      legend.text = ggplot2::element_text(colour = pal$ink, size = ggplot2::rel(0.9)),
      legend.key.size = ggplot2::unit(0.95, "lines"),
      strip.text = ggplot2::element_text(
        face = "bold", colour = pal$ink, size = ggplot2::rel(0.95),
        margin = ggplot2::margin(4, 4, 4, 4)
      ),
      strip.background = ggplot2::element_rect(fill = "#f3f4f6", colour = NA),
      plot.margin = ggplot2::margin(12, 14, 10, 12)
    )
}

# Discrete fill/colour scales using the fixed categorical palette.
scale_fill_ibgb <- function(...) {
  ggplot2::discrete_scale(aesthetics = "fill", palette = ibgb_qual_colors, ...)
}

scale_colour_ibgb <- function(...) {
  ggplot2::discrete_scale(aesthetics = "colour", palette = ibgb_qual_colors, ...)
}

# Sequential single-hue fill for magnitude (e.g. heatmaps).
scale_fill_ibgb_seq <- function(name = "Mean count", ...) {
  pal <- ibgb_palette()
  ggplot2::scale_fill_gradient(low = pal$sequential_low, high = pal$sequential_high, name = name, ...)
}
