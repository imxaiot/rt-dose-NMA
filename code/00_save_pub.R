#!/usr/bin/env Rscript
# ============================================================
# 00_save_pub.R
# Publication-grade figure saving helper
#   Outputs in three vector/raster formats:
#     - SVG (editable in Illustrator/Inkscape)
#     - PDF (Cairo with embedded fonts, journal standard)
#     - TIFF 600 dpi (optional, if reviewers request)
#     - PNG 600 dpi (preview)
#   Default font: Arial 8 pt body, 10 pt title
#
# Usage (from any analysis script):
#   source("code/00_save_pub.R")
#   save_pub(p, "Figure_1", width_mm = 183, height_mm = 120)
#
# Output directory is `figures/` relative to the project root,
# created automatically if it does not exist.
# ============================================================

# Install required packages on first run
for (pkg in c("svglite", "ragg", "systemfonts")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}
suppressPackageStartupMessages({
  library(ggplot2)
  library(svglite)
  library(ragg)
})

# Publication theme (Arial 8 pt)
theme_pub <- function(base_size = 8) {
  theme_minimal(base_size = base_size, base_family = "Arial") +
    theme(
      axis.line          = element_line(linewidth = 0.35, colour = "black"),
      axis.ticks         = element_line(linewidth = 0.35, colour = "black"),
      axis.text          = element_text(size = base_size - 0.5),
      axis.title         = element_text(size = base_size + 0.5),
      legend.title       = element_text(size = base_size + 0.2),
      legend.text        = element_text(size = base_size - 0.5),
      legend.key.size    = unit(0.4, "cm"),
      strip.text         = element_text(size = base_size + 0.2,
                                        face = "bold"),
      plot.title         = element_text(size = base_size + 2,
                                        face = "bold", hjust = 0),
      plot.subtitle      = element_text(size = base_size, colour = "grey30"),
      plot.caption       = element_text(size = base_size - 1,
                                        colour = "grey45"),
      panel.grid.minor   = element_blank()
    )
}

# Main save function
# Arguments:
#   plot       : a ggplot object (or any base-R plot wrapped in a function)
#   filename   : output filename stem (no extension)
#   output_dir : output directory relative to working directory (default "figures")
#   width_mm   : figure width in millimetres
#   height_mm  : figure height in millimetres
#   dpi        : resolution for raster outputs (default 600)
#   formats    : character vector subset of c("svg", "pdf", "tiff", "png")
save_pub <- function(plot, filename,
                     output_dir = "figures",
                     width_mm = 183, height_mm = 120,
                     dpi = 600,
                     formats = c("svg", "pdf", "png")) {
  w <- width_mm / 25.4
  h <- height_mm / 25.4

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  base <- file.path(output_dir, filename)

  if ("svg" %in% formats) {
    svglite::svglite(paste0(base, ".svg"),
                     width = w, height = h,
                     bg = "white")
    print(plot); invisible(dev.off())
  }
  if ("pdf" %in% formats) {
    grDevices::cairo_pdf(paste0(base, ".pdf"),
                         width = w, height = h,
                         family = "Arial", bg = "white")
    print(plot); invisible(dev.off())
  }
  if ("tiff" %in% formats) {
    ragg::agg_tiff(paste0(base, ".tiff"),
                   width = w, height = h, units = "in",
                   res = dpi, compression = "lzw",
                   background = "white")
    print(plot); invisible(dev.off())
  }
  if ("png" %in% formats) {
    ragg::agg_png(paste0(base, ".png"),
                  width = w, height = h, units = "in",
                  res = dpi, background = "white")
    print(plot); invisible(dev.off())
  }

  cat("Saved:", paste0(base, ".[", paste(formats, collapse = "|"), "]"), "\n")
}
