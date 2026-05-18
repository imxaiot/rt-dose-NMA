#!/usr/bin/env Rscript
# ============================================================
# 01_PRISMA_flow.R
# PRISMA 2020 flow diagram (official style)
#   - Orange banner at top
#   - Rotated blue side bars (Identification / Screening / Included)
#   - Plain white boxes with thin black borders
# Output: figures/Figure_1_PRISMA.{svg, pdf, png}
# ============================================================

library(ggplot2)
library(dplyr)

# ---- 1. Counts at each PRISMA stage ------------------------
counts <- list(
  records_db          = 5759,
  duplicates          = 2330,
  records_screened    = 3429,
  records_excluded    = 2965,
  reports_sought      = 464,
  reports_unretrieved = 3,
  reports_assessed    = 461,
  reports_excluded    = 438,   # 461 - 23
  studies_included    = 23,

  excl_reasons = list(
    "Not RCT / quasi-RCT"                            = 7,
    "Intervention not pure RT (mixed training)"      = 14,
    "Population not healthy older adults / age <60"  = 2,
    "No functional outcome reported"                 = 9,
    "Intervention duration < 8 weeks"                = 1,
    "Full text unobtainable / data not extractable"  = 2,
    "Other (e.g. study design issue)"                = 1,
    "Broad ineligibility (wrong P/I/C/O)"            = 402   # 438 - 36
  )
)

# ---- 2. Coordinates ----------------------------------------
# x: three columns
x_sidebar  <- -4.7
x_left     <- -2.3
x_right    <-  2.5

# y: main flow (top-down)
y_banner   <- 13.5   # top orange banner
y_db       <- 12.0
y_sc2      <-  9.5
y_sc3      <-  7.5
y_sc4      <-  4.5
y_inc2     <-  1.5

# Sidebar y ranges
yb_id_top  <- y_db + 1.0
yb_id_bot  <- y_db - 1.0
yb_sc_top  <- y_sc2 + 1.0
yb_sc_bot  <- y_sc4 - 3.4/2 - 0.4
yb_inc_top <- y_inc2 + 0.9
yb_inc_bot <- y_inc2 - 0.9

# Colours
col_orange <- "#e89125"
col_blue   <- "#357ec7"

# ---- 3. Text formatting helpers ----------------------------
fmt <- function(title, n, sub = NULL) {
  s <- sprintf("%s\n(n = %s)", title, format(n, big.mark = ","))
  if (!is.null(sub)) s <- paste0(s, "\n\n", sub)
  s
}
fmt_excl <- function(reasons) {
  lines <- sapply(seq_along(reasons), function(i) {
    sprintf("   %s (n = %s)", names(reasons)[i],
            format(reasons[[i]], big.mark = ","))
  })
  paste0(sprintf("Reports excluded with reasons (n = %s):\n",
                 format(sum(unlist(reasons)), big.mark = ",")),
         paste(lines, collapse = "\n"))
}

# ---- 4. Flow-diagram boxes ---------------------------------
box <- function(xc, yc, w = 3.0, h = 1.4, label = "") {
  tibble(xc = xc, yc = yc, w = w, h = h, label = label)
}

boxes <- bind_rows(
  # Identification row
  box(x_left,  y_db, w = 3.4, h = 1.5,
      label = fmt("Records identified from:\nDatabases", counts$records_db)),
  box(x_right, y_db, w = 3.3, h = 1.5,
      label = sprintf("Records removed before\nscreening:\n  Duplicate records removed\n  (n = %s)",
                      format(counts$duplicates, big.mark = ","))),
  # Screening rows
  box(x_left,  y_sc2, w = 3.0, h = 1.1,
      label = fmt("Records screened", counts$records_screened)),
  box(x_right, y_sc2, w = 3.0, h = 1.1,
      label = fmt("Records excluded", counts$records_excluded)),
  box(x_left,  y_sc3, w = 3.0, h = 1.1,
      label = fmt("Reports sought for retrieval", counts$reports_sought)),
  box(x_right, y_sc3, w = 3.0, h = 1.1,
      label = fmt("Reports not retrieved", counts$reports_unretrieved)),
  box(x_left,  y_sc4, w = 3.0, h = 1.1,
      label = fmt("Reports assessed for eligibility", counts$reports_assessed)),
  box(x_right, y_sc4, w = 4.4, h = 3.4,
      label = fmt_excl(counts$excl_reasons)),
  # Included row
  box(x_left,  y_inc2, w = 3.4, h = 1.2,
      label = sprintf("Studies included in NMA (n = %d)\nReports of included studies (n = %d)",
                      counts$studies_included, counts$studies_included))
)

# ---- 5. Top banner and rotated side bars -------------------
banner <- tibble(
  xmin = -5.0, xmax = 5.0,
  ymin = y_banner - 0.4, ymax = y_banner + 0.4,
  label = "Identification of studies via databases and registers"
)

sidebars <- tibble(
  xmin = x_sidebar - 0.35,
  xmax = x_sidebar + 0.35,
  ymin = c(yb_id_bot, yb_sc_bot, yb_inc_bot),
  ymax = c(yb_id_top, yb_sc_top, yb_inc_top),
  yc   = c((yb_id_bot + yb_id_top)/2,
           (yb_sc_bot + yb_sc_top)/2,
           (yb_inc_bot + yb_inc_top)/2),
  label = c("Identification", "Screening", "Included"),
  fill  = c(col_orange, col_blue, col_blue)
)

# ---- 6. Arrows ---------------------------------------------
arrows <- tribble(
  ~x,     ~y,        ~xend,    ~yend,
  # Left vertical chain
  x_left, y_db - 0.75,  x_left,  y_sc2 + 0.55,
  x_left, y_sc2 - 0.55, x_left,  y_sc3 + 0.55,
  x_left, y_sc3 - 0.55, x_left,  y_sc4 + 0.55,
  x_left, y_sc4 - 0.55, x_left,  y_inc2 + 0.6,
  # Right horizontal arrows (exclusion side)
  x_left + 1.7, y_db,   x_right - 1.65, y_db,
  x_left + 1.5, y_sc2,  x_right - 1.5,  y_sc2,
  x_left + 1.5, y_sc3,  x_right - 1.5,  y_sc3,
  x_left + 1.5, y_sc4,  x_right - 2.2,  y_sc4
)

# ---- 7. Compose plot ---------------------------------------
p <- ggplot() +
  # Top banner
  geom_rect(data = banner,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = col_orange, color = NA) +
  geom_text(data = banner,
            aes(x = (xmin + xmax)/2, y = (ymin + ymax)/2, label = label),
            color = "white", fontface = "bold", size = 4.3) +

  # Rotated side bars
  geom_rect(data = sidebars,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                fill = fill),
            color = NA) +
  geom_text(data = sidebars,
            aes(x = (xmin + xmax)/2, y = yc, label = label),
            color = "white", fontface = "bold", size = 3.4, angle = 90) +
  scale_fill_identity() +

  # Flow-diagram boxes (white fill + thin black border)
  geom_rect(data = boxes,
            aes(xmin = xc - w/2, xmax = xc + w/2,
                ymin = yc - h/2, ymax = yc + h/2),
            fill = "white", color = "grey15", linewidth = 0.5) +
  geom_text(data = boxes,
            aes(x = xc - w/2 + 0.12, y = yc, label = label),
            size = 2.65, lineheight = 1.05, color = "grey10",
            hjust = 0, vjust = 0.5) +

  # Arrows
  geom_segment(data = arrows,
               aes(x = x, y = y, xend = xend, yend = yend),
               arrow = arrow(length = unit(0.18, "cm"),
                             type = "closed"),
               linewidth = 0.45, color = "grey15") +

  coord_cartesian(xlim = c(-5.3, 5.3), ylim = c(0.3, 14.2)) +
  theme_void() +
  theme(
    plot.margin = margin(15, 10, 15, 10),
    plot.title  = element_text(face = "bold", size = 13, hjust = 0.5,
                               margin = margin(b = 10))
  ) +
  labs(title = "PRISMA 2020 flow diagram — NMA of dose-response of resistance training in older adults")

source("code/00_save_pub.R")
save_pub(p, "Figure_1_PRISMA",
         width_mm = 279, height_mm = 254,
         formats = c("svg", "pdf", "png"))

# ---- Sanity check: counts must balance ---------------------
cat("\n=== Flow consistency check ===\n")
cat(sprintf("5759 - 2330 = %d  (should equal records_screened = %d)\n",
            5759 - 2330, counts$records_screened))
cat(sprintf("%d - %d = %d  (should equal reports_sought = %d)\n",
            counts$records_screened, counts$records_excluded,
            counts$records_screened - counts$records_excluded,
            counts$reports_sought))
cat(sprintf("%d - %d = %d  (should equal reports_assessed = %d)\n",
            counts$reports_sought, counts$reports_unretrieved,
            counts$reports_sought - counts$reports_unretrieved,
            counts$reports_assessed))
cat(sprintf("%d - %d = %d  (should equal studies_included = %d)\n",
            counts$reports_assessed, counts$reports_excluded,
            counts$reports_assessed - counts$reports_excluded,
            counts$studies_included))
cat(sprintf("Sum of exclusion reasons = %d  (should equal reports_excluded = %d)\n",
            sum(unlist(counts$excl_reasons)), counts$reports_excluded))
