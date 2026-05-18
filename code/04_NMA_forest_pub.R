#!/usr/bin/env Rscript
# ============================================================
# 04_NMA_forest_pub.R
# Main-analysis NMA: 8-panel publication-quality forest plot
#   Each panel: one outcome x 2 treatments (low- vs conventional-dose),
#               vs non-exercise control
#   Direction-aligned: positive SMD = functional improvement
#
# Input : outputs/league_tables.xlsx
# Output: figures/Figure_4_Forest.{svg, pdf, png}
# ============================================================

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

# ---- 1. Extract SMD + 95% CI vs noRT from league tables ----
LEAGUE <- "outputs/league_tables.xlsx"
sheets <- excel_sheets(LEAGUE)

parse_smd <- function(x) {
  # Accept formats like "-1.21 (-1.97, -0.46)" with various unicode minus signs
  s <- str_replace_all(x, "[−–—]", "-")
  m <- str_match(s, "^\\s*(-?[0-9.]+)\\s*\\(\\s*(-?[0-9.]+)\\s*,\\s*(-?[0-9.]+)\\s*\\)\\s*$")
  list(
    smd   = as.numeric(m[, 2]),
    lower = as.numeric(m[, 3]),
    upper = as.numeric(m[, 4])
  )
}

extract_one <- function(sheet_name) {
  d <- read_excel(LEAGUE, sheet = sheet_name)
  # First row: Treatment = non_exercise_control;
  # in this row, the low_dose_RT / conventional_dose_RT columns
  # hold the RT-vs-noRT SMDs.
  ref_row <- d %>% filter(Treatment == "non_exercise_control")

  lo  <- parse_smd(ref_row$low_dose_RT)
  cnv <- parse_smd(ref_row$conventional_dose_RT)

  tibble(
    outcome   = sheet_name,
    treatment = c("low_dose_RT", "conventional_dose_RT"),
    SMD       = c(lo$smd,   cnv$smd),
    lower     = c(lo$lower, cnv$lower),
    upper     = c(lo$upper, cnv$upper)
  )
}

forest_df <- bind_rows(lapply(sheets, extract_one))

cat("=== forest_df ===\n")
print(forest_df, n = Inf)

# ---- 2. Direction alignment ---------------------------------
# League tables report SMD without direction-alignment.
# For outcomes where a lower raw score = better function (TUG, 8-FUGT,
# stair_climb, 5xSTS), we flip the sign so that positive SMD uniformly
# indicates functional improvement across all 8 panels.
flip_outcomes <- c("TUG", "8-FUGT", "stair_climb", "5xSTS")
forest_df <- forest_df %>%
  mutate(
    flip = outcome %in% flip_outcomes,
    SMD_dir   = if_else(flip, -SMD,   SMD),
    lower_dir = if_else(flip, -upper, lower),
    upper_dir = if_else(flip, -lower, upper),
    sig       = lower_dir > 0 | upper_dir < 0
  )

# ---- 3. Factor ordering and label formatting ----------------
outcome_order <- c("TUG", "8-FUGT", "stair_climb", "6MWT",
                   "gait_speed_usual", "gait_speed_fast",
                   "5xSTS", "30sSTS")

forest_df <- forest_df %>%
  mutate(
    outcome   = factor(outcome,   levels = outcome_order),
    treatment = factor(treatment,
                       levels = c("conventional_dose_RT", "low_dose_RT"),
                       labels = c("conventional-dose RT", "low-dose RT")),
    label_text = sprintf("%.2f (%.2f, %.2f)%s",
                         SMD_dir, lower_dir, upper_dir,
                         if_else(sig, " *", ""))
  )

# ---- 4. Plot ------------------------------------------------
lim_max <- max(abs(c(forest_df$lower_dir, forest_df$upper_dir)), na.rm = TRUE)
lim_max <- ceiling(lim_max * 10) / 10
label_x <- lim_max + 0.2            # x-position for label column
x_right <- lim_max * 2 + 0.5        # extend canvas: label area ~= data area width

p <- ggplot(forest_df, aes(x = SMD_dir, y = treatment, color = sig)) +
  geom_vline(xintercept = 0, color = "grey60",
             linetype = "dashed", linewidth = 0.4) +
  geom_vline(xintercept = lim_max, color = "grey85",
             linetype = "solid", linewidth = 0.4) +
  geom_errorbarh(aes(xmin = lower_dir, xmax = upper_dir),
                 height = 0.18, linewidth = 0.7) +
  geom_point(size = 3, shape = 15) +
  geom_text(aes(x = label_x, label = label_text),
            hjust = 0, size = 2.4, color = "grey15") +
  scale_color_manual(values = c("TRUE" = "#c0392b", "FALSE" = "#3a6ea5"),
                     guide = "none") +
  scale_x_continuous(
    limits = c(-lim_max, x_right),
    breaks = seq(-floor(lim_max), floor(lim_max), by = 1),
    expand = c(0, 0)
  ) +
  facet_wrap(~ outcome, ncol = 4, scales = "fixed") +
  labs(
    title = "Main NMA effects: low- and conventional-dose RT versus no-RT",
    subtitle = "Random-effects NMA estimates from netmeta. Squares = SMD; horizontal lines = 95% CI; red = CI excludes 0. Positive SMD = functional improvement (direction-aligned).",
    x = "Standardised mean difference (vs no-RT)  ->  improvement",
    y = NULL,
    caption = "Source: outputs/league_tables.xlsx. Effect sign-flipped for outcomes where lower scores mean better function (TUG, 8-FUGT, stair-climb, 5xSTS) so that positive SMDs uniformly indicate improvement across all eight panels."
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.y        = element_text(size = 8.5, color = "grey15"),
    axis.text.x        = element_text(size = 7.5),
    strip.text         = element_text(face = "bold", size = 10),
    plot.title         = element_text(face = "bold", size = 12),
    plot.subtitle      = element_text(size = 9, color = "grey30"),
    plot.caption       = element_text(size = 7.5, color = "grey45"),
    panel.spacing.x    = unit(0.6, "cm"),
    panel.spacing.y    = unit(0.4, "cm")
  )

source("code/00_save_pub.R")
save_pub(p, "Figure_4_Forest",
         width_mm = 356, height_mm = 178,
         formats = c("svg", "pdf", "png"))
