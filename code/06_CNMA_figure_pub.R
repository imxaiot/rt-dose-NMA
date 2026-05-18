#!/usr/bin/env Rscript
# ============================================================
# 06_CNMA_figure_pub.R
# Publication-quality CNMA figure
#   (a) 9-cell heatmap: SMD x frequency x intensity x outcome
#   (b) Ranked horizontal bars: approximate P-score per dose cell, by outcome
#
# Input : outputs/CNMA_treatment_effects_with_k.xlsx
# Output: figures/Figure_3_CNMA.{svg, pdf, png}
# Depends: patchwork, tidytext (auto-installed if missing)
# ============================================================

for (pkg in c("patchwork", "tidytext")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(tidytext)

# ============================================================
# 1. Read data + normalise column names
# ============================================================
te <- read_excel("outputs/CNMA_treatment_effects_with_k.xlsx")
cn <- tolower(colnames(te)); colnames(te) <- cn

ren <- function(df, new, patterns) {
  hit <- grep(patterns, colnames(df), value = TRUE)
  if (length(hit) >= 1) {
    colnames(df)[colnames(df) == hit[1]] <- new
  }
  df
}
te <- te |>
  ren("outcome",   "^outcome") |>
  ren("treatment", "^(treatment|treat|treat_comp)$") |>
  ren("SMD",       "^(smd|te|effect)$") |>
  ren("lower",     "^(lower|lo|ci.l|lci|ll)$") |>
  ren("upper",     "^(upper|hi|ci.u|uci|ul)$")

# Direct-trial count column (k); default to 0 if missing
if (!"k" %in% colnames(te)) te$k <- 0L
te$k <- as.integer(te$k)

# ============================================================
# 2. Parse freq x intensity + assemble plotting dataframe
# ============================================================
te <- te %>%
  mutate(
    is_noRT  = treatment == "noRT" | is.na(freq_lab),
    freq_lvl = case_when(
      is_noRT             ~ NA_character_,
      freq_lab == "freqlow"  ~ "Low (1x/wk)",
      freq_lab == "freqmid"  ~ "Mid (2x/wk)",
      freq_lab == "freqhigh" ~ "High (>=3x/wk)"
    ),
    int_lvl = case_when(
      is_noRT             ~ NA_character_,
      int_lab == "intlow"  ~ "Low (<60% 1RM)",
      int_lab == "intmid"  ~ "Mid (60-75%)",
      int_lab == "inthigh" ~ "High (>=75%)"
    ),
    SMD_dir   = SMD,
    lower_dir = lower,
    upper_dir = upper,
    se_dir    = (upper_dir - lower_dir) / (2 * 1.96)
  )

freq_levels <- c("Low (1x/wk)", "Mid (2x/wk)", "High (>=3x/wk)")
int_levels  <- c("Low (<60% 1RM)", "Mid (60-75%)", "High (>=75%)")
outcome_order <- c("TUG", "8-FUGT", "stair_climb", "6MWT",
                   "gait_speed_usual", "gait_speed_fast",
                   "5xSTS", "30sSTS")

te_grid <- te %>%
  filter(!is_noRT) %>%
  mutate(
    freq_lvl = factor(freq_lvl, levels = freq_levels),
    int_lvl  = factor(int_lvl,  levels = int_levels),
    outcome  = factor(outcome,  levels = outcome_order)
  )

full_grid <- expand_grid(
  outcome  = outcome_order,
  freq_lvl = freq_levels,
  int_lvl  = int_levels
) %>%
  mutate(
    outcome  = factor(outcome,  levels = outcome_order),
    freq_lvl = factor(freq_lvl, levels = freq_levels),
    int_lvl  = factor(int_lvl,  levels = int_levels)
  )

plot_df <- full_grid %>%
  left_join(
    te_grid %>% select(outcome, freq_lvl, int_lvl,
                       SMD_dir, lower_dir, upper_dir, k, se_dir),
    by = c("outcome", "freq_lvl", "int_lvl")
  )

# ============================================================
# 3. Approximate P-score (Rücker & Schwarzer 2015 closed-form)
#    Assumes contrast independence; primary inference should
#    still rely on treatment-level NMA estimates.
# ============================================================
compute_pscore <- function(df) {
  d <- df %>% filter(!is.na(SMD_dir))
  if (nrow(d) < 2) {
    df$P_score <- NA_real_
    return(df)
  }
  K  <- nrow(d); ps <- numeric(K)
  for (k in seq_len(K)) {
    j <- setdiff(seq_len(K), k)
    diff <- d$SMD_dir[k] - d$SMD_dir[j]
    se_c <- sqrt(d$se_dir[k]^2 + d$se_dir[j]^2)
    se_c[se_c == 0] <- 1e-6
    ps[k] <- mean(pnorm(diff / se_c))
  }
  d$P_score <- ps
  df %>% left_join(
    d %>% select(freq_lvl, int_lvl, P_score),
    by = c("freq_lvl", "int_lvl")
  )
}

plot_df <- plot_df %>%
  group_by(outcome) %>%
  group_modify(~ compute_pscore(.x)) %>%
  ungroup() %>%
  mutate(
    k_cat = case_when(
      is.na(k) | k == 0 ~ "Indirect (k=0)",
      k == 1            ~ "Single trial (k=1)",
      k == 2            ~ "k = 2",
      k >= 3            ~ "k >= 3 (robust)"
    ),
    k_cat = factor(k_cat,
                   levels = c("Indirect (k=0)", "Single trial (k=1)",
                              "k = 2", "k >= 3 (robust)"))
  )

best_cell <- plot_df %>%
  filter(!is.na(P_score)) %>%
  group_by(outcome) %>%
  slice_max(P_score, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(outcome, freq_lvl, int_lvl, is_best = TRUE)

plot_df <- plot_df %>%
  left_join(best_cell, by = c("outcome", "freq_lvl", "int_lvl")) %>%
  mutate(
    is_best = !is.na(is_best),
    cell_lbl = sprintf("%s x %s",
                       gsub("\\s.*", "", as.character(freq_lvl)),
                       gsub("\\s.*", "", as.character(int_lvl))),
    label_hm = case_when(
      is.na(SMD_dir) ~ "—",
      is.na(k) | k == 0 ~ sprintf("%.2f [%.2f, %.2f]\nk=0+",
                                  SMD_dir, lower_dir, upper_dir),
      TRUE              ~ sprintf("%.2f [%.2f, %.2f]\nk=%d",
                                  SMD_dir, lower_dir, upper_dir, k)
    )
  )

# ============================================================
# 4. Panel (a): heatmap
# ============================================================
lim <- max(abs(plot_df$SMD_dir), na.rm = TRUE)
lim <- ceiling(lim * 10) / 10

p_hm <- ggplot(plot_df,
               aes(x = int_lvl, y = freq_lvl, fill = SMD_dir)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_tile(
    data = plot_df %>% filter(is_best),
    aes(x = int_lvl, y = freq_lvl),
    fill = NA, color = "#d4a017", linewidth = 1.6, inherit.aes = FALSE
  ) +
  geom_text(aes(label = label_hm), size = 2.4, lineheight = 0.9,
            color = "grey15") +
  scale_fill_gradient2(
    low = "#3b6db5", mid = "#f7f7f7", high = "#c0392b",
    midpoint = 0, limits = c(-lim, lim),
    name = "SMD",
    na.value = "grey92"
  ) +
  facet_wrap(~ outcome, ncol = 4) +
  labs(
    x = NULL, y = "Frequency bin",
    subtitle = "(a) Component dose-response heatmap (SMD vs noRT). Gold border = best-ranked cell per outcome. + = network-derived (no direct trial)."
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid       = element_blank(),
    axis.text.x      = element_text(angle = 25, hjust = 1, size = 7.5),
    axis.text.y      = element_text(size = 7.5),
    strip.text       = element_text(face = "bold", size = 9.5),
    plot.subtitle    = element_text(size = 9, color = "grey25",
                                    margin = margin(b = 6)),
    legend.position  = "right",
    legend.key.height = unit(0.9, "cm"),
    legend.title     = element_text(size = 9),
    legend.text      = element_text(size = 8)
  )

# ============================================================
# 5. Panel (b): P-score ranked bars (per outcome, descending)
# ============================================================
rank_df <- plot_df %>%
  filter(!is.na(P_score)) %>%
  mutate(
    sig    = if_else(lower_dir > 0 | upper_dir < 0, " *", ""),
    y_lbl  = sprintf("%s  %+.2f%s", cell_lbl, SMD_dir, sig)
  )

k_colors <- c(
  "Indirect (k=0)"     = "#bdbdbd",
  "Single trial (k=1)" = "#fdae61",
  "k = 2"              = "#ef8a62",
  "k >= 3 (robust)"    = "#b2182b"
)

p_rank <- ggplot(rank_df,
                 aes(x = P_score,
                     y = reorder_within(y_lbl, P_score, outcome),
                     fill = k_cat)) +
  geom_col(width = 0.7, orientation = "y") +
  geom_vline(xintercept = 0.5, linetype = "dashed",
             color = "grey50", linewidth = 0.4) +
  scale_y_reordered() +
  scale_fill_manual(values = k_colors, name = "Direct evidence",
                    drop = FALSE) +
  scale_x_continuous(
    limits = c(0, 1.0), expand = c(0, 0),
    breaks = c(0, 0.25, 0.5, 0.75, 1.0)
  ) +
  facet_wrap(~ outcome, ncol = 4, scales = "free_y") +
  labs(
    x = "Approximate P-score (higher = better-ranked)",
    y = NULL,
    subtitle = "(b) Dose ranking with evidence strength. Y-axis: cell label + SMD (* = 95% CI excludes 0); bar fill = number of contributing trials; dashed line = 0.5 (chance)."
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y        = element_text(size = 7.5, color = "grey15"),
    axis.ticks.y       = element_blank(),
    strip.text         = element_text(face = "bold", size = 9.5),
    plot.subtitle      = element_text(size = 9, color = "grey25",
                                      margin = margin(b = 6)),
    legend.position    = "right",
    legend.title       = element_text(size = 9),
    legend.text        = element_text(size = 8),
    plot.margin        = margin(t = 5, r = 10, b = 5, l = 5),
    panel.spacing.x    = unit(0.6, "cm")
  )

# ============================================================
# 6. Combine + save
# ============================================================
p_combined <- p_hm / p_rank +
  plot_layout(heights = c(1.0, 1.2)) +
  plot_annotation(
    title = "Component network meta-analysis: dose-response across functional outcomes",
    caption = "Direction-aligned: positive SMD = functional improvement. P-score uses Rücker & Schwarzer (2015) closed-form assuming contrast independence. k = number of direct trials; + = additive component model interpolation.",
    theme = theme(
      plot.title    = element_text(face = "bold", size = 13, hjust = 0),
      plot.caption  = element_text(size = 7.5, color = "grey40",
                                   hjust = 0, margin = margin(t = 8))
    )
  )

source("code/00_save_pub.R")
save_pub(p_combined, "Figure_3_CNMA",
         width_mm = 406, height_mm = 330,
         formats = c("svg", "pdf", "png"))
cat("Heatmap cells: ", nrow(plot_df),
    "; ranking bars: ", nrow(rank_df), "\n", sep = "")
