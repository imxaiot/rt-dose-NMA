#!/usr/bin/env Rscript
# ============================================================
# 05_NMA_funnel_pub.R
# Comparison-adjusted funnel plot — 8-panel publication composite
#   One panel per outcome (adjusted SMD vs SE), shared coordinate system.
#
# Within-study pairwise SMDs are computed with Hedges' g.
# Each point's x-value is the study-level SMD minus the
# comparison-specific NMA estimate, so symmetric distribution
# around zero indicates absence of small-study effects.
#
# Input :
#   data/NMA_final.xlsx (sheet: 主分析_结局数据_)
#   outputs/league_tables.xlsx
# Output: figures/Figure_5_Funnel.{svg, pdf, png}
# ============================================================

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(purrr)

# ---- 1. Read raw outcome data + apply aggregation rules ----
outc <- read_excel("data/NMA_final.xlsx",
                   sheet = "主分析_结局数据_")

outcome_map <- c(
  "TUG" = "TUG", "8-FUGT" = "8-FUGT",
  "stair_climb" = "stair_climb", "6MWT" = "6MWT",
  "5xSTS" = "5xSTS", "30sSTS" = "30sSTS",
  "gait_speed_preferred" = "gait_speed_usual",
  "gait_speed_usual_4m"  = "gait_speed_usual",
  "gait_speed"           = "gait_speed_fast",
  "gait_speed_fast"      = "gait_speed_fast",
  "gait_speed_fast_10m"  = "gait_speed_fast",
  "gait_speed_maximal"   = "gait_speed_fast"
)
outcome_order <- c("TUG", "8-FUGT", "stair_climb", "6MWT",
                   "gait_speed_usual", "gait_speed_fast",
                   "5xSTS", "30sSTS")

dat <- outc %>%
  mutate(
    final_id = `研究编号`,
    outcome  = unname(outcome_map[`分析结局名称`]),
    node     = `节点`,
    n  = suppressWarnings(as.numeric(n)),
    m  = suppressWarnings(as.numeric(`均值`)),
    sd = suppressWarnings(as.numeric(SD))
  ) %>%
  filter(!is.na(outcome), !is.na(n), !is.na(m), !is.na(sd))

# Multiple arms on the same node: weighted mean + pooled SD
dat_agg <- dat %>%
  group_by(final_id, outcome, node) %>%
  summarise(
    n_a  = sum(n),
    m_a  = sum(n * m) / sum(n),
    sd_a = sqrt(sum((n - 1) * sd^2) / sum(n - 1)),
    .groups = "drop"
  )

# ---- 2. Pairwise Hedges' g + SE within each study ----------
compute_pair_smd <- function(n1, m1, s1, n2, m2, s2) {
  s_pool <- sqrt(((n1-1)*s1^2 + (n2-1)*s2^2) / (n1 + n2 - 2))
  d <- (m1 - m2) / s_pool
  # Hedges' small-sample correction
  J <- 1 - 3 / (4*(n1+n2) - 9)
  g <- d * J
  se <- sqrt((n1+n2)/(n1*n2) + g^2 / (2*(n1+n2)))
  list(smd = g, se = se)
}

# Fixed direction convention (treatment - reference):
#   noRT : low_dose_RT          -> (low  - noRT)
#   noRT : conventional_dose_RT -> (conv - noRT)
#   low_dose_RT : conv_dose_RT  -> (conv - low)
build_pair <- function(arms, treat_target, treat_ref, label) {
  if (!treat_target %in% arms$node || !treat_ref %in% arms$node) return(NULL)
  a <- arms[arms$node == treat_target, ]
  b <- arms[arms$node == treat_ref,    ]
  r <- compute_pair_smd(a$n_a, a$m_a, a$sd_a,
                        b$n_a, b$m_a, b$sd_a)
  tibble(comparison = label, SMD = r$smd, SE = r$se)
}

pairs_df <- dat_agg %>%
  group_by(final_id, outcome) %>%
  filter(n() >= 2) %>%
  group_modify(~ {
    arms <- .x
    bind_rows(
      build_pair(arms, "low_dose_RT",          "non_exercise_control",
                 "non_exercise_control : low_dose_RT"),
      build_pair(arms, "conventional_dose_RT", "non_exercise_control",
                 "non_exercise_control : conventional_dose_RT"),
      build_pair(arms, "conventional_dose_RT", "low_dose_RT",
                 "low_dose_RT : conventional_dose_RT")
    )
  }) %>%
  ungroup()

# ---- 3. NMA reference estimates (anchor) -------------------
LEAGUE <- "outputs/league_tables.xlsx"
parse_smd <- function(x) {
  s <- str_replace_all(x, "[−–—]", "-")
  m <- str_match(s, "^\\s*(-?[0-9.]+)\\s*\\(\\s*(-?[0-9.]+)\\s*,\\s*(-?[0-9.]+)\\s*\\)\\s*$")
  as.numeric(m[, 2])
}

nma_anchor <- map_dfr(excel_sheets(LEAGUE), function(s) {
  d <- read_excel(LEAGUE, sheet = s) %>%
    filter(Treatment == "non_exercise_control")
  tibble(
    outcome = s,
    `non_exercise_control : low_dose_RT`          = parse_smd(d$low_dose_RT),
    `non_exercise_control : conventional_dose_RT` = parse_smd(d$conventional_dose_RT)
  )
}) %>%
  pivot_longer(-outcome, names_to = "comparison", values_to = "nma_smd")

# Anchor for low vs conv: take from non-reference row (lo - conv)
lc <- map_dfr(excel_sheets(LEAGUE), function(s) {
  d <- read_excel(LEAGUE, sheet = s) %>%
    filter(Treatment == "low_dose_RT")
  tibble(outcome = s,
         comparison = "low_dose_RT : conventional_dose_RT",
         nma_smd = parse_smd(d$conventional_dose_RT))
})
nma_anchor <- bind_rows(nma_anchor, lc)

# ---- 4. Comparison-adjusted x = study SMD - NMA SMD --------
plot_df <- pairs_df %>%
  left_join(nma_anchor, by = c("outcome", "comparison")) %>%
  filter(!is.na(nma_smd)) %>%
  mutate(
    SMD_adj = SMD - nma_smd,
    outcome = factor(outcome, levels = outcome_order),
    comparison = factor(comparison, levels = c(
      "non_exercise_control : low_dose_RT",
      "non_exercise_control : conventional_dose_RT",
      "low_dose_RT : conventional_dose_RT"
    ))
  )

cat("=== Funnel plot data: trial-contrasts per outcome ===\n")
print(plot_df %>% count(outcome, comparison))

# ---- 5. Plot -----------------------------------------------
# Pseudo-CI per panel: independent SE limits but shared x-axis centred on 0
lim_se <- max(plot_df$SE, na.rm = TRUE) * 1.05
lim_x  <- max(abs(plot_df$SMD_adj), na.rm = TRUE) * 1.05

# Funnel guides at +/-1.96 * SE
funnel_lines <- tibble(
  SE = seq(0, lim_se, length.out = 100)
) %>%
  mutate(
    x_lo = -1.96 * SE,
    x_hi =  1.96 * SE
  )

comp_colors <- c(
  "non_exercise_control : low_dose_RT"          = "#3a6ea5",
  "non_exercise_control : conventional_dose_RT" = "#d97a2e",
  "low_dose_RT : conventional_dose_RT"          = "#2d8c4f"
)

p <- ggplot() +
  # Funnel boundaries (dashed)
  geom_line(data = funnel_lines,
            aes(x = x_lo, y = SE),
            color = "grey50", linetype = "dashed", linewidth = 0.4) +
  geom_line(data = funnel_lines,
            aes(x = x_hi, y = SE),
            color = "grey50", linetype = "dashed", linewidth = 0.4) +
  # Zero reference line
  geom_vline(xintercept = 0, color = "grey25", linewidth = 0.4) +
  # Trial-contrast points
  geom_point(data = plot_df,
             aes(x = SMD_adj, y = SE, color = comparison,
                 shape = comparison),
             size = 2.2, alpha = 0.9, stroke = 0.5) +
  scale_color_manual(values = comp_colors,
                     name = "Comparison") +
  scale_shape_manual(values = c(16, 17, 15),
                     name = "Comparison") +
  scale_x_continuous(limits = c(-lim_x, lim_x),
                     breaks = scales::pretty_breaks(n = 5)) +
  scale_y_reverse(limits = c(lim_se, 0)) +
  facet_wrap(~ outcome, ncol = 4) +
  labs(
    title = "Comparison-adjusted funnel plots for all eight functional outcomes",
    subtitle = "Each point = one within-study pairwise contrast. X-axis: SMD centered on the comparison-specific NMA estimate; dashed lines = pseudo 95% confidence boundaries (+/-1.96 x SE).",
    x = "SMD centered at comparison-specific NMA estimate",
    y = "Standard error",
    caption = "Symmetric distribution around 0 indicates absence of small-study effects. Asymmetric distribution suggests potential reporting bias. With <10 trials per comparison, results should be interpreted qualitatively (PRISMA-NMA)."
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey94", linewidth = 0.3),
    strip.text       = element_text(face = "bold", size = 10),
    plot.title       = element_text(face = "bold", size = 12),
    plot.subtitle    = element_text(size = 9, color = "grey25"),
    plot.caption     = element_text(size = 8, color = "grey45"),
    legend.position  = "bottom",
    legend.title     = element_text(size = 9),
    legend.text      = element_text(size = 8),
    panel.spacing.x  = unit(0.5, "cm"),
    panel.spacing.y  = unit(0.5, "cm")
  )

source("code/00_save_pub.R")
save_pub(p, "Figure_5_Funnel",
         width_mm = 356, height_mm = 216,
         formats = c("svg", "pdf", "png"))
