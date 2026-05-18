#!/usr/bin/env Rscript
# ============================================================
# 03_CNMA_network.R
# Component NMA network geometry (one panel per outcome)
#   Nodes : 9 treatment combinations (freq × intensity) + noRT
#   Edges : direct comparisons within studies
#   Layout: fixed 3 x 3 grid (frequency × intensity) + noRT off-grid
#
# Input :
#   outputs/CNMA_dose_template.xlsx
#   data/NMA_final.xlsx (sheet: 主分析_结局数据_)
# Output: figures/Figure_2b_CNMA_network.{svg, pdf, png}
# ============================================================

library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

# ---- 1. dose_template -> arm-level treatment combination ---
dt <- read_excel("outputs/CNMA_dose_template.xlsx")
dt2 <- dt %>%
  mutate(
    is_noRT = freq_per_week == 0 | is.na(freq_per_week),
    freq_lab = case_when(
      is_noRT             ~ NA_character_,
      freq_per_week == 1  ~ "freqlow",
      freq_per_week == 2  ~ "freqmid",
      freq_per_week >= 3  ~ "freqhigh"
    ),
    int_lab = case_when(
      is_noRT                            ~ NA_character_,
      tolower(intensity_band) == "low"   ~ "intlow",
      tolower(intensity_band) == "mid"   ~ "intmid",
      tolower(intensity_band) == "high"  ~ "inthigh"
    ),
    treat_comp = if_else(is_noRT, "noRT",
                         paste0(freq_lab, "+", int_lab))
  ) %>%
  select(final_id, arm_label, treat_comp)

# ---- 2. Outcome aggregation rules --------------------------
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

joined <- outc %>%
  rename(final_id = `研究编号`, arm_label = `组别名称`) %>%
  left_join(dt2, by = c("final_id", "arm_label")) %>%
  filter(!is.na(treat_comp)) %>%
  mutate(outcome = unname(outcome_map[`分析结局名称`])) %>%
  filter(!is.na(outcome)) %>%
  distinct(outcome, final_id, treat_comp)

# ---- 3. Fixed node coordinates -----------------------------
freq_lvls <- c("freqlow", "freqmid", "freqhigh")
int_lvls  <- c("intlow", "intmid", "inthigh")

node_layout <- expand_grid(
  freq_lab = freq_lvls,
  int_lab  = int_lvls
) %>%
  mutate(
    treat_comp = paste0(freq_lab, "+", int_lab),
    x = as.numeric(factor(int_lab, levels = int_lvls)),
    y = as.numeric(factor(freq_lab, levels = freq_lvls)),
    node_label = paste0(
      substr(freq_lab, 5, 5) |> toupper(), "f-",
      substr(int_lab, 4, 4)  |> toupper(), "i"
    )
  ) %>%
  bind_rows(
    tibble(freq_lab = NA, int_lab = NA, treat_comp = "noRT",
           x = -0.5, y = 2, node_label = "noRT")
  )

# ---- 4. Nodes (k studies) and edges per outcome ------------
nodes_long <- joined %>%
  group_by(outcome, treat_comp) %>%
  summarise(k_studies = n_distinct(final_id), .groups = "drop") %>%
  left_join(node_layout, by = "treat_comp") %>%
  mutate(outcome = factor(outcome, levels = outcome_order))

edges_long <- joined %>%
  group_by(outcome, final_id) %>%
  summarise(treats = list(unique(treat_comp)), .groups = "drop") %>%
  filter(map_int(treats, length) >= 2) %>%
  mutate(pairs = map(treats, ~ {
    cb <- t(combn(sort(.x), 2))
    tibble(from = cb[, 1], to = cb[, 2])
  })) %>%
  select(outcome, final_id, pairs) %>%
  unnest(pairs) %>%
  group_by(outcome, from, to) %>%
  summarise(weight = n_distinct(final_id), .groups = "drop") %>%
  left_join(node_layout %>% select(treat_comp, x_from = x, y_from = y),
            by = c("from" = "treat_comp")) %>%
  left_join(node_layout %>% select(treat_comp, x_to = x, y_to = y),
            by = c("to" = "treat_comp")) %>%
  mutate(outcome = factor(outcome, levels = outcome_order))

cat("Total node rows (outcome x treat):", nrow(nodes_long), "\n")
cat("Total edge rows (outcome x pair):", nrow(edges_long), "\n")
cat("\n=== Network size per outcome ===\n")
print(nodes_long %>%
        group_by(outcome) %>%
        summarise(n_nodes = n(), .groups = "drop") %>%
        left_join(
          edges_long %>%
            group_by(outcome) %>%
            summarise(n_edges = n(), .groups = "drop"),
          by = "outcome"
        ))

# ---- 5. Plot -----------------------------------------------
p <- ggplot() +
  # 3 x 3 background reference circles
  geom_point(
    data = node_layout %>% filter(treat_comp != "noRT"),
    aes(x = x, y = y),
    color = "grey88", size = 14, shape = 21, stroke = 0.4
  ) +
  # Edges
  geom_segment(
    data = edges_long,
    aes(x = x_from, y = y_from, xend = x_to, yend = y_to,
        linewidth = weight),
    color = "#3a6ea5", alpha = 0.55, lineend = "round"
  ) +
  # Nodes
  geom_point(
    data = nodes_long,
    aes(x = x, y = y, size = k_studies,
        fill = treat_comp == "noRT"),
    shape = 21, color = "grey15", stroke = 0.6
  ) +
  # k studies number on each node
  geom_text(
    data = nodes_long,
    aes(x = x, y = y, label = k_studies),
    size = 2.4, color = "white", fontface = "bold"
  ) +
  # Node names below each node
  geom_text(
    data = nodes_long,
    aes(x = x, y = y - 0.35, label = node_label),
    size = 2.2, color = "grey25"
  ) +
  scale_linewidth_continuous(
    range = c(0.4, 2.5),
    name = "Direct\nstudies/edge"
  ) +
  scale_size_continuous(
    range = c(3, 9),
    name = "Studies\nper node"
  ) +
  scale_fill_manual(
    values = c("TRUE" = "#7f7f7f", "FALSE" = "#c0392b"),
    guide = "none"
  ) +
  scale_x_continuous(
    breaks = c(-0.5, 1, 2, 3),
    labels = c("noRT", "Low int", "Mid int", "High int"),
    expand = expansion(add = 0.4)
  ) +
  scale_y_continuous(
    breaks = 1:3,
    labels = c("Low freq\n(1x/wk)", "Mid freq\n(2x/wk)", "High freq\n(>=3x/wk)"),
    expand = expansion(add = 0.5)
  ) +
  facet_wrap(~ outcome, ncol = 4) +
  labs(
    title = "Component NMA network geometry across outcomes",
    subtitle = "Nodes = treatment combinations (red = active RT, grey = no RT). Node size & number = k studies; edge width = direct comparisons.",
    caption = "Empty grey circles mark dose cells that exist in the wider network but were not measured for that outcome. Coordinates: frequency on y-axis, intensity on x-axis.",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 9) +
  theme(
    panel.grid.major = element_line(color = "grey94", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.text        = element_text(size = 7, color = "grey30"),
    strip.text       = element_text(face = "bold", size = 9.5),
    plot.title       = element_text(face = "bold", size = 12),
    plot.subtitle    = element_text(size = 9, color = "grey30"),
    plot.caption     = element_text(size = 7.5, color = "grey45"),
    legend.position  = "right",
    legend.title     = element_text(size = 8.5),
    legend.text      = element_text(size = 7.5),
    panel.spacing.x  = unit(0.6, "cm"),
    panel.spacing.y  = unit(0.6, "cm")
  )

source("code/00_save_pub.R")
save_pub(p, "Figure_2b_CNMA_network",
         width_mm = 356, height_mm = 203,
         formats = c("svg", "pdf", "png"))
