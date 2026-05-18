#!/usr/bin/env Rscript
# ============================================================
# 02_NMA_network_main.R
# Main-analysis NMA network geometry diagram
#   Three-node network: non-exercise control / low-dose RT / conventional-dose RT
#   8 panels (one per functional outcome)
#
# Input : data/NMA_final.xlsx (sheet: 主分析_结局数据_)
# Output: figures/Figure_2a_Network_main.{svg, pdf, png}
# ============================================================

library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

# ---- 1. Read raw outcome data + apply aggregation rules ----
outc <- read_excel("data/NMA_final.xlsx",
                   sheet = "主分析_结局数据_")

# Map raw outcome labels onto eight aggregated CNMA outcomes
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

# ---- 2. Normalise node labels to three canonical names -----
node_clean <- function(x) {
  x <- tolower(trimws(x))
  case_when(
    grepl("non.?exer|control|noRT", x, ignore.case = TRUE) ~ "noRT",
    grepl("low.?dose|low_dose", x)                          ~ "low_dose_RT",
    grepl("convention|conv", x)                             ~ "conventional_dose_RT",
    TRUE                                                    ~ NA_character_
  )
}

joined <- outc %>%
  rename(final_id = `研究编号`, node_raw = `节点`) %>%
  mutate(
    outcome = unname(outcome_map[`分析结局名称`]),
    node    = node_clean(node_raw)
  ) %>%
  filter(!is.na(outcome), !is.na(node)) %>%
  distinct(outcome, final_id, node)

cat("=== Raw node labels in the master file ===\n")
print(table(outc$`节点`, useNA = "always"))
cat("\n=== Cleaned 3-node distribution ===\n")
print(table(joined$node, useNA = "always"))

# ---- 3. Fixed node coordinates (triangle layout) -----------
node_layout <- tibble(
  node = c("noRT", "low_dose_RT", "conventional_dose_RT"),
  x    = c(0,      -0.87,         0.87),
  y    = c(1,      -0.5,         -0.5),
  label = c("noRT", "low-dose\nRT", "conventional-\ndose RT")
)

# ---- 4. Nodes (k studies) and edges per outcome ------------
nodes_long <- joined %>%
  group_by(outcome, node) %>%
  summarise(k_studies = n_distinct(final_id), .groups = "drop") %>%
  left_join(node_layout, by = "node") %>%
  mutate(outcome = factor(outcome, levels = outcome_order))

edges_long <- joined %>%
  group_by(outcome, final_id) %>%
  summarise(nodes_in_study = list(unique(node)), .groups = "drop") %>%
  filter(map_int(nodes_in_study, length) >= 2) %>%
  mutate(pairs = map(nodes_in_study, ~ {
    cb <- t(combn(sort(.x), 2))
    tibble(from = cb[, 1], to = cb[, 2])
  })) %>%
  select(outcome, final_id, pairs) %>%
  unnest(pairs) %>%
  group_by(outcome, from, to) %>%
  summarise(weight = n_distinct(final_id), .groups = "drop") %>%
  left_join(node_layout %>% select(node, x_from = x, y_from = y),
            by = c("from" = "node")) %>%
  left_join(node_layout %>% select(node, x_to = x, y_to = y),
            by = c("to" = "node")) %>%
  mutate(
    outcome = factor(outcome, levels = outcome_order),
    x_mid = (x_from + x_to) / 2,
    y_mid = (y_from + y_to) / 2
  )

cat("\n=== Network size per outcome ===\n")
print(nodes_long %>% group_by(outcome) %>%
        summarise(n_nodes = n(), .groups = "drop") %>%
        left_join(edges_long %>% group_by(outcome) %>%
                    summarise(n_edges = n(), total_k = sum(weight),
                              .groups = "drop"),
                  by = "outcome"))

# ---- 5. Plot -----------------------------------------------
p <- ggplot() +
  # Edges
  geom_segment(
    data = edges_long,
    aes(x = x_from, y = y_from, xend = x_to, yend = y_to,
        linewidth = weight),
    color = "#3a6ea5", alpha = 0.6, lineend = "round"
  ) +
  # Edge labels = number of direct studies
  geom_label(
    data = edges_long,
    aes(x = x_mid, y = y_mid, label = weight),
    size = 2.6, label.padding = unit(0.12, "lines"),
    label.size = 0.2, color = "grey15", fill = "white"
  ) +
  # Nodes
  geom_point(
    data = nodes_long,
    aes(x = x, y = y, size = k_studies,
        fill = node == "noRT"),
    shape = 21, color = "grey15", stroke = 0.6
  ) +
  # k study number on each node
  geom_text(
    data = nodes_long,
    aes(x = x, y = y, label = k_studies),
    size = 3.0, color = "white", fontface = "bold"
  ) +
  # Node names (offset outward)
  geom_text(
    data = nodes_long,
    aes(x = x + ifelse(x == 0, 0, x * 0.55),
        y = y + ifelse(y == 1, 0.35, -0.35),
        label = label),
    size = 2.7, color = "grey20", lineheight = 0.9
  ) +
  scale_linewidth_continuous(
    range = c(0.6, 3.5),
    name = "Direct\nstudies/edge"
  ) +
  scale_size_continuous(
    range = c(5, 14),
    name = "Studies\nper node"
  ) +
  scale_fill_manual(
    values = c("TRUE" = "#7f7f7f", "FALSE" = "#c0392b"),
    guide = "none"
  ) +
  facet_wrap(~ outcome, ncol = 4) +
  coord_cartesian(xlim = c(-1.9, 1.9), ylim = c(-1.3, 1.6)) +
  labs(
    title = "Main NMA network geometry across eight functional outcomes",
    subtitle = "Three-node network: non-exercise control (grey), low-dose RT, conventional-dose RT. Node size & number = k studies; edge width & label = direct trials connecting pair.",
    caption = "Edge label = number of direct trials. Networks differ across outcomes because not every trial measured every outcome."
  ) +
  theme_void(base_size = 9) +
  theme(
    strip.text       = element_text(face = "bold", size = 10,
                                    margin = margin(b = 4)),
    plot.title       = element_text(face = "bold", size = 12,
                                    margin = margin(b = 2)),
    plot.subtitle    = element_text(size = 9, color = "grey30",
                                    margin = margin(b = 6)),
    plot.caption     = element_text(size = 7.5, color = "grey45",
                                    margin = margin(t = 6)),
    legend.position  = "right",
    legend.title     = element_text(size = 8.5),
    legend.text      = element_text(size = 7.5),
    panel.spacing.x  = unit(0.4, "cm"),
    panel.spacing.y  = unit(0.6, "cm"),
    plot.margin      = margin(t = 8, r = 8, b = 8, l = 8)
  )

source("code/00_save_pub.R")
save_pub(p, "Figure_2a_Network_main",
         width_mm = 356, height_mm = 191,
         formats = c("svg", "pdf", "png"))
