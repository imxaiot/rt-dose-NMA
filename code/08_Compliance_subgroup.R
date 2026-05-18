#!/usr/bin/env Rscript
# ============================================================
# 08_Compliance_subgroup.R
# Compliance subgroup forest - low-dose vs conventional-dose RT
#   A study contributing both dose arms appears in both subgroups.
#
# Input : data/NMA_final.xlsx (sheet: 可行性结局)
# Output: figures/Supplementary_Figure_S2_Compliance_subgroup.{svg, pdf, png}
# Note  : Arial 8pt, 600 dpi
# ============================================================

for (pkg in c("svglite", "ragg", "systemfonts")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

library(readxl)
library(dplyr)
library(meta)

dir.create("figures", showWarnings = FALSE, recursive = TRUE)

# ---- 1. Read + normalise node labels -----------------------
fe <- read_excel("data/NMA_final.xlsx",
                 sheet = "可行性结局")

node_clean <- function(x) {
  x <- tolower(trimws(x))
  dplyr::case_when(
    grepl("non.?exer|control|noRT", x, ignore.case = TRUE) ~ "noRT",
    grepl("low.?dose|low_dose", x)                          ~ "low_dose_RT",
    grepl("convention|conv", x)                             ~ "conventional_dose_RT",
    TRUE                                                    ~ NA_character_
  )
}

fe_num <- fe %>%
  mutate(
    n_rand = suppressWarnings(as.numeric(随机人数)),
    n_done = suppressWarnings(as.numeric(干预完成人数)),
    node   = node_clean(节点)
  ) %>%
  filter(node %in% c("low_dose_RT", "conventional_dose_RT"),
         !is.na(n_done), !is.na(n_rand), n_done <= n_rand)

# ---- 2. Aggregate per study x dose -------------------------
study_df <- fe_num %>%
  group_by(study_id = `研究编号`, label = `作者年份`, node) %>%
  summarise(
    events = sum(n_done),
    total  = sum(n_rand),
    .groups = "drop"
  ) %>%
  arrange(node, study_id) %>%
  mutate(
    label = sprintf("%s (%s)", label, study_id),
    dose  = factor(node,
                   levels = c("low_dose_RT", "conventional_dose_RT"),
                   labels = c("Low-dose RT", "Conventional-dose RT"))
  )

cat("=== Rows by subgroup ===\n")
print(study_df %>% count(dose))
cat("\n=== Study list per subgroup ===\n")
print(study_df, n = Inf)

# ---- 3. metaprop with subgroup -----------------------------
m <- metaprop(
  event      = study_df$events,
  n          = study_df$total,
  studlab    = study_df$label,
  subgroup   = study_df$dose,
  sm         = "PLOGIT",
  method     = "Inverse",
  random     = TRUE,
  common     = FALSE,
  method.tau = "REML",
  prediction = FALSE,
  test.subgroup = TRUE
)

cat("\n=== Subgroup meta-analysis ===\n")
print(summary(m))

# ---- 4. Plot parameters ------------------------------------
# Rows: header + 2 subgroup titles + study rows + 2 subgroup diamonds
#       + 1 overall diamond + statistics ~= 32-34
WIDTH_IN  <- 10.0
HEIGHT_IN <- 8.4
DPI       <- 600

draw_forest <- function() {
  par(family = "Arial")
  forest(m,
         digits      = 2,
         digits.tau2 = 3,
         leftcols    = c("studlab", "event", "n"),
         leftlabs    = c("Study", "Completed", "Randomised"),
         rightcols   = c("effect", "ci"),
         rightlabs   = c("Proportion", "95% CI"),
         xlim        = c(0, 1),
         smlab       = "Intervention completion",
         text.random = "Subgroup random-effects estimate",
         text.random.w = "Subgroup pooled",
         text.fixed  = NULL,
         col.square        = "#3a6ea5",
         col.square.lines  = "#3a6ea5",
         col.diamond       = "#c0392b",
         col.diamond.lines = "grey15",
         col.diamond.random.w = "#2d8c4f",
         fontfamily        = "Arial",
         fontsize          = 8,
         spacing           = 0.95,
         squaresize        = 0.85,
         hetstat           = TRUE,
         print.subgroup.labels = TRUE,
         print.tau2 = TRUE,
         print.I2   = TRUE,
         overall.hetstat = TRUE,
         test.subgroup   = TRUE)
}

# ---- 5. Three output formats -------------------------------
out_base <- "figures/Supplementary_Figure_S2_Compliance_subgroup"

svglite::svglite(paste0(out_base, ".svg"),
                 width = WIDTH_IN, height = HEIGHT_IN, bg = "white")
draw_forest(); dev.off()

grDevices::cairo_pdf(paste0(out_base, ".pdf"),
                     width = WIDTH_IN, height = HEIGHT_IN,
                     family = "Arial", bg = "white")
draw_forest(); dev.off()

ragg::agg_png(paste0(out_base, ".png"),
              width = WIDTH_IN, height = HEIGHT_IN, units = "in",
              res = DPI, background = "white")
draw_forest(); dev.off()

# ---- 6. Text summary ---------------------------------------
cat("\n=== Subgroup pooled estimates ===\n")
for (i in seq_along(m$bylevs)) {
  cat(sprintf("  %s: %.1f%% (95%% CI %.1f%%-%.1f%%), k=%d, I^2=%.0f%%\n",
              m$bylevs[i],
              plogis(m$TE.random.w[i]) * 100,
              plogis(m$lower.random.w[i]) * 100,
              plogis(m$upper.random.w[i]) * 100,
              m$k.w[i],
              m$I2.w[i] * 100))
}
cat(sprintf("\nSubgroup difference test: Q = %.2f, df = %d, p = %.3f\n",
            m$Q.b.random, m$df.Q.b, m$pval.Q.b.random))
cat("\nSaved: ", out_base, ".[svg|pdf|png]\n", sep = "")
