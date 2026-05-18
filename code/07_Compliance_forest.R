#!/usr/bin/env Rscript
# ============================================================
# 07_Compliance_forest.R
# Compliance / adherence forest plot - publication vectors
#   Random-effects proportion meta-analysis of programme
#   completion across resistance-training arms.
#   Style follows Ricci 2024 Fig 1.
#
# Input : data/NMA_final.xlsx (sheet: 可行性结局)
# Output: figures/Figure_6_Compliance.{svg, pdf, png}
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

# ---- 1. Read raw feasibility data, keep RT arms only -------
fe <- read_excel("data/NMA_final.xlsx",
                 sheet = "可行性结局")

fe_num <- fe %>%
  mutate(
    n_rand = suppressWarnings(as.numeric(随机人数)),
    n_done = suppressWarnings(as.numeric(干预完成人数)),
    is_RT  = !grepl("non_exer", 节点, ignore.case = TRUE)
  )

# ---- 2. Aggregate all RT arms within a study ---------------
study_df <- fe_num %>%
  filter(is_RT,
         !is.na(n_done),
         !is.na(n_rand),
         n_done <= n_rand) %>%
  group_by(study_id = `研究编号`, label = `作者年份`) %>%
  summarise(
    events = sum(n_done),
    total  = sum(n_rand),
    .groups = "drop"
  ) %>%
  arrange(study_id) %>%
  mutate(label = sprintf("%s (%s)", label, study_id))

cat("=== Trials contributing to compliance MA ===\n")
print(study_df, n = Inf)
cat("\nTrials:", nrow(study_df),
    " | Total events:", sum(study_df$events),
    " | Total randomised:", sum(study_df$total), "\n")

# ---- 3. Random-effects proportion meta-analysis ------------
m <- metaprop(
  event      = study_df$events,
  n          = study_df$total,
  studlab    = study_df$label,
  sm         = "PLOGIT",
  method     = "Inverse",
  random     = TRUE,
  common     = FALSE,
  method.tau = "REML",
  prediction = FALSE
)

cat("\n=== Meta-analysis summary ===\n")
print(summary(m))

# ---- 4. Shared plotting parameters -------------------------
# Layout reserves room for ~26 trial rows + header + pooled + statistics
WIDTH_IN  <- 10.0          # 254 mm (double-column width)
HEIGHT_IN <- 7.2           # 183 mm
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
         text.random = "Random-effects pooled estimate",
         text.fixed  = NULL,
         col.square        = "#3a6ea5",
         col.square.lines  = "#3a6ea5",
         col.diamond       = "#c0392b",
         col.diamond.lines = "grey15",
         col.predict       = "grey50",
         fontfamily        = "Arial",
         fontsize          = 8,
         spacing           = 0.95,
         squaresize        = 0.85,
         hetstat    = TRUE,
         print.tau2 = TRUE,
         print.I2   = TRUE)
}

# ---- 5. Three output formats -------------------------------
# SVG
svglite::svglite("figures/Figure_6_Compliance.svg",
                 width = WIDTH_IN, height = HEIGHT_IN, bg = "white")
draw_forest(); dev.off()

# PDF (Cairo with embedded Arial)
grDevices::cairo_pdf("figures/Figure_6_Compliance.pdf",
                     width = WIDTH_IN, height = HEIGHT_IN,
                     family = "Arial", bg = "white")
draw_forest(); dev.off()

# PNG at 600 dpi (preview)
ragg::agg_png("figures/Figure_6_Compliance.png",
              width = WIDTH_IN, height = HEIGHT_IN, units = "in",
              res = DPI, background = "white")
draw_forest(); dev.off()

cat("\nSaved (3 formats):\n",
    "  - figures/Figure_6_Compliance.svg\n",
    "  - figures/Figure_6_Compliance.pdf\n",
    "  - figures/Figure_6_Compliance.png\n",
    sep = "")
cat("\nPooled compliance: ",
    sprintf("%.1f%% (95%% CI %.1f%%-%.1f%%)",
            plogis(m$TE.random) * 100,
            plogis(m$lower.random) * 100,
            plogis(m$upper.random) * 100), "\n")
cat("Heterogeneity: I^2 =",
    sprintf("%.0f%%, tau^2 = %.3f, p = %.3f",
            m$I2 * 100, m$tau2, m$pval.Q), "\n")
