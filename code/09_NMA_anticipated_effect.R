#!/usr/bin/env Rscript
# ============================================================
# 09_NMA_anticipated_effect.R
# Anticipated absolute effect table (Ricci 2024 Table 2 style)
#   - Per outcome: noRT baseline (pooled mean +/- SD)
#   - Per treatment: SMD [95% CI]
#   - Per treatment: Delta in original units [95% CI]
#
# Input :
#   outputs/league_tables.xlsx
#   data/NMA_final.xlsx (sheet: 主分析_结局数据_)
# Output: outputs/NMA_anticipated_effect.xlsx
# ============================================================

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(writexl)

dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

# ---- 1. Outcome aggregation rules + units ------------------
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
units <- c(
  "TUG" = "s",  "8-FUGT" = "s",  "stair_climb" = "s",
  "6MWT" = "m", "gait_speed_usual" = "m/s",
  "gait_speed_fast" = "m/s",
  "5xSTS" = "s",  "30sSTS" = "reps"
)
# TRUE = lower raw score is better (reduction = improvement)
lower_better <- c("TUG", "8-FUGT", "stair_climb", "5xSTS")

# ---- 2. Pull noRT baseline mean/SD/n from main analysis data
outc <- read_excel("data/NMA_final.xlsx",
                   sheet = "主分析_结局数据_")

cat("=== Raw values in 'Node' column ===\n")
print(table(outc$`节点`, useNA = "always"))
cat("\n")

noRT <- outc %>%
  filter(grepl("non.?exer|control", `节点`, ignore.case = TRUE)) %>%
  mutate(
    outcome = unname(outcome_map[`分析结局名称`]),
    n       = suppressWarnings(as.numeric(n)),
    m       = suppressWarnings(as.numeric(`均值`)),
    sd      = suppressWarnings(as.numeric(SD))
  ) %>%
  filter(!is.na(outcome), !is.na(n), !is.na(m), !is.na(sd))

# Weighted pooled mean + Cochran pooled SD
baseline <- noRT %>%
  group_by(outcome) %>%
  summarise(
    k_noRT   = n(),
    n_total  = sum(n),
    mean_pool = sum(n * m) / sum(n),
    sd_pool   = sqrt(sum((n - 1) * sd^2) / sum(n - 1)),
    .groups = "drop"
  ) %>%
  mutate(outcome = factor(outcome, levels = outcome_order)) %>%
  arrange(outcome)

cat("=== noRT baseline per outcome (pooled) ===\n")
print(baseline)

# ---- 3. SMD vs noRT per outcome from league_tables ---------
LEAGUE <- "outputs/league_tables.xlsx"

parse_smd <- function(x) {
  s <- str_replace_all(x, "[−–—]", "-")
  m <- str_match(s, "^\\s*(-?[0-9.]+)\\s*\\(\\s*(-?[0-9.]+)\\s*,\\s*(-?[0-9.]+)\\s*\\)\\s*$")
  list(smd = as.numeric(m[, 2]),
       lower = as.numeric(m[, 3]),
       upper = as.numeric(m[, 4]))
}

extract_smd <- function(sheet) {
  d <- read_excel(LEAGUE, sheet = sheet) %>%
    filter(Treatment == "non_exercise_control")
  lo  <- parse_smd(d$low_dose_RT)
  cnv <- parse_smd(d$conventional_dose_RT)
  tibble(outcome = sheet,
         treatment = c("Low-dose RT", "Conventional-dose RT"),
         SMD = c(lo$smd, cnv$smd),
         SMD_lo = c(lo$lower, cnv$lower),
         SMD_hi = c(lo$upper, cnv$upper))
}
smd_df <- bind_rows(lapply(excel_sheets(LEAGUE), extract_smd))

# ---- 4. Join + back-translate to original units ------------
final <- smd_df %>%
  left_join(baseline, by = "outcome") %>%
  mutate(
    unit         = units[outcome],
    flip         = outcome %in% lower_better,
    # Delta in original units = SMD x pooled SD
    delta        = SMD    * sd_pool,
    delta_lo     = SMD_lo * sd_pool,
    delta_hi     = SMD_hi * sd_pool,
    # Anticipated intervention mean
    intv_mean    = mean_pool + delta,
    # Direction label
    direction = case_when(
      flip & SMD < 0  ~ "improvement",
      flip & SMD >= 0 ~ "worsening",
      !flip & SMD > 0 ~ "improvement",
      !flip & SMD <= 0 ~ "worsening"
    ),
    # Significance (CI excludes 0)
    sig = (SMD_lo > 0 & SMD_hi > 0) | (SMD_lo < 0 & SMD_hi < 0),
    # Column formatting
    baseline_fmt  = sprintf("%.2f ± %.2f", mean_pool, sd_pool),
    SMD_fmt       = sprintf("%.2f (%.2f, %.2f)%s",
                            SMD, SMD_lo, SMD_hi,
                            if_else(sig, " *", "")),
    intv_mean_fmt = sprintf("%.2f", intv_mean),
    arrow_sym = if_else(delta < 0, " ↓", " ↑"),
    delta_fmt = sprintf("%+.2f (%.2f, %.2f) %s%s  [%s]",
                        delta, pmin(delta_lo, delta_hi),
                        pmax(delta_lo, delta_hi),
                        unit, arrow_sym,
                        if_else(direction == "improvement",
                                "improvement", "worsening"))
  ) %>%
  mutate(outcome = factor(outcome, levels = outcome_order)) %>%
  arrange(outcome, desc(treatment))

# ---- 5. Clean output table ---------------------------------
out_table <- final %>%
  transmute(
    Outcome   = paste0(outcome, " (", unit, ")"),
    Treatment = treatment,
    `n studies (noRT arms)` = k_noRT,
    `Baseline noRT (mean ± SD)` = baseline_fmt,
    `Anticipated intervention mean` = intv_mean_fmt,
    `Anticipated Δ (95% CI) in original units` = delta_fmt,
    `SMD (95% CI)` = SMD_fmt,
    Direction = direction
  )

cat("\n=== Anticipated absolute effect table ===\n")
print(out_table, n = Inf)

# ---- 6. Write Excel output ---------------------------------
write_xlsx(out_table, "outputs/NMA_anticipated_effect.xlsx")
cat("\nSaved: outputs/NMA_anticipated_effect.xlsx\n")
