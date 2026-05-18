#!/usr/bin/env Rscript
# ============================================================
# 10_NMA_table2_with_CINeMA.R
# Build Table 2: NMA main results x anticipated effect x CINeMA grade
#   Integrates CINeMA Summary-of-Findings outputs (F4 and F5).
#
# Input :
#   outputs/NMA_anticipated_effect.xlsx
#   outputs/F4_CINeMA_final.xlsx
#   outputs/F5_Summary_of_Findings.xlsx
# Output: outputs/NMA_Table2_full.xlsx
# ============================================================

library(readxl)
library(dplyr)
library(stringr)
library(writexl)

dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

# ---- 1. Anticipated effect table (from script 09) ----------
ant <- read_excel("outputs/NMA_anticipated_effect.xlsx") %>%
  mutate(
    treat_short = case_when(
      grepl("Low",          Treatment) ~ "low_dose_RT",
      grepl("Conventional", Treatment) ~ "conventional_dose_RT"
    ),
    # Recover outcome short name from "TUG (s)" style label
    outcome_short = str_extract(Outcome, "^[^ ]+")
  )

cat("=== Anticipated-effect rows ===\n", nrow(ant), "\n")

# ---- 2. CINeMA F4 source (F5 omits gait_speed) --------------
sof <- read_excel("outputs/F4_CINeMA_final.xlsx") %>%
  mutate(
    keep = str_detect(comparison, "vs non_exercise_control"),
    treat_short = str_replace(comparison, " vs non_exercise_control", ""),
    treat_short = str_trim(treat_short)
  ) %>%
  filter(keep) %>%
  select(outcome_short = outcome, treat_short,
         final_confidence,
         within_study_bias, imprecision, heterogeneity, incoherence)

# F5 carries `statistical_strength` + `clinical_interpretation`;
# read separately and join.
extras <- read_excel("outputs/F5_Summary_of_Findings.xlsx") %>%
  mutate(
    keep = str_detect(comparison, "vs non_exercise_control"),
    treat_short = str_replace(comparison, " vs non_exercise_control", ""),
    treat_short = str_trim(treat_short)
  ) %>%
  filter(keep) %>%
  select(outcome_short = outcome, treat_short,
         statistical_strength, clinical_interpretation)

sof <- sof %>% left_join(extras, by = c("outcome_short", "treat_short"))

cat("\n=== F4 vs noRT rows ===\n", nrow(sof), "\n")
cat("Comparisons available per outcome:\n")
print(sof %>% count(outcome_short))

# ---- 3. Join -----------------------------------------------
table2 <- ant %>%
  left_join(sof, by = c("outcome_short", "treat_short")) %>%
  mutate(
    # Compact flag string for downgrade reasons (keep "Major" / "Some")
    domain_flags = paste0(
      if_else(grepl("Major", within_study_bias),  "WSB ",  ""),
      if_else(grepl("Major", imprecision),        "Imp ",  ""),
      if_else(grepl("Major", heterogeneity),      "Het ",  ""),
      if_else(grepl("Major", incoherence),        "Inc ",  "")
    ),
    # GRADE certainty symbols
    grade_sym = case_when(
      final_confidence == "High"     ~ "⊕⊕⊕⊕ High",
      final_confidence == "Moderate" ~ "⊕⊕⊕○ Moderate",
      final_confidence == "Low"      ~ "⊕⊕○○ Low",
      final_confidence == "Very Low" ~ "⊕○○○ Very Low",
      TRUE ~ NA_character_
    )
  )

# ---- 4. Final Table 2 layout -------------------------------
out_table <- table2 %>%
  transmute(
    Outcome                                   = Outcome,
    Treatment                                 = Treatment,
    `k (direct)`                              = `n studies (noRT arms)`,
    `Baseline noRT (mean ± SD)`               = `Baseline noRT (mean ± SD)`,
    `Anticipated intervention mean`           = `Anticipated intervention mean`,
    `Anticipated Δ (95% CI), original units`  = `Anticipated Δ (95% CI) in original units`,
    `SMD (95% CI)`                            = `SMD (95% CI)`,
    Direction                                  = Direction,
    `Certainty (CINeMA)`                       = grade_sym,
    `Downgrade flags`                          = domain_flags,
    `Clinical interpretation`                  = clinical_interpretation
  )

cat("\n=== Full Table 2 ===\n")
options(width = 250)
print(out_table, n = Inf, width = Inf)

write_xlsx(out_table, "outputs/NMA_Table2_full.xlsx")
cat("\nSaved: outputs/NMA_Table2_full.xlsx\n")

# ---- 5. Certainty grade distribution ------------------------
cat("\n=== Certainty distribution (16 comparisons) ===\n")
print(table(out_table$`Certainty (CINeMA)`, useNA = "always"))
