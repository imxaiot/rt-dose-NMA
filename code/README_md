# Code — analytical pipeline

This directory contains the full R pipeline used to produce the main and supplementary figures, the Summary-of-Findings tables, and the compliance synthesis reported in the manuscript.

> **Repository scope**: this archive contains analytical code only. The aggregate trial-level data and intermediate outputs are available from the manuscript's corresponding author on reasonable request for any non-commercial scientific use.

All scripts are written for **R ≥ 4.5** and use `relative paths` from the project root, in which the `data/`, `outputs/`, `figures/` and `code/` folders live side-by-side. Place the requested data files into `data/` and `outputs/` before running the pipeline.

## Reproducing the analysis from scratch

```r
setwd("/path/to/osf-project-root")   # ./code, ./data, ./outputs, ./figures
source("code/01_PRISMA_flow.R")          # Figure 1
source("code/02_NMA_network_main.R")     # Figure 2a — three-node network geometry
source("code/03_CNMA_network.R")         # Figure 2b — 9-cell component network
source("code/04_NMA_forest_pub.R")       # Figure 4 — NMA forest plot
source("code/05_NMA_funnel_pub.R")       # Figure 5 — comparison-adjusted funnels
source("code/06_CNMA_figure_pub.R")      # Figure 3 — CNMA heatmap + ranking
source("code/07_Compliance_forest.R")    # Figure 6 — overall compliance forest
source("code/08_Compliance_subgroup.R")  # Supplementary Figure S2 — dose subgroup
source("code/09_NMA_anticipated_effect.R") # outputs/NMA_anticipated_effect.xlsx
source("code/10_NMA_table2_with_CINeMA.R") # outputs/NMA_Table2_full.xlsx (Table 2)
```

`00_save_pub.R` is a helper sourced by every figure script — no need to run it directly.

## Script-by-script summary

| # | Script | Reads | Writes |
|---|---|---|---|
| 00 | `00_save_pub.R` | (helper) | (none) |
| 01 | `01_PRISMA_flow.R` | hard-coded counts | `figures/Figure_1_PRISMA.{svg,pdf,png}` |
| 02 | `02_NMA_network_main.R` | `data/NMA_final.xlsx` | `figures/Figure_2a_Network_main.{svg,pdf,png}` |
| 03 | `03_CNMA_network.R` | `outputs/CNMA_dose_template.xlsx`, `data/NMA_final.xlsx` | `figures/Figure_2b_CNMA_network.{svg,pdf,png}` |
| 04 | `04_NMA_forest_pub.R` | `outputs/league_tables.xlsx` | `figures/Figure_4_Forest.{svg,pdf,png}` |
| 05 | `05_NMA_funnel_pub.R` | `data/NMA_final.xlsx`, `outputs/league_tables.xlsx` | `figures/Figure_5_Funnel.{svg,pdf,png}` |
| 06 | `06_CNMA_figure_pub.R` | `outputs/CNMA_treatment_effects_with_k.xlsx` | `figures/Figure_3_CNMA.{svg,pdf,png}` |
| 07 | `07_Compliance_forest.R` | `data/NMA_final.xlsx` | `figures/Figure_6_Compliance.{svg,pdf,png}` |
| 08 | `08_Compliance_subgroup.R` | `data/NMA_final.xlsx` | `figures/Supplementary_Figure_S2_Compliance_subgroup.{svg,pdf,png}` |
| 09 | `09_NMA_anticipated_effect.R` | `outputs/league_tables.xlsx`, `data/NMA_final.xlsx` | `outputs/NMA_anticipated_effect.xlsx` |
| 10 | `10_NMA_table2_with_CINeMA.R` | `outputs/NMA_anticipated_effect.xlsx`, `outputs/F4_CINeMA_final.xlsx`, `outputs/F5_Summary_of_Findings.xlsx` | `outputs/NMA_Table2_full.xlsx` |

## Input data dependencies

The pipeline assumes the following files are present at the project root:

```
data/
  NMA_final.xlsx                          # master extraction (raw trial data;
                                          #   column headers in Chinese — see
                                          #   data_dictionary.md in ./data/)
  RoB2_per_study.xlsx                     # RoB 2 ratings, per study x domain
outputs/                                  # produced by upstream NMA fits
                                          # (netmeta / netcomb — not included
                                          # in this minimal pipeline)
  league_tables.xlsx                      # 8 sheets, one per outcome
  CNMA_dose_template.xlsx                 # arm-level freq x intensity mapping
  CNMA_treatment_effects_with_k.xlsx      # CNMA per-cell SMDs + direct k
  F4_CINeMA_final.xlsx                    # CINeMA 6-domain matrix
  F5_Summary_of_Findings.xlsx             # GRADE narrative + clinical interp
  E5_global_inconsistency.xlsx            # per-outcome tau^2 / I^2 / Q
  E6_summary_table.xlsx                   # direct vs NMA comparison summary
```

The `outputs/league_tables.xlsx`, `outputs/CNMA_*.xlsx`, and `outputs/F4_*.xlsx` /
`outputs/F5_*.xlsx` files are produced upstream by running `netmeta::netmeta()`
and `netmeta::netcomb()` on the extracted trial data, then exporting league
tables and applying the CINeMA framework. The minimum-reproduction code for
those upstream fits is **omitted from this distribution** because it depends on
a private extraction template; the post-NMA outputs are sufficient to reproduce
all figures and tables shown in the manuscript.

## R session

The published analysis used R 4.5 on Windows 10 with the following key packages:

- **netmeta** (Rücker et al., for NMA + netcomb CNMA)
- **meta** (Schwarzer et al., for proportion meta-analysis)
- **metafor** (Viechtbauer, for `rma()` in sensitivity analyses)
- **ggplot2 / patchwork / tidytext** (figures)
- **svglite / ragg / systemfonts** (vector + 600 dpi raster export with Arial embedding)
- **readxl / writexl / dplyr / tidyr / stringr / purrr** (data wrangling)

Run `sessionInfo()` after sourcing any script for a complete dependency snapshot;
a frozen `sessionInfo.txt` is included alongside this README.

## Licence

Code in this directory is released under the **MIT licence**. See the repository-level `LICENSE` file for the full text. Data are not redistributed with this code repository; they are available on request (see the manuscript's *Availability of data and material* statement).

## Citation

If you reuse this code, please cite:

> [Author list]. Low-dose versus conventional-dose resistance training for physical function in older adults: a systematic review and network meta-analysis. *Sports Medicine - Open* 2026; **in submission**. PROSPERO CRD420261292646. Code repository: [GitHub URL to be added at acceptance].
