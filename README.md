# The Paradox of Experience — replication code

Code for *"The Paradox of Experience: Public Procurement and Delivery
Timelines across Digital Solution Types"* (GIQ-D-25-01214).

The raw administrative data cannot be redistributed. `data/df_cup.rds`, our
own constructed project-level dataset, is included in this repository. See
`DATA.md` for each raw source, how to obtain it, and what the `df_cup.rds`
shortcut does and does not let you verify.

## Usage

```
Rscript run_all.R
```

from the repository root. Three ways to provide the data, in order of
preference:

- use the included `data/df_cup.rds` (`DATA.md`, "The shortcut" — skips the
  raw sources entirely)
- set `STAGE_1_BUILD <- TRUE` with the raw sources in place
- load `df1_cig2` into the session yourself before running the script

Output goes to `outputs/`.

R 4.5 or later. Packages needed to run the analysis from `data/df_cup.rds`:
`dplyr`, `tidyr`, `tibble`, `survival`, `ggplot2`, `sandwich`, `lmtest`,
`car`, `moments`. `robomit` is optional, used only as a cross-check.

Building from the raw sources (`STAGE_1_BUILD <- TRUE`) additionally needs
`data.table`, `readr`, `readxl`, `writexl`, `stringr`, `lubridate`, `anytime`,
`janitor`, `purrr`, `jsonlite`, `rjson`, `rsdmx`, `rvest` and `stargazer`.

Each script in `R/main_tables/`, `R/appendix_a/`, and `R/appendix_b/` runs in
its own environment under `run_all.R`, because several scripts reuse the
same local variable names. Running scripts by hand: restart from
`R/02_analysis.R` between scripts, or use
`sys.source(path, envir = new.env(parent = globalenv()))`.

## Folder structure

- `R/main_tables/` — tables in the body of the paper. Tables 4 and 6 come
  directly from `R/02_analysis.R`, sharing model objects with the rest of
  that script.
- `R/appendix_a/` — Appendix A tables.
- `R/appendix_b/` — Appendix B tables and robustness checks. Appendix B is
  titled "Robustness checks" in the manuscript; the two categories are the
  same tables.

`R/appendix_b/04_build_cio_subset.R` needs the 2024 ANCI survey, which is
restricted-access (`DATA.md`) and used only for Table B5. That survey is not
redistributed here, so Table B5 is the one table in the paper that cannot be
reproduced from this repository alone; `run_all.R` completes normally without
it and simply does not produce that output. Every other table and figure is
reproducible from `data/df_cup.rds`.

## Script-to-table mapping

| Manuscript item | Script | Output file |
|---|---|---|
| Table 2, descriptive statistics | `R/main_tables/table2_descriptives.R` | `table2_descriptives.csv` |
| Table 3, pairwise correlations | `R/main_tables/table3_correlations.R` | `table3_correlations.csv` |
| Table 4, baseline models | `R/02_analysis.R` | `table_main_regressions.csv` |
| Table 5, model comparison and LR tests | `R/main_tables/table5_comparison.R` | `table5_comparison.csv` |
| Table 6, interaction models | `R/02_analysis.R` | `table_main_regressions.csv` |
| Figure 4, total effects | `R/02_analysis.R` | `figure4.png`, `figure4_data.csv` |
| Figure 5, effects in days | `R/02_analysis.R` | `figure5.png`, `predicted_days.csv` |
| Table A1, sample attrition | `R/02_analysis.R` | `table_c1_attrition.csv`† |
| Table A3/A4, descriptives (completed/ongoing subsamples) | `R/appendix_a/tableA3_A4_descriptives.R` | `tableA3_completed.csv`, `tableA4_ongoing.csv` |
| Table A5, multicollinearity diagnostics (GVIF) | `R/appendix_a/tableA5_gvif.R` | `tableA5_gvif.csv` |
| Table B1, model performance (Cox vs. Weibull, with interactions) | `R/appendix_b/tableB1_comparison.R` | `tableB1_cox_vs_weibull.csv` |
| Table B2, Cox with interactions | `R/appendix_b/tableB2_B3_B5.R` | `tableB2_cox.csv` |
| Table B3, direct awards only | `R/appendix_b/tableB2_B3_B5.R` | `tableB3_direct.csv` |
| Table B4, full project frame with lump-sum funding | `R/appendix_b/tableB4_fullsample.R` | `tableB4_fullsample.csv`, `tableB4_vs_main.csv` |
| Table B5, municipal IT capacity | `R/appendix_b/04_build_cio_subset.R` + `R/appendix_b/tableB2_B3_B5.R` | `tableB5_itcapacity.csv` |
| Appendix comparison table (all specifications) | `R/02_analysis.R` | `appendix_table.csv` |
| Table B6, municipality-stratified Cox | `R/02_analysis.R` + `R/appendix_b/tableB6_fit.R` + `R/appendix_b/tableB6_significance.R` | `fe_common_reference.csv`, `tableB6_with_se.csv`, `tableB6_counts.csv` |
| Raw stratified-Cox coefficients (feeds Table B6) | `R/02_analysis.R` | `fe_results.csv` |
| Table B7, control for the number of contracts per project (Section 4.3) | `R/appendix_b/complexity_within_type.R`, Part 1 | `complexity_fragmentation.csv` |
| Table B8, stratified Cox at increasing minimum category coverage (Section 4.3) | `R/appendix_b/selection_into_types.R`, Part 4 | `selection_breadth.csv` |
| Table B9, relative financial scale of the contracted work (Section 4.3) | `R/appendix_b/complexity_within_type.R`, Parts 2 and 2b | `complexity_scope.csv`, `tableB9_scope_full.csv` |
| Table B10, adoption of each category and estimates on the internal-contrast subsample, all three phases (Section 4.3) | `R/appendix_b/selection_into_types.R`, Parts 2 and 5 | `selection_by_type.csv`, `selection_internal_contrast_all_phases.csv` |
| Table B11, sensitivity to late-admission projects (Section 3.2) | `R/appendix_b/tableB11_2024check.R` | `tableB11_2024_admissions.csv` |
| Supplementary sample counts (Appendix C, not in the main tables) | `R/02_analysis.R` | `appendix_c_supplementary.csv` |
| Unit-of-analysis correction (Section 3.2) | `R/02_analysis.R`, Parts 1 and 4 | `collapse_comparison.csv` |

† Output filename predates a table renumbering; the table itself is correct
and is Table A1 in the current manuscript.

## Modeling notes

**Unit of analysis.** The CUP-CIG linkage is many-to-many; all three
duration outcomes are project-level milestones. Estimating at contract
level repeats identical outcome values across rows. `R/02_analysis.R` Part
1 documents this, Part 2 collapses to the project, Part 4 quantifies the
effect.

**Inference.** All AFT and Cox models cluster standard errors at the
municipality level (`cluster_vcov()` for AFT; `cluster = codice_ipa` for
both the no-strata and stratified Cox models in
`R/appendix_b/tableB6_fit.R`). `R/appendix_b/tableB6_significance.R`
derives each cell's standard error via the delta method, since every
reported figure is a contrast against Digital Identity, not a raw
coefficient.

`df1_cig2` is an alias for `df1_cig`, created at the end of
`R/01_build_database.R`. Both names work.

## Licence

Code in this repository is released under the MIT Licence (`LICENSE`). The
constructed dataset `data/df_cup.rds` is released under CC BY 4.0
(`data/LICENSE`). The underlying administrative sources remain subject to the
terms of their respective providers; see `DATA.md`.
