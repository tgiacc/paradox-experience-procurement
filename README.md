# The Paradox of Experience — replication code

Code for *"The Paradox of Experience: Public Procurement and Delivery
Timelines across Digital Solution Types"* (GIQ-D-25-01214).

The raw administrative data cannot be redistributed; `data/df_cup.rds`, our
own constructed dataset, can be and is included when present. See `DATA.md`
for each raw source, what it contributes, how to obtain it, and what the
`df_cup.rds` shortcut does and does not let you verify.

## Running it

```
Rscript run_all.R
```

from the repository root. Three ways to provide the data it needs, in order
of preference: place a pre-built `data/df_cup.rds` (see `DATA.md`, "The
shortcut" — this skips the raw sources entirely), set `STAGE_1_BUILD <- TRUE`
with the raw sources in place, or load `df1_cig2` into the session yourself
before running the script. Output goes to `outputs/`.

Each script in `R/main_tables/`, `R/appendix_a/` and `R/appendix_b/` is sourced
into its own
environment by `run_all.R`. They were written independently and several reuse
the same short names for local objects, so sourcing them into a shared
environment lets one silently overwrite another's formula or helper. If you run
them by hand, restart from `R/02_analysis.R` between scripts, or use
`sys.source(path, envir = new.env(parent = globalenv()))`.

R 4.5 or later. Packages: `dplyr`, `tidyr`, `tibble`, `survival`, `ggplot2`,
`sandwich`, `lmtest`, `car`. `robomit` is optional and used only as a
cross-check.

Folder layout mirrors how the manuscript itself is organized, not an
arbitrary split: `R/main_tables/` produces the tables in the body of the
paper (Table 4 and 6 are produced directly inside `R/02_analysis.R` instead,
since they come out of the same model objects everything else in that script
uses). `R/appendix_a/` and `R/appendix_b/` produce the tables in Appendix A
and Appendix B respectively - Appendix B is titled "Robustness checks" in the
manuscript itself, which is why every script that used to live in a separate
`R/robustness/` folder is inside `R/appendix_b/` now: from the paper's own
point of view there was never a real distinction between "a robustness
script" and "a script that produces an Appendix B table" - they are the same
thing, and keeping them in two folders on the repository side obscured that
rather than clarifying it.

`R/appendix_b/04_build_cio_subset.R` is the one exception to "everything an
appendix script needs comes from the core pipeline in R/01-03": it needs the
2024 ANCI survey, which is restricted-access unlike the ANAC/PA Digitale data
the rest of the repository runs on (see DATA.md) and is used for nothing
except Table B5. It sits with the other Appendix B scripts because that is
the only place its output is used, not because it shares their data
requirements.

Every script and output filename below now matches the table number it
actually produces in the current manuscript - this was not always true (an
earlier version of this README carried notes explaining several mismatches
between a script's internal numbering and the manuscript's), and the
renaming is the fix, not just a relabelling of the same problem.

Note on `R/appendix_b/oster_bounds.R`: this script computes Oster (2019)
coefficient-stability bounds for the interaction terms and was, at one point,
intended for the paper. It is not cited anywhere in the current manuscript or
appendix - checked directly against both, not inferred - after a review
concluded the bounds were not a reliable indicator here: the derivation is
for OLS and the paper's models are AFT/Cox with censoring; the delta values
obtained (13.6, 22.0) reflect the interaction coefficient barely moving when
controls are added rather than R² failing to move, but the proportional-
selection assumption underlying the method still has no particular
justification when the "treatment" is a project category and the controls
are municipal characteristics. The script is left in the repository, correct
and runnable, but `run_all.R` does not source it - see the comment there. If
a future review round raises omitted-variable concerns, it is ready to use;
until then, running it produces a table that has no home in the paper.

Note on `R/appendix_b/sensitivity_aft.R`: not cited in the paper either, for
reasons specific to each of its two parts rather than one shared problem.
Part 1 (coefficient movement across nested control blocks) is not evidence of
stability for Digital Notices or Digital Services and Payments - the two
categories that were already the weakest in every other robustness check in
this project - since their largest excursion reaches 32-41% of the final
coefficient; it only supports a stability claim for the categories that were
solid regardless. Part 2 (a simulated municipality-level confounder) is
constructed correlated with prior digital expenditure but orthogonal to
solution type by design, so it cannot break the interaction whether or not a
true confounder would; a pass is close to guaranteed and is weak evidence in
either direction. `run_all.R` does not source this script - see the comment
there - but it is left in the repository for the same reason as
`oster_bounds.R`: correct, runnable, and ready if a future round asks for
this kind of check done in a way that actually has power to fail.

Note on Tables B7-B9 (marked with * in the table below): the scripts and their outputs are
correct and were checked against the response letter's own quoted figures
(see the reconciliation step at the end of `complexity_within_type.R`). An
earlier appendix version checked did not contain them as numbered tables;
their presence has since been confirmed in the current appendix. The
asterisk is kept as a reminder to re-verify against whichever appendix file
is current, since the same gap could recur silently if a future edit starts
from an older copy.

## What produces what

Entries marked with * have a note explaining their status above.

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
| Table B2, Cox with interactions | `R/appendix_b/tableB2_B3_B5.R` | `tableB2_cox.csv` |
| Table B3, direct awards only | `R/appendix_b/tableB2_B3_B5.R` | `tableB3_direct.csv` |
| Table B4, full project frame with lump-sum funding | `R/appendix_b/tableB4_fullsample.R` | `tableB4_fullsample.csv`, `tableB4_vs_main.csv` |
| Table B5, municipal IT capacity | `R/appendix_b/04_build_cio_subset.R` + `R/appendix_b/tableB2_B3_B5.R` | `tableB5_itcapacity.csv` |
| Appendix comparison table (all specifications) | `R/02_analysis.R` | `appendix_table.csv` |
| Table B6, municipality-stratified Cox | `R/02_analysis.R` + `R/appendix_b/tableB6_fit.R` + `R/appendix_b/tableB6_significance.R` | `fe_common_reference.csv`, `tableB6_with_se.csv`, `tableB6_counts.csv` |
| Raw stratified-Cox coefficients (feeds Table B6) | `R/02_analysis.R` | `fe_results.csv` |
| Table B7*, fragmentation and financial-scope checks (Section 6.3) | `R/appendix_b/complexity_within_type.R`, Parts 1-2 | `complexity_fragmentation.csv`, `complexity_scope.csv` |
| Table B8*, selection into solution types / portfolio breadth (Section 6.3) | `R/appendix_b/selection_into_types.R` | `selection_by_type.csv`, `selection_breadth.csv` |
| Table B9*, financial-scope check with the full Table 4/6 covariate set | `R/appendix_b/complexity_within_type.R`, Part 2b | `tableB9_scope_full.csv` |
| Supplementary sample counts (Appendix C, not in the main tables) | `R/02_analysis.R` | `appendix_c_supplementary.csv` |
| Unit-of-analysis correction (Section 3.2) | `R/02_analysis.R`, Parts 1 and 4 | `collapse_comparison.csv` |

† Named for an earlier plan to publish sample attrition as a separate
"Table C1"; the table itself is correct and is Table A1 in the current
manuscript. The output filename was not renamed, since doing so would not
change what the script computes and CSV filenames aren't cited anywhere.

## Two things to know before reading the code

**The unit of analysis.** The CUP–CIG linkage is many-to-many, and all three
duration outcomes are project-level milestones. Estimating at contract level
repeats identical outcome values across rows, unevenly across solution types.
Part 1 of `R/02_analysis.R` documents this and Part 2 collapses to the project.
Part 4 quantifies what changed.

**Inference.** The AFT tables cluster standard errors at the municipality level
(`cluster_vcov()` in Part 5). The stratified Cox models (Table B6,
`R/appendix_b/tableB6_fit.R`) also cluster at the municipality level, matching the
main specification - both the no-strata and stratified columns use
`cluster = codice_ipa`. `R/appendix_b/tableB6_significance.R` derives each cell's
standard error from that clustered variance-covariance matrix via the delta
method, since every reported figure is a contrast against Digital Identity,
not a raw coefficient.

**`df1_cig2`.** `R/01_build_database.R` builds a frame called `df1_cig`
throughout and aliases it to `df1_cig2` in its last line. That alias exists
because the version of the script that produced the reported results referred
to `df1_cig2` from partway through without ever creating an object under that
name — a rename that happened once, interactively, and was never saved. The
alias is documented where it appears rather than resolved by renaming
everything upstream, so the history stays visible.

## Before submission

The manuscript (Section 3, and the Data availability statement) and the
response letter (R2.2) currently point to
`https://github.com/anonymous/paradox-experience-procurement`, which is not a
real, resolvable link — it is a placeholder for the double-blind review
requirement. Three steps remain, in order:

1. Push this repository's contents to a real GitHub repository (it can stay
   private for now).
2. Generate an anonymized viewing link with
   [anonymous.4open.science](https://anonymous.4open.science) (point it at the
   real repo above) or an OSF view-only link, whichever fits the journal's
   submission workflow.
3. Replace the placeholder URL with that anonymized link in all three places:
   the manuscript's Section 3, its Data availability statement, and the
   response letter's R2.2 response — they must all say the same thing.

After acceptance, when anonymity is no longer required, swap the anonymized
link for the permanent one and consider a Zenodo deposit for a citable DOI.

## Completeness

Every file the table above promises was checked against what the scripts
actually write (`grep` for every `write.csv("outputs/...")` call, diffed
against the table), not assumed from the table alone. Two tables were found
with no reproducing script at all - Table 2 and Table 5 - and
`R/main_tables/table2_descriptives.R` and `R/main_tables/table5_comparison.R` were
written to close that gap; unlike the Table B2/B3/B5 script, these are
reconstructions rather than copies of a verified session, and each checks its
own output against the N or log-likelihood values already published in the
paper, printing OK or MISMATCH rather than assuming success.

## What is not here

Work carried out during the revision that reached a dead end or was superseded
was not kept in this repository: a difference-in-discontinuities design on the
population thresholds that set contractual deadlines (abandoned because the
treatment bundles a longer deadline with a larger grant, and the effect does
not replicate at the second threshold where the same deadline rule applies),
and a specification adding population-band-by-solution-type interactions,
which the published models omit (with those terms included, the
experience-by-type interactions shrink substantially and the joint tests do
not reject at the 5% level in any phase; a likelihood ratio test rejects
additivity of the band-by-type cells). Both are available on request.
