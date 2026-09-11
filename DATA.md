# Data sources

None of the raw administrative source data is redistributed here. Some of it
is public and can be downloaded; one source is not ours to release. What is
included is `data/df_cup.rds` if present — see "The shortcut" below — which is
our own constructed, aggregated dataset rather than a copy of the raw records,
and which we can therefore share directly.

A caveat worth a final check before distributing further: "publicly
available" and "redistributable" are not the same thing. A source can be free
to query from a public portal while its terms still prohibit redistributing
copies of the records themselves. We have not individually verified the
redistribution terms of each source below; the raw files are described here
so anyone building from scratch can locate and download them under whatever
terms apply, not shipped as copies.

| Source | What it contributes | Availability |
|---|---|---|
| PA Digitale 2026 (Italia Domani / Dipartimento per la trasformazione digitale) | funded projects, decree dates, solution type (`avviso`), funding amounts, administrative milestones | public — CSV/JSON updated daily via GitHub Action: [github.com/teamdigitale/padigitale2026-opendata](https://github.com/teamdigitale/padigitale2026-opendata) (`data/KPI/candidature_comuni_finanziate.csv`); portal: [padigitale2026.gov.it/open-data](https://padigitale2026.gov.it/open-data) |
| ANAC open data on public contracts | contract records, CIG identifiers, award value, procedure type, publication and completion timestamps, CPV codes | public — [dati.anticorruzione.it/opendata](https://dati.anticorruzione.it/opendata); per-year, per-month CIG and SmartCIG datasets follow the pattern `dati.anticorruzione.it/opendata/dataset/cig-{year}` and `.../smartcig-{year}`, matching the filenames `R/01_build_database.R` expects |
| CUP–CIG linkage table | joins projects to contracts | public, published by ANAC alongside the contract data above |
| AgID IndicePA — RTD dataset | municipal identifiers, digital transformation officer (RTD) establishment dates | public — [indicepa.gov.it/ipa-dati/dataset/responsabili-della-transizione-al-digitale](https://indicepa.gov.it/ipa-dati/dataset/responsabili-dellatransizione-al-digitale) |
| OpenBDAP / Ministry of Economy | supplier payment times, workforce composition | public — payment times: [rgs.mef.gov.it/.../tempi_di_pagamento_e_debiti_commerciali_delle_pubbliche_amministrazioni](https://www.rgs.mef.gov.it/VERSIONE-I/tempi_di_pagamento_e_debiti_commerciali_delle_pubbliche_amministrazioni/) (the manuscript's citation of this URL is truncated by one word - corrected here); workforce: [bdap-opendata.rgs.mef.gov.it/content/2022-dipendenti-pubblici-occupazione-complessiva-dati-analitici-ente](https://bdap-opendata.rgs.mef.gov.it/content/2022-dipendenti-pubblici-occupazione-complessiva-dati-analitici-ente) and the sibling "Anzianità" dataset on the same portal |
| ISTAT | resident population, municipality codes and names | public — [istat.it/classificazione/codici-dei-comuni-delle-province-e-delle-regioni](https://www.istat.it/classificazione/codici-dei-comuni-delle-province-e-delle-regioni/) (the standard downloadable file includes a population column alongside codes and names) |
| ANCI survey on municipal IT capacity, 2024 | IT staff, CIO profile, used only in the Table B5 robustness check | **not ours to redistribute**; requests to ANCI |

## Where the raw files go

`R/01_build_database.R` expects the raw sources under `data/raw/`, in the
subfolders below (these paths were hardcoded to a local machine in an earlier
version of this script; they are relative now, both for portability and
because the absolute path included the author's name and institution):

```
data/raw/OpenCUP/candidature_comuni.csv
data/raw/PNRR/PNRR_Iter_di_progetto_v3_M1.xlsx
data/raw/ANCI/2024_10_29_data_cleaned.xlsx
data/raw/Codice fiscale/enti.xlsx
data/raw/Dipendenti_PAL/*.xlsx           (four files)
data/raw/Tempi_pagamento_PA/*.xlsx
data/raw/RTD/RTD.xlsx
data/raw/Population/*.xlsx
data/raw/Contracts/CIG/                  (folder of per-year files)
data/raw/Contracts/SMARTCIG/             (folder of per-year files)
```

Two sources that an earlier version of the script loaded - municipal income
tax data ("GDP per capita") and a public-libraries dataset - are not in this
list any more. Both were loaded and, in the case of income tax, joined into
an intermediate frame, but neither ever fed a variable in the published
models; removed from `R/01_build_database.R` rather than left as required
inputs for nothing. A third source, a set of ISTAT municipal balance-sheet
indicators pulled via SDMX, was also removed on the same grounds - it was
actively joined in, but none of its nine financial indicators ever reached
a published specification either. Removing it also fixes what would
otherwise be a blocking bug: the script's own second join to a lagged
version of this data (`balance_lagged`) referenced an object that was never
created, since the code that would have built it was commented out.
Anyone running the script from scratch would have hit an "object not found"
error at that point, before ever reaching the payment-times merge or the
past-expenditure construction later in the file.

`data/` is gitignored except for `data/README.md`, so none of these ever get
committed - see the note there.

## The two routes into stage 1

`R/01_build_database.R` reads the raw files and constructs the contract-level
frame, ending with an explicit alias to `df1_cig2`, the name every downstream
script expects (see the comment at the end of the file for why the alias is
there). It still contains the full construction pipeline as it was run,
unpruned, on the principle that a build script that ran is worth more than a
tidier one that has not been re-verified; expect to read it with the variable
list below in hand rather than top to bottom.

What is not in it any more is roughly 1,950 lines that followed the
construction of `df1_cig` in the original file: exploratory models (Tobit,
SEM, log-logistic and lognormal survival specifications) that do not appear in
the paper, ad hoc plots, and a hierarchical clustering exercise, none of which
feeds any reported result. One block from that range was not exploratory,
however — the loading and cleaning of the ANCI CIO survey — and is preserved
as `R/appendix_b/04_build_cio_subset.R`, since it genuinely builds a covariate table
rather than analysing one.

Alternatively, skip stage 1 entirely with the shortcut below.

## Which projects are admitted

Two filters determine which PA Digitale 2026 candidatures become part of the
project frame, both applied in `R/01_build_database.R`.

**Candidature status.** Only `stato_candidatura %in% c("A", "E")` is kept —
accepted or executed candidatures (line 38). Rejected/withdrawn candidatures
(`stato_candidatura == "R"`) are excluded: a withdrawn candidature never
proceeds to procurement and generates no duration to observe.

**Funding decree label.** `filter(str_detect(decreto_finanziamento, "2022|2023"))`
(line 35) keeps candidatures whose decree text contains "2022" or "2023" -
this is the origin of the "restricted to decrees issued by the end of 2023"
language that was in an earlier design stage. `decreto_finanziamento` is a
text label (e.g. "Decreto n.24 - 2 / 2022 - PNRR"), not the same field as
`data_finanziamento` (the date `year` is derived from) - the two need not
agree for every candidature, and empirically some do not: `year == "2024"`
observations are present in the final frame (1,261 in the award and
completion samples, 325 in execution) despite this filter. This was checked,
not just noted: re-fitting the main models with these excluded changes no
interaction coefficient's sign or significance level in any phase
(`R/appendix_b/tableB11_2024check.R`, reported as Table B11 in the
appendix).

## The shortcut: `df_cup.rds`

`df_cup` is the constructed, project-level frame that every table and figure
script consumes — 40,991 rows, one per funded project, built by collapsing
`df1_cig2` in Part 2 of `R/02_analysis.R` and then enriched with
`log_population`, `importo_finanziamento` and `codice_istat` by
`R/03_prepare_extras.R`. Unlike the raw administrative sources above, this is
our own derived, aggregated dataset, not a copy of anyone else's records, so
we can share it directly.

To export it from a session where the full pipeline has already run once:

```r
saveRDS(df_cup, "data/df_cup.rds")
```

do this **after** `R/03_prepare_extras.R` has run, so the three joined columns
travel with it, then place the file at `data/df_cup.rds` in this repository.
`run_all.R` finds it automatically: with `STAGE_1_BUILD <- FALSE`, if
`data/df_cup.rds` exists it is loaded directly and stage 1 is skipped, and
`R/02_analysis.R` skips Parts 1, 2 and 4 — the duplication diagnostics, the
collapse itself, and the before/after comparison — since none of those are
meaningful without the raw contract-level frame they compare against.

What this shortcut buys you: every reported table and figure downstream of
the collapse (Tables 3 through B5, Figures 3 through 5, every robustness
check) can be reproduced and checked against the published numbers without
touching the raw administrative sources at all. What it does not let you
check: the unit-of-analysis correction itself, which is the specific,
verifiable claim that estimating at contract level (`df1_cig2`) inflates N by
a factor that varies by solution type. That claim can only be re-verified by
running stage 1 in full, which needs the raw sources in the table above.

## Variables that matter

Outcomes, all project-level milestones:

- `contracting` / `contractual_status` — award duration, from admission to
  funding through contract finalization. **Not** the column named `award`, which
  takes negative values and is not the award-phase outcome.
- `execution` / `execution_status` — execution duration, from start of contract
  execution to delivery.
- `completion` / `execution_status` — the sum of the two.

Moderator and controls: `log_digital_expenditure` (prior expenditure on
solutions with CPV codes beginning 48 or 72, 2007–2021),
`log_past_expenditure`, `log_importo_complessivo_gara`, `type`, `pop_cluster`,
`regione`, `year`, `DTO`, `weighted_payment_time`, `u35_pct`, `female_pct`.

## Three caveats that affect anything built on these columns

**The log floor, and it is not uniform across variables.** `db_building.R`
constructs three of these with a small epsilon (`log(x + 1e-10)`), not `log1p`:
`log_past_expenditure`, `log_importo_complessivo_gara`, and
`log_importo_finanziamento`. A true zero on any of these becomes roughly −23
rather than 0; the value is finite, so `is.finite()` does not flag it.
`log_digital_expenditure` is the exception and was verified directly rather than
assumed: `identical(round(log_digital_expenditure, 6), round(log1p(digital_expenditure), 6))`
returns `TRUE` on `df1_cig2`. It is `log1p(x)`, so a true zero — which is the
majority case, 56.4% of municipalities in the estimation sample — correctly maps
to 0 and is retained, not filtered out. Do not assume the same construction
applies to the other three just because they look similar; check the specific
column before writing code that depends on it.

In practice this means: `log_past_expenditure` never actually hits its floor
(`mean(past_expenditure == 0)` is 0 — no municipality reports zero general
procurement activity over 2007–2021), so the epsilon is inert for that column.
`log_importo_complessivo_gara` does hit it, for the 10,132 of 40,991 projects with
no matched contract record (Section 3 of the manuscript); those rows are the
ones removed by the `log_importo_complessivo_gara >= 0` filter in the main
models, not a uniform ~1.25 attrition spread evenly across covariates. `Ln(Value)`
on the unfiltered frame is where this matters most for anyone doing exploratory
work outside the main pipeline: leaving the floor in place changes several
correlations by more than 0.3 and flips five signs relative to the filtered,
reported values.

**CPV coverage.** Contracts below the simplified-procedure threshold carry no
CPV code, so historical IT activity conducted through many small awards is
under-recorded, and disproportionately for smaller municipalities: measured
expenditure is zero for roughly three quarters of municipalities in the smallest
population band and for none in the largest. The measure is a floor on
historical activity rather than a noisy estimate of it. Because the
under-recording is patterned, it does not bias the estimates in a known
direction.

**Unmatched projects.** For 10,132 of the 40,991 funded projects no contract
record is retrievable, and the share varies by solution type (35.3% for digital
services and payments, 14.7% for citizen experience). These projects cannot
enter models that condition on contract characteristics. Part 4 of
`R/02_analysis.R` and the full-sample check re-estimate with the lump-sum
funding amount in place of contract value.
