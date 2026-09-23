# data/

This directory holds `df_cup.rds`, the constructed, project-level analytic
frame used in the paper. See "The shortcut" in `DATA.md` for what it contains,
how it is built, and what running the pipeline from this file does and does not
let you verify.

`run_all.R` does not require this file: without it, the pipeline builds
`df1_cig2` from the raw administrative sources (`STAGE_1_BUILD <- TRUE`) or
expects `df1_cig2` to be loaded in the session.

The dataset is released under CC BY 4.0; see `LICENSE` in this directory.
