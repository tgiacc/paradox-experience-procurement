# data/

This directory is empty in the repository as distributed. It exists to hold
`df_cup.rds`, the constructed, project-level analytic frame — see "The
shortcut" in `DATA.md` for what it is, how to export it, and what running the
pipeline from this file does and does not let you check.

Nothing in `run_all.R` requires this file to be present: without it, the
pipeline falls back to building `df1_cig2` from the raw administrative
sources (`STAGE_1_BUILD <- TRUE`) or expects `df1_cig2` to already be loaded
in the session.
