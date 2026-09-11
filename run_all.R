# =============================================================================
# RUN EVERYTHING, IN ORDER
#
# Usage:  Rscript run_all.R          from the repository root
#
# WHY EACH SCRIPT GETS ITS OWN ENVIRONMENT
#   The table and robustness scripts were written independently and several of
#   them use the same short names for local objects (RHS, PH, est, grab, pick).
#   Sourced into the global environment they would overwrite one another, and the
#   failure surfaces far from its cause: a formula from one script silently used
#   by another. sys.source() with a fresh environment whose parent is the global
#   one gives each script its own namespace for assignment while still letting it
#   read df_cup, d_award, cluster_vcov and the rest from the global environment.
#
#   Consequence: a script cannot pass anything back to the global environment
#   except by assigning explicitly. Only 01, 02 and 03 do that, which is why
#   they are sourced normally.
# =============================================================================

STAGE_1_BUILD <- FALSE   # set TRUE only if the raw source files are in place;
                         # see DATA.md for what is needed and where to get it

isolated <- function(path) {
  cat("\n\n==================================================================\n")
  cat("  ", path, "\n")
  cat("==================================================================\n")
  sys.source(path, envir = new.env(parent = globalenv()))
}

dir.create("outputs", showWarnings = FALSE)

# --- stage 1: raw sources to the contract-level frame -------------------------
# R/01_build_database.R ends by aliasing its result to df1_cig2, which is the
# name every downstream script expects; see the comment at the end of that
# file for why the alias is there rather than a rename throughout.
#
# SHORTCUT: if data/df_cup.rds exists, it is loaded instead of running stage 1
# at all, and 02_analysis.R skips the diagnostics/collapse/comparison parts
# that only make sense starting from the raw contract-level frame. This is for
# verifying the models and tables without needing the raw administrative
# sources - see DATA.md for what the shortcut does and does not let you check.
DF_CUP_SHORTCUT <- "data/df_cup.rds"

if (!STAGE_1_BUILD && file.exists(DF_CUP_SHORTCUT) && !exists("df1_cig2")) {
  df_cup <- readRDS(DF_CUP_SHORTCUT)
  cat(sprintf("Loaded %s: %d projects. Skipping stage 1 and the diagnostics/\n",
              DF_CUP_SHORTCUT, nrow(df_cup)))
  cat("collapse/comparison parts of 02_analysis.R that need the raw frame.\n")
} else if (STAGE_1_BUILD) {
  source("R/01_build_database.R")            # produces df1_cig2
} else {
  cat("STAGE_1_BUILD is FALSE and", DF_CUP_SHORTCUT, "was not found.\n")
  cat("Either set STAGE_1_BUILD TRUE with the raw files in place, place a\n")
  cat("pre-built df_cup at", DF_CUP_SHORTCUT, ", or load df1_cig2 into the\n")
  cat("session yourself before running this script.\n")
  stopifnot(exists("df1_cig2") || exists("df_cup"))
}

# --- stage 2: the paper's models, tables and figures --------------------------
source("R/02_analysis.R")                    # df_cup, d_award/d_exec/d_comp,
                                             # cluster_vcov(), stars(), TYPES...
source("R/03_prepare_extras.R")              # log_population, grant, codice_istat

# --- stage 3: main-text tables (Table 2, 3, 5 - Table 4 and 6 are produced
# directly inside 02_analysis.R) -----------------------------------------------
isolated("R/main_tables/table2_descriptives.R")
isolated("R/main_tables/table5_comparison.R")
isolated("R/main_tables/table3_correlations.R")

# --- stage 4: Appendix A -------------------------------------------------------
isolated("R/appendix_a/tableA5_gvif.R")
isolated("R/appendix_a/tableA3_A4_descriptives.R")

# --- stage 5: Appendix B ("Robustness checks" in the manuscript's own words) -
# 04_build_cio_subset.R lives here, not with the core pipeline in stage 1/2:
# it needs the 2024 ANCI survey, which is restricted-access (see DATA.md) and
# is used for nothing except Table B5. Nothing downstream of stage 2 depends
# on it except the two lines immediately below.
source("R/appendix_b/04_build_cio_subset.R") # cio_subset, for Table B5 only

isolated("R/appendix_b/tableB1_comparison.R")
isolated("R/appendix_b/tableB2_B3_B5.R")
isolated("R/appendix_b/tableB4_fullsample.R")
isolated("R/appendix_b/tableB6_fit.R")           # must run before the next
                                                 # line - it assigns the six
                                                 # Cox models to globalenv()
                                                 # explicitly
isolated("R/appendix_b/tableB6_significance.R")  # Table B6's significance/SE

isolated("R/appendix_b/complexity_within_type.R")
isolated("R/appendix_b/selection_into_types.R")
isolated("R/appendix_b/tableB11_2024check.R")

# R/appendix_b/sensitivity_aft.R is deliberately NOT sourced here, for the
# same reason as oster_bounds.R below: it is not cited in the paper. Part 1's
# "largest excursion" reaches 32-41% of the final coefficient for Digital
# Notices and Digital Services and Payments (the two weakest categories
# throughout this project's other robustness checks), so it does not support
# a stability claim for them - only for the categories that were already
# solid. Part 2's simulated confounder is constructed correlated with prior
# digital expenditure but orthogonal to solution type by design, so it cannot
# break the interaction regardless of whether the true confound would; a
# pass here is close to guaranteed and is weak evidence either way. The
# script is left in the repository, correct and runnable, in case a future
# review round asks for exactly this kind of check done properly.

# R/appendix_b/oster_bounds.R is deliberately NOT sourced here. It is not
# reported in the paper - see the note at the top of that script and in
# README.md for why. The script itself is left in the repository, correct and
# runnable, in case the question of omitted-variable sensitivity is raised in
# a future review round.

cat("\n\nAll stages complete. Outputs in outputs/\n")
