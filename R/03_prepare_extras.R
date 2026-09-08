# =============================================================================
# DERIVED COLUMNS NEEDED BY THE TABLE AND ROBUSTNESS SCRIPTS
#
# Run after R/02_analysis.R. The collapse in Part 2 of that script keeps only
# the variables the main models use, so three columns that later scripts need
# are joined back here rather than inside each of them. Doing it once, in the
# global environment, is what makes the later scripts safe to run in isolation.
#
#   log_population           continuous log of resident population. The collapse
#                            keeps only pop_cluster, the banded version used in
#                            estimation. Table 3 reports Ln(Pop.), and the
#                            size-by-type checks need it in continuous form.
#   importo_finanziamento    the lump-sum grant, fixed by demographic band and
#                            solution type. Needed for the project-scope measure
#                            in R/robustness/complexity_within_type.R.
#   codice_istat             needed by the direct-awards subsample.
#
# All three are constant within project, which is asserted rather than assumed.
# =============================================================================

library(dplyr)

stopifnot(exists("df_cup"), exists("d_award"), exists("d_exec"), exists("d_comp"))

join_back <- function(col) {
  if (!exists("df1_cig2")) {
    stop(sprintf("%s is missing from df_cup and df1_cig2 is not available to ",
                 col), "recover it from. Export df_cup only after running this ",
         "script once, so these columns travel with it.", call. = FALSE)
  }
  if (!col %in% names(df1_cig2)) {
    cat(sprintf("  %s is not in df1_cig2: skipped\n", col))
    return(NULL)
  }
  chk <- df1_cig2 %>% group_by(cup) %>%
    summarise(k = n_distinct(.data[[col]], na.rm = TRUE), .groups = "drop")
  if (any(chk$k > 1)) {
    warning(sprintf("%s varies within project for %d projects; first() is used",
                    col, sum(chk$k > 1)), call. = FALSE)
  }
  df1_cig2 %>% group_by(cup) %>%
    summarise(!!col := first(.data[[col]]), .groups = "drop")
}

cat("Joining derived columns back onto the project-level frames:\n")

for (col in c("log_population", "importo_finanziamento", "codice_istat")) {
  if (col %in% names(df_cup)) {
    cat(sprintf("  %-24s already present in df_cup: skipped\n", col))
    next
  }
  src <- join_back(col)
  if (is.null(src)) next
  for (nm in c("df_cup", "d_award", "d_exec", "d_comp")) {
    d <- get(nm, envir = globalenv())
    if (col %in% names(d)) next
    assign(nm, left_join(d, src, by = "cup"), envir = globalenv())
  }
  miss <- sum(is.na(get("df_cup", envir = globalenv())[[col]]))
  cat(sprintf("  %-24s joined | missing: %d of %d projects\n",
              col, miss, nrow(get("df_cup", envir = globalenv()))))
}

# A note that matters for anything reading these columns: the build script
# constructs several log variables with a small epsilon rather than log1p, so a
# true zero becomes a large negative number that is finite. Guard against it by
# treating anything below -20 as missing before use, as the table scripts do.
for (nm in c("df_cup", "d_award", "d_exec", "d_comp")) {
  d <- get(nm, envir = globalenv())
  n_floor <- sum(sapply(d[, grepl("^log_", names(d)), drop = FALSE],
                        function(x) sum(is.numeric(x) & x < -20, na.rm = TRUE)))
  if (n_floor > 0)
    cat(sprintf("  %s: %d values at the log floor; see the note in DATA.md\n",
                nm, n_floor))
}

cat("Done.\n")
