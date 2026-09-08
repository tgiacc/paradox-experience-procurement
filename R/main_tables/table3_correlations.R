# =============================================================================
# TABLE 3 ON THE PROJECT-LEVEL ESTIMATION SAMPLE, + AUDIT OF THE 1e-10 LOG FLOOR
#
# Run AFTER giq_r2_analysis.R.
#
# WHICH UNIT OF ANALYSIS, AND WHY THIS MATTERS HERE SPECIFICALLY
#   An earlier version of this script preferred df1_cig2 (contract/CIG rows,
#   pre-collapse) over df_cup (project/CUP rows) whenever both were in the
#   session - which they normally are - so Table 3 was silently computed at
#   contract level. That is inconsistent with the paper's own argument in
#   Section 3.2: duration outcomes are project-level facts, and analysing them
#   at contract level replicates the same value once per associated contract,
#   over-weighting fragmented projects in a way that Part 1 of 02_analysis.R
#   shows varies systematically by solution type. A table under the heading
#   "duration outcomes and explanatory variables" cannot silently use the unit
#   of analysis the rest of the paper argues against. This version fixes the
#   source to df_cup and does not fall back to df1_cig2.
#
# BACKGROUND ON THE LOG FLOOR
#   db_building.R builds four log variables as log(x + 1e-10), not log(x + 1):
#       line 497  log_importo_finanziamento
#       line 507  log_past_expenditure_digital
#       line 626  log_past_expenditure
#       line 961  log_importo_complessivo_gara
#   A true zero therefore becomes log(1e-10) = -23.0259 instead of 0. The value
#   is finite, so is.finite() does not flag it.
#
#   The phase models filter `log_* >= 0`, which given the +1e-10 floor means
#   x >= 1: those rows are already out of the estimation sample. Parts 1-2
#   below still audit df1_cig2, since the floor is a property of the raw
#   columns and is easiest to see before any collapse; Part 3, the table
#   itself, uses df_cup.
#
# WHAT THIS DOES
#   PART 1  audits every log_ column on df1_cig2: share at the floor, min,
#           documented-vs-actual transformation
#   PART 2  quantifies what the `>= 0` filters remove, also on df1_cig2
#   PART 3  computes Table 3 on df_cup (project level), matching Table 2's
#           own estimation frame, and checks the resulting pairwise-N range
#           against the manuscript's own published note as a correctness
#           check
# =============================================================================

library(dplyr)

dir.create("outputs", showWarnings = FALSE)
FLOOR <- log(1e-10)                      # -23.02585

# --- resolve names once, so a rename upstream does not break the script -------
pick <- function(df, ...) {
    for (cand in c(...)) if (cand %in% names(df)) return(cand)
    stop(sprintf("None of these columns exist in %s: %s",
                 deparse(substitute(df)), paste(c(...), collapse = ", ")))
}

DIAG_SRC <- if (exists("df1_cig2")) df1_cig2 else
       if (exists("df_cup"))  df_cup  else
       stop("Neither df1_cig2 nor df_cup is in the environment.")
cat(sprintf("Source frame for Parts 1-2 (pre-collapse diagnostics): %s rows\n",
            format(nrow(DIAG_SRC), big.mark = ",")))

stopifnot(exists("df_cup"))
cat(sprintf("Source frame for Part 3 (Table 3 itself, project level): %s rows\n",
            format(nrow(df_cup), big.mark = ",")))

V_DIG   <- pick(DIAG_SRC, "log_digital_expenditure", "log_past_expenditure_digital")
V_PROC  <- pick(DIAG_SRC, "log_past_expenditure")
V_VALUE <- pick(DIAG_SRC, "log_importo_complessivo_gara")
V_POP   <- pick(DIAG_SRC, "log_population")


# =============================== PART 1 ======================================
# Every log_ column, not just the four known ones: if the pattern is in
# db_building it may be elsewhere too.
cat("\n=== PART 1: log columns at the 1e-10 floor ===\n\n")

logcols <- names(DIAG_SRC)[grepl("^log_", names(DIAG_SRC)) & sapply(DIAG_SRC, is.numeric)]
audit <- lapply(logcols, function(v) {
    x <- DIAG_SRC[[v]]
    data.frame(
        variable   = v,
        n          = sum(!is.na(x)),
        at_floor   = sum(x < -20, na.rm = TRUE),
        share      = mean(x < -20, na.rm = TRUE),
        min        = min(x, na.rm = TRUE),
        min_is_eps = isTRUE(abs(min(x, na.rm = TRUE) - FLOOR) < 1e-4),
        stringsAsFactors = FALSE)
}) %>% bind_rows()

print(audit %>% mutate(share = sprintf("%.1f%%", 100 * share),
                       min = sprintf("%.4f", min)), row.names = FALSE)

# What log(x + 1) would have given for the same rows, for the paper's claim.
cat("\nDocumented transformation is log(x + 1). Difference at the floor:\n")
cat(sprintf("  log(0 + 1e-10) = %.4f   vs   log(0 + 1) = %.4f\n", FLOOR, 0))
cat("  For x above a few units the two agree to three decimals; the entire\n")
cat("  discrepancy is concentrated on the exact zeros.\n")


# =============================== PART 2 ======================================
# The `>= 0` filters in the model calls. With the +1e-10 floor these are
# effectively x >= 1, so they drop the zeros rather than guarding against them.
cat("\n=== PART 2: what the >= 0 filters remove ===\n\n")

flt <- DIAG_SRC %>%
    transmute(
        dig_zero   = .data[[V_DIG]]   < -20,
        proc_zero  = .data[[V_PROC]]  < -20,
        value_zero = .data[[V_VALUE]] < -20,
        kept       = .data[[V_DIG]] >= 0 & .data[[V_PROC]] >= 0 & .data[[V_VALUE]] >= 0
    )

cat(sprintf("Rows with zero prior digital expenditure : %s (%.1f%%)\n",
            format(sum(flt$dig_zero, na.rm = TRUE), big.mark = ","),
            100 * mean(flt$dig_zero, na.rm = TRUE)))
cat(sprintf("Rows with zero prior procurement         : %s (%.1f%%)\n",
            format(sum(flt$proc_zero, na.rm = TRUE), big.mark = ","),
            100 * mean(flt$proc_zero, na.rm = TRUE)))
cat(sprintf("Rows with zero contract value            : %s (%.1f%%)\n",
            format(sum(flt$value_zero, na.rm = TRUE), big.mark = ","),
            100 * mean(flt$value_zero, na.rm = TRUE)))
cat(sprintf("Rows surviving all three >= 0 filters    : %s of %s (%.1f%%)\n",
            format(sum(flt$kept, na.rm = TRUE), big.mark = ","),
            format(nrow(DIAG_SRC), big.mark = ","),
            100 * mean(flt$kept, na.rm = TRUE)))

# Are the dropped rows systematically different? This is the question a referee
# asks about a sample restriction, so answer it here rather than be asked.
if ("pop_cluster" %in% names(DIAG_SRC)) {
    cat("\nShare of rows dropped by the digital-expenditure filter, by population band:\n")
    DIAG_SRC %>%
        mutate(dropped = .data[[V_DIG]] < 0) %>%
        group_by(pop_cluster) %>%
        summarise(n = n(), dropped = sprintf("%.1f%%", 100 * mean(dropped, na.rm = TRUE)),
                  .groups = "drop") %>%
        as.data.frame() %>% print(row.names = FALSE)
}


# =============================== PART 3 ======================================
# Table 3 on the estimation frame: the rows the models actually use. Durations
# enter with pairwise deletion, so each phase contributes its own observations,
# exactly as in the three phase models.
cat("\n=== PART 3: Table 3 on the estimation frame ===\n\n")

AWARD <- if (exists("AWARD_TIME")) AWARD_TIME else pick(df_cup, "award", "contracting")

est <- df_cup %>%
    filter(.data[[V_DIG]] >= 0, .data[[V_PROC]] >= 0, .data[[V_VALUE]] >= 0)

cat(sprintf("Estimation frame: %s rows\n", format(nrow(est), big.mark = ",")))

dat <- est %>%
    transmute(
        Award                                      = ifelse(.data[[AWARD]]  > 0, .data[[AWARD]],  NA_real_),
        Execution                                   = ifelse(execution       > 0, execution,       NA_real_),
        Completion                                  = ifelse(completion      > 0, completion,      NA_real_),
        DTO                                          = DTO,
        `Supplier payment time`                      = weighted_payment_time,
        `Empl. 25-34`                                = u35_pct,
        `Female empl.`                               = female_pct,
        `Population (log)`                           = .data[[V_POP]],
        `Contract award value (log)`                 = .data[[V_VALUE]],
        `General procurement experience (log)`       = .data[[V_PROC]],
        `Procurement of digital solutions (log)`     = .data[[V_DIG]]
    ) %>%
    mutate(across(everything(), ~ ifelse(is.finite(.x), .x, NA_real_)))

# Guard: nothing at the floor should survive into the correlations.
left <- sapply(dat, function(x) sum(x < -20, na.rm = TRUE))
if (any(left > 0)) {
    cat("\nWARNING - values still at the log floor after filtering:\n")
    print(left[left > 0])
}

V <- names(dat); k <- length(V)
R <- P <- N <- matrix(NA_real_, k, k, dimnames = list(V, V))
for (i in seq_len(k)) for (j in seq_len(k)) {
    if (i == j) { R[i, j] <- 1; P[i, j] <- 0; N[i, j] <- sum(!is.na(dat[[i]])); next }
    ok <- stats::complete.cases(dat[[i]], dat[[j]])
    N[i, j] <- sum(ok)
    if (sum(ok) > 3) {
        ct <- suppressWarnings(cor.test(dat[[i]][ok], dat[[j]][ok]))
        R[i, j] <- ct$estimate; P[i, j] <- ct$p.value
    }
}

stars3 <- function(p) {
    ifelse(is.na(p), "",
    ifelse(p < 0.001, "***",
    ifelse(p < 0.01,  "**",
    ifelse(p < 0.05,  "*",
    ifelse(p < 0.1,   ".", "")))))
}

cells <- matrix(paste0(sprintf("%.2f", R), stars3(P)), nrow = k, dimnames = list(V, V))
cells[upper.tri(cells, diag = TRUE)] <- ""

table3 <- data.frame(
    `#`      = paste0("[", seq_len(k), "]"),
    Variable = V,
    cells[, seq_len(k - 1), drop = FALSE],
    check.names = FALSE, row.names = NULL)
names(table3)[-(1:2)] <- paste0("[", seq_len(k - 1), "]")

print(table3, row.names = FALSE)
write.csv(table3, "outputs/table3_correlations.csv", row.names = FALSE)

off <- N[lower.tri(N)]
cat(sprintf("\nPairwise N ranges from %s to %s. Report the range in the note.\n",
            format(min(off, na.rm = TRUE), big.mark = ","),
            format(max(off, na.rm = TRUE), big.mark = ",")))

NOTE <- paste(
    "Notes: Pairwise correlation coefficients computed on the estimation sample,",
    sprintf("with pairwise deletion (N between %s and %s).",
            format(min(off, na.rm = TRUE), big.mark = ","),
            format(max(off, na.rm = TRUE), big.mark = ",")),
    "Significance: . p<0.1, * p<0.05, ** p<0.01, *** p<0.001. Monetary and",
    "population variables are log-transformed to maintain consistency with",
    "subsequent regression models, reduce right skewness, and limit the impact of",
    "extreme values.")
writeLines(NOTE, "outputs/table3_note.txt")
cat("\n--- note ---\n"); cat(NOTE, "\n")

# The published manuscript's own note already states "N between 28,086 and
# 30,791" - if the range above matches that exactly, it confirms the
# published Table 3 was project-level all along, and it was an earlier
# version of this script (defaulting to df1_cig2) that was wrong, not the
# manuscript's note.
published_range <- c(28086, 30791)
got_range <- c(min(off, na.rm = TRUE), max(off, na.rm = TRUE))
if (all(got_range == published_range)) {
    cat("\nThis matches the manuscript's own published N range exactly: Table 3 was\n")
    cat("already project-level, and this script now reproduces it correctly.\n")
} else {
    cat(sprintf("\nDoes not match the manuscript's published range (%s-%s) - revisit.\n",
                format(published_range[1], big.mark = ","),
                format(published_range[2], big.mark = ",")))
}
