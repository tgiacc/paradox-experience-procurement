# =============================================================================
# TABLE A3 / A4 - DESCRIPTIVE STATISTICS, COMPLETED AND ONGOING SUBSAMPLES
#
# Run AFTER giq_r2_analysis.R (needs df_cup). Splits by completion status,
# then computes every variable's N/mean/SD/min/max/skew at the project level.
#
# Both subsamples are drawn from the same restricted frame as Table 2 (the
# 30,791 projects with log_digital_expenditure, log_past_expenditure and
# log_importo_complessivo_gara all >= 0), not from the unrestricted 40,991.
# The two N's below sum to Table 2's own N, not to 40,991.
#
# Population, general procurement experience and experience in the
# procurement of digital solutions are reported in log scale, matching
# Table 2 - not raw population counts or raw euro amounts.
# =============================================================================

library(dplyr)
library(moments)   # for skewness(); install.packages("moments") if needed

STATUS_VAR <- "execution_status"   # completion's own censoring indicator

stopifnot(exists("df_cup"))
if (!STATUS_VAR %in% names(df_cup)) {
    cat(sprintf("'%s' is not a column here. Columns available:\n", STATUS_VAR))
    print(grep("status", names(df_cup), value = TRUE, ignore.case = TRUE))
    stop("Set STATUS_VAR to the correct column name above and re-run.")
}

completed <- df_cup %>%
    filter(completion > 0, .data[[STATUS_VAR]] == 1,
           is.finite(log_digital_expenditure), log_digital_expenditure >= 0,
           is.finite(log_past_expenditure), log_past_expenditure >= 0,
           is.finite(log_importo_complessivo_gara), log_importo_complessivo_gara >= 0)
ongoing   <- df_cup %>%
    filter(completion > 0, .data[[STATUS_VAR]] == 0,
           is.finite(log_digital_expenditure), log_digital_expenditure >= 0,
           is.finite(log_past_expenditure), log_past_expenditure >= 0,
           is.finite(log_importo_complessivo_gara), log_importo_complessivo_gara >= 0)

cat(sprintf("Completed subsample: %d projects | Ongoing subsample: %d projects\n",
            nrow(completed), nrow(ongoing)))

VARS <- c(
    "DTO"                     = "DTO",
    "weighted_payment_time"   = "Supplier payment time",
    "u35_pct"                 = "Employees 25-34 (share)",
    "female_pct"              = "Female employees (share)",
    "log_population"          = "Population (log)",
    "importo_gara_tot"        = "Contract award value",
    "log_past_expenditure"    = "General procurement experience (log)",
    "log_digital_expenditure" = "Experience in procurement of digital solutions (log)"
)

describe_one <- function(df, col, label) {
    x <- df[[col]]
    x <- x[is.finite(x)]   # excludes NA AND -Inf/Inf - is.na() alone would
                           # miss the handful of -Inf values found in
                           # log_digital_expenditure for 5 municipalities
                           # (root cause not yet resolved - see project notes)
    tibble(Variable = label, N = length(x), Mean = mean(x), SD = sd(x),
           Min = min(x), Max = max(x), Skew = moments::skewness(x))
}

build_table <- function(df, label) {
    rows <- list()
    for (col in names(VARS)) {
        if (!col %in% names(df)) {
            cat(sprintf("  [%s] column '%s' not found - skipped\n", label, col))
            next
        }
        rows[[length(rows) + 1]] <- describe_one(df, col, VARS[[col]])
    }
    bind_rows(rows)
}

tableA3 <- build_table(completed, "completed")
tableA4 <- build_table(ongoing, "ongoing")

cat("\n=== Table A3 (completed subsample) ===\n\n")
print(as.data.frame(tableA3), row.names = FALSE)
cat("\n=== Table A4 (ongoing subsample) ===\n\n")
print(as.data.frame(tableA4), row.names = FALSE)

dir.create("outputs", showWarnings = FALSE)
write.csv(tableA3, "outputs/tableA3_completed.csv", row.names = FALSE)
write.csv(tableA4, "outputs/tableA4_ongoing.csv", row.names = FALSE)

cat("\n--- check: completed + ongoing should sum to Table 2's N (30,791) ---\n")
n_sum <- nrow(completed) + nrow(ongoing)
if (n_sum == 30791) {
    cat(sprintf("  %d + %d = %d  OK\n", nrow(completed), nrow(ongoing), n_sum))
} else {
    cat(sprintf("  %d + %d = %d  MISMATCH (expected 30,791)\n",
                nrow(completed), nrow(ongoing), n_sum))
}
