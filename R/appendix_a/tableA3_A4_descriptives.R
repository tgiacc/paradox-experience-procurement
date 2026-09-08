# =============================================================================
# TABLE A3 / A4 - DESCRIPTIVE STATISTICS, COMPLETED AND ONGOING SUBSAMPLES
#
# Run AFTER giq_r2_analysis.R (needs df_cup). Splits by completion status,
# then computes every variable's N/mean/SD/min/max/skew at the PROJECT level
# throughout - matching Table 2's own convention (N=30,791 there for DTO,
# experience, and workforce composition alike, not deduplicated to
# municipalities). No variable is deduplicated here either, regardless of
# whether it is constant within municipality, so that A3/A4 report on the
# same basis as Table 2 rather than introducing a second convention.
#
# Population, Past proc. and Past digital are reported in log scale, matching
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

completed <- df_cup %>% filter(completion > 0, .data[[STATUS_VAR]] == 1)
ongoing   <- df_cup %>% filter(completion > 0, .data[[STATUS_VAR]] == 0)

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
