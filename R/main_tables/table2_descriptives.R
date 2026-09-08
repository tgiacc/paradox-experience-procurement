# =============================================================================
# TABLE 2 - DESCRIPTIVE STATISTICS FOR THE MAIN COVARIATES
#
# Run AFTER giq_r2_analysis.R (needs df_cup, AWARD_TIME, AWARD_STATUS).
#
# WHY THIS SCRIPT EXISTS
#   Found missing during a completeness audit of this repository: the README
#   claimed a script produced Table 2, and none did. This is a reconstruction,
#   not a copy of a verified console session (unlike the Table B2/B3/B5 script,
#   which came from a transcript). The published N's are 30,791 (Award),
#   29,043 (Execution), 30,791 (Completion); the check at the end of this
#   script compares against them and says so if they do not match, rather than
#   silently reporting a different sample.
#
# SAMPLE DEFINITION USED HERE (fixed twice now - see both notes below)
#   Every row of Table 2 is computed on the SAME estimation frame - the
#   project-level sample restricted to log_digital_expenditure >= 0,
#   log_past_expenditure >= 0 and log_importo_complessivo_gara >= 0, the same
#   filters used throughout this project's other scripts - not on ad hoc
#   per-variable slices of df_cup. A first version computed each block on its
#   own subset, which both inflated the Duration N's and diluted the Contract
#   award value mean with zero-value, unmatched-contract projects.
#
#   A second version then over-corrected: it additionally restricted Award and
#   Completion to the observed (event = 1) subsample, on the assumption that
#   descriptive statistics for a duration are only informative where the
#   duration is actually known. The published N's contradict that assumption
#   directly - Award and Completion are BOTH 30,791, equal to the full frame,
#   with no status filtering at all; only Execution is smaller (29,043), and
#   for an unrelated reason (4,865 projects have no execution timestamp at
#   all, a genuine missing-value issue, not a censoring one). Durations here
#   are therefore taken directly off the frame with no status filter.
# =============================================================================

library(dplyr)
library(tibble)

dir.create("outputs", showWarnings = FALSE)
stopifnot(exists("df_cup"), exists("AWARD_TIME"), exists("AWARD_STATUS"))

frame <- df_cup %>%
  filter(is.finite(log_digital_expenditure), log_digital_expenditure >= 0,
         is.finite(log_past_expenditure),    log_past_expenditure    >= 0,
         is.finite(log_importo_complessivo_gara),
         log_importo_complessivo_gara >= 0)

cat(sprintf("Estimation frame: %s rows (from %s in df_cup)\n",
            format(nrow(frame), big.mark = ","), format(nrow(df_cup), big.mark = ",")))

descr <- function(x) {
  x <- x[is.finite(x)]
  c(N = length(x), Mean = mean(x), SD = sd(x), Min = min(x), Max = max(x),
    Skew = mean((x - mean(x))^3) / sd(x)^3)
}

VARS <- list(
  "Duration measures" = list(
    Award      = frame[[AWARD_TIME]],
    Execution  = frame$execution,
    Completion = frame$completion
  ),
  "Project characteristics" = list(
    # Confirmed NOT to need scaling: the published Table 2 mean (28,456.5) and
    # max (9,505,029.8) are raw euros, not thousands - the ratio against this
    # script's earlier /1000 output was 999.88 and 1000.00 respectively,
    # which settles it. Table 2's own note calling this column "thousands of
    # euros" is the actual bug; Table A3/A4 show the same raw-euro numbers
    # under the same wrong label and need the same fix in the manuscript.
    "Contract award value" = frame$importo_gara_tot,
    "Contracts per project" = frame$n_cig
  ),
  "Municipality characteristics" = list(
    "Digital transformation office" = frame$DTO,
    "Supplier payment time"         = frame$weighted_payment_time,
    "Employees 25-34 (share)"       = frame$u35_pct,
    "Female employees (share)"      = frame$female_pct
  ),
  "Experience measures" = list(
    "General procurement experience (log)"                = frame$log_past_expenditure,
    "Experience in procurement of digital solutions (log)" = frame$log_digital_expenditure
  )
)

rows <- list()
for (group in names(VARS)) {
  rows[[length(rows) + 1]] <- tibble(Variable = group, N = NA, Mean = NA, SD = NA,
                                     Min = NA, Max = NA, Skew = NA, group = TRUE)
  for (nm in names(VARS[[group]])) {
    d <- descr(VARS[[group]][[nm]])
    rows[[length(rows) + 1]] <- tibble(Variable = nm, N = d["N"], Mean = d["Mean"],
                                       SD = d["SD"], Min = d["Min"], Max = d["Max"],
                                       Skew = d["Skew"], group = FALSE)
  }
}
table2 <- bind_rows(rows) %>%
  mutate(across(c(Mean, SD, Min, Max, Skew), ~ round(.x, 2)))

cat("=== Table 2: descriptive statistics ===\n\n")
print(as.data.frame(table2 %>% select(-group)), row.names = FALSE)
write.csv(table2 %>% select(-group), "outputs/table2_descriptives.csv", row.names = FALSE)

cat("\n--- check against the published N's ---\n")
published <- c(Award = 30791, Execution = 29043, Completion = 30791)
got <- table2$N[table2$Variable %in% names(published)]
names(got) <- table2$Variable[table2$Variable %in% names(published)]
for (nm in names(published)) {
  ok <- got[[nm]] == published[[nm]]
  cat(sprintf("  %-11s script: %s | published: %s  %s\n",
              nm, format(got[[nm]], big.mark = ","), format(published[[nm]], big.mark = ","),
              if (ok) "OK" else "MISMATCH - revisit the sample definition above"))
}

cat("\n--- check against the published mean (raw euros, not thousands) ---\n")
# Settled by the ratio check, not assumed: the published mean/max (28,456.5 /
# 9,505,029.8) divided by this script's original /1000 output (28.46 /
# 9,505.03) gave 999.88 and 1000.00 - so importo_gara_tot is raw euros, and
# Table 2's own "(thousands of euros)" note is the bug, not this script.
published_mean <- 28456.5
got_mean <- table2$Mean[table2$Variable == "Contract award value"]
ok <- abs(got_mean - published_mean) < 1
cat(sprintf("  Contract award value  script: %s | published: %s  %s\n",
            format(got_mean, big.mark = ","), format(published_mean, big.mark = ","),
            if (ok) "OK"
            else "MISMATCH - revisit the sample or the raw-euro assumption"))
