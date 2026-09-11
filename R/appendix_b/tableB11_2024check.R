# =============================================================================
# TABLE B11 - ROBUSTNESS TO 2024-ADMITTED PROJECTS
#
# Run AFTER giq_r2_analysis.R (needs df_cup, REF_TYPE, AWARD_TIME,
# AWARD_STATUS, cluster_vcov - the last defined in that script and reused
# here so standard errors match Table 6's own clustering, not a model-based
# SE that would understate uncertainty relative to it).
#
# WHY THIS EXISTS
#   A filter on the funding decree's text label (see DATA.md, "Which
#   projects are admitted") does not fully restrict the frame to 2022-2023
#   admissions: `year`, derived from the admission date rather than the
#   decree label, still shows "2024" for some projects. This script checks
#   whether that matters: re-fitting the main models with those projects
#   excluded and comparing every interaction coefficient's sign and
#   significance against the full sample.
# =============================================================================

library(dplyr)
library(survival)
library(sandwich)
library(tidyr)

dir.create("outputs", showWarnings = FALSE)

stopifnot(exists("df_cup"), exists("REF_TYPE"), exists("AWARD_TIME"),
          exists("AWARD_STATUS"), exists("cluster_vcov"))

df_cup$admitted_2024 <- as.integer(df_cup$year == "2024")

fit_check <- function(dat, outcome, status, exclude_2024) {
    d <- dat[!is.na(dat[[outcome]]) & dat[[outcome]] > 0 &
                 is.finite(dat$log_digital_expenditure) & dat$log_digital_expenditure >= 0 &
                 is.finite(dat$log_past_expenditure) & dat$log_past_expenditure >= 0 &
                 is.finite(dat$log_importo_complessivo_gara) & dat$log_importo_complessivo_gara >= 0, ]
    if (exclude_2024) d <- d[d$admitted_2024 == 0, ]
    d$type <- relevel(factor(d$type), ref = REF_TYPE)
    m <- survreg(as.formula(sprintf(
        "Surv(%s, %s) ~ factor(tipo_scelta_contraente) + factor(pop_cluster) + factor(regione) + factor(year) + log_importo_complessivo_gara + log_past_expenditure + DTO + weighted_payment_time + u35_pct + female_pct + log_digital_expenditure * type",
        outcome, status)), data = d, dist = "weibull")
    # Clustered at the municipality level, matching Table 6's own models
    # (cluster_vcov(), defined in 02_analysis.R) - not the model-based SE
    # that summary(m)$table would give. Naive SE understate uncertainty
    # relative to Table 6 whenever observations cluster within municipality,
    # which changes which cells cross a significance threshold even when
    # the point estimate itself matches exactly.
    cf <- coef(m); se_all <- sqrt(diag(cluster_vcov(m, d)))
    keep <- grep("log_digital_expenditure:type", names(cf), value = TRUE)
    est <- cf[keep]; se <- se_all[keep]
    z <- est / se; p <- 2 * pnorm(-abs(z))
    data.frame(type = sub("^log_digital_expenditure:type", "", keep),
               est = round(as.numeric(est), 4),
               se = round(as.numeric(se), 4),
               p = as.numeric(p),
               n = length(m$linear.predictors),  # rows actually used by the
                                                  # fit; nrow(d) overcounts
                                                  # rows dropped by missing
                                                  # covariates
               stringsAsFactors = FALSE)
}

phases <- list(Award = c(AWARD_TIME, AWARD_STATUS),
               Execution = c("execution", "execution_status"),
               Completion = c("completion", "execution_status"))

check_2024 <- bind_rows(lapply(names(phases), function(ph) {
    outcome <- phases[[ph]][1]; status <- phases[[ph]][2]
    bind_rows(
        fit_check(df_cup, outcome, status, FALSE) %>% mutate(phase = ph, sample = "Full (2022-24)"),
        fit_check(df_cup, outcome, status, TRUE)  %>% mutate(phase = ph, sample = "Excl. 2024 admissions")
    )
}))

wide <- check_2024 %>%
    mutate(stars = case_when(p < 0.001 ~ "***", p < 0.01 ~ "**", p < 0.05 ~ "*",
                             p < 0.1 ~ ".", TRUE ~ "")) %>%
    mutate(cell = sprintf("%.4f%s (%.4f)", est, stars, se)) %>%
    select(phase, type, sample, cell) %>%
    tidyr::pivot_wider(names_from = sample, values_from = cell)

cat("=== Table B11: coefficients, full sample vs excluding 2024 admissions ===\n\n")
print(as.data.frame(wide), row.names = FALSE)

counts <- check_2024 %>% distinct(phase, sample, n)
cat("\n=== Observations ===\n\n")
print(as.data.frame(counts), row.names = FALSE)

sign_flip <- check_2024 %>%
    select(phase, type, sample, est) %>%
    tidyr::pivot_wider(names_from = sample, values_from = est) %>%
    mutate(sign_flip = sign(.data[["Full (2022-24)"]]) != sign(.data[["Excl. 2024 admissions"]]))
cat(sprintf("\nSign flips: %d of %d cells\n", sum(sign_flip$sign_flip), nrow(sign_flip)))

write.csv(check_2024, "outputs/tableB11_2024_admissions.csv", row.names = FALSE)
