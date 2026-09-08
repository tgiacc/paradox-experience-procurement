# =============================================================================
# TABLE 5 - MODEL COMPARISON: BASELINE VS. INTERACTION, WITH LR TESTS
#
# Run AFTER giq_r2_analysis.R (needs d_award, d_exec, d_comp, AWARD_TIME,
# AWARD_STATUS, m_award/m_exec/m_comp - the fitted interaction models from
# Part 3 of that script).
#
# WHY THIS SCRIPT EXISTS
#   Found missing during a completeness audit: the README claimed a script
#   produced Table 5, and none did. The interaction models (m_award, m_exec,
#   m_comp) already exist from giq_r2_analysis.R Part 3; this script adds the
#   BASELINE (no-interaction) models and the paired LR test, which had never
#   been written into a file.
#
# WHAT THE TEST IS
#   Both models are full maximum likelihood on identical rows (same filters,
#   same phase), so the likelihood-ratio test is valid: the baseline model is
#   nested within the interaction model (interaction terms set to zero
#   recovers it exactly). LR = 2*(logLik_interaction - logLik_baseline),
#   df = difference in the number of estimated parameters, both read directly
#   from the fitted objects rather than counted by hand.
# =============================================================================

library(dplyr)
library(survival)

dir.create("outputs", showWarnings = FALSE)
stopifnot(exists("d_award"), exists("d_exec"), exists("d_comp"),
          exists("AWARD_TIME"), exists("AWARD_STATUS"),
          exists("m_award"), exists("m_exec"), exists("m_comp"))

BASE_RHS <- paste(
  "factor(tipo_scelta_contraente) + factor(pop_cluster) + factor(regione) +",
  "factor(year) + log_importo_complessivo_gara + log_past_expenditure +",
  "DTO + weighted_payment_time + u35_pct + female_pct + log_digital_expenditure + type"
)
# NOTE: this must match the interaction model's RHS with ONLY the interaction
# term removed, keeping BOTH main effects (log_digital_expenditure and type).
# "log_digital_expenditure * type" in R's formula syntax expands to
# "log_digital_expenditure + type + log_digital_expenditure:type" - dropping
# "+ type" here as well would make the LR test compare the wrong two models.

fit_baseline <- function(time, status, dat) {
  survreg(as.formula(sprintf("Surv(%s, %s) ~ %s", time, status, BASE_RHS)),
          data = dat, dist = "weibull")
}

m_award_base <- fit_baseline(AWARD_TIME,  AWARD_STATUS,      d_award)
m_exec_base  <- fit_baseline("execution", "execution_status", d_exec)
m_comp_base  <- fit_baseline("completion","execution_status", d_comp)

compare <- function(base, full, phase) {
  ll0 <- as.numeric(logLik(base)); ll1 <- as.numeric(logLik(full))
  k0 <- attr(logLik(base), "df"); k1 <- attr(logLik(full), "df")
  lr <- 2 * (ll1 - ll0); df <- k1 - k0
  data.frame(Phase = phase,
             `LL without` = round(ll0, 1), `LL with` = round(ll1, 1),
             `AIC without` = round(-2 * ll0 + 2 * k0, 1),
             `AIC with` = round(-2 * ll1 + 2 * k1, 1),
             `LR test` = round(lr, 2), df = df,
             `p-value` = format.pval(pchisq(lr, df, lower.tail = FALSE), digits = 3, eps = 1e-16),
             check.names = FALSE)
}

table5 <- bind_rows(
  compare(m_award_base, m_award, "Award"),
  compare(m_exec_base,  m_exec,  "Execution"),
  compare(m_comp_base,  m_comp,  "Completion")
)

cat("=== Table 5: model comparison, without vs. with interaction ===\n\n")
print(table5, row.names = FALSE)
write.csv(table5, "outputs/table5_comparison.csv", row.names = FALSE)

cat("\n--- check against the published LL figures ---\n")
published_with <- c(Award = -170610.4, Execution = -127946.6, Completion = -135167.0)
for (ph in names(published_with)) {
  got <- table5$`LL with`[table5$Phase == ph]
  ok <- abs(got - published_with[[ph]]) < 1
  cat(sprintf("  %-11s script: %s | published: %s  %s\n",
              ph, got, published_with[[ph]], if (ok) "OK" else "MISMATCH"))
}
