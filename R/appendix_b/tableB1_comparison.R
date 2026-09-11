# =============================================================================
# TABLE B1 - MODEL PERFORMANCE, COX VS. WEIBULL, WITH INTERACTIONS
#
# Run AFTER R/02_analysis.R (needs d_award, d_exec, d_comp, AWARD_TIME,
# AWARD_STATUS - the same project-level frames Table 4/6 and Table B2 use).
#
# Refits both model types here rather than reusing objects from elsewhere,
# so this script is self-contained and not dependent on what an earlier
# script happened to leave in globalenv().
#
# Cox's partial likelihood and Weibull's full likelihood are constructed
# differently and are not directly comparable in magnitude; within each
# model family, lower AIC indicates better fit. Both are reported to show
# the interaction pattern holds under a different parametric assumption,
# not to rank the two specifications against each other.
# =============================================================================

library(dplyr)
library(survival)

stopifnot(exists("d_award"), exists("d_exec"), exists("d_comp"),
          exists("AWARD_TIME"), exists("AWARD_STATUS"))

RHS <- paste(
  "factor(tipo_scelta_contraente) + factor(pop_cluster) + factor(regione) +",
  "factor(year) + log_importo_complessivo_gara + log_past_expenditure +",
  "DTO + weighted_payment_time + u35_pct + female_pct +",
  "log_digital_expenditure * type"
)

PH <- list(
  Award      = list(t = AWARD_TIME,   s = AWARD_STATUS,       d = d_award),
  Execution  = list(t = "execution",  s = "execution_status", d = d_exec),
  Completion = list(t = "completion", s = "execution_status", d = d_comp)
)

fit_pair <- function(p) {
  f_cox <- as.formula(sprintf("Surv(%s, %s) ~ %s", p$t, p$s, RHS))
  f_aft <- f_cox  # identical RHS; only the model call differs
  m_cox <- coxph(f_cox, data = p$d)
  m_aft <- survreg(f_aft, data = p$d, dist = "weibull")
  data.frame(
    ll_cox  = as.numeric(logLik(m_cox)),
    ll_aft  = as.numeric(logLik(m_aft)),
    aic_cox = AIC(m_cox),
    aic_aft = AIC(m_aft)
  )
}

results <- bind_rows(lapply(names(PH), function(ph) {
  cbind(Model = ph, fit_pair(PH[[ph]]))
}))

cat("=== Table B1: Cox vs. Weibull, log-likelihood and AIC ===\n\n")
print(results %>%
        mutate(across(starts_with("ll_")|starts_with("aic_"), ~ round(.x, 1))),
      row.names = FALSE)

dir.create("outputs", showWarnings = FALSE)
write.csv(results, "outputs/tableB1_cox_vs_weibull.csv", row.names = FALSE)

cat("\n--- check against the published values ---\n")
published <- data.frame(
  Model = c("Award", "Execution", "Completion"),
  ll_cox  = c(-254556.5, -176058.1, -180358.2),
  ll_aft  = c(-170610.4, -127946.6, -135167.0)
)
chk <- results %>% select(Model, ll_cox, ll_aft) %>%
  left_join(published, by = "Model", suffix = c("_got", "_pub"))
for (i in seq_len(nrow(chk))) {
  ok_cox <- abs(chk$ll_cox_got[i] - chk$ll_cox_pub[i]) < 1
  ok_aft <- abs(chk$ll_aft_got[i] - chk$ll_aft_pub[i]) < 1
  cat(sprintf("  %-11s Cox: %s | Weibull: %s\n", chk$Model[i],
              if (ok_cox) "OK" else sprintf("MISMATCH (got %.1f, published %.1f)", chk$ll_cox_got[i], chk$ll_cox_pub[i]),
              if (ok_aft) "OK" else sprintf("MISMATCH (got %.1f, published %.1f)", chk$ll_aft_got[i], chk$ll_aft_pub[i])))
}
