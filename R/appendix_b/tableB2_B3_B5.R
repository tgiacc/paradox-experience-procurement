# =============================================================================
# APPENDIX TABLES B2, B3 AND B4, ON THE CORRECTED PROJECT-LEVEL SAMPLE
#
# Run AFTER R/02_analysis.R, R/03_prepare_extras.R and R/04_build_cio_subset.R.
#
# WHY THIS SCRIPT EXISTS
#   These three tables were built once, interactively, and never saved as a
#   file of their own; the numbers they produce are in the manuscript, but
#   until now the code that produces them was not in the repository. This
#   reconstructs it from that session, adapted to run in an isolated
#   environment like the rest of R/tables and R/robustness.
#
#   B2  Cox regressions with interactions, project level, no municipality
#       strata. Signs are reversed relative to the AFT tables: a positive Cox
#       coefficient is a HIGHER completion hazard, that is a SHORTER duration.
#   B3  Weibull AFT with interactions, direct awards only. At project level,
#       tipo_scelta_contraente is the procedure of the economically dominant
#       lot, so this subsample is not the previous one collapsed: it is
#       composed differently, and its N is not directly comparable to a
#       contract-level figure.
#   B4  Weibull AFT with municipal IT capacity controls (ANCI 2024 survey).
#       The survey covers a subset of municipalities, which is why this
#       table's N is far smaller than the others.
# =============================================================================

library(dplyr)
library(tibble)
library(survival)
library(sandwich)

dir.create("outputs", showWarnings = FALSE)

stopifnot(exists("d_award"), exists("d_exec"), exists("d_comp"),
          exists("AWARD_TIME"), exists("AWARD_STATUS"), exists("stars"),
          exists("cluster_vcov"))

if (!"codice_istat" %in% names(d_award)) {
  stop("codice_istat is not on the estimation frames: extend R/03_prepare_extras.R.")
}
# cio_subset is checked just before the one section that needs it (Table B5,
# below), not here - B2 and B3 have nothing to do with IT capacity and should
# not fail just because the restricted-access ANCI survey is not available.

PH <- list(
  Award      = list(t = AWARD_TIME,   s = AWARD_STATUS,       d = d_award),
  Execution  = list(t = "execution",  s = "execution_status", d = d_exec),
  Completion = list(t = "completion", s = "execution_status", d = d_comp)
)

RHS_INT <- paste(
  "factor(tipo_scelta_contraente) + factor(pop_cluster) + factor(regione) +",
  "factor(year) + log_importo_complessivo_gara + log_past_expenditure +",
  "DTO + weighted_payment_time + u35_pct + female_pct +",
  "log_digital_expenditure * type"
)

LAB <- c(
  "log_digital_expenditure"                                   = "Experience in the procurement of digital solutions (log)",
  "log_digital_expenditure:typeCitizen Experience"            = "x Citizen Experience",
  "log_digital_expenditure:typeCloud"                         = "x Cloud",
  "log_digital_expenditure:typeDigital Notices"               = "x Digital Notices",
  "log_digital_expenditure:typeDigital Services and Payments" = "x Digital Services and Payments",
  "log_digital_expenditure:typeInteroperability"              = "x Interoperability",
  "log_past_expenditure"                                      = "General procurement experience (log)",
  "log_importo_complessivo_gara"                              = "Contract award value (log)",
  "DTO"                                                       = "DTO",
  "weighted_payment_time"                                     = "Supplier payment time",
  "u35_pct"                                                   = "Employees 25-34",
  "female_pct"                                                = "Female employees",
  "log_it_staff"                                              = "log(IT staff)"
)

collect <- function(models, datasets, se_fun, extra_lab = NULL) {
  lab <- if (is.null(extra_lab)) LAB else c(LAB, extra_lab)
  out <- NULL
  for (nm in names(models)) {
    mod <- models[[nm]]
    cf  <- coef(mod)
    se  <- se_fun(mod, datasets[[nm]])
    p   <- 2 * pnorm(-abs(cf / se))
    tb  <- tibble(term = names(cf),
                  cell = sprintf("%.3f%s\n(%.3f)", cf, stars(p), se))
    names(tb)[2] <- nm
    out <- if (is.null(out)) tb else full_join(out, tb, by = "term")
  }
  out %>% filter(term %in% names(lab)) %>%
    mutate(Variable = lab[term]) %>%
    slice(match(lab, Variable)) %>% filter(!is.na(Variable)) %>%
    select(Variable, everything(), -term)
}

se_aft <- function(mod, dat) sqrt(diag(cluster_vcov(mod, dat)))
se_cox <- function(mod, dat) sqrt(diag(vcov(mod)))   # coxph keeps dimnames natively

nrow_row <- function(models) {
  tibble(Variable   = "Observations",
         Award      = format(length(models$Award$linear.predictors), big.mark = ","),
         Execution  = format(length(models$Execution$linear.predictors), big.mark = ","),
         Completion = format(length(models$Completion$linear.predictors), big.mark = ","))
}

# =============================================================================
# B2 - Cox regressions with interactions
# =============================================================================
cat("=== Table B2 (Cox, interactions, project level) ===\n\n")

cox <- lapply(names(PH), function(nm) {
  p <- PH[[nm]]
  coxph(as.formula(sprintf("Surv(%s, %s) ~ %s", p$t, p$s, RHS_INT)), data = p$d)
})
names(cox) <- names(PH)

B2 <- collect(cox, lapply(PH, `[[`, "d"), se_cox)
B2 <- bind_rows(B2, tibble(
  Variable   = c("Observations", "Log-likelihood"),
  Award      = c(format(cox$Award$n, big.mark = ","),      sprintf("%.1f", cox$Award$loglik[2])),
  Execution  = c(format(cox$Execution$n, big.mark = ","),  sprintf("%.1f", cox$Execution$loglik[2])),
  Completion = c(format(cox$Completion$n, big.mark = ","), sprintf("%.1f", cox$Completion$loglik[2]))
))
write.csv(B2, "outputs/tableB2_cox.csv", row.names = FALSE)
print(as.data.frame(B2), row.names = FALSE)

# =============================================================================
# B3 - Direct awards only
# =============================================================================
cat("\n\n=== Table B3 (direct awards only) ===\n\n")

direct <- function(dat) {
  dat[dat$tipo_scelta_contraente == "AFFIDAMENTO DIRETTO" & !is.na(dat$codice_istat), ]
}
RHS_B3 <- sub("factor\\(tipo_scelta_contraente\\) \\+ ", "", RHS_INT)

b3_data <- lapply(PH, function(p) direct(p$d))
cat(sprintf("Direct-award subsamples: %s\n",
            paste(sprintf("%s %d", names(b3_data), sapply(b3_data, nrow)), collapse = " | ")))
if (any(sapply(b3_data, nrow) == 0)) {
  cat("\nProcedures present in the award frame:\n")
  print(sort(table(d_award$tipo_scelta_contraente), decreasing = TRUE)[1:10])
  stop("Direct-award filter returned no rows; check the label above.")
}

b3 <- lapply(names(PH), function(nm) {
  p <- PH[[nm]]
  survreg(as.formula(sprintf("Surv(%s, %s) ~ %s", p$t, p$s, RHS_B3)),
          data = b3_data[[nm]], dist = "weibull")
})
names(b3) <- names(PH)

B3 <- bind_rows(collect(b3, b3_data, se_aft), nrow_row(b3))
write.csv(B3, "outputs/tableB3_direct.csv", row.names = FALSE)
print(as.data.frame(B3), row.names = FALSE)

# =============================================================================
# B5 - Municipal IT capacity controls
# =============================================================================
if (!exists("cio_subset")) {
  cat("\n\n=== Table B5 (IT capacity) skipped ===\n")
  cat("cio_subset is not in the environment - the 2024 ANCI survey behind it is\n")
  cat("restricted-access and is not part of this repository (see DATA.md). Run\n")
  cat("R/appendix_b/04_build_cio_subset.R first if you have that file.\n")
  cat("Table B2 and B3 above do not depend on it and were built normally.\n")
} else {

cat("\n\n=== Table B5 (IT capacity) ===\n\n")

ph4 <- lapply(PH, function(p) {
  dd <- merge(p$d, cio_subset, by = "codice_ipa", all = FALSE)
  dd$type <- relevel(factor(dd$type), ref = REF_TYPE)
  dd
})
cat(sprintf("CIO-matched subsamples: %s\n",
            paste(sprintf("%s %d", names(ph4), sapply(ph4, nrow)), collapse = " | ")))

RHS_B5 <- paste(RHS_INT, "+ log_it_staff +",
                "factor(cio_responsabile_dedicato) + factor(cio_profilo_prevalente)")

b5 <- lapply(names(PH), function(nm) {
  p <- PH[[nm]]
  survreg(as.formula(sprintf("Surv(%s, %s) ~ %s", p$t, p$s, RHS_B5)),
          data = ph4[[nm]], dist = "weibull")
})
names(b5) <- names(PH)

# CIO categorical levels are labelled from the estimated coefficients, not
# assumed, so a change in how the survey responses are spelled cannot silently
# blank these rows.
cio_terms <- grep("^factor\\(cio_", names(coef(b5$Award)), value = TRUE)
cio_lab <- setNames(
  sub("^factor\\(cio_responsabile_dedicato\\)", "CIO dedicated: ",
      sub("^factor\\(cio_profilo_prevalente\\)", "CIO profile: ", cio_terms)),
  cio_terms)

B5 <- bind_rows(collect(b5, ph4, se_aft, extra_lab = cio_lab), nrow_row(b5))
write.csv(B5, "outputs/tableB5_itcapacity.csv", row.names = FALSE)
print(as.data.frame(B5), row.names = FALSE)

}

cat("\nDone. Files in outputs/: tableB2_cox.csv, tableB3_direct.csv, and\n")
cat("tableB5_itcapacity.csv if the CIO survey was available.\n")
