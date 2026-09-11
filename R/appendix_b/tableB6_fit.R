# =============================================================================
# TABLE B6 - FIT THE SIX COX MODELS (no-strata and stratified, three phases)
#
# Run AFTER giq_r2_analysis.R, in the SAME session (needs d_award, d_exec,
# d_comp, AWARD_TIME, AWARD_STATUS, REF_TYPE). Feeds tableB5_significance.R,
# which was written assuming these six model objects already exist but never
# had a script that actually produced them - this is that script.
#
# The stratified specification keeps factor(tipo_scelta_contraente),
# factor(year) and log_importo_complessivo_gara alongside type and the
# interaction. These are PROJECT attributes, not municipality-constant ones,
# so municipality strata do not absorb them; dropping them (an earlier,
# incorrect version of this script did) changes the estimates rather than
# merely reducing them to what stratification alone identifies. This
# specification is the one already used independently in
# R/robustness/selection_into_types.R's breadth_fit() (its min_types = 2
# case), and the two now agree to 3 decimals on the completion phase - see
# the reconciliation check in tableB5_significance.R.
# =============================================================================

library(dplyr)
library(survival)

stopifnot(exists("d_award"), exists("d_exec"), exists("d_comp"),
          exists("AWARD_TIME"), exists("AWARD_STATUS"), exists("REF_TYPE"))

prep_type <- function(d) {
    d <- d[!is.na(d$type), ]
    d$type <- relevel(factor(d$type), ref = REF_TYPE)
    d
}
d_award <- prep_type(d_award); d_exec <- prep_type(d_exec); d_comp <- prep_type(d_comp)

RHS_NOFE <- paste(
    "factor(tipo_scelta_contraente) + factor(pop_cluster) + factor(regione) +",
    "factor(year) + log_importo_complessivo_gara + log_past_expenditure +",
    "DTO + weighted_payment_time + u35_pct + female_pct +",
    "log_digital_expenditure * type"
)
# stratification absorbs municipality-constant terms only: population band,
# region, DTO, payment time, workforce composition, general procurement
# experience, and the un-interacted log_digital_expenditure main effect (no
# within-stratum variation either way, since it too is municipality-constant).
# Procedure, year and contract value are project attributes and stay in.
RHS_FE <- paste("factor(tipo_scelta_contraente) + factor(year) +",
                "log_importo_complessivo_gara + type +",
                "log_digital_expenditure:type + strata(codice_ipa)")

fit_cox <- function(time, status, dat, rhs) {
    coxph(as.formula(sprintf("Surv(%s, %s) ~ %s", time, status, rhs)),
          data = dat, cluster = codice_ipa, model = TRUE)
}

cat("Fitting 'no strata' models (full covariate set, matches Table B2)...\n")
m_award_nofe <- fit_cox(AWARD_TIME,  AWARD_STATUS,       d_award, RHS_NOFE)
m_exec_nofe  <- fit_cox("execution", "execution_status", d_exec,  RHS_NOFE)
m_comp_nofe  <- fit_cox("completion","execution_status", d_comp,  RHS_NOFE)

cat("Fitting 'stratified' models (municipality strata, project attributes kept)...\n")
m_award_fe <- fit_cox(AWARD_TIME,  AWARD_STATUS,       d_award, RHS_FE)
m_exec_fe  <- fit_cox("execution", "execution_status", d_exec,  RHS_FE)
m_comp_fe  <- fit_cox("completion","execution_status", d_comp,  RHS_FE)

# run_all.R sources this script in its own isolated environment, so nothing
# created above is visible afterward unless assigned to globalenv() here -
# tableB6_significance.R needs to find these six model objects when it runs
# next, along with the exact d_award/d_exec/d_comp used to fit them (post
# prep_type() filtering), not whatever those names happen to hold elsewhere
# in the session.
for (nm in c("m_award_nofe", "m_exec_nofe", "m_comp_nofe",
            "m_award_fe", "m_exec_fe", "m_comp_fe",
            "d_award", "d_exec", "d_comp")) {
    assign(nm, get(nm), envir = globalenv())
}

cat("\nAll six models are now in the environment: m_award_nofe, m_award_fe,\n")
cat("m_exec_nofe, m_exec_fe, m_comp_nofe, m_comp_fe.\n")
cat("Next: R/tables/tableB5_significance.R\n")

# --- sanity check: does the stratified sample match what the table's note --
# already reports (municipalities with more than one solution type)?
count_multi_type <- function(dat) {
    keep <- dat %>% group_by(codice_ipa) %>% filter(n_distinct(type) > 1) %>% ungroup()
    tibble(projects = nrow(keep), municipalities = n_distinct(keep$codice_ipa))
}
cat("\nMunicipalities with more than one solution type:\n")
print(bind_rows(Award = count_multi_type(d_award), Execution = count_multi_type(d_exec),
                Completion = count_multi_type(d_comp), .id = "phase"))
