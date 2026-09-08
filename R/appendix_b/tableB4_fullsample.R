# =============================================================================
# TABLE B4 - FULL PROJECT FRAME, LUMP-SUM FUNDING AS THE SIZE MEASURE
#
# Run AFTER giq_r2_analysis.R (needs df_cup, RHS, prep(), stars(), TYPES,
# REF_TYPE, AWARD_TIME, AWARD_STATUS, m_award/m_exec/m_comp).
#
# WHY
#   The main models require a positive contract value, which drops 10,132 of
#   40,991 projects (24.7%) - those without a retrievable ANAC contract
#   record. This tests whether the interaction pattern survives when that
#   quarter of the sample is restored, using the program's lump-sum funding
#   amount as a coarser but universally available proxy for project scale.
#   Contract value remains the primary size measure throughout the paper
#   because it is a direct, project-specific observation, whereas the
#   funding amount is fixed by demographic band and solution type.
#
# INFERENCE
#   Standard errors clustered at the municipality level, matching the main
#   specification.
# =============================================================================

library(dplyr)
library(tibble)
library(survival)
library(sandwich)

dir.create("outputs", showWarnings = FALSE)

stopifnot(exists("df_cup"), exists("m_award"), exists("m_exec"), exists("m_comp"),
          exists("AWARD_TIME"), exists("AWARD_STATUS"), exists("REF_TYPE"),
          exists("TYPES"), exists("stars"))

# --- Build the full sample ---------------------------------------------------
df_full <- df_cup %>%
    mutate(
        log_funding = log(importo_finanziamento + 1),
        no_contract = as.integer(importo_gara_tot <= 0)
    )

cat(sprintf("\nFull sample: %d projects (%d without a matched contract, %.1f%%)\n",
            nrow(df_full), sum(df_full$no_contract),
            100 * mean(df_full$no_contract)))

prep_full <- function(d) d %>% mutate(type = relevel(factor(type), ref = REF_TYPE))

filt_full <- function(d, outcome) {
    d <- d[!is.na(d[[outcome]]) & d[[outcome]] > 0 &
               d$log_past_expenditure    >= 0 &
               d$log_digital_expenditure >= 0, ]
    d$tipo_scelta_contraente <- ifelse(is.na(d$tipo_scelta_contraente),
                                       "NO MATCHED CONTRACT",
                                       as.character(d$tipo_scelta_contraente))
    d
}

f_award <- prep_full(filt_full(df_full, AWARD_TIME))
f_exec  <- prep_full(filt_full(df_full, "execution"))
f_comp  <- prep_full(filt_full(df_full, "completion"))

# --- Specification -------------------------------------------------------------
RHS_FULL <- paste(
    "factor(tipo_scelta_contraente) + factor(pop_cluster) + factor(regione) +",
    "factor(year) + log_funding + no_contract + log_past_expenditure +",
    "DTO + weighted_payment_time + u35_pct + female_pct +",
    "log_digital_expenditure * type"
)

fit_full <- function(time, status, dat) {
    survreg(as.formula(sprintf("Surv(%s, %s) ~ %s", time, status, RHS_FULL)),
            data = dat, dist = "weibull")
}

r_award <- fit_full(AWARD_TIME,  AWARD_STATUS,      f_award)
r_exec  <- fit_full("execution", "execution_status", f_exec)
r_comp  <- fit_full("completion","execution_status", f_comp)

cat(sprintf("\nFitted N (starting frame is 40,991; this is the estimated ",
            "sample after listwise deletion on the remaining covariates):\n"))
cat(sprintf("  award %d | execution %d | completion %d\n",
            length(r_award$linear.predictors), length(r_exec$linear.predictors),
            length(r_comp$linear.predictors)))

# --- Extract, with clustered SEs ----------------------------------------------
cvcov <- function(mod, dat) {
    V <- cluster_vcov(mod, dat)
    if (is.null(dimnames(V)) || is.null(rownames(V))) {
        nm <- names(coef(mod))
        stopifnot(nrow(V) == length(nm))
        dimnames(V) <- list(nm, nm)
    }
    V
}

grab_row <- function(mod, dat, term, label) {
    cf <- coef(mod); V <- cvcov(mod, dat)
    se <- sqrt(diag(V))[term]
    z  <- cf[term] / se
    p  <- 2 * pnorm(-abs(z))
    tibble(type = label, est = round(as.numeric(cf[term]), 4),
           se = round(as.numeric(se), 4), p = as.numeric(p)) %>%
        mutate(sig = stars(p))
}

extract_phase <- function(mod, dat, phase) {
    main <- grab_row(mod, dat, "log_digital_expenditure",
                     "Experience in the procurement of digital solutions (log)")
    ints <- bind_rows(lapply(setdiff(TYPES, REF_TYPE), function(ty) {
        term <- paste0("log_digital_expenditure:type", ty)
        if (!term %in% names(coef(mod))) return(NULL)
        grab_row(mod, dat, term, paste0("x ", ty))
    }))
    ctrl_terms <- c("log_past_expenditure" = "log(Past proc.)",
                    "log_funding" = "log(Funding)",
                    "no_contract" = "No matched contract",
                    "DTO" = "DTO", "weighted_payment_time" = "Supplier payment time",
                    "u35_pct" = "Employees 25-34", "female_pct" = "Female employees")
    ctrl_rows <- bind_rows(lapply(names(ctrl_terms), function(term) {
        if (!term %in% names(coef(mod))) return(NULL)
        grab_row(mod, dat, term, ctrl_terms[[term]])
    }))

    bind_rows(main, ints, ctrl_rows) %>%
        mutate(phase = phase, n = length(mod$linear.predictors))
}

tableB4 <- bind_rows(
    extract_phase(r_award, f_award, "Award"),
    extract_phase(r_exec,  f_exec,  "Execution"),
    extract_phase(r_comp,  f_comp,  "Completion")
)

cat("\n=== Table B4: full sample, funding value as project-size proxy ===\n\n")
print(as.data.frame(tableB4 %>% select(phase, type, est, se, sig, n)), row.names = FALSE)
write.csv(tableB4, "outputs/tableB4_fullsample.csv", row.names = FALSE)

# --- Compare against the main-sample interaction terms (sign/significance) ---
grab_main_model <- function(mod, dat, phase) {
    bind_rows(lapply(setdiff(TYPES, REF_TYPE), function(ty) {
        term <- paste0("log_digital_expenditure:type", ty)
        if (!term %in% names(coef(mod))) return(NULL)
        grab_row(mod, dat, term, ty)
    })) %>% mutate(phase = phase)
}

main_ints <- bind_rows(
    grab_main_model(m_award, df_cup %>% filter(!is.na(.data[[AWARD_TIME]]), .data[[AWARD_TIME]] > 0,
                                               log_importo_complessivo_gara >= 0,
                                               log_past_expenditure >= 0, log_digital_expenditure >= 0),
                    "Award"),
    grab_main_model(m_exec, df_cup %>% filter(!is.na(execution), execution > 0,
                                               log_importo_complessivo_gara >= 0,
                                               log_past_expenditure >= 0, log_digital_expenditure >= 0),
                    "Execution"),
    grab_main_model(m_comp, df_cup %>% filter(!is.na(completion), completion > 0,
                                               log_importo_complessivo_gara >= 0,
                                               log_past_expenditure >= 0, log_digital_expenditure >= 0),
                    "Completion")
)

full_ints <- tableB4 %>% filter(grepl("^x ", type)) %>%
    mutate(type = sub("^x ", "", type))

cmp <- main_ints %>% rename(est_main = est, sig_main = sig) %>% select(phase, type, est_main, sig_main) %>%
    inner_join(full_ints %>% rename(est_full = est, sig_full = sig) %>% select(phase, type, est_full, sig_full),
              by = c("phase", "type")) %>%
    mutate(sign_flip = sign(est_main) != sign(est_full)) %>%
    arrange(match(phase, c("Award", "Execution", "Completion")), match(type, TYPES))

cat("\n=== Main sample vs. full sample, with clustered SEs ===\n\n")
print(as.data.frame(cmp), row.names = FALSE)
cat(sprintf("\nSign flips: %d | interactions losing significance at 5%%: %d (of %d matched rows)\n",
            sum(cmp$sign_flip, na.rm = TRUE),
            sum(cmp$sig_main %in% c("*", "**", "***") & !cmp$sig_full %in% c("*", "**", "***")),
            nrow(cmp)))
write.csv(cmp, "outputs/tableB4_vs_main.csv", row.names = FALSE)
