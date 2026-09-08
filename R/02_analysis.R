# =============================================================================
# MAIN ANALYSIS
#
# Manuscript: "The Paradox of Experience: How Organizational Complexity and
# Technological Integration Shape Public Procurement Timelines for Digital
# Solutions" (GIQ-D-25-01214R1)
#
# Requires either `df1_cig2` (produced by R/01_build_database.R) or `df_cup`
# (the already-collapsed project-level frame) in the environment. If df_cup is
# present and df1_cig2 is not, Parts 1, 2 and 4 - the duplication diagnostics,
# the collapse itself, and the before/after comparison - are skipped, since
# they exist to justify and document a collapse that has already happened.
# Run top to bottom. Everything is written to outputs/.
#
# -----------------------------------------------------------------------------
# WHAT THIS SCRIPT DOES AND WHY
#
# 1. DIAGNOSES A UNIT-OF-ANALYSIS PROBLEM.
#    The CUP-CIG merge in the build script joins project-level records to a
#    many-to-many CUP-CIG bridge table. Execution and completion durations come
#    from project-level (CUP) administrative milestones and are constant within
#    CUP - verified: 100% of multi-row projects have a single distinct value.
#    The merge therefore replicates identical outcomes across contract rows,
#    inflating N by x1.25. This is a counting error, not a modelling choice:
#    the same project-level duration is counted once per associated contract
#    instead of once per project, regardless of how that inflation happens to
#    be distributed across solution types or anything else.
#
#    ALL THREE outcomes are project-level. Award duration too is constant within
#    CUP (verified: 100%), so all three are collapsed to the project.
#
# 2. RE-ESTIMATES EVERYTHING at the corrected unit of analysis and quantifies
#    what changed relative to the replicated sample.
#
# 3. ADDRESSES REVIEWER #2 ON CONFOUNDING. A Cox model stratified by
#    municipality absorbs every time-invariant municipal characteristic
#    (legacy system complexity, vendor lock-in, prior integration architecture)
#    non-parametrically, without estimating a single stratum parameter.
#
# 4. REGENERATES all tables, figures, predicted days and appendix material.
#
# -----------------------------------------------------------------------------
# OUTCOME VARIABLES - DO NOT GUESS THESE
#   award phase      : contracting / contractual_status
#   execution phase  : execution   / execution_status
#   completion       : completion  / execution_status
#
#   NOTE: df1_cig2 also contains a column literally named `award`. It is NOT the
#   award-phase outcome: it takes negative values (min -1502) and has 9,312 NAs.
#   The correct pair is contracting / contractual_status.
#
# -----------------------------------------------------------------------------
# SIGN CONVENTIONS
#   Weibull AFT : positive coefficient = LONGER duration
#   Cox         : positive coefficient = HIGHER completion hazard = SHORTER
#   Conversion for a Weibull: beta_Cox = -beta_AFT / sigma
# =============================================================================

library(dplyr)
library(tidyr)
library(tibble)
library(survival)
library(ggplot2)
library(sandwich)

dir.create("outputs", showWarnings = FALSE)

# --- Configuration -----------------------------------------------------------
AWARD_TIME   <- "contracting"
AWARD_STATUS <- "contractual_status"

REF_TYPE <- "Digital Identity"          # reference category in the AFT tables

TYPES <- c("Digital Identity", "Digital Notices", "Digital Services and Payments",
           "Interoperability", "Cloud", "Citizen Experience")

MODEL_VARS <- c("tipo_scelta_contraente", "pop_cluster", "regione", "year",
                "log_importo_complessivo_gara", "log_past_expenditure",
                "DTO", "weighted_payment_time", "u35_pct", "female_pct",
                "log_digital_expenditure", "type")

stars <- function(p) {
  ifelse(is.na(p), "",
    ifelse(p < 0.001, "***",
      ifelse(p < 0.01, "**",
        ifelse(p < 0.05, "*", ifelse(p < 0.1, ".", "")))))
}

stopifnot(exists("df1_cig2") || exists("df_cup"))

if (exists("df_cup") && !exists("df1_cig2")) {
  cat("df_cup found and df1_cig2 absent: skipping Parts 1-2 (duplication\n")
  cat("diagnostics and the project-level collapse), which need the raw\n")
  cat("contract-level frame. Using the pre-built df_cup as-is.\n")
} else {

stopifnot(AWARD_TIME %in% names(df1_cig2), AWARD_STATUS %in% names(df1_cig2))


# =============================================================================
# PART 1 - DIAGNOSTICS: IS THERE A DUPLICATION PROBLEM?
# =============================================================================
cat("\n\n########## PART 1 - DUPLICATION DIAGNOSTICS ##########\n")

cat(sprintf("\nContracts (CIG): %d | Projects (CUP): %d | expansion factor: x%.2f\n",
            nrow(df1_cig2), n_distinct(df1_cig2$cup),
            nrow(df1_cig2) / n_distinct(df1_cig2$cup)))

rows_per_cup <- df1_cig2 %>% count(cup, name = "n_rows")
cat("\nContracts (CIG) per project:\n")
print(table(cut(rows_per_cup$n_rows, breaks = c(0, 1, 2, 5, 10, 50, Inf),
                labels = c("1", "2", "3-5", "6-10", "11-50", ">50"))))

outcome_variation <- df1_cig2 %>%
  group_by(cup) %>%
  summarise(n_rows     = n(),
            n_contract = n_distinct(.data[[AWARD_TIME]], na.rm = TRUE),
            n_exec     = n_distinct(execution,  na.rm = TRUE),
            n_comp     = n_distinct(completion, na.rm = TRUE),
            .groups = "drop") %>%
  filter(n_rows > 1)

# Raw counts, not percentages: with n=7,350 the two read very differently at
# a glance, and the earlier "<- replicated, collapse" annotation read like an
# instruction rather than a description of what was found.
n_multi <- nrow(outcome_variation)
cat(sprintf("\nMulti-contract projects: %d\n", n_multi))
cat(sprintf("  single distinct AWARD value:      %d of %d\n",
            sum(outcome_variation$n_contract <= 1), n_multi))
cat(sprintf("  single distinct EXECUTION value:  %d of %d\n",
            sum(outcome_variation$n_exec <= 1), n_multi))
cat(sprintf("  single distinct COMPLETION value: %d of %d\n",
            sum(outcome_variation$n_comp <= 1), n_multi))


# =============================================================================
# PART 2 - COLLAPSE TO PROJECT (CUP) LEVEL
# =============================================================================
# Aggregation rules:
#   outcomes and municipal covariates : constant within CUP -> first()
#   contract value                    : SUM across the project's contracts,
#                                       then logged (the project's total value)
#   procedure type                    : procedure of the economically dominant
#                                       lot (weighted mode by contract value)
#   n_cig                             : retained as a fragmentation control
cat("\n\n########## PART 2 - COLLAPSE ##########\n")

mode_by_weight <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (!any(ok)) return(NA_character_)
  tab <- tapply(w[ok], x[ok], sum, na.rm = TRUE)
  names(tab)[which.max(tab)]
}

df_cup <- df1_cig2 %>%
  group_by(cup) %>%
  summarise(
    execution          = first(execution),
    completion         = first(completion),
    contracting        = first(contracting),
    execution_status   = first(execution_status),
    contractual_status = first(contractual_status),

    n_cig            = n(),
    importo_gara_tot = sum(importo_complessivo_gara, na.rm = TRUE),

    tipo_scelta_contraente = mode_by_weight(as.character(tipo_scelta_contraente),
                                            importo_complessivo_gara),

    type                    = first(type),
    pop_cluster             = first(pop_cluster),
    regione                 = first(regione),
    area                    = first(area),
    year                    = first(year),
    codice_ipa              = first(codice_ipa),
    log_past_expenditure    = first(log_past_expenditure),
    log_digital_expenditure = first(log_digital_expenditure),
    DTO                     = first(DTO),
    weighted_payment_time   = first(weighted_payment_time),
    u35_pct                 = first(u35_pct),
    female_pct              = first(female_pct),
    .groups = "drop"
  ) %>%
  mutate(
    log_importo_complessivo_gara = log(importo_gara_tot + 1e-10),
    log_n_cig = log(n_cig)
  )

cat(sprintf("Collapsed: %d rows -> %d projects\n", nrow(df1_cig2), nrow(df_cup)))

}   # end of the df1_cig2-only block (diagnostics + collapse)


# =============================================================================
# PART 3 - MAIN MODELS AT THE CORRECTED UNIT OF ANALYSIS
# =============================================================================
cat("\n\n########## PART 3 - MAIN MODELS ##########\n")

filt <- function(d, outcome) {
  d[!is.na(d[[outcome]]) & d[[outcome]] > 0 &
      d$log_importo_complessivo_gara >= 0 &
      d$log_past_expenditure >= 0 &
      d$log_digital_expenditure >= 0, ]
}

prep <- function(d) d %>% mutate(type = relevel(factor(type), ref = REF_TYPE))

d_award <- prep(filt(df_cup,  AWARD_TIME))
d_exec  <- prep(filt(df_cup,  "execution"))
d_comp  <- prep(filt(df_cup,  "completion"))

RHS <- paste(
  "factor(tipo_scelta_contraente) + factor(pop_cluster) + factor(regione) +",
  "factor(year) + log_importo_complessivo_gara + log_past_expenditure +",
  "DTO + weighted_payment_time + u35_pct + female_pct +",
  "log_digital_expenditure * type"
)

fit_aft <- function(time, status, dat, rhs = RHS) {
  survreg(as.formula(sprintf("Surv(%s, %s) ~ %s", time, status, rhs)),
          data = dat, dist = "weibull")
}

m_award <- fit_aft(AWARD_TIME,  AWARD_STATUS,      d_award)
m_exec  <- fit_aft("execution", "execution_status", d_exec)
m_comp  <- fit_aft("completion","execution_status", d_comp)

cat(sprintf("  award      N = %6d\n", length(m_award$linear.predictors)))
cat(sprintf("  execution  N = %6d\n", length(m_exec$linear.predictors)))
cat(sprintf("  completion N = %6d\n", length(m_comp$linear.predictors)))


# =============================================================================
# PART 4 - WHAT THE COLLAPSE CHANGED
#
# Needs df1_cig2 (the pre-collapse comparison this part exists for), so it is
# skipped in the same shortcut mode as Parts 1-2.
# =============================================================================
if (!exists("df1_cig2")) {
  cat("\n\ndf1_cig2 not in the environment: skipping Part 4 (there is nothing to\n")
  cat("compare the collapse against).\n")
} else {
cat("\n\n########## PART 4 - EFFECT OF THE COLLAPSE ##########\n")

d_award_old <- prep(filt(df1_cig2, AWARD_TIME))
d_exec_old  <- prep(filt(df1_cig2, "execution"))
d_comp_old  <- prep(filt(df1_cig2, "completion"))

m_award_old <- fit_aft(AWARD_TIME,  AWARD_STATUS,      d_award_old)
m_exec_old  <- fit_aft("execution", "execution_status", d_exec_old)
m_comp_old  <- fit_aft("completion","execution_status", d_comp_old)

# Fragmentation control: does the number of contracts per project explain the
# interaction away?
m_award_ncig <- fit_aft(AWARD_TIME,  AWARD_STATUS,      d_award,
                        paste(RHS, "+ log_n_cig"))
m_exec_ncig  <- fit_aft("execution", "execution_status", d_exec,
                        paste(RHS, "+ log_n_cig"))
m_comp_ncig  <- fit_aft("completion","execution_status", d_comp,
                        paste(RHS, "+ log_n_cig"))

# NOTE: the argument is `mod`, not `model`. Inside tibble() columns are
# evaluated in sequence, so a `model = label` column would mask an argument of
# the same name and break `mod$...` lookups.
extract_aft <- function(mod, label) {
  s    <- summary(mod)$table
  keep <- grep("log_digital_expenditure", rownames(s), value = TRUE)
  tibble(model = label, term = keep,
         est = as.numeric(s[keep, "Value"]),
         se  = as.numeric(s[keep, "Std. Error"]),
         p   = as.numeric(s[keep, "p"]),
         n   = length(mod$linear.predictors))
}

comparison <- bind_rows(
  extract_aft(m_award_old,  "AWARD original (CIG)"),
  extract_aft(m_award,      "AWARD collapsed (CUP)"),
  extract_aft(m_award_ncig, "AWARD collapsed + n_cig"),
  extract_aft(m_exec_old,   "EXEC original (CIG)"),
  extract_aft(m_exec,       "EXEC collapsed (CUP)"),
  extract_aft(m_exec_ncig,  "EXEC collapsed + n_cig"),
  extract_aft(m_comp_old,   "COMP original (CIG)"),
  extract_aft(m_comp,       "COMP collapsed (CUP)"),
  extract_aft(m_comp_ncig,  "COMP collapsed + n_cig")
)

write.csv(comparison %>% mutate(sig = stars(p)),
          "outputs/collapse_comparison.csv", row.names = FALSE)

compare_pair <- function(lab_old, lab_new, header) {
  d <- comparison %>% filter(model %in% c(lab_old, lab_new)) %>%
    select(term, model, est, se) %>%
    pivot_wider(names_from = model, values_from = c(est, se))
  oe <- d[[paste0("est_", lab_old)]]; ne <- d[[paste0("est_", lab_new)]]
  os <- d[[paste0("se_",  lab_old)]]; ns <- d[[paste0("se_",  lab_new)]]
  out <- tibble(term = d$term,
                est_old = round(oe, 4), est_new = round(ne, 4),
                pct_change = round(100 * (ne - oe) / abs(oe), 1),
                se_ratio = round(ns / os, 3),
                sign_flip = sign(oe) != sign(ne))
  cat("\n--- ", header, " ---\n", sep = "")
  print(as.data.frame(out), row.names = FALSE)
  invisible(out)
}

compare_pair("AWARD original (CIG)", "AWARD collapsed (CUP)", "Award")
compare_pair("EXEC original (CIG)",  "EXEC collapsed (CUP)",  "Execution")
compare_pair("COMP original (CIG)",  "COMP collapsed (CUP)",  "Completion")

}   # end of the df1_cig2-only block (Part 4)


# =============================================================================
# PART 5 - FULL REGRESSION TABLES
# =============================================================================
cat("\n\n########## PART 5 - REGRESSION TABLES ##########\n")

# vcovCL returns a matrix without dimnames, so anything that indexes it by
# coefficient name would silently get NA. The names are restored here once, for
# every downstream script.
cluster_vcov <- function(mod, dat) {
  V <- tryCatch(vcovCL(mod, cluster = dat$codice_ipa, type = "HC0"),
                error = function(e) NULL)
  if (is.null(V)) {
    warning("Cluster-robust vcov unavailable for this model; using model vcov.")
    V <- vcov(mod)
  }
  V <- V[seq_along(coef(mod)), seq_along(coef(mod)), drop = FALSE]
  if (is.null(rownames(V))) {
    nm <- names(coef(mod))
    dimnames(V) <- list(nm, nm)
  }
  V
}

build_table <- function(mod, dat, phase) {
  cf <- coef(mod); se <- sqrt(diag(cluster_vcov(mod, dat)))
  z  <- cf / se;   p  <- 2 * pnorm(-abs(z))
  tibble(phase = phase, term = names(cf),
         est = round(cf, 4), se = round(se, 4),
         z = round(z, 3), p = p, sig = stars(p))
}

tables <- bind_rows(
  build_table(m_award, d_award, "award"),
  build_table(m_exec,  d_exec,  "execution"),
  build_table(m_comp,  d_comp,  "completion")
)
write.csv(tables, "outputs/table_main_regressions.csv", row.names = FALSE)

cat("\nInteraction terms, main specification:\n")
print(as.data.frame(tables %>% filter(grepl("log_digital_expenditure", term))),
      row.names = FALSE)


# =============================================================================
# PART 6 - TOTAL EFFECTS AND FIGURE 4
# The total effect for a type is the main effect plus that type's interaction.
# Delta method: Var(b_main + b_int) = Var(main) + Var(int) + 2Cov(main, int).
# =============================================================================
cat("\n\n########## PART 6 - TOTAL EFFECTS ##########\n")

total_effects <- function(mod, dat, phase) {
  cf <- coef(mod); V <- cluster_vcov(mod, dat)
  i_main <- which(names(cf) == "log_digital_expenditure")
  bind_rows(lapply(TYPES, function(tp) {
    if (tp == REF_TYPE) {
      est <- cf[i_main]; var <- V[i_main, i_main]
    } else {
      nm <- paste0("log_digital_expenditure:type", tp)
      if (!nm %in% names(cf)) return(NULL)
      i <- which(names(cf) == nm)
      est <- cf[i_main] + cf[i]
      var <- V[i_main, i_main] + V[i, i] + 2 * V[i_main, i]
    }
    tibble(phase = phase, type = tp, est = as.numeric(est), se = sqrt(as.numeric(var)))
  })) %>%
    mutate(lo = est - 1.96 * se, hi = est + 1.96 * se,
           p = 2 * pnorm(-abs(est / se)), sig = stars(p))
}

fig4_data <- bind_rows(
  total_effects(m_award, d_award, "Award"),
  total_effects(m_exec,  d_exec,  "Execution"),
  total_effects(m_comp,  d_comp,  "Completion")
) %>%
  mutate(phase = factor(phase, levels = c("Award", "Execution", "Completion")),
         type  = factor(type, levels = rev(TYPES)))

write.csv(fig4_data, "outputs/figure4_data.csv", row.names = FALSE)
print(as.data.frame(fig4_data %>% select(phase, type, est, lo, hi, sig)),
      row.names = FALSE)

# geom_errorbarh() is deprecated in ggplot2 4.0; use orientation = "y".
plot_forest <- function(dat, xvar, xlab) {
  ggplot(dat, aes(x = .data[[xvar]], y = type)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_errorbar(aes(xmin = .data[[paste0(xvar, "_lo")]],
                      xmax = .data[[paste0(xvar, "_hi")]]),
                  orientation = "y", width = 0.18, linewidth = 0.5) +
    geom_point(size = 2.2) +
    facet_wrap(~ phase, nrow = 1, scales = "free_x") +
    labs(x = xlab, y = NULL) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.major.y = element_blank(),
          strip.text = element_text(face = "bold"))
}

ggsave("outputs/figure4.png",
       plot_forest(fig4_data %>% rename(est_lo = lo, est_hi = hi), "est",
                   "Total effect of prior digital expenditure (log-time)"),
       width = 10, height = 3.6, units = "in", dpi = 300)


# =============================================================================
# PART 7 - PREDICTED DAYS AND FIGURE 5
# In an AFT model the coefficient acts on log-time, so a change of delta in the
# covariate multiplies expected duration by exp(beta * delta).
# =============================================================================
cat("\n\n########## PART 7 - PREDICTED DAYS ##########\n")

iqr_delta <- function(d) {
  q <- quantile(d$log_digital_expenditure, c(.25, .75), na.rm = TRUE)
  as.numeric(q[2] - q[1])
}
med_dur <- function(d, v) median(d[[v]][d[[v]] > 0], na.rm = TRUE)

MEDIANS <- c(award      = med_dur(d_award, AWARD_TIME),
             execution  = med_dur(d_exec,  "execution"),
             completion = med_dur(d_comp,  "completion"))
DELTAS  <- c(award      = iqr_delta(d_award),
             execution  = iqr_delta(d_exec),
             completion = iqr_delta(d_comp))

cat("\nBaselines on the corrected samples:\n")
for (ph in names(MEDIANS))
  cat(sprintf("  %-11s median %5.0f days | IQR of log experience %.4f\n",
              ph, MEDIANS[ph], DELTAS[ph]))

predicted_days <- fig4_data %>%
  mutate(k = tolower(as.character(phase))) %>%
  rowwise() %>%
  mutate(delta = DELTAS[[k]], base = MEDIANS[[k]],
         days    = base * (exp(est * delta) - 1),
         days_lo = base * (exp(lo  * delta) - 1),
         days_hi = base * (exp(hi  * delta) - 1),
         pct     = 100 * (exp(est * delta) - 1)) %>%
  ungroup() %>%
  mutate(across(c(days, days_lo, days_hi, pct), ~ round(.x, 1))) %>%
  select(phase, type, est, days, days_lo, days_hi, pct, sig)

write.csv(predicted_days, "outputs/predicted_days.csv", row.names = FALSE)
print(as.data.frame(predicted_days), row.names = FALSE)

ggsave("outputs/figure5.png",
       plot_forest(predicted_days %>% mutate(type = factor(type, levels = rev(TYPES))),
                   "days", "Change in days for the median-duration project"),
       width = 10, height = 3.6, units = "in", dpi = 300)


# =============================================================================
# PART 8 - MUNICIPALITY FIXED EFFECTS VIA STRATIFIED COX
#
# Why not dummies: survreg with ~6,600 municipality indicators builds a dense
# 28,000 x 6,700 design matrix and inverts a 6,700 x 6,700 Hessian each
# iteration. It does not converge in practice. A stratified Cox gives each
# municipality its own baseline hazard through the partial likelihood; no
# stratum parameter is estimated and it runs in seconds.
#
# Identification: log_digital_expenditure is constant within municipality, so
# its main effect drops out. The interaction with solution type is identified
# from municipalities that ran more than one type of solution.
#
# TWO SPECIFICATION TRAPS:
#   (a) factor(type) MUST be in the model as a main effect. Without it, the
#       interaction terms absorb the level differences in duration between
#       solution types instead of the moderating effect of experience.
#   (b) The six type-specific slopes are collinear, so one is aliased away. R
#       drops the LAST factor level, so REF_TYPE is placed last to make the
#       dropped category match the AFT tables. The UNSTRATIFIED models still use
#       the FIRST level as reference; Part 9 puts everything on a common
#       baseline.
#
# NOTE ON INFERENCE: these models are fitted without cluster/robust, so the
# standard errors are model-based, unlike the AFT tables which cluster at the
# municipality level. Any table reporting them should say so.
# =============================================================================
cat("\n\n########## PART 8 - MUNICIPALITY FIXED EFFECTS ##########\n")

set_ref_last <- function(x, ref = REF_TYPE) {
  lv <- levels(factor(x))
  factor(x, levels = c(setdiff(lv, ref), ref))
}

make_fe_sample <- function(dat) {
  dat %>% group_by(codice_ipa) %>%
    filter(n_distinct(type) > 1) %>% ungroup() %>%
    mutate(type = set_ref_last(type))
}

fe_award <- make_fe_sample(d_award)
fe_exec  <- make_fe_sample(d_exec)
fe_comp  <- make_fe_sample(d_comp)

cat(sprintf("  AWARD FE sample: %6d projects,  %d municipalities\n",
            nrow(fe_award), n_distinct(fe_award$codice_ipa)))
cat(sprintf("  EXEC  FE sample: %6d projects,  %d municipalities\n",
            nrow(fe_exec),  n_distinct(fe_exec$codice_ipa)))
cat(sprintf("  COMP  FE sample: %6d projects,  %d municipalities\n",
            nrow(fe_comp),  n_distinct(fe_comp$codice_ipa)))

FE_RHS <- paste("factor(tipo_scelta_contraente) + factor(year) +",
                "log_importo_complessivo_gara + type +",
                "log_digital_expenditure:type + strata(codice_ipa)")

NOFE_RHS <- paste("factor(tipo_scelta_contraente) + factor(year) +",
                  "log_importo_complessivo_gara + factor(pop_cluster) +",
                  "factor(regione) + log_past_expenditure + DTO +",
                  "weighted_payment_time + u35_pct + female_pct +",
                  "log_digital_expenditure * type")

fit_cox <- function(time, status, rhs, dat) {
  coxph(as.formula(sprintf("Surv(%s, %s) ~ %s", time, status, rhs)), data = dat)
}

m_award_fe   <- fit_cox(AWARD_TIME,  AWARD_STATUS,      FE_RHS,   fe_award)
m_award_nofe <- fit_cox(AWARD_TIME,  AWARD_STATUS,      NOFE_RHS, fe_award)
m_exec_fe    <- fit_cox("execution", "execution_status", FE_RHS,   fe_exec)
m_exec_nofe  <- fit_cox("execution", "execution_status", NOFE_RHS, fe_exec)
m_comp_fe    <- fit_cox("completion","execution_status", FE_RHS,   fe_comp)
m_comp_nofe  <- fit_cox("completion","execution_status", NOFE_RHS, fe_comp)

clean_term <- function(x, ref_first) {
  x <- gsub("type(.*?):log_digital_expenditure", "\\1", x)
  x <- gsub("log_digital_expenditure:type", "", x)
  x <- gsub("^log_digital_expenditure$", paste0(ref_first, " (main effect)"), x)
  trimws(x)
}

extract_cox <- function(mod, label, dat, stratified) {
  s     <- summary(mod)$coefficients
  keep  <- grep("log_digital_expenditure", rownames(s), value = TRUE)
  coefs <- as.numeric(s[keep, "coef"])
  ref   <- if (stratified) REF_TYPE else levels(dat$type)[1]
  tibble(model = label, reference = ref, term = clean_term(keep, ref),
         cox_coef = round(coefs, 4),
         hazard_ratio = round(as.numeric(s[keep, "exp(coef)"]), 4),
         se = round(as.numeric(s[keep, "se(coef)"]), 4),
         p = as.numeric(s[keep, "Pr(>|z|)"]),
         direction = ifelse(is.na(coefs), NA_character_,
                            ifelse(coefs < 0, "longer", "shorter")),
         n = mod$n, n_strata = n_distinct(dat$codice_ipa)) %>%
    mutate(sig = stars(p))
}

fe_results <- bind_rows(
  extract_cox(m_award_nofe, "AWARD no strata", fe_award, FALSE),
  extract_cox(m_award_fe,   "AWARD FE strata", fe_award, TRUE),
  extract_cox(m_exec_nofe,  "EXEC  no strata", fe_exec,  FALSE),
  extract_cox(m_exec_fe,    "EXEC  FE strata", fe_exec,  TRUE),
  extract_cox(m_comp_nofe,  "COMP  no strata", fe_comp,  FALSE),
  extract_cox(m_comp_fe,    "COMP  FE strata", fe_comp,  TRUE)
)
write.csv(fe_results, "outputs/fe_results.csv", row.names = FALSE)
print(as.data.frame(fe_results), row.names = FALSE)


# =============================================================================
# PART 9 - PUT EVERY MODEL ON A COMMON REFERENCE
# Slopes are identified only up to an additive constant, so a model reported
# against reference A shifts to reference B by subtracting B's slope from all.
# =============================================================================
cat("\n\n########## PART 9 - COMMON REFERENCE ##########\n")

to_common_reference <- function(mod, label, dat, stratified) {
  s     <- summary(mod)$coefficients
  keep  <- grep("log_digital_expenditure", rownames(s), value = TRUE)
  coefs <- as.numeric(s[keep, "coef"])
  lv    <- levels(dat$type)
  ref   <- if (stratified) REF_TYPE else lv[1]
  nm    <- clean_term(keep, ref)

  slopes <- setNames(rep(NA_real_, length(lv)), lv)
  if (stratified) {
    # `keep` still holds the aliased term (coef NA). Assigning it would
    # overwrite the baseline with NA, so skip NA coefficients and set the
    # baseline AFTER the loop.
    for (i in seq_along(nm))
      if (nm[i] %in% lv && !is.na(coefs[i])) slopes[nm[i]] <- coefs[i]
    slopes[REF_TYPE] <- 0
  } else {
    main <- coefs[grepl("main effect", nm)]
    slopes[ref] <- main
    for (i in seq_along(nm))
      if (nm[i] %in% lv && !is.na(coefs[i])) slopes[nm[i]] <- main + coefs[i]
  }
  slopes <- slopes - slopes[REF_TYPE]
  tibble(model = label, type = names(slopes), slope = round(slopes, 4)) %>%
    filter(type != REF_TYPE)
}

common <- bind_rows(
  to_common_reference(m_award_nofe, "AWARD no strata", fe_award, FALSE),
  to_common_reference(m_award_fe,   "AWARD FE strata", fe_award, TRUE),
  to_common_reference(m_exec_nofe,  "EXEC  no strata", fe_exec,  FALSE),
  to_common_reference(m_exec_fe,    "EXEC  FE strata", fe_exec,  TRUE),
  to_common_reference(m_comp_nofe,  "COMP  no strata", fe_comp,  FALSE),
  to_common_reference(m_comp_fe,    "COMP  FE strata", fe_comp,  TRUE)
) %>%
  pivot_wider(names_from = model, values_from = slope) %>%
  arrange(`EXEC  FE strata`)

stopifnot(!any(is.na(common)))
write.csv(common, "outputs/fe_common_reference.csv", row.names = FALSE)

cat(sprintf("All models with %s = 0; negative = LONGER duration\n\n", REF_TYPE))
print(as.data.frame(common), row.names = FALSE)


# =============================================================================
# PART 10 - APPENDIX TABLE AND SAMPLE ATTRITION
# =============================================================================
cat("\n\n########## PART 10 - APPENDIX MATERIAL ##########\n")

phase_key <- function(lbl) {
  k <- tolower(sub(" .*", "", trimws(lbl)))
  map <- c(exec = "execution", comp = "completion")
  ifelse(k %in% names(map), unname(map[k]), k)
}

aft_interactions <- function(mod, phase) {
  s  <- summary(mod)$table
  nm <- grep("^log_digital_expenditure:type", rownames(s), value = TRUE)
  tibble(phase = phase, type = sub("^log_digital_expenditure:type", "", nm),
         aft_cup = round(as.numeric(s[nm, "Value"]), 4),
         cox_equivalent = round(-as.numeric(s[nm, "Value"]) / mod$scale, 4))
}

PHASE_MAP <- c(AWARD = "award", EXEC = "execution", COMP = "completion")
aft_original <- comparison %>%
  filter(grepl("original \\(CIG\\)$", model), grepl(":type", term)) %>%
  transmute(phase = unname(PHASE_MAP[sub(" .*", "", model)]),
            type = sub(".*:type", "", term), aft_cig = round(est, 4))

appendix_table <- bind_rows(
  aft_interactions(m_award, "award"),
  aft_interactions(m_exec,  "execution"),
  aft_interactions(m_comp,  "completion")
) %>%
  left_join(aft_original, by = c("phase", "type")) %>%
  left_join(common %>% pivot_longer(-type, names_to = "model", values_to = "cox") %>%
              mutate(phase = phase_key(model),
                     spec = ifelse(grepl("FE", model), "cox_fe", "cox_nofe")) %>%
              select(type, phase, spec, cox) %>%
              pivot_wider(names_from = spec, values_from = cox),
            by = c("type", "phase")) %>%
  select(phase, type, aft_cig, aft_cup, cox_equivalent, cox_nofe, cox_fe) %>%
  arrange(match(phase, c("award", "execution", "completion")), match(type, TYPES))

stopifnot(sum(is.na(appendix_table$cox_fe)) == 0)
write.csv(appendix_table, "outputs/appendix_table.csv", row.names = FALSE)
print(as.data.frame(appendix_table), row.names = FALSE)

# --- Table C1 / Table A1 Panel A: every row counts projects ------------------
# Restructured to match the paper's own three-step breakdown exactly, rather
# than the previous two-step version, which silently folded the "no ANAC
# contract record" exclusion into what it labelled "log-covariate filter" -
# importo_gara_tot = 0 for those 10,132 projects fails log(...) >= 0 along
# with genuine covariate-missingness cases, so the old single step reported
# 30,835 where the two causes, split apart as the paper does, are 10,132 and
# 68 (award/completion) or 1,816 (execution) respectively.
attrition_col <- function(dat, outcome, status) {
  s1 <- dat[!is.na(dat$importo_gara_tot) & dat$importo_gara_tot > 0, ]
  # "outcome could not be constructed" checks only that the duration exists
  # (is.na), not that it is positive - a handful of projects have a genuine,
  # non-missing duration of exactly zero (contract signed the same day as the
  # funding decree; verified against df1_cig2: zero NAs on contracting or
  # completion at that stage). Those are real values, not missing ones, but
  # log(0) is undefined for the Weibull AFT estimation, so they still cannot
  # enter the model - that exclusion belongs in the covariate-completeness
  # step below, not here.
  s2 <- s1[!is.na(s1[[outcome]]) &
             is.finite(s1$log_past_expenditure)    & s1$log_past_expenditure    >= 0 &
             is.finite(s1$log_digital_expenditure) & s1$log_digital_expenditure >= 0, ]
  v  <- c(MODEL_VARS, outcome, status); v <- v[v %in% names(s2)]
  keep_final <- stats::complete.cases(s2[, v, drop = FALSE]) & s2[[outcome]] > 0
  c(`Initial project frame: unique projects identified by CUP`                  = nrow(dat),
    `Excluded: no retrievable ANAC contract record through the CUP-CIG linkage` = nrow(dat) - nrow(s1),
    `Remaining with a retrievable ANAC record`                                  = nrow(s1),
    `Excluded because outcome could not be constructed`                        = nrow(s1) - nrow(s2),
    `Remaining with a constructible duration outcome`                          = nrow(s2),
    `Excluded: missing values in one or more independent or control variables` = nrow(s2) - sum(keep_final),
    `Final analytical sample`                                                  = sum(keep_final))
}

table_c1 <- cbind(
  Award      = attrition_col(df_cup, AWARD_TIME,  AWARD_STATUS),
  Execution  = attrition_col(df_cup, "execution", "execution_status"),
  Completion = attrition_col(df_cup, "completion","execution_status")
)

cat("\n--- Table A1, Panel A: sample construction (projects) ---\n")
print(table_c1)
write.csv(data.frame(Stage = rownames(table_c1), table_c1),
          "outputs/table_c1_attrition.csv", row.names = FALSE)

published_A1 <- list(
  Award      = c(40991, 10132, 30859, 68,    30791, 1562, 29229),
  Execution  = c(40991, 10132, 30859, 1816,  29043, 1512, 27531),
  Completion = c(40991, 10132, 30859, 68,    30791, 1556, 29235)
)
cat("\n--- check against Table A1, Panel A as published ---\n")
for (col in c("Award", "Execution", "Completion")) {
  now <- as.integer(table_c1[, col])
  pub <- published_A1[[col]]
  ok <- all(now == pub)
  cat(sprintf("  %-10s %s\n", col, if (ok) "OK - every row matches"
                                     else paste("MISMATCH at row(s):",
                                                paste(which(now != pub), collapse = ", "))))
}

cat(sprintf("\nProjects with no execution timestamp: %d\n",
            sum(is.na(df_cup$execution))))

appendix_c <- tibble(
  step = c("Contract rows after CUP-CIG linkage (pre-collapse)",
           "Projects after collapse to CUP level",
           "Award FE subsample (municipalities with >1 solution type)",
           "Execution FE subsample (municipalities with >1 solution type)",
           "Completion FE subsample (municipalities with >1 solution type)"),
  n = c(if (exists("df1_cig2")) nrow(df1_cig2) else NA_integer_,
        nrow(df_cup), nrow(fe_award), nrow(fe_exec), nrow(fe_comp))
)
write.csv(appendix_c, "outputs/appendix_c_supplementary.csv", row.names = FALSE)
cat("\n--- Supplementary counts ---\n")
print(as.data.frame(appendix_c), row.names = FALSE)

na_audit <- function(dat, tv, sv, label) {
  v <- c(MODEL_VARS, tv, sv); v <- v[v %in% names(dat)]
  cnt <- sort(sapply(v, function(x) sum(is.na(dat[[x]]))), decreasing = TRUE)
  cnt <- cnt[cnt > 0]
  cat(sprintf("\n%s: %d rows -> %d complete\n", label, nrow(dat),
              sum(stats::complete.cases(dat[, v, drop = FALSE]))))
  if (!length(cnt)) cat("  no missing values\n")
  else for (x in names(cnt))
    cat(sprintf("  %-30s %6d (%.1f%%)\n", x, cnt[[x]], 100 * cnt[[x]] / nrow(dat)))
}
cat("\n--- Listwise deletion audit ---")
na_audit(d_award, AWARD_TIME, AWARD_STATUS, "Award")
na_audit(d_exec,  "execution", "execution_status", "Execution")
na_audit(d_comp,  "completion","execution_status", "Completion")


# =============================================================================
# PART 11 - CONVERGENCE DIAGNOSTICS
# "Loglik converged before variable N" signals monotone likelihood or near
# separation. Harmless when confined to sparse control dummies; a problem if it
# touches the interaction terms.
# =============================================================================
cat("\n\n########## PART 11 - CONVERGENCE DIAGNOSTICS ##########\n")

diagnose <- function(mod, label, dat) {
  cf <- coef(mod); se <- sqrt(diag(vcov(mod)))
  aliased <- names(cf)[is.na(cf)]
  flagged <- unique(c(names(cf)[!is.na(cf) & abs(cf) > 10],
                      names(cf)[!is.na(se) & se > 10]))
  cat(sprintf("\n--- %s ---\n", label))
  cat("  aliased:", ifelse(!length(aliased), "none", paste(aliased, collapse = ", ")), "\n")
  touched <- grep("log_digital_expenditure", flagged, value = TRUE)
  cat("  affects interaction terms:",
      ifelse(!length(touched), "NO - confined to control dummies",
             paste("YES -", paste(touched, collapse = ", "))), "\n")
  for (v in flagged) {
    col <- sub("^factor\\((.*?)\\).*$", "\\1", v); lvl <- sub("^factor\\(.*?\\)", "", v)
    if (col %in% names(dat))
      cat(sprintf("    %s: %d rows\n", lvl, sum(as.character(dat[[col]]) == lvl, na.rm = TRUE)))
  }
  invisible(list(aliased = aliased, flagged = flagged))
}

invisible(list(
  diagnose(m_award_fe, "AWARD FE strata", fe_award),
  diagnose(m_exec_fe,  "EXEC FE strata",  fe_exec),
  diagnose(m_comp_fe,  "COMP FE strata",  fe_comp)
))

chk <- fe_exec %>% group_by(codice_ipa) %>%
  summarise(k = n_distinct(round(log_digital_expenditure, 8)), .groups = "drop")
cat(sprintf("\nlog_digital_expenditure constant within municipality: %s\n",
            ifelse(all(chk$k == 1), "yes (expected - hence the aliased slope)",
                   sprintf("NO - %d municipalities vary", sum(chk$k > 1)))))


# =============================================================================
# PART 12 - NUMBERS QUOTED IN THE MANUSCRIPT TEXT
# =============================================================================
cat("\n\n########## PART 12 - NUMBERS QUOTED IN THE TEXT ##########\n")

cat("\nInteraction vs total effect (these can differ in significance):\n")
print(as.data.frame(
  tables %>%
    filter(grepl("log_digital_expenditure:type", term)) %>%
    transmute(phase, type = sub(".*:type", "", term),
              interaction = est, int_sig = sig) %>%
    left_join(fig4_data %>%
                transmute(phase = tolower(as.character(phase)),
                          type = as.character(type),
                          total = round(est, 4), tot_sig = sig),
              by = c("phase", "type"))), row.names = FALSE)

cat("\nPhase-by-phase, in days:\n")
print(as.data.frame(predicted_days %>%
        filter(type %in% c("Citizen Experience", "Digital Notices", "Cloud")) %>%
        arrange(phase, type)), row.names = FALSE)

gap <- predicted_days %>% filter(phase == "Completion") %>%
  summarise(max_delay = max(days), max_speedup = min(days),
            total_gap = max(days) - min(days),
            gap_pct = round(100 * (max(days) - min(days)) / MEDIANS[["completion"]], 1))

cat("\nAbstract headline figures (completion phase):\n")
print(as.data.frame(gap), row.names = FALSE)

cat("\n\nAll outputs written to outputs/\n")
