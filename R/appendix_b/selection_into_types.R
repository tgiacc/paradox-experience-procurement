# =============================================================================
# SELECTION INTO SOLUTION TYPES - THE REMAINING R2.3 OBJECTION
#
# Run AFTER giq_r2_analysis.R (needs df_cup, fe_award/fe_exec/fe_comp,
# AWARD_TIME, AWARD_STATUS, REF_TYPE, TYPES, stars()).
#
# WHAT R2.3 ASKS AND WHAT IS ALREADY ANSWERED
#   Legacy complexity, vendor lock-in and prior integration architecture are
#   municipality-level and time-invariant over 2022-2024: the stratified Cox
#   absorbs them. Reverse causality is ruled out by dates - prior expenditure is
#   2007-2021, decrees are 2022-2023. Omitted variables are bounded by Oster in
#   the award phase and by the block-stability table elsewhere.
#
#   What is NOT yet answered is selection into complex projects: municipalities
#   might choose which solution types to procure in a way correlated with their
#   own experience, so the experience-by-type interaction would reflect who
#   chose what rather than how experience operates.
#
# THE STRUCTURE OF THE ANSWER
#   PART 1  the extensive margin: is there selection INTO the programme at all?
#   PART 2  does prior experience predict which types a municipality procures?
#   PART 3  the invariance argument, verified rather than asserted: experience is
#           constant within municipality, so municipality-level selection cannot
#           confound an interaction identified within strata
#   PART 4  robustness to portfolio breadth: municipalities holding more types
#           have less room to select, so the estimate should not drift
# =============================================================================

library(dplyr)
library(survival)

dir.create("outputs", showWarnings = FALSE)

ALL_TYPES <- c("Digital Identity", "Digital Notices", "Digital Services and Payments",
               "Interoperability", "Cloud", "Citizen Experience")

# =============================== PART 1 ======================================
# Roughly 7,900 municipalities exist in Italy. If almost all of them appear, the
# programme was close to universal and there is little scope for selection into
# it - which is a different and weaker claim than no selection across types.
cat("=== PART 1: extensive margin ===\n\n")

muni <- df_cup %>%
    group_by(codice_ipa) %>%
    summarise(n_projects = n(),
              n_types = n_distinct(type),
              types = paste(sort(unique(type)), collapse = " | "),
              ldig = first(log_digital_expenditure),
              pop_band = first(pop_cluster),
              regione = first(regione),
              .groups = "drop")

cat(sprintf("Municipalities in the data: %s\n",
            format(nrow(muni), big.mark = ",")))
cat(sprintf("Projects per municipality: median %.0f, mean %.2f, max %d\n",
            median(muni$n_projects), mean(muni$n_projects), max(muni$n_projects)))

cat("\nDistinct solution types per municipality:\n")
print(as.data.frame(muni %>% count(n_types, name = "municipalities") %>%
                        mutate(share = sprintf("%.1f%%", 100 * municipalities / nrow(muni)))),
      row.names = FALSE)
cat(sprintf("\nWith at least two types: %.1f%% | with all six: %.1f%%\n",
            100 * mean(muni$n_types >= 2), 100 * mean(muni$n_types == 6)))

cat("\nAdoption rate of each type, across municipalities:\n")
adopt <- bind_rows(lapply(ALL_TYPES, function(ty) {
    a <- df_cup %>% group_by(codice_ipa) %>%
        summarise(has = any(type == ty), .groups = "drop")
    data.frame(type = ty, municipalities = sum(a$has),
               share = sprintf("%.1f%%", 100 * mean(a$has)),
               stringsAsFactors = FALSE)
}))
print(as.data.frame(adopt), row.names = FALSE)


# =============================== PART 2 ======================================
# Two questions. Does experience predict HOW MANY types a municipality takes,
# and does it predict WHICH ones? The second is the one that matters: a
# correlation with breadth is not selection on the moderator.
cat("\n\n=== PART 2: does prior experience predict the portfolio? ===\n\n")

m_breadth <- lm(n_types ~ ldig + factor(pop_band) + factor(regione),
                data = muni %>% filter(is.finite(ldig)))
s <- summary(m_breadth)$coefficients
cat("Number of distinct types ~ prior digital expenditure:\n")
print(round(s["ldig", , drop = FALSE], 4))
cat(sprintf("  a one-log-point increase in experience changes breadth by %.4f types\n",
            s["ldig", 1]))
cat(sprintf("  (mean breadth %.2f, so %.2f%% of the mean)\n",
            mean(muni$n_types), 100 * s["ldig", 1] / mean(muni$n_types)))

cat("\nAdoption of each type ~ prior digital expenditure (logit, municipality level):\n")
sel <- bind_rows(lapply(ALL_TYPES, function(ty) {
    d <- df_cup %>% group_by(codice_ipa) %>%
        summarise(has = as.integer(any(type == ty)),
                  ldig = first(log_digital_expenditure),
                  pop_band = first(pop_cluster),
                  regione = first(regione), .groups = "drop") %>%
        filter(is.finite(ldig))
    m <- tryCatch(glm(has ~ ldig + factor(pop_band) + factor(regione),
                      data = d, family = binomial()),
                  error = function(e) NULL)
    if (is.null(m)) return(NULL)
    cf <- summary(m)$coefficients
    # average marginal effect, so the number is on the probability scale
    p <- predict(m, type = "response")
    ame <- mean(p * (1 - p)) * cf["ldig", 1]
    data.frame(type = ty, logit_coef = round(cf["ldig", 1], 4),
               se = round(cf["ldig", 2], 4), p = round(cf["ldig", 4], 4),
               AME_pp = round(100 * ame, 3), stringsAsFactors = FALSE)
}))
print(as.data.frame(sel), row.names = FALSE)

cat("\nRead the AME column: percentage points of adoption probability per log\n")
cat("point of experience. If the effects are small and do not line up with the\n")
cat("sign pattern of the interaction - positive for Citizen Experience and Cloud,\n")
cat("negative for Digital Notices - then selection is not generating the result.\n")
cat("If they DO line up, the size matters: report it and discuss it.\n")

# Does the selection pattern correlate with the interaction pattern? The
# interaction is positive for Citizen Experience, Cloud, Interoperability and
# negative for Digital Notices in the AFT tables.
int_sign <- c("Citizen Experience" = 1, "Cloud" = 1, "Interoperability" = 1,
              "Digital Services and Payments" = 1, "Digital Notices" = -1,
              "Digital Identity" = 0)
sel$interaction_sign <- int_sign[sel$type]
cat(sprintf("\nCorrelation between selection AME and interaction sign: %.3f\n",
            suppressWarnings(cor(sel$AME_pp, sel$interaction_sign,
                                 use = "complete.obs"))))
write.csv(sel, "outputs/selection_by_type.csv", row.names = FALSE)


# =============================== PART 3 ======================================
# The logical core. Prior digital expenditure is a municipality attribute: it
# does not vary across a municipality's own projects. So any selection that
# operates at the municipality level - including selection on experience itself -
# is absorbed by the strata, and cannot generate the within-stratum interaction.
cat("\n\n=== PART 3: is experience really constant within municipality? ===\n\n")

chk <- df_cup %>%
    group_by(codice_ipa) %>%
    summarise(k = n_distinct(round(log_digital_expenditure, 8)), .groups = "drop")
cat(sprintf("Municipalities with more than one value of experience: %d of %d\n",
            sum(chk$k > 1), nrow(chk)))
if (all(chk$k == 1)) {
    cat("Constant throughout. Municipality-level selection on experience therefore\n")
    cat("cannot produce the within-stratum interaction: the regressor has no\n")
    cat("within-municipality variation to correlate with the selection.\n")
    cat("What remains is selection WITHIN municipality across types - a\n")
    cat("municipality that is experienced AND systematically given harder Citizen\n")
    cat("Experience projects than its own other projects. That is a narrower claim\n")
    cat("than the one R2.3 makes, and it is the one to address in the text.\n")
} else {
    cat("NOT constant: the invariance argument does not hold as stated and the\n")
    cat("within-municipality variation must be described before relying on it.\n")
}


# =============================== PART 4 ======================================
# Municipalities holding more types have less discretion left: with all six,
# there is no portfolio choice to make. If the interaction is stable as breadth
# increases, selection across types is not what drives it.
cat("\n\n=== PART 4: stability across portfolio breadth ===\n\n")

set_ref_last <- function(x, ref = REF_TYPE) {
    lv <- levels(factor(x))
    factor(x, levels = c(setdiff(lv, ref), ref))
}

FE_RHS <- paste("factor(tipo_scelta_contraente) + factor(year) +",
                "log_importo_complessivo_gara + type +",
                "log_digital_expenditure:type + strata(codice_ipa)")

breadth_fit <- function(dat, min_types, phase_t, phase_s) {
    d <- dat %>% group_by(codice_ipa) %>%
        filter(n_distinct(type) >= min_types) %>% ungroup() %>%
        mutate(type = set_ref_last(type))
    if (nrow(d) < 500 || n_distinct(d$codice_ipa) < 100) return(NULL)
    m <- tryCatch(
        coxph(as.formula(sprintf("Surv(%s, %s) ~ %s", phase_t, phase_s, FE_RHS)),
              data = d),
        error = function(e) NULL, warning = function(w) NULL)
    if (is.null(m)) {
        m <- suppressWarnings(coxph(
            as.formula(sprintf("Surv(%s, %s) ~ %s", phase_t, phase_s, FE_RHS)), data = d))
    }
    s <- summary(m)$coefficients
    keep <- grep("log_digital_expenditure", rownames(s), value = TRUE)
    data.frame(min_types = min_types,
               projects = nrow(d), municipalities = n_distinct(d$codice_ipa),
               type = gsub("^type|:log_digital_expenditure$|^log_digital_expenditure:type", "",
                           keep),
               est = round(as.numeric(s[keep, "coef"]), 4),
               se = round(as.numeric(s[keep, "se(coef)"]), 4),
               p = as.numeric(s[keep, "Pr(>|z|)"]),
               stringsAsFactors = FALSE, row.names = NULL) %>%
        filter(!is.na(est))
}

# completion phase, the one the abstract leads on
res4 <- bind_rows(lapply(2:5, function(k)
    breadth_fit(fe_comp, k, "completion", "execution_status")))

if (nrow(res4)) {
    wide <- res4 %>%
        mutate(sig = stars(p)) %>%
        mutate(cell = sprintf("%.4f%s", est, sig)) %>%
        select(type, min_types, cell) %>%
        tidyr::pivot_wider(names_from = min_types, values_from = cell,
                           names_prefix = ">=")
    cat("Completion phase, stratified Cox, by minimum portfolio breadth:\n")
    cat("(estimate and significance; se is in the written CSV, not this console view)\n\n")
    print(as.data.frame(wide), row.names = FALSE)
    cat("\nSample sizes:\n")
    print(as.data.frame(res4 %>% distinct(min_types, projects, municipalities)),
          row.names = FALSE)
    write.csv(res4, "outputs/selection_breadth.csv", row.names = FALSE)
} else {
    cat("Not enough municipalities at higher breadth thresholds to estimate.\n")
}

# POS/NEG define the internal-contrast subsample: municipalities holding
# both a type with a positive interaction and one with a negative
# interaction, where the contrast is internal by construction and no
# portfolio choice can generate it. Used by Part 5 below, which runs this
# check across all three phases.
POS <- c("Citizen Experience", "Cloud")
NEG <- c("Digital Notices")


# =============================================================================
# PART 5 - INTERNAL-CONTRAST SUBSAMPLE (TABLE B10), ALL THREE PHASES
#
# Municipalities holding both a positive-interaction type (POS) and a
# negative-interaction type (NEG, both defined in Part 4 above) - the
# contrast is internal by construction there, so portfolio selection cannot
# explain it. Run across all three phases, not completion alone.
# =============================================================================

fit_internal_contrast <- function(dat, phase_t, phase_s, phase_label) {
    both <- dat %>% group_by(codice_ipa) %>%
        filter(any(type %in% POS), any(type %in% NEG)) %>% ungroup() %>%
        mutate(type = set_ref_last(type))
    cat(sprintf("\n%s - projects: %s | municipalities: %s\n", phase_label,
                format(nrow(both), big.mark = ","),
                format(n_distinct(both$codice_ipa), big.mark = ",")))
    if (nrow(both) < 500 || n_distinct(both$codice_ipa) < 100) {
        cat("  Too few municipalities to estimate.\n")
        return(NULL)
    }
    m <- suppressWarnings(coxph(
        as.formula(sprintf("Surv(%s, %s) ~ %s", phase_t, phase_s, FE_RHS)), data = both))
    s <- summary(m)$coefficients
    keep <- grep("log_digital_expenditure", rownames(s), value = TRUE)
    data.frame(phase = phase_label,
               type = gsub("^type|:log_digital_expenditure$|^log_digital_expenditure:type", "", keep),
               est = round(as.numeric(s[keep, "coef"]), 4),
               se = round(as.numeric(s[keep, "se(coef)"]), 4),
               sig = stars(as.numeric(s[keep, "Pr(>|z|)"])),
               stringsAsFactors = FALSE) %>%
        filter(!is.na(est))
}

contrast_all <- bind_rows(
    fit_internal_contrast(fe_award, AWARD_TIME, AWARD_STATUS, "Award"),
    fit_internal_contrast(fe_exec, "execution", "execution_status", "Execution"),
    fit_internal_contrast(fe_comp, "completion", "execution_status", "Completion")
)

cat("\n=== Internal contrast, all three phases ===\n\n")
print(as.data.frame(contrast_all), row.names = FALSE)
write.csv(contrast_all, "outputs/selection_internal_contrast_all_phases.csv", row.names = FALSE)
