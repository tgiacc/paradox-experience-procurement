# =============================================================================
# SELECTION INTO COMPLEX PROJECTS - WITHIN-TYPE TESTS CITED IN THE R2.3 RESPONSE
#
# Run AFTER giq_r2_analysis.R (needs df_cup, d_award, d_exec, d_comp, RHS,
# AWARD_TIME, AWARD_STATUS, REF_TYPE, TYPES, stars(), cluster_vcov()).
#
# WHY THIS EXISTS
#   Reviewer #2's "selection into complex projects" is best read as selection
#   WITHIN solution type: a more experienced municipality may attempt a more
#   ambitious project of a given kind, so the interaction would reflect what was
#   attempted rather than how experience operated. The response letter cites two
#   tests for this, packaged here so the numbers quoted to reviewers are
#   reproducible from the public repository.
#
#   PART 1  fragmentation: does the number of contracts per project account for
#           the interaction?
#   PART 2  scope: does experience predict how much contract value a project
#           mobilises per euro of grant, and does that differential follow the
#           duration pattern? Leaner specification (Tables 4/6 minus DTO,
#           payment time, workforce composition, procedure).
#   PART 2b Table B9: the same regression with the full Table 4/6 covariate
#           set, at the cost of a smaller sample. The letter quotes this
#           version, since it is the one directly comparable to the main
#           duration models.
#   PART 3  reconciliation against the figures quoted in the letter (checked
#           against the Part 2b/Table B9 numbers, not Part 2's)
#
# WHY THIS VERSION DOES NOT USE THE GLOBAL `RHS`
#   The diff-in-disc scripts define an object called RHS for their own formula,
#   so a session in which those have been run holds "above * treat + ..." under
#   that name. survreg then fails with `object 'above' not found`. This version
#   builds its own MAIN_RHS and checks it before estimating anything, rather
#   than trusting a name that another script may own.
#
# ONE THING TO DECIDE BEFORE QUOTING P-VALUES
#   The scope regression was originally run with classical OLS standard errors,
#   and those are the p-values currently in the letter. Everything else in the
#   paper clusters at the municipality level, and projects are nested within
#   municipalities here too. PART 2 reports both. If clustering changes which
#   coefficients are significant, the letter must be updated to the clustered
#   ones, not the other way round.
# =============================================================================

library(dplyr)
library(survival)
library(sandwich)
library(lmtest)

dir.create("outputs", showWarnings = FALSE)

stopifnot(exists("df_cup"), exists("d_comp"))

# --- the manuscript's specification, defined here rather than inherited -------
MAIN_RHS <- paste(
    "factor(tipo_scelta_contraente) + factor(pop_cluster) + factor(regione) +",
    "factor(year) + log_importo_complessivo_gara + log_past_expenditure +",
    "DTO + weighted_payment_time + u35_pct + female_pct +",
    "log_digital_expenditure * type"
)

# if the session already holds the paper's RHS, prefer it, but only after
# checking that it is the paper's and not another script's
if (exists("RHS") && is.character(RHS) && length(RHS) == 1 &&
    grepl("log_digital_expenditure", RHS) && !grepl("\\babove\\b", RHS)) {
    MAIN_RHS <- RHS
    cat("Using the RHS found in the session.\n")
} else {
    cat("Using the specification defined in this script",
        if (exists("RHS")) "(the RHS in the session belongs to another script)." else ".", "\n")
}

all_vars <- c("tipo_scelta_contraente", "pop_cluster", "regione", "year",
              "log_importo_complessivo_gara", "log_past_expenditure", "DTO",
              "weighted_payment_time", "u35_pct", "female_pct",
              "log_digital_expenditure", "type")
absent <- all_vars[!all_vars %in% names(d_comp)]
if (length(absent)) {
    cat("Missing from the estimation frames:\n"); print(absent)
    stop("Re-run giq_r2_analysis.R before this script.")
}

INT_TERMS <- paste0("log_digital_expenditure:type", TYPES[TYPES != REF_TYPE])

# =============================== PART 1 ======================================
# n_cig counts the contracts a project was split into. If experienced
# municipalities ran more fragmented projects and fragmentation drives duration,
# adding it should move the interaction. It does not.
cat("=== PART 1: fragmentation control ===\n\n")

PH <- list(
    Award      = list(t = AWARD_TIME,   s = AWARD_STATUS,       d = d_award),
    Execution  = list(t = "execution",  s = "execution_status", d = d_exec),
    Completion = list(t = "completion", s = "execution_status", d = d_comp)
)

frag <- bind_rows(lapply(names(PH), function(nm) {
    p <- PH[[nm]]
    f0 <- survreg(as.formula(sprintf("Surv(%s, %s) ~ %s", p$t, p$s, MAIN_RHS)),
                  data = p$d, dist = "weibull")
    f1 <- survreg(as.formula(sprintf("Surv(%s, %s) ~ %s + log_n_cig", p$t, p$s, MAIN_RHS)),
                  data = p$d, dist = "weibull")
    s0 <- summary(f0)$table; s1 <- summary(f1)$table
    keep <- intersect(INT_TERMS, rownames(s0))
    data.frame(phase = nm,
               type = sub("^log_digital_expenditure:type", "", keep),
               without = round(as.numeric(s0[keep, "Value"]), 4),
               se_without = round(as.numeric(s0[keep, "Std. Error"]), 4),
               p_without = as.numeric(s0[keep, "p"]),
               with_ncig = round(as.numeric(s1[keep, "Value"]), 4),
               se_with_ncig = round(as.numeric(s1[keep, "Std. Error"]), 4),
               p_with_ncig = as.numeric(s1[keep, "p"]),
               n_cig_coef = round(as.numeric(s1["log_n_cig", "Value"]), 4),
               n_cig_p = signif(as.numeric(s1["log_n_cig", "p"]), 3),
               stringsAsFactors = FALSE, row.names = NULL)
}))
frag$pct_change <- round(100 * (frag$with_ncig - frag$without) / abs(frag$without), 1)
frag$sig_without <- stars(frag$p_without)
frag$sig_with_ncig <- stars(frag$p_with_ncig)
frag$sig_changed <- (frag$p_without < 0.05) != (frag$p_with_ncig < 0.05)
print(as.data.frame(frag %>% select(phase, type, without, se_without, sig_without,
                                    with_ncig, se_with_ncig, sig_with_ncig,
                                    pct_change, sig_changed)), row.names = FALSE)
write.csv(frag, "outputs/complexity_fragmentation.csv", row.names = FALSE)
cat(sprintf("\nLargest movement in any interaction: %.1f%%\n",
            max(abs(frag$pct_change), na.rm = TRUE)))
cat("Quote this number rather than the word 'unchanged'.\n")
cat(sprintf("Any category crossing the 5%% significance threshold: %d of %d\n",
            sum(frag$sig_changed), nrow(frag)))

# =============================== PART 2 ======================================
# Scope, measured against a grant the municipality did not choose: NGEU M1C1
# pays lump sums by demographic band and solution type.
cat("\n\n=== PART 2: project scope per euro of grant ===\n\n")

if (!"importo_finanziamento" %in% names(df_cup)) {
    if (exists("df1_cig2") && "importo_finanziamento" %in% names(df1_cig2)) {
        j <- df1_cig2 %>% group_by(cup) %>%
            summarise(importo_finanziamento = first(importo_finanziamento),
                      .groups = "drop")
        df_cup <- left_join(df_cup, j, by = "cup")
        cat("importo_finanziamento joined from df1_cig2 by cup\n")
    } else stop("importo_finanziamento is not available.")
}

scope <- df_cup %>%
    filter(importo_gara_tot > 0, importo_finanziamento > 0,
           is.finite(log_past_expenditure),    log_past_expenditure    >= 0,
           is.finite(log_digital_expenditure), log_digital_expenditure >= 0) %>%
    mutate(log_ratio = log(importo_gara_tot / importo_finanziamento),
           type = relevel(factor(type), ref = REF_TYPE)) %>%
    filter(is.finite(log_ratio))

cat(sprintf("Scope sample: %s projects across %s municipalities\n",
            format(nrow(scope), big.mark = ","),
            format(n_distinct(scope$codice_ipa), big.mark = ",")))
cat(sprintf("Median contract value per euro of grant: %.2f\n",
            median(exp(scope$log_ratio))))

m_scope <- lm(log_ratio ~ factor(pop_cluster) + factor(regione) + factor(year) +
                  log_past_expenditure + log_digital_expenditure * type,
              data = scope)

keep <- grep("log_digital_expenditure", names(coef(m_scope)), value = TRUE)
ols <- summary(m_scope)$coefficients[keep, , drop = FALSE]
cl  <- coeftest(m_scope, vcov = vcovCL, cluster = scope$codice_ipa)[keep, , drop = FALSE]

out <- data.frame(
    term = sub("^log_digital_expenditure:type", "  x ", keep),
    estimate = round(ols[, 1], 4),
    pct_per_log_point = round(100 * ols[, 1], 2),
    se_ols = round(ols[, 2], 4), p_ols = signif(ols[, 4], 3),
    se_clustered = round(cl[, 2], 4), p_clustered = signif(cl[, 4], 3),
    stringsAsFactors = FALSE, row.names = NULL)
print(as.data.frame(out), row.names = FALSE)
write.csv(out, "outputs/complexity_scope.csv", row.names = FALSE)
cat(sprintf("\nR-squared: %.3f\n", summary(m_scope)$r.squared))

changed <- out$term[(out$p_ols < 0.05) != (out$p_clustered < 0.05)]
if (length(changed)) {
    cat("\nWARNING - clustering changes significance at 5% for:\n")
    print(changed)
    cat("Update the p-values in the response letter to the clustered column.\n")
} else {
    cat("\nClustering does not change which coefficients are significant at 5%.\n")
}

# Does the scope pattern line up with the duration pattern? If experienced
# municipalities bought more scope precisely where durations lengthen, scope
# would be a candidate explanation. The letter's claim is that it does not.
dur <- bind_rows(lapply(names(PH), function(nm) {
    p <- PH[[nm]]
    m <- survreg(as.formula(sprintf("Surv(%s, %s) ~ %s", p$t, p$s, MAIN_RHS)),
                 data = p$d, dist = "weibull")
    s <- summary(m)$table
    k <- intersect(INT_TERMS, rownames(s))
    data.frame(type = sub("^log_digital_expenditure:type", "", k),
               phase = nm, duration = as.numeric(s[k, "Value"]),
               stringsAsFactors = FALSE, row.names = NULL)
}))
align <- dur %>%
    group_by(type) %>% summarise(duration_mean = mean(duration), .groups = "drop") %>%
    left_join(data.frame(type = sub("^log_digital_expenditure:type", "", keep),
                         scope = as.numeric(ols[, 1]), stringsAsFactors = FALSE),
              by = "type") %>%
    filter(!is.na(scope))
cat("\nScope differential against mean duration interaction, by type:\n")
print(as.data.frame(align %>% mutate(across(c(duration_mean, scope), ~ round(.x, 4)))),
      row.names = FALSE)
if (nrow(align) > 2) {
    cat(sprintf("\nCorrelation across the five types: %.3f\n",
                cor(align$duration_mean, align$scope)))
    cat("A correlation near zero or negative supports the letter's reading: scope\n")
    cat("selection does not track the durations. A strong positive correlation would\n")
    cat("mean the opposite, and the letter would need rewriting.\n")
}

# =============================== PART 2b =====================================
# TABLE B9. The specification above omits DTO, payment time, workforce
# composition and procurement procedure, so its sample (30,791 projects,
# 7,461 municipalities) is not directly comparable to Tables 4 and 6. This
# adds those four covariates, matching Tables 4/6 exactly; the smaller sample
# that results is a genuine cost of comparability, not an error - see the
# municipality-count note below.
cat("\n\n=== PART 2b: project scope, full covariate set (Table B9) ===\n\n")

scope_full <- scope %>%
    filter(!is.na(DTO), !is.na(weighted_payment_time), !is.na(u35_pct),
           !is.na(female_pct), !is.na(tipo_scelta_contraente))

cat(sprintf("Scope sample, full specification: %s projects across %s municipalities\n",
            format(nrow(scope_full), big.mark = ","),
            format(n_distinct(scope_full$codice_ipa), big.mark = ",")))
cat("(smaller than Table A1's 7,047 municipalities because this regression has\n")
cat(" no duration requirement and so does not share the award models' own\n")
cat(" listwise deletion on AWARD_TIME)\n")

m_scope_full <- lm(log_ratio ~ factor(pop_cluster) + factor(regione) + factor(year) +
                      log_past_expenditure + log_digital_expenditure * type +
                      DTO + weighted_payment_time + u35_pct + female_pct +
                      factor(tipo_scelta_contraente),
                    data = scope_full)

keep_full <- grep("log_digital_expenditure", names(coef(m_scope_full)), value = TRUE)
cl_full <- coeftest(m_scope_full, vcov = vcovCL, cluster = scope_full$codice_ipa)[keep_full, , drop = FALSE]

out_full <- data.frame(
    term = sub("^log_digital_expenditure:type", "  x ", keep_full),
    estimate = round(cl_full[, 1], 4),
    pct_per_log_point = round(100 * cl_full[, 1], 2),
    se = round(cl_full[, 2], 4), p = signif(cl_full[, 4], 3),
    stringsAsFactors = FALSE, row.names = NULL)
print(as.data.frame(out_full), row.names = FALSE)
cat(sprintf("\nR-squared: %.3f\n", summary(m_scope_full)$r.squared))
write.csv(out_full, "outputs/tableB9_scope_full.csv", row.names = FALSE)

# =============================== PART 3 ======================================
# Letter figures now cite the full-covariate specification (Table B9), not
# the leaner one above - Tables 4/6 use the full covariate set, so the letter
# quotes the number that is directly comparable to them.
cat("\n\n=== PART 3: figures quoted in the response letter ===\n\n")
quoted <- data.frame(
    claim = c("scope, reference category (% per log point)",
              "scope differential, digital notices",
              "scope differential, digital services and payments",
              "scope differential, interoperability"),
    in_letter = c(0.84, 1.31, -1.03, -0.95),
    stringsAsFactors = FALSE)
lookup <- function(pat) {
    i <- grep(pat, out_full$term, fixed = TRUE)
    if (!length(i)) return(NA_real_)
    out_full$pct_per_log_point[i[1]]
}
quoted$now <- c(lookup("log_digital_expenditure"), lookup("Digital Notices"),
                lookup("Digital Services and Payments"), lookup("Interoperability"))
quoted$diff <- round(quoted$now - quoted$in_letter, 2)
print(as.data.frame(quoted), row.names = FALSE)
cat("\nAny row where `diff` is not close to zero means the letter and the code have\n")
cat("drifted apart. Fix the letter, not the table.\n")
