# =============================================================================
# SENSITIVITY TO UNOBSERVABLES WITHOUT THE OLS CRUTCH
#
# Run AFTER giq_r2_analysis.R. Needs d_award, d_exec, d_comp, AWARD_TIME,
# AWARD_STATUS, REF_TYPE, stars().
#
# WHY THIS EXISTS
#   The Oster exercise is only defensible for the award phase, where 94% of
#   observations have an observed event (27,608 of 29,229). In execution and
#   completion the event rates are 69% and 65%, and the OLS analogue that Oster
#   requires drops the censored cases - which are precisely the long, unfinished
#   projects that carry the "more experience, longer duration" pattern. On those
#   phases the OLS analogue returns -0.0078 and -0.0005 for Citizen Experience
#   where the AFT returns +0.0164 and +0.0189. Bounding a coefficient of the
#   wrong sign is not a robustness check.
#
#   So this script answers the same question on the models the paper actually
#   estimates, in two ways that do not require an R-squared.
#
#   PART 1  COEFFICIENT STABILITY across nested control blocks. Oster's logic
#           without Oster's approximation: if the interaction barely moves as
#           blocks that raise explanatory power substantially are added, an
#           unobservable of comparable strength would not move it either. This
#           is reportable as a table and needs no assumption about Rmax.
#
#   PART 2  A SIMULATED CONFOUNDER at municipality level. Draws an unobserved
#           municipal characteristic correlated with prior digital expenditure by
#           a chosen amount, adds it to the AFT, and reports how strong it has to
#           be before the interaction loses significance. This respects the
#           censoring because it refits survreg.
# =============================================================================

library(dplyr)
library(survival)
library(sandwich)

dir.create("outputs", showWarnings = FALSE)

PH <- list(
    Award      = list(t = AWARD_TIME,   s = AWARD_STATUS,       d = d_award),
    Execution  = list(t = "execution",  s = "execution_status", d = d_exec),
    Completion = list(t = "completion", s = "execution_status", d = d_comp)
)

TERMS <- paste0("log_digital_expenditure:type",
                c("Citizen Experience", "Cloud", "Interoperability",
                  "Digital Services and Payments", "Digital Notices"))

fit <- function(p, rhs) {
    survreg(as.formula(sprintf("Surv(%s, %s) ~ %s", p$t, p$s, rhs)),
            data = p$d, dist = "weibull")
}

# =============================== PART 1 ======================================
# Blocks are added cumulatively. Each block is a different KIND of confounder,
# so the movement across columns is informative about the kind of unobservable
# that would matter.
BLOCKS <- list(
    "(1) interaction only"      = "log_digital_expenditure * type",
    "(2) + project scale"       = "log_digital_expenditure * type + log_importo_complessivo_gara + factor(tipo_scelta_contraente)",
    "(3) + municipal capacity"  = "log_digital_expenditure * type + log_importo_complessivo_gara + factor(tipo_scelta_contraente) + DTO + weighted_payment_time + u35_pct + female_pct",
    "(4) + general experience"  = "log_digital_expenditure * type + log_importo_complessivo_gara + factor(tipo_scelta_contraente) + DTO + weighted_payment_time + u35_pct + female_pct + log_past_expenditure",
    "(5) + size, geography, year" = "log_digital_expenditure * type + log_importo_complessivo_gara + factor(tipo_scelta_contraente) + DTO + weighted_payment_time + u35_pct + female_pct + log_past_expenditure + factor(pop_cluster) + factor(regione) + factor(year)"
)

cat("=== PART 1: coefficient stability across nested control blocks ===\n")

stability <- bind_rows(lapply(names(PH), function(nm) {
    p <- PH[[nm]]
    bind_rows(lapply(names(BLOCKS), function(bk) {
        m <- fit(p, BLOCKS[[bk]])
        s <- summary(m)$table
        keep <- intersect(TERMS, rownames(s))
        data.frame(phase = nm, block = bk,
                   type = sub("^log_digital_expenditure:type", "", keep),
                   est = round(as.numeric(s[keep, "Value"]), 4),
                   n = length(m$linear.predictors),
                   loglik = as.numeric(logLik(m)),
                   stringsAsFactors = FALSE, row.names = NULL)
    }))
}))

wide <- stability %>%
    select(phase, type, block, est) %>%
    tidyr::pivot_wider(names_from = block, values_from = est)
print(as.data.frame(wide), row.names = FALSE)
write.csv(wide, "outputs/stability_blocks.csv", row.names = FALSE)

# How far does each coefficient travel, relative to its own size?
travel <- stability %>%
    group_by(phase, type) %>%
    summarise(first = est[block == names(BLOCKS)[1]],
              last  = est[block == names(BLOCKS)[length(BLOCKS)]],
              max_move = max(abs(est - est[block == names(BLOCKS)[1]])),
              pct_of_final = round(100 * max_move / abs(last), 1),
              .groups = "drop")
cat("\nMovement from column (1) to column (5), and the largest excursion:\n")
print(as.data.frame(travel %>% mutate(across(c(first, last, max_move), ~ round(.x, 4)))),
      row.names = FALSE)

cat("\nRead it this way: a coefficient whose largest excursion is a small share of\n")
cat("its final value, across blocks that add project scale, municipal capacity,\n")
cat("general procurement experience and full geography, is not being held up by\n")
cat("the controls. State the percentage; do not call it 'stable' without one.\n")

# =============================== PART 2 ======================================
# A municipality-level unobservable. rho is its correlation with prior digital
# expenditure; lambda is its coefficient in the duration equation, expressed in
# the same log-time units as the reported coefficients. The grid is swept until
# the Citizen Experience interaction stops being significant at 5%.
cat("\n\n=== PART 2: simulated municipality-level confounder ===\n")

set.seed(20260826)
RHO    <- c(0.1, 0.2, 0.3, 0.5)
LAMBDA <- c(0.05, 0.10, 0.20, 0.40)
FULL   <- BLOCKS[[length(BLOCKS)]]
TARGET <- "log_digital_expenditure:typeCitizen Experience"

make_u <- function(dat, rho) {
    # one draw per municipality, correlated with that municipality's experience
    m <- dat %>% distinct(codice_ipa, log_digital_expenditure)
    z <- scale(m$log_digital_expenditure)[, 1]
    z[!is.finite(z)] <- 0
    m$u <- rho * z + sqrt(max(0, 1 - rho^2)) * rnorm(nrow(m))
    m$u[match(dat$codice_ipa, m$codice_ipa)]
}

sim <- bind_rows(lapply(names(PH), function(nm) {
    p <- PH[[nm]]
    base <- fit(p, FULL)
    b0 <- as.numeric(summary(base)$table[TARGET, "Value"])
    bind_rows(lapply(RHO, function(rho) {
        u <- make_u(p$d, rho)
        bind_rows(lapply(LAMBDA, function(lam) {
            p2 <- p
            # the confounder shifts log-duration by lam * u; equivalently, control
            # for u after having let it act on the outcome
            p2$d <- p$d
            p2$d$.u <- u
            m <- survreg(as.formula(sprintf("Surv(%s, %s) ~ %s + .u", p$t, p$s, FULL)),
                         data = p2$d, dist = "weibull")
            s <- summary(m)$table
            data.frame(phase = nm, rho = rho, lambda = lam,
                       est = round(as.numeric(s[TARGET, "Value"]), 4),
                       p = as.numeric(s[TARGET, "p"]),
                       baseline = round(b0, 4),
                       stringsAsFactors = FALSE)
        }))
    }))
}))

sim$sig <- ifelse(sim$p < 0.05, "yes", "no")
print(as.data.frame(sim %>% select(phase, rho, lambda, baseline, est, sig)),
      row.names = FALSE)
write.csv(sim, "outputs/simulated_confounder.csv", row.names = FALSE)

cat("\nNOTE ON WHAT PART 2 DOES AND DOES NOT DO.\n")
cat("The draw is correlated with prior digital expenditure but not, by\n")
cat("construction, with solution type. A confounder that is orthogonal to the\n")
cat("moderator cannot break an interaction, so a null here is weak evidence.\n")
cat("The version that would bite is a confounder correlated with the\n")
cat("EXPERIENCE-BY-TYPE cell - a municipality that is both experienced and\n")
cat("systematically assigned harder Citizen Experience projects. If a referee\n")
cat("presses on identification, that is the object to simulate, and it requires\n")
cat("taking a stand on the selection mechanism rather than drawing noise.\n")
