# =============================================================================
# OSTER (2019) COEFFICIENT-STABILITY BOUNDS FOR THE INTERACTION TERMS  (v3)
#
# v3 fixes the failure in v2. `coef(m)[".x"]` returns a NAMED vector, the name
# propagates through the arithmetic, and c(beta = ..., delta = ...) then produces
# elements called "beta..x" and "delta..x", so r13[["beta"]] was out of bounds.
# Both coefficients are now stripped to plain numerics before use.
#
# WHAT THIS ANSWERS
#   R2.3 raises omitted variable bias. Oster's framework asks how important
#   unobserved confounders would have to be, relative to the observed controls,
#   to drive the estimate to zero. delta = 1 means "as important as everything
#   already controlled for"; |delta| > 1 is conventionally read as reassuring.
#
# TWO CAVEATS FOR THE PAPER
#   1. Oster's derivation is for OLS; the models are AFT and Cox. This runs the
#      OLS analogue on observed events only, so censoring is dropped rather than
#      modelled. It is a diagnostic about selection on unobservables.
#   2. The estimand is an INTERACTION. The main effect of prior activity and the
#      solution-type dummies stay in both regressions (Oster's `w`), so delta
#      refers to the differential slope, which is what the paper claims.
# =============================================================================

library(dplyr)

dir.create("outputs", showWarnings = FALSE)

if (!exists("df_cup")) stop("df_cup is not in the environment.")
SRC <- df_cup

pick <- function(...) {
    for (cand in c(...)) if (cand %in% names(SRC)) return(cand)
    NA_character_
}

COLS <- list(
    type      = pick("type", "tech", "solution_type"),
    year      = pick("year", "anno"),
    regione   = pick("regione", "region"),
    procedure = pick("tipo_scelta_contraente", "procedure"),
    pop_band  = pick("pop_cluster"),
    DTO       = pick("DTO"),
    payment   = pick("weighted_payment_time"),
    u35       = pick("u35_pct"),
    female    = pick("female_pct"),
    lvalue    = pick("log_importo_complessivo_gara"),
    lproc     = pick("log_past_expenditure"),
    ldig      = pick("log_digital_expenditure", "log_past_expenditure_digital"),
    dur_award = pick(if (exists("AWARD_TIME")) AWARD_TIME else "award", "award", "contracting"),
    dur_exec  = pick("execution"),
    dur_comp  = pick("completion"),
    ev_award  = pick("contractual_status", "award_status"),
    ev_exec   = pick("execution_status")
)

absent <- names(COLS)[is.na(unlist(COLS))]
if (length(absent)) {
    cat("These columns could not be found in df_cup:\n"); print(absent)
    cat("\nColumns available:\n"); print(sort(names(SRC)))
    stop("Map the missing names in COLS above, then re-run.")
}

WK <- data.frame(
    type      = as.character(SRC[[COLS$type]]),
    year      = SRC[[COLS$year]],
    regione   = SRC[[COLS$regione]],
    procedure = SRC[[COLS$procedure]],
    pop_band  = SRC[[COLS$pop_band]],
    DTO       = SRC[[COLS$DTO]],
    payment   = SRC[[COLS$payment]],
    u35       = SRC[[COLS$u35]],
    female    = SRC[[COLS$female]],
    lvalue    = SRC[[COLS$lvalue]],
    lproc     = SRC[[COLS$lproc]],
    ldig      = SRC[[COLS$ldig]],
    dur_award = SRC[[COLS$dur_award]],
    dur_exec  = SRC[[COLS$dur_exec]],
    dur_comp  = SRC[[COLS$dur_comp]],
    ev_award  = SRC[[COLS$ev_award]],
    ev_exec   = SRC[[COLS$ev_exec]],
    stringsAsFactors = FALSE
) %>%
    mutate(across(c(lvalue, lproc, ldig), ~ ifelse(is.finite(.x), .x, NA_real_)))

REF   <- "Digital Identity"
TYPES <- c("Citizen Experience", "Cloud", "Interoperability",
           "Digital Services and Payments", "Digital Notices")

if (!all(c(REF, TYPES) %in% unique(WK$type))) {
    cat("Solution-type labels in df_cup:\n"); print(sort(unique(WK$type)))
    stop("Adjust REF / TYPES to the labels above.")
}

PHASES <- list(
    Award      = c("dur_award", "ev_award"),
    Execution  = c("dur_exec",  "ev_exec"),
    Completion = c("dur_comp",  "ev_exec")
)

W_TERMS   <- c("ldig", "factor(type)")
CON_TERMS <- c("DTO", "payment", "u35", "female", "factor(pop_band)",
               "factor(regione)", "factor(year)", "factor(procedure)",
               "lvalue", "lproc")
RMAX_RULE <- 1.3

# --- the fix: every argument arrives unnamed, so the result keeps its own names
oster <- function(b0, r0, bt, rt, rmax, delta = 1) {
    b0 <- as.numeric(b0); bt <- as.numeric(bt)
    r0 <- as.numeric(r0); rt <- as.numeric(rt); rmax <- as.numeric(rmax)
    if (!is.finite(rt - r0) || abs(rt - r0) < 1e-12 || !is.finite(rmax - rt)) {
        return(c(beta = NA_real_, delta = NA_real_))
    }
    c(beta  = bt - delta * (b0 - bt) * (rmax - rt) / (rt - r0),
      delta = bt * (rt - r0) / ((b0 - bt) * (rmax - rt)))
}

run_phase <- function(phase) {
    dur <- PHASES[[phase]][1]; evt <- PHASES[[phase]][2]
    d <- WK %>%
        filter(ldig >= 0, lproc >= 0, lvalue >= 0,
               .data[[dur]] > 0, .data[[evt]] == 1,
               type %in% c(TYPES, REF)) %>%
        mutate(type = relevel(factor(type), ref = REF),
               .y = log(.data[[dur]]))

    bind_rows(lapply(TYPES, function(ty) {
        d2 <- d
        d2$.x <- d2$ldig * as.numeric(d2$type == ty)

        f0 <- as.formula(paste(".y ~ .x +", paste(W_TERMS, collapse = " + ")))
        ft <- as.formula(paste(".y ~ .x +", paste(c(W_TERMS, CON_TERMS), collapse = " + ")))

        keep <- complete.cases(d2[, c(".y", ".x", "ldig", "type", "DTO", "payment",
                                      "u35", "female", "pop_band", "regione", "year",
                                      "procedure", "lvalue", "lproc")])
        d2 <- d2[keep, ]

        m0 <- lm(f0, data = d2); mt <- lm(ft, data = d2)
        b0 <- as.numeric(coef(m0)[".x"]); r0 <- summary(m0)$r.squared
        bt <- as.numeric(coef(mt)[".x"]); rt <- summary(mt)$r.squared

        r13 <- oster(b0, r0, bt, rt, RMAX_RULE * rt)
        r10 <- oster(b0, r0, bt, rt, 1.0)

        data.frame(phase = phase, type = ty, n = nobs(mt),
                   beta_uncontrolled = b0, R2_uncontrolled = r0,
                   beta_controlled = bt, R2_controlled = rt,
                   beta_star_13 = as.numeric(r13["beta"]),
                   delta_13     = as.numeric(r13["delta"]),
                   beta_star_10 = as.numeric(r10["beta"]),
                   delta_10     = as.numeric(r10["delta"]),
                   sign_preserved_13 = sign(as.numeric(r13["beta"])) == sign(bt),
                   stringsAsFactors = FALSE, row.names = NULL)
    }))
}

res <- bind_rows(lapply(names(PHASES), run_phase))
write.csv(res, "outputs/oster_bounds.csv", row.names = FALSE)

cat("=== Oster bounds, Rmax = 1.3 x R2(controlled), delta = 1 ===\n\n")
print(res %>% transmute(phase, type, n,
        `beta (uncontr.)` = round(beta_uncontrolled, 4),
        `beta (contr.)`   = round(beta_controlled, 4),
        `beta*`           = round(beta_star_13, 4),
        `delta at 0`      = round(delta_13, 2),
        `sign kept`       = sign_preserved_13) %>%
      as.data.frame(), row.names = FALSE)

cat("\n=== same, Rmax = 1 (most demanding benchmark) ===\n\n")
print(res %>% transmute(phase, type, `beta*` = round(beta_star_10, 4),
                        `delta at 0` = round(delta_10, 2)) %>%
      as.data.frame(), row.names = FALSE)

cat(sprintf("\nR2 controlled %.3f-%.3f | uncontrolled %.3f-%.3f\n",
            min(res$R2_controlled), max(res$R2_controlled),
            min(res$R2_uncontrolled), max(res$R2_uncontrolled)))

ok <- res %>% filter(abs(delta_13) > 1, sign_preserved_13)
cat(sprintf("\n|delta| > 1 with sign preserved: %d of %d interactions\n",
            nrow(ok), nrow(res)))
bad <- res %>% filter(!(abs(delta_13) > 1 & sign_preserved_13))
if (nrow(bad)) {
    cat("Not robust at delta = 1 - name these, do not average over them:\n")
    print(bad %>% transmute(phase, type, delta = round(delta_13, 2),
                            beta_star = round(beta_star_13, 4)) %>%
          as.data.frame(), row.names = FALSE)
}

cat("\nNOTE for the paper: OLS analogues on observed events only. Censored\n")
cat("observations are dropped, so the bounds speak to selection on unobservables,\n")
cat("not to the censoring correction handled by the AFT models.\n")
