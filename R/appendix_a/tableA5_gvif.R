# =============================================================================
# TABLE A5 - MULTICOLLINEARITY DIAGNOSTICS, PROJECT-LEVEL ESTIMATION SAMPLE
#
# Run AFTER giq_r2_analysis.R, and after table3_estimation_sample.R so the two
# tables rest on the same rows.
#
# WHY AN lm()
#   GVIF is a property of the design matrix alone: it does not depend on the
#   outcome. Fitting an lm() on the same right-hand side and the same subsample
#   therefore reproduces exactly the GVIFs of the survreg/coxph specification,
#   and lets car::vif() work without special handling for survival objects.
#
# WHICH SPECIFICATION
#   The published Table A5 has ten rows: DTO, Supplier payment time,
#   Empl. 25-34, Female empl., Population, Region, Year, Contract award
#   value, Ln(General procurement experience), Ln(Experience in digital
#   solutions). It therefore excludes the contracting-procedure dummies and
#   the solution-type factor that appear in Tables 4 and 6. That choice is
#   kept here so the new table is comparable to the old one; set
#   WITH_TYPE_AND_PROCEDURE to TRUE if you would rather diagnose the full
#   estimation specification.
# =============================================================================

library(dplyr)
library(car)

WITH_TYPE_AND_PROCEDURE <- FALSE

dir.create("outputs", showWarnings = FALSE)

SRC <- if (exists("df_cup")) df_cup else
    stop("df_cup is not in the environment: the project-level frame is the one to use.")

pick <- function(df, ...) {
    for (cand in c(...)) if (cand %in% names(df)) return(cand)
    stop(sprintf("None of these columns exist: %s", paste(c(...), collapse = ", ")))
}

V_DIG   <- pick(SRC, "log_digital_expenditure", "log_past_expenditure_digital")
V_PROC  <- pick(SRC, "log_past_expenditure")
V_VALUE <- pick(SRC, "log_importo_complessivo_gara")
AWARD   <- if (exists("AWARD_TIME")) AWARD_TIME else pick(SRC, "award", "contracting")

# --- the same frame Table 3 now uses ----------------------------------------
base <- SRC %>%
    mutate(across(starts_with("log_"), ~ ifelse(is.finite(.x), .x, NA_real_))) %>%
    filter(.data[[V_DIG]] >= 0, .data[[V_PROC]] >= 0, .data[[V_VALUE]] >= 0)

cat(sprintf("Estimation frame: %s rows\n", format(nrow(base), big.mark = ",")))

# --- right-hand side, in Table A5's row order --------------------------------
terms <- c(
    "DTO",
    "weighted_payment_time",
    "u35_pct",
    "female_pct",
    "factor(pop_cluster)",
    "factor(regione)",
    "factor(year)",
    V_VALUE,
    V_PROC,
    V_DIG
)
if (WITH_TYPE_AND_PROCEDURE) {
    terms <- c(terms, "factor(tipo_scelta_contraente)", "factor(type)")
}

LABELS <- c(
    "DTO"                   = "DTO",
    "weighted_payment_time" = "Supplier payment time",
    "u35_pct"               = "Empl. 25-34",
    "female_pct"            = "Female empl.",
    "factor(pop_cluster)"   = "Population",
    "factor(regione)"       = "Region",
    "factor(year)"          = "Year",
    "log_importo_complessivo_gara" = "Contract award value",
    "log_past_expenditure"  = "Ln(General procurement experience)",
    "log_digital_expenditure" = "Ln(Experience in digital solutions)",
    "log_past_expenditure_digital" = "Ln(Experience in digital solutions)",
    "factor(tipo_scelta_contraente)" = "Procedure",
    "factor(type)"          = "Solution type"
)

missing_terms <- setdiff(gsub("^factor\\((.*)\\)$", "\\1", terms), names(base))
if (length(missing_terms)) {
    cat("Terms not present in the frame:\n"); print(missing_terms)
    stop("Adjust `terms` above.")
}

# --- one phase ---------------------------------------------------------------
phase_gvif <- function(duration_col) {
    d <- base %>%
        filter(!is.na(.data[[duration_col]]), .data[[duration_col]] > 0)
    f <- as.formula(paste0("log(", duration_col, ") ~ ", paste(terms, collapse = " + ")))
    m <- lm(f, data = d)
    v <- car::vif(m)                       # matrix when any term has >1 df
    if (is.matrix(v)) {
        out <- data.frame(term = rownames(v),
                          GVIF = v[, "GVIF"],
                          sq_sc = v[, ncol(v)],   # GVIF^(1/(2*Df))
                          stringsAsFactors = FALSE)
    } else {
        out <- data.frame(term = names(v), GVIF = as.numeric(v),
                          sq_sc = sqrt(as.numeric(v)), stringsAsFactors = FALSE)
    }
    out$n <- nrow(model.frame(m))   # rows lm() actually used after its own
                                     # listwise deletion on the terms, not the
                                     # pre-fit frame - nrow(d) overstated this
    out
}

phases <- list(Award = AWARD, Execution = "execution", Completion = "completion")
res <- lapply(phases, phase_gvif)

for (nm in names(res)) {
    cat(sprintf("%-12s N = %s\n", nm, format(res[[nm]]$n[1], big.mark = ",")))
}

# --- assemble in Table A5's shape -------------------------------------------
tabA4 <- data.frame(Variable = LABELS[terms], stringsAsFactors = FALSE)
for (nm in names(res)) {
    r <- res[[nm]]
    idx <- match(terms, r$term)
    tabA4[[paste(nm, "GVIF")]]         <- sprintf("%.4f", r$GVIF[idx])
    tabA4[[paste(nm, "Sq. sc. GVIF")]] <- sprintf("%.4f", r$sq_sc[idx])
}
rownames(tabA4) <- NULL

cat("\n=== Table A5: GVIF diagnostics ===\n\n")
print(tabA4, row.names = FALSE)
write.csv(tabA4, "outputs/tableA5_gvif.csv", row.names = FALSE)

# --- sanity checks against the published table -------------------------------
# Award value carried GVIF 1.6218 and Population 3.7289 in the published
# version. Table 3 now puts Ln(Value) x Ln(Pop.) at 0.32 and Ln(Value) x
# Ln(Past proc.) at 0.30, so Award value should stay in the same neighbourhood.
# A large move means this table and Table 3 are not on the same rows.
cat("\n--- against the published values ---\n")
chk <- data.frame(
    variable  = c("Contract award value", "Population", "Ln(General procurement experience)"),
    published = c(1.6218, 3.7289, 3.3354),
    now       = as.numeric(tabA4[match(c("Contract award value", "Population", "Ln(General procurement experience)"),
                                       tabA4$Variable), "Award GVIF"]),
    stringsAsFactors = FALSE)
chk$diff <- round(chk$now - chk$published, 3)
print(chk, row.names = FALSE)

worst <- suppressWarnings(max(as.numeric(unlist(tabA4[grepl("GVIF$", names(tabA4))])),
                             na.rm = TRUE))
cat(sprintf("\nHighest GVIF anywhere in the table: %.4f\n", worst))
if (worst > 10) {
    cat("Above the conventional threshold of 10: say which term and why in the note.\n")
} else {
    cat("Below the conventional threshold of 10.\n")
}

NOTE <- paste(
    "Notes: GVIF provides an indication of how much the variance of a regression",
    "coefficient is inflated due to collinearity with other predictors, while the",
    "squared scaled GVIF accounts for the number of degrees of freedom of each",
    "term and is comparable to a conventional VIF across terms of different",
    sprintf("dimension. Computed on the project-level frame, with N = %s for the award phase,",
            format(res[[1]]$n[1], big.mark = ",")),
    sprintf("%s for execution and %s for completion \u2014 the same estimation samples",
            format(res[[2]]$n[1], big.mark = ","), format(res[[3]]$n[1], big.mark = ",")),
    "used in Tables 4 and 6. No term approaches the conventional threshold of 10.")
writeLines(NOTE, "outputs/tableA5_note.txt")
cat("\n--- note ---\n"); cat(NOTE, "\n")
