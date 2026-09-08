# =============================================================================
# TABLE B5 - SIGNIFICANCE FOR THE COMMON-REFERENCE CONTRASTS  (v2)
#
# Changes against v1
#   * coefficient names are SEARCHED per model, not built with paste0. In the
#     stratified models the interaction terms carry the factors in the opposite
#     order, which is why v1 returned NA for all six stratified columns.
#   * the probe now runs on EVERY model, not just the first one, and prints the
#     matched name for each cell so an unmatched term can never pass silently.
#
# Run AFTER giq_r2_analysis.R, in the SAME session: needs the coxph objects.
# =============================================================================

library(dplyr)

STARS <- function(p) {
    ifelse(is.na(p), "",
    ifelse(p < 0.001, "***",
    ifelse(p < 0.01,  "**",
    ifelse(p < 0.05,  "*",
    ifelse(p < 0.1,   ".", "")))))
}

TYPES <- c("Citizen Experience", "Cloud", "Interoperability",
           "Digital Services and Payments", "Digital Notices")
REF   <- "Digital Identity"
INTERACT <- "log_digital_expenditure"

MODELS <- list(
    "Award / no strata"       = "m_award_nofe",
    "Award / stratified"      = "m_award_fe",
    "Execution / no strata"   = "m_exec_nofe",
    "Execution / stratified"  = "m_exec_fe",
    "Completion / no strata"  = "m_comp_nofe",
    "Completion / stratified" = "m_comp_fe"
)

absent <- Filter(function(nm) !exists(nm), unlist(MODELS))
if (length(absent)) {
    cat("Model objects not in the environment:\n"); print(unname(absent))
    cat("\ncoxph objects currently available:\n")
    print(Filter(function(o) inherits(get(o), "coxph"), ls(envir = .GlobalEnv)))
    stop("Re-run Part 8 of giq_r2_analysis.R, or edit MODELS above.")
}

# --- find the interaction coefficient for a type, whatever the term order ----
# Returns NA_character_ when the type is the model's own base level: its
# coefficient is then implicitly zero, which is a valid contrast, not an error.
find_coef <- function(b_names, type) {
    hits <- b_names[
        grepl(":", b_names, fixed = TRUE) &
        grepl(INTERACT, b_names, fixed = TRUE) &
        grepl(type, b_names, fixed = TRUE)
    ]
    # "Digital Notices" must not also match "Digital Services and Payments"
    if (length(hits) > 1) {
        others <- setdiff(c(TYPES, REF), type)
        hits <- hits[!sapply(hits, function(h)
            any(sapply(others, function(o) grepl(o, h, fixed = TRUE) && nchar(o) > nchar(type))))]
    }
    if (length(hits) > 1) {
        stop(sprintf("Ambiguous match for '%s': %s", type, paste(hits, collapse = ", ")))
    }
    if (!length(hits)) NA_character_ else hits
}

# --- diagnostic: what matched, in every model --------------------------------
cat("=== coefficient matching ===\n")
for (col in names(MODELS)) {
    b <- coef(get(MODELS[[col]])); b <- b[!is.na(b)]
    cat(sprintf("\n%s  (%s)\n", col, MODELS[[col]]))
    for (ty in c(TYPES, REF)) {
        nm <- find_coef(names(b), ty)
        cat(sprintf("   %-32s %s\n", ty,
                    if (is.na(nm)) "-- base level, implicit 0" else nm))
    }
    inter <- grep(":", names(b), value = TRUE)
    inter <- inter[grepl(INTERACT, inter, fixed = TRUE)]
    unmatched <- setdiff(inter, na.omit(sapply(c(TYPES, REF), function(t) find_coef(names(b), t))))
    if (length(unmatched)) {
        cat("   unmatched interaction terms present in the model:\n")
        cat(paste0("     ", unmatched, collapse = "\n"), "\n")
    }
}

# --- clustering check --------------------------------------------------------
for (nm in unlist(MODELS)) {
    m <- get(nm)
    if (is.null(m$call$cluster) && !isTRUE(m$call$robust)) {
        warning(sprintf(
            "%s fitted without cluster/robust: vcov is model-based, not clustered at municipality level.",
            nm), call. = FALSE)
    }
}

# --- delta method on the contrast beta_type - beta_ref -----------------------
contrast <- function(m, type, ref = REF) {
    b <- coef(m); b <- b[!is.na(b)]
    V <- vcov(m)[names(b), names(b), drop = FALSE]

    cv <- setNames(numeric(length(b)), names(b))
    nm_j <- find_coef(names(b), type)
    nm_r <- find_coef(names(b), ref)
    if (!is.na(nm_j)) cv[nm_j] <-  1
    if (!is.na(nm_r)) cv[nm_r] <- -1

    if (all(cv == 0)) return(c(est = NA_real_, se = NA_real_, p = NA_real_))
    est <- drop(cv %*% b)
    se  <- sqrt(drop(t(cv) %*% V %*% cv))
    c(est = est, se = se, p = 2 * pnorm(-abs(est / se)))
}

res <- do.call(rbind, lapply(names(MODELS), function(col) {
    m <- get(MODELS[[col]])
    do.call(rbind, lapply(TYPES, function(ty) {
        x <- contrast(m, ty)
        data.frame(column = col, type = ty, estimate = x[["est"]],
                   se = x[["se"]], p = x[["p"]], stars = STARS(x[["p"]]),
                   stringsAsFactors = FALSE)
    }))
}))
res$z <- res$estimate / res$se

if (any(is.na(res$estimate))) {
    cat("\nStill unresolved cells:\n")
    print(as.data.frame(res[is.na(res$estimate), c("column", "type")]), row.names = FALSE)
    cat("Read the matching block above: the type is neither a named coefficient\n",
        "nor the base level, so the formula differs from what this script assumes.\n")
}

dir.create("outputs", showWarnings = FALSE)
write.csv(res[c("column", "type", "estimate", "se", "z", "p", "stars")],
          "outputs/tableB5_significance.csv", row.names = FALSE)

cells <- res %>%
    mutate(cell = ifelse(is.na(estimate), "NA",
                         paste0(sprintf("%.4f", estimate), stars))) %>%
    select(type, column, cell) %>%
    tidyr::pivot_wider(names_from = column, values_from = cell) %>%
    slice(match(TYPES, type))

cat("\n=== Table B6 with significance ===\n")
cat("    (contrasts against Digital Identity; delta-method standard errors)\n\n")
print(as.data.frame(cells), row.names = FALSE)

# --- reconciliation, now expected to cover all 30 cells ----------------------
ref_path <- "outputs/fe_common_reference.csv"
if (file.exists(ref_path)) {
    old <- read.csv(ref_path, check.names = FALSE)
    map <- c("Award / no strata" = "AWARD no strata", "Award / stratified" = "AWARD FE strata",
             "Execution / no strata" = "EXEC  no strata", "Execution / stratified" = "EXEC  FE strata",
             "Completion / no strata" = "COMP  no strata", "Completion / stratified" = "COMP  FE strata")
    d <- res %>%
        mutate(oldcol = map[column]) %>%
        rowwise() %>%
        mutate(published = suppressWarnings(as.numeric(old[match(type, old$type), oldcol]))) %>%
        ungroup() %>%
        mutate(diff = abs(estimate - published))
    cat(sprintf("\nReconciliation with %s: max |difference| = %.6f over %d of %d cells\n",
                ref_path, max(d$diff, na.rm = TRUE), sum(!is.na(d$diff)), nrow(d)))
    bad <- d %>% filter(diff > 5e-4)
    if (nrow(bad)) {
        cat("Cells that do NOT reproduce - do not attach these stars:\n")
        print(as.data.frame(bad[c("column", "type", "published", "estimate", "diff")]),
              row.names = FALSE)
    } else if (sum(!is.na(d$diff)) == nrow(d)) {
        cat("All 30 cells reproduce to 4 decimals; the stars can be attached as printed.\n")
    }
}

cat(sprintf(
    "\nBy level: *** %d | ** %d | * %d | . %d | n.s. %d | unresolved %d (of %d cells)\n",
    sum(res$stars == "***", na.rm = TRUE), sum(res$stars == "**", na.rm = TRUE),
    sum(res$stars == "*",   na.rm = TRUE), sum(res$stars == ".",  na.rm = TRUE),
    sum(res$stars == "",    na.rm = TRUE), sum(is.na(res$estimate)), nrow(res)))

# =============================================================================
# ADDITIONS: standard errors shown in the table, and observation/municipality
# counts. The estimate/stars logic above is untouched; this only adds display
# and counts that the original version omitted.
# =============================================================================

cells_with_se <- res %>%
    mutate(cell = ifelse(is.na(estimate), "--",
                         sprintf("%.4f%s\n(%.4f)", estimate, stars, se))) %>%
    select(type, column, cell) %>%
    tidyr::pivot_wider(names_from = column, values_from = cell) %>%
    slice(match(TYPES, type))

cat("\n=== Table B6 with standard errors shown ===\n\n")
print(as.data.frame(cells_with_se), row.names = FALSE)

# Digital Identity, the reference category: not a contrast, so it is pulled
# directly from each model's own coefficient rather than from res above. Not
# identified in the stratified columns - stratification absorbs every
# municipality-constant term, including the reference level itself, so only
# the deviation of each non-reference category remains estimable there.
baseline <- bind_rows(lapply(names(MODELS), function(col) {
    m <- get(MODELS[[col]])
    b <- coef(m); se_all <- sqrt(diag(vcov(m)))
    nm <- "log_digital_expenditure"
    if (!nm %in% names(b)) {
        return(tibble(column = col, type = "Digital Identity",
                      estimate = NA_real_, se = NA_real_, p = NA_real_))
    }
    est <- b[nm]; se <- se_all[nm]
    tibble(column = col, type = "Digital Identity", estimate = as.numeric(est),
           se = as.numeric(se), p = 2 * pnorm(-abs(est / se)))
})) %>% mutate(stars = STARS(p))

cat("\n=== Reference category (Digital Identity), for completeness ===\n\n")
print(as.data.frame(baseline), row.names = FALSE)

# Observations and municipalities. No-strata columns: every fitted row counts
# (these models use between- as well as within-municipality variation), so
# the model's own $n is correct. Stratified columns: only municipalities with
# more than one solution type carry information in a stratified partial
# likelihood - using $n there would overstate the effective sample, the same
# mistake this project's own GVIF table (R/tables/tableA4_gvif.R) turned out
# to have made before it was fixed.
count_multi_type <- function(dat) {
    keep <- dat %>% group_by(codice_ipa) %>% filter(n_distinct(type) > 1) %>% ungroup()
    tibble(projects = nrow(keep), municipalities = n_distinct(keep$codice_ipa))
}
DATA_FOR <- list(
    "Award / no strata" = d_award, "Award / stratified" = d_award,
    "Execution / no strata" = d_exec, "Execution / stratified" = d_exec,
    "Completion / no strata" = d_comp, "Completion / stratified" = d_comp
)

counts <- bind_rows(lapply(names(MODELS), function(col) {
    m <- get(MODELS[[col]])
    if (grepl("stratified", col)) {
        informative <- count_multi_type(DATA_FOR[[col]])
        tibble(column = col, observations = informative$projects,
               municipalities = informative$municipalities, raw_model_n = m$n)
    } else {
        # cluster = codice_ipa was passed as a plain coxph() argument, not a
        # +cluster(...) formula term, so it is NOT a model.frame() column;
        # fall back to the row names model.frame() always keeps, indexing
        # back into the source data used to fit this specific model
        mf <- model.frame(m)
        dat <- DATA_FOR[[col]]
        muni <- if ("codice_ipa" %in% names(mf)) n_distinct(mf$codice_ipa)
                else if (!is.null(rownames(mf)) && all(rownames(mf) %in% rownames(dat)))
                    n_distinct(dat[rownames(mf), "codice_ipa"])
                else NA_integer_
        tibble(column = col, observations = m$n, municipalities = muni, raw_model_n = m$n)
    }
}))

cat("\n=== Observations and municipality counts (use these in the table) ===\n\n")
print(as.data.frame(counts), row.names = FALSE)

dir.create("outputs", showWarnings = FALSE)
write.csv(cells_with_se, "outputs/tableB6_with_se.csv", row.names = FALSE)
write.csv(counts, "outputs/tableB6_counts.csv", row.names = FALSE)
