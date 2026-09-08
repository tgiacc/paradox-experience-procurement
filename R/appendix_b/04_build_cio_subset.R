# =============================================================================
# BUILD THE CIO SURVEY SUBSET, FOR TABLE B5 (MUNICIPAL IT CAPACITY CONTROLS)
#
# Run AFTER R/01_build_database.R. Extracted from what was originally a single
# undifferentiated block inside that file, mixed in with the modelling code
# that used it. It is separated out because it is a genuine build step -
# raw survey responses in, a clean covariate table out - and belongs with
# the other data-preparation scripts rather than with an analysis script.
#
# The survey (ANCI, 2024) is not ours to redistribute; see DATA.md.
# =============================================================================

CIO_FILE <- "CIO.xlsx - Sheet1.csv"

if (!file.exists(CIO_FILE)) {
  cat(sprintf("'%s' not found - skipping. This file is the restricted-access\n", CIO_FILE))
  cat("2024 ANCI survey (see DATA.md); it is not part of this repository. Table B5\n")
  cat("(IT capacity) will be skipped downstream; every other table is unaffected.\n")
} else {

cio_data <- read.csv(CIO_FILE, stringsAsFactors = FALSE)

# Fix the join key immediately to match the project-level frames.
colnames(cio_data)[1] <- "codice_ipa"

# Column names are matched by keyword, not by position, so a reordering of the
# survey export does not silently misalign the covariates.
col_dedicato_name <- grep("esclusivamente a quella funzione", colnames(cio_data), value = TRUE)[1]
col_profilo_name  <- grep("profilo prevalente", colnames(cio_data), value = TRUE)[1]
col_it_staff_name <- grep("effettivamente operative", colnames(cio_data), value = TRUE)[1]

missing_cols <- c(dedicato = col_dedicato_name, profilo = col_profilo_name,
                  it_staff = col_it_staff_name)
if (any(is.na(missing_cols))) {
  cat("Could not match these CIO survey columns by keyword:\n")
  print(names(missing_cols)[is.na(missing_cols)])
  cat("\nColumns available in the survey export:\n")
  print(colnames(cio_data))
  stop("Update the keyword patterns above to match the current export.")
}

cio_subset <- data.frame(
  codice_ipa                = cio_data$codice_ipa,
  cio_responsabile_dedicato = cio_data[[col_dedicato_name]],
  cio_profilo_prevalente    = cio_data[[col_profilo_name]],
  cio_it_staff_operative    = cio_data[[col_it_staff_name]],
  stringsAsFactors          = FALSE
)

cio_subset$cio_responsabile_dedicato[cio_subset$cio_responsabile_dedicato == ""] <- NA
cio_subset$cio_profilo_prevalente[cio_subset$cio_profilo_prevalente == ""] <- NA

cio_subset$cio_it_staff_operative <- as.numeric(cio_subset$cio_it_staff_operative)
cio_subset$log_it_staff <- log1p(cio_subset$cio_it_staff_operative)

cat(sprintf("cio_subset ready: %d municipalities, %d with a complete IT-staff figure\n",
            nrow(cio_subset), sum(!is.na(cio_subset$cio_it_staff_operative))))

}
