# DATA LOADING AND CLEANING --------------------------------------------------------------------
#load libraries
library(readxl)
library(dplyr)
library(rvest)
library(stringr)
library(tidyr)
library(ggplot2)
library(writexl)
library(purrr)
library(rsdmx)
library(tidyverse)
library(rjson)
library(jsonlite)
library(rsdmx)
library(stargazer)
library(janitor)
library(readxl)
library(readr)
library(data.table)
library(lubridate)
library(anytime)
library(survival)

### CANDIDATURE - last update: 03182025 ###
all <- read.csv2("data/raw/OpenCUP/candidature_comuni.csv", sep=";")

all <- all %>%
 clean_names()

all <- all %>%
 rename(cup = codice_cup)

all <- all %>%
 filter(str_detect(decreto_finanziamento, "2022|2023"))

admitted <- all %>%
 filter(stato_candidatura == "A" | stato_candidatura == "E")

### ITER PNRR ###

# Replace with the correct path if working locally

#import and clean column names
file_path <- "data/raw/PNRR/PNRR_Iter_di_progetto_v3_M1.xlsx"

date_columns <- 7:11  # Columns 7 to 11 are the ones that start with "data_inizio" or "data_fine"

# Step 2: Read the CSV file with the specified colClasses for the date columns
col_classes <- rep("character", length(names(data)))  # Set all columns to "character"
names(col_classes) <- names(data)  # Name the vector to match column names

# Set columns 7 to 11 as "character"
col_classes[date_columns] <- "character"

# Read the CSV file with custom column types
data <- read_excel(file_path)

# Step 3: Convert the date columns to Date format
for (col in date_columns) {
 data[[col]] <- as.Date(as.character(data[[col]]), format = "%Y-%m-%d")  # Adjust format if needed
}

data <- data %>%
 clean_names()

rm(file_path)

### MERGE CUP data with ITER data ###

projects <- merge(admitted, data, by = "cup", all.x = TRUE)

projects <- projects %>%
 rename(last_observation = data_di_estrazione)

# Check for duplicates in the key columns and get their number
overlaps <- projects %>%
 group_by(cup, descrizione_fase) %>%
 summarise(row_count = n(), .groups = "drop") %>%
 filter(row_count > 1)

# Remove the only remaining duplicate cup identified
if (nrow(overlaps) > 0) {
 print("Overlaps detected:")
 print(overlaps) }

projects <- projects %>%
 filter(!cup %in% overlaps$cup)

# Transform the data from long to wide format
# data_minimized <- data %>%
#  select(cup, descrizione_fase, data_inizio_prevista, data_inizio_effettiva, data_fine_prevista, data_fine_effettiva)

data_wide <- projects %>%
 filter(!is.na(data_inizio_effettiva) | !is.na(data_fine_effettiva)) %>%
 group_by(cup, descrizione_fase) %>%
 summarise(
  data_inizio_effettiva = first(na.omit(data_inizio_effettiva)),
  data_fine_effettiva = first(na.omit(data_fine_effettiva)),
  .groups = "drop"
 ) %>%
 # Pivot the table to wider format by 'descrizione_fase'
 pivot_wider(
  names_from = descrizione_fase,  # Use 'descrizione_fase' for new column names
  values_from = c(data_inizio_effettiva, data_fine_effettiva),
  values_fn = list,  # This ensures we combine multiple rows into a single one
  values_fill = list(data_inizio_effettiva = NA, data_fine_effettiva = NA)  # Fill missing values with NA
 ) %>%
 # Ensure 'cup' is retained by joining with the distinct cups from the original dataset
 left_join(select(projects, cup) %>% distinct(), by = "cup")

# date_columns <- 2:13  # Columns 2 to 13 are the ones that start with "data_inizio" or "data_fine"
# 
# for (col in date_columns) {
#  data_wide[[col]] <- as.Date(as.character(data_wide[[col]]), format = "%Y-%m-%d")  # Adjust format if needed
# }

#doublecheck uniqueness ny ensuring # of unique CUPs coincides with df's length
length(unique(data_wide$cup))
print(dim(data_wide))

df1 <- merge(admitted, data_wide, by = "cup", all.x = TRUE)

# List the columns that need to be converted to Date format
date_columns <- grep("data_inizio_effettiva_|data_fine_effettiva_", colnames(df1), value = TRUE)

#unlist
for (col in date_columns) {
 df1[[col]] <- sapply(df1[[col]], function(x) if (is.null(x)) NA else x)  # Replace NULL with NA
 df1[[col]] <- unlist(df1[[col]])  # Unlist to get the Date vector
}

# Check structure after processing
str(df1[, date_columns])

# Convert numeric values back to Date
for (col in date_columns) {
 df1[[col]] <- as.Date(df1[[col]], origin = "1970-01-01")
}

# Check the structure again
str(df1[, date_columns])

# Convert to dd/mm/yyy Date
for (col in date_columns) {
 df1[[col]] <- as.Date(df1[[col]], format="%d/%m/%Y")
}

df1$data_inizio_effettiva_ESECUZIONE <- NULL
df1$data_fine_effettiva_ESECUZIONE <- NULL
df1$`data_inizio_effettiva_VERIFICA DI CONFORMITÀ/REGOLARE ESECUZIONE` <- NULL
df1$`data_fine_effettiva_VERIFICA DI CONFORMITÀ/REGOLARE ESECUZIONE` <- NULL

df1$data_stato_candidatura <- as.Date(df1$data_stato_candidatura, format="%d/%m/%Y")
df1$data_invio_candidatura <- as.Date(df1$data_invio_candidatura)
df1$data_finanziamento <- as.Date(df1$data_finanziamento, format="%d/%m/%Y")

### ANCI ###

file_path <- "data/raw/ANCI/2024_10_29_data_cleaned.xlsx"
ANCI <- read_xlsx(file_path)
ANCI <- ANCI %>%
 clean_names()

### CODICE FISCALE (fiscal ID) ###

file_path <- "data/raw/Codice fiscale/enti.xlsx"
codice_fiscale <- read_xlsx(file_path)
codice_fiscale <- codice_fiscale %>%
 rename(codice_ipa = Codice_IPA, codice_fiscale = Codice_fiscale_ente)

### WORKFORCE ###

file_path <- "data/raw/Dipendenti_PAL/Anagrafe-Enti---Ente.xlsx"
BDAP_id <- read_excel(file_path)
BDAP_id <- BDAP_id %>%
 rename(codice_ipa = Codice_Ente_IPA)
BDAP_id <- BDAP_id %>%
 mutate(codice_ipa = tolower(codice_ipa))

file_path <- "data/raw/Dipendenti_PAL/2022---Dipendenti-Pubblici---Anzianita---Dati-analitici-per-Ente.xlsx"
civ_servants_age <- read_excel(file_path) %>%
 clean_names()

# Reshape the data using pivot_wider()
civ_servants_age <- civ_servants_age %>%
 group_by(codice_ente_bdap, descrizione_fascia_eta) %>%
 summarise(
  numero_dipendenti_uomini = sum(numero_dipendenti_uomini, na.rm = TRUE),
  numero_dipendenti_donne = sum(numero_dipendenti_donne, na.rm = TRUE),
  .groups = "drop"
 )

civ_servants_age <- civ_servants_age %>%
 pivot_wider(
  id_cols = "codice_ente_bdap",
  names_from = "descrizione_fascia_eta",
  values_from = c("numero_dipendenti_uomini", "numero_dipendenti_donne"),
  names_sep = "_"
 )

civ_servants_age <- civ_servants_age %>%
 mutate(
  men = rowSums(select(., contains("uomini")), na.rm = TRUE),
  women = rowSums(select(., contains("donne")), na.rm = TRUE),
  u35 = rowSums(select(., matches("Da 25 a 29 anni|Da 30 a 34 anni")), na.rm = TRUE)  # Corrected selection
 )

civ_servants_age$servants <- civ_servants_age$men+civ_servants_age$women
civ_servants_age$u35_pct <- civ_servants_age$u35/civ_servants_age$servants*100


file_path <- "data/raw/Dipendenti_PAL/2022---Dipendenti-Pubblici---Occupazione-Complessiva---Dati-analitici-per-Ente.xlsx"
civ_servants_contract <- read_excel(file_path)


file_path <- "data/raw/Dipendenti_PAL/2022---Dipendenti-Pubblici---Occupazione-e-Turnover---Dati-analitici-per-Ente.xlsx"
civ_servants_turnover <- read_excel(file_path)

###GDP###

file_path <- "data/raw/GDP per capita/Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2022.xlsx"
GDP <- read_excel(file_path)

GDP <- GDP %>%
 clean_names()

###BILANCIO ENTI LOCALI###

url <- "https://esploradati.istat.it/SDMXWS/rest/data/IT1,123_712_DF_DCAR_INDBILPER_1,1.0/A../ALL/?detail=full&dimensionAtObservation=TIME_PERIOD"
balance_sheets <- as.data.frame(readSDMX(url))
balance_sheets <- balance_sheets[balance_sheets$obsTime>2020,]
balance_sheets$OBS_STATUS <- NULL

rm(url)

###TEMPI DI PAGAMENTO ENTI LOCALI###

file_path <- "data/raw/Tempi_pagamento_PA/Tempi_pagamento_PA_FB.xlsx"
payment <- read_excel(file_path)

payment <- payment %>%
 clean_names()

payment <- payment %>%
 mutate(across(starts_with("weighted_payment_time_"), 
               ~ na_if(na_if(., "-"), "n.a.") %>% as.numeric()))

###BIBLIOTECHE###

file_path <- "data/raw/Libraries/Libraries.xlsx"
libraries <- read_excel(file_path)
libraries <- clean_names(libraries)

### RTD ###

file_path <- "data/raw/RTD/RTD.xlsx"
RTD <- read_excel(file_path)
RTD <- clean_names(RTD)

### POPULATION ###

file_path <- "data/raw/Population/Codici-statistici-e-denominazioni-al-01_01_2022.xlsx"
population <- read_excel(file_path)
population <- clean_names(population)
population <- population %>%
 mutate(codice_istat=as.character(codice_istat))

rm(file_path)

### JOIN DATASETS ###

df1 <- df1 %>%
 rename(codice_istat = cod_comune)

df1 <- df1 %>%
 left_join(population[ ,c("codice_istat", "popolazione", "area_code", "area")], by="codice_istat")

df1 <- df1 %>%
 left_join(codice_fiscale, c("codice_fiscale", "codice_ipa"), by="codice_ipa")

### PAST EXPENDITURE (from 2007 to 2021) ###

# Define the folder path
folder_path_CIG <- "data/raw/Contracts/CIG"
folder_path_SMARTCIG <- "data/raw/Contracts/SMARTCIG"

# Create a vector of years
years <- 2007:2021

# Loop through each year
for (year in years) {
 # Create a vector of file names for the current year
 file_names <- paste0("cig_csv_", year, "_", sprintf("%02d", 1:12), ".csv")
 
 # Full paths of the CSV files for the current year
 file_paths <- file.path(folder_path_CIG, file_names)
 
 # Import and append all CSVs for the current year into a data frame
 cig_data <- file_paths %>%
  lapply(function(file) {
   if (file.exists(file)) {
    df <- read.csv(file, sep = ";", stringsAsFactors = FALSE)
    # Standardize problematic columns to character
    df <- df %>%
     mutate(across(where(is.numeric), as.character)) # Convert numeric to character if necessary
    return(df)
   } else {
    message(paste("File not found:", file))  # Optional: Print a message for missing file
    return(NULL)  # Return NULL for missing file to skip it
   }
  }) %>%
  # Remove NULL values (if any files were skipped) and then bind rows
  bind_rows()
 
 # Dynamically assign the data frame to a variable named cig16, cig17, etc.
 assign(paste0("cig", year %% 100), cig_data)
}


for (year in years) {
 # Create a vector of file names for the current year
 file_names <- paste0("smartcig_csv_", year, "_", sprintf("%02d", 1:12), ".csv")
 
 # Full paths of the CSV files for the current year
 file_paths <- file.path(folder_path_SMARTCIG, file_names)
 
 # Import and append all CSVs for the current year into a data frame
 smartcig_data <- file_paths %>%
  lapply(function(file) {
   if (file.exists(file)) {
    df <- read.csv(file, sep = ";", stringsAsFactors = FALSE)
    # Standardize problematic columns to character
    df <- df %>%
     mutate(across(where(is.numeric), as.character)) # Convert numeric to character if necessary
    return(df)
   } else {
    message(paste("File not found:", file))  # Optional: Print a message for missing file
    return(NULL)  # Return NULL for missing file to skip it
   }
  }) %>%
  # Remove NULL values (if any files were skipped) and then bind rows
  bind_rows()
 
 # Dynamically assign the data frame to a variable named smartcig16, smartcig17, etc.
 assign(paste0("smartcig", year %% 100), cig_data)
}

# Read and combine all past CSV files
cig <- rbind(cig7, cig8, cig9, cig10, cig11, cig12, cig13, cig14, cig15, cig16, cig17, cig18, cig19, cig20, cig21)
rm(cig7, cig8, cig9, cig10, cig11, cig12, cig13, cig14, cig15, cig16, cig17, cig18, cig19, cig20, cig21)
smartcig <- rbind(smartcig7, smartcig8, smartcig9, smartcig10, smartcig11, smartcig12, smartcig13, smartcig14, smartcig15, smartcig16, smartcig17, smartcig18, smartcig19, smartcig20, smartcig21)
rm(smartcig7, smartcig8, smartcig9, smartcig10, smartcig11, smartcig12, smartcig13, smartcig14, smartcig15, smartcig16, smartcig17, smartcig18, smartcig19, smartcig20, smartcig21)

#clean the joined df
pastcig2 <- bind_rows(cig, smartcig)
pastcig2 <- pastcig2[ , c("cig", "cig_accordo_quadro", "numero_gara", "oggetto_gara", "importo_complessivo_gara", "n_lotti_componenti", "oggetto_lotto", "importo_lotto", "stato", "luogo_istat", "data_pubblicazione", "data_scadenza_offerta", "cod_tipo_scelta_contraente", "tipo_scelta_contraente", "cod_modalita_realizzazione", "modalita_realizzazione", "cf_amministrazione_appaltante", "denominazione_amministrazione_appaltante", "id_centro_costo", "denominazione_centro_costo", "anno_pubblicazione", "mese_pubblicazione", "cod_cpv", "descrizione_cpv", "DATA_ULTIMO_PERFEZIONAMENTO", "DURATA_PREVISTA", "COD_STRUMENTO_SVOLGIMENTO", "STRUMENTO_SVOLGIMENTO", "COD_ESITO", "ESITO", "DATA_COMUNICAZIONE_ESITO", "data_comunicazione", "id_tipo_fattispecie_contrattuale", "tipo_fattispecie_contrattuale", "istat_comune", "regione")]
pastcig2 <- pastcig2 %>%
 mutate(data_pubblicazione = if_else(
  is.na(data_pubblicazione) & !is.na(data_comunicazione),
  data_comunicazione,
  data_pubblicazione
 ))

rm(cig, smartcig)

# Remove commas and then convert to numeric
pastcig2$importo_lotto <- as.numeric(gsub(",", "", pastcig2$importo_lotto))

df_total_past2 <- pastcig2 %>%
 group_by(cf_amministrazione_appaltante) %>%
 summarise(total_importo_lotto = sum(importo_lotto, na.rm = TRUE)) %>%
 ungroup()

# Define the list of CPV codes related to digital technologies
df_digital2 <- pastcig2 %>%
 filter(grepl("^48|^72", cod_cpv)) %>%  # Match CPV codes starting with "48" or "72"
 group_by(cf_amministrazione_appaltante) %>%
 summarize(digital_expenditure = sum(importo_lotto, na.rm = TRUE))

# Join the summarized digital expenditure with the `df_total_past`
df_total_past2 <- df_total_past2 %>%
 left_join(df_digital2, by = "cf_amministrazione_appaltante")

# Replace NA values in `digital_expenditure` with 0
df_total_past2 <- df_total_past2 %>%
 mutate(digital_expenditure = ifelse(is.na(digital_expenditure), 0, digital_expenditure))

#compute portion of past procurement dedicated to digital solutions
df_total_past2$digital_expenditure_quota <- df_total_past2$digital_expenditure/df_total_past2$total_importo_lotto
rm(cig_data, smartcig_data, file_names, file_paths, folder_path_CIG, folder_path_SMARTCIG, pastcig2, year, years,)
# Join df1 (main df) with data on past expenditure in general and on digital maturity

df1 <- df1 %>%
 clean_names()

df1 <- df1 %>%
 mutate(type = case_when(
  grepl("1.2", avviso) ~ "Cloud",
  grepl("1.3.1", avviso) ~ "Interoperability",
  grepl("1.4.1", avviso) ~ "Citizen Experience",
  grepl("1.4.3", avviso) ~ "Digital Services and Payments",
  grepl("1.4.4", avviso) ~ "Digital Identity",
  grepl("1.4.5", avviso) ~ "Digital Notices",
  TRUE ~ NA_character_  # Default value if no match is found
 ))

df1 <- df1 %>%
 left_join(df_total_past[ ,c("codice_fiscale", "total_importo_lotto", "digital_expenditure", "digital_expenditure_quota")], by = "codice_fiscale")

df1 <- df1 %>%
 rename(past_expenditure=total_importo_lotto)


# Define a function to assign deadlines based on avviso and popolazione
assign_deadlines <- function(avviso, popolazione) {
 if (is.na(popolazione)) {
  # Default behavior for missing population
  award_deadline <- NA
  completion_deadline <- NA
 } else if (grepl("1.4.3", avviso)) {
  award_deadline <- 180
  completion_deadline <- 240
 } else if (grepl("1.3.1", avviso)) {
  if (popolazione <= 50000) {
   award_deadline <- 90
   completion_deadline <- 180
  } else {
   award_deadline <- 180
   completion_deadline <- 180
  }
 } else if (grepl("1.4.1", avviso)) {
  if (popolazione <= 5000) {
   award_deadline <- 180
   completion_deadline <- 270
  } else {
   award_deadline <- 270
   completion_deadline <- 360
  }
 } else if (grepl("1.4.3", avviso)) {
  award_deadline <- 180
  completion_deadline <- 240
 } else if (grepl("1.4.4", avviso)) {
  award_deadline <- 360
  completion_deadline <- 300
 } else if (grepl("1.4.5 'Piattaforma Notifiche Digitali' Comuni (maggio 2024)", avviso, fixed = TRUE)) {
  award_deadline <- 120
  completion_deadline <- 240
 } else if (grepl("1.4.5 'Piattaforma Notifiche Digitali' Comuni (Settembre 2022)", avviso, fixed = TRUE)) {
  award_deadline <- 90
  completion_deadline <- 180
 } else if (grepl("1.2", avviso)) {
  if (popolazione <= 20000) {
   award_deadline <- 180
   completion_deadline <- 450
  } else {
   award_deadline <- 270
   completion_deadline <- 540
  }
 } else {
  award_deadline <- NA
  completion_deadline <- NA
 }
 total_deadline <- ifelse(is.na(award_deadline) | is.na(completion_deadline), NA, award_deadline + completion_deadline)
 return(c(award_deadline, completion_deadline, total_deadline))
}

# Apply the function to the dataset

df1 <- df1 %>%
 rowwise() %>%
 mutate(
  deadlines = list(assign_deadlines(avviso, popolazione)),
  award_deadline = deadlines[[1]],
  completion_deadline = deadlines[[2]],
  total_deadline = deadlines[[3]]
 ) %>%
 ungroup() %>%
 select(-deadlines)  # Remove the intermediate column

rm(assign_deadlines)

df1 <- df1 %>%
 rename(population=popolazione)

df1$pop_cluster <- NA
df1$pop_cluster[df1$population<2501]<-1
df1$pop_cluster[df1$population>2500 & df1$population<5001]<-2
df1$pop_cluster[df1$population>5000 & df1$population<20001]<-3
df1$pop_cluster[df1$population>20000 & df1$population<50001]<-4
df1$pop_cluster[df1$population>50000 & df1$population<100001]<-5
df1$pop_cluster[df1$population>100000 & df1$population<250001]<-6
df1$pop_cluster[df1$population>250000]<-7

df1$importo_finanziamento <- as.numeric(df1$importo_finanziamento)
df1$log_importo_finanziamento <- NA
df1$log_importo_finanziamento <- log(df1$importo_finanziamento + 0.0000000001)

df_total_past$codice_fiscale <- df_total_past$cf_amministrazione_appaltante
df1 <- df1 %>%
 left_join(df_total_past, by="codice_fiscale")

df1$digital_expenditure_dummy <- 0
df1$digital_expenditure_dummy[df1$digital_expenditure>0] <- 1
df1$log_population <- log(df1$population)

df1$log_past_expenditure_digital <- log(df1$digital_expenditure+0.0000000001)
df1$past_digital_expenditure_pct <- df1$digital_expenditure_quota*100

df1$codice_ipa <- trimws(df1$codice_ipa)
BDAP_id$codice_ipa <- trimws(BDAP_id$codice_ipa)
df1 <- df1 %>%
 left_join(BDAP_id %>% select(Id_Ente, codice_ipa), by = "codice_ipa")

#add GDP_perCapita + other info on population dependency

GDP <- GDP %>%
 rename(codice_istat = codice_istat_comune)

GDP$codice_istat <- as.character(GDP$codice_istat)

df1 <- df1 %>%
 left_join(GDP %>% select(codice_istat, numero_contribuenti, reddito_da_pensione_frequenza, reddito_imponibile_ammontare_in_euro), by = "codice_istat")

df1$GDP_pc <- df1$reddito_imponibile_ammontare_in_euro/df1$population

df1 <- df1 %>%
 rename(codice_ente_bdap = Id_Ente)

df1 <- df1 %>%
 left_join(
  civ_servants_age %>% select(codice_ente_bdap, men, women, u35, servants),
  by = "codice_ente_bdap"  # Ensure both datasets share this column
 )

ANCI <- ANCI %>%
 rename(codice_istat=login_name)

df1 <- df1 %>%
 left_join(
  ANCI %>% select(codice_istat, gestione_associata_dell_ict, x2_1_il_comune_partecipa_a_una_gestione_ict_associata_con_altri_enti_pubblici, x2_1_il_comune_si_avvale_di_una_societa_in_house, x2_1_il_comune_si_avvale_di_uno_o_piu_fornitori_esterni_per_i_servizi_ict, x2_1_1_unione, x2_4_responsabile_dei_sistemi_informativi_dell_ente_interno_all_ente, x2_4_responsabile_dei_sistemi_informativi_dell_ente_interno_all_ente, x2_5_riguardo_la_figura_del_responsabile_dei_sistemi_informativi_indica_se_e_dedicata_esclusivamente_a_quella_funzione_o_svolge_anche_altre_funzioni_non_dedicata, x2_6_riguardo_la_figura_del_responsabile_dei_sistemi_informativi_indicane_il_profilo_prevalente, x2_7_il_comune_ha_nominato_come_responsabile_per_la_transizione_digitale_rtd_una_figura),
  by = "codice_istat"
 )

df1$u35_pct <- df1$u35/df1$servants
df1$female_pct <- df1$women/(df1$men+df1$women)
df1$servants_pc <- df1$servants/df1$population

df1$area[df1$area_code==01] <- "North"
df1$area[df1$area_code==02] <- "North"
df1$area[df1$area_code==03] <- "Centre"
df1$area[df1$area_code==04] <- "South"
df1$area[df1$area_code==05] <- "South"

# #merge with libraries#
# df1 <- df1 %>%
#  left_join(libraries %>% select(codice_istat, utenti_attivi), by="codice_istat")
# df1$utenti_attivi[df1$utenti_attivi=="."]<-NA
# df1$utenti_attivi<-as.numeric(df1$utenti_attivi)
# df1$utenti_attivi[df1$utenti_attivi==0] <- NA
# df1$lib_users_1000<-df1$utenti_attivi/df1$population*1000

#merge with RTD
df1 <- df1 %>%
 left_join(RTD %>% select(codice_ipa, data_istituzione), by="codice_ipa")

df1$DTO <- 0
df1$DTO[df1$data_istituzione<df1$data_finanziamento] <- 1

###merging with balance sheet indicators###
balance_sheets$obsTime <- as.integer(balance_sheets$obsTime)

# Define selected priority indicators
financial_indicators <- c(
 "TAXAUT",      # Autonomia finanziaria
 "DEGDEPCEN",   # Dipendenza da trasferimenti centrali
 "DEGDEPLOC",   # Dipendenza da trasferimenti locali
 "EXPCAP",      # Capacità di spesa
 "EXPRIG",      # Rigidità della spesa
 "REVLOAREV",   # Finanziamento tramite prestiti
 "LOAREPREV",   # Spese per rimborso prestiti
 "COLCAP",       # Capacità di riscossione
 "INTPACUR"     # Spese per interessi passivi in relazione alle entrate correnti
)

# Filter balance_sheets for only the selected indicators
balance_filtered <- balance_sheets %>%
 filter(str_starts(BODY_TYPE, "EC_"))  %>%
 filter (DATA_TYPE %in% financial_indicators) %>%
 filter (obsTime == 2022) %>%
 select(obsTime, BODY_TYPE, DATA_TYPE, obsValue) %>%
 mutate(
  BODY_TYPE = str_remove(BODY_TYPE, "^EC_"),   # Remove "EC_" at the beginning
  BODY_TYPE = str_replace(BODY_TYPE, "^0+", "") # Remove leading zeros
 )

# Pivot balance_sheets to wide format and rename BODY_TYPE to istat_comune
balance_wide <- balance_filtered %>%
 pivot_wider(names_from = DATA_TYPE, values_from = obsValue) %>%
 rename(istat_comune = BODY_TYPE) %>%
 mutate(istat_comune = as.numeric(istat_comune))  # Convert to character

# Merge with df1_b (current-year indicators) using anno_pubblicazione and istat_comune

balance_wide$istat_comune <- as.character(balance_wide$istat_comune)
balance_wide <- balance_wide %>%
 rename(codice_istat = istat_comune)
balance_wide$obsTime <- NULL

df1 <- df1 %>%
 left_join(balance_wide, by = "codice_istat")

# # Create lagged financial variables (shift obsTime forward by 1 year)
# balance_lagged <- balance_wide %>%
#  rename_with(~ paste0(.x, "_lagged"), -c(obsTime, istat_comune)) %>%
#  mutate(obsTime = obsTime + 1)

# Merge lagged indicators with df1_b
df1 <- df1 %>%
 left_join(balance_lagged, by = c("anno_pubblicazione" = "obsTime",
                                  "codice_istat"="istat_comune"))

df1 <- df1 %>%
 rename(past_digital_expenditure = digital_expenditure)

df1$log_past_expenditure <- log(df1$past_expenditure+0.0000000001)

### Merge with payment times ###
payment$codice_ipa <- as.character(payment$codice_ipa)

payment <- payment %>%
 mutate(codice_ipa = tolower(codice_ipa))

df1$codice_ipa <- trimws(df1$codice_ipa)
payment$codice_ipa <- trimws(payment$codice_ipa)

payment <- payment %>%
 distinct(codice_ipa, .keep_all = TRUE)

df1 <- df1 %>%
 left_join(payment[,c("codice_ipa", "weighted_payment_time_2019", "weighted_payment_time_2020", "weighted_payment_time_2021", "weighted_payment_time_2022", "weighted_payment_time_2023")], by = "codice_ipa")

df1 <- df1 %>%
 mutate(
  weighted_payment_time = case_when(
   format(data_finanziamento, "%Y") == "2022" ~ weighted_payment_time_2021,
   format(data_finanziamento, "%Y") == "2023" ~ weighted_payment_time_2022,
   format(data_finanziamento, "%Y") == "2024" ~ weighted_payment_time_2023,
   TRUE ~ NA_real_
  )
 )

# identify and handle duplicate rows
df1 <- df1 %>%
 group_by(cup, avviso) %>%  # Group by 'cup' and 'avviso'
 arrange(desc(importo_complessivo_gara)) %>%  # Arrange by 'data_pubblicazione' in ascending order (earliest first)
 filter(row_number() == 1) %>%  # Select the earliest row within each group
 ungroup()

df1 <- df1[df1$importo_finanziamento>=1 & df1$data_pubblicazione>df1$data_finanziamento,]

df1$award_group<-NA
df1$award_group[df1$award<=quantile(df1$award, 0.25, na.rm=T)] <- "early"
df1$award_group[df1$award>=quantile(df1$award, 0.75, na.rm=T)] <- "late"
df1$award_group[df1$award>quantile(df1$award, 0.25, na.rm=T) & df1$award<quantile(df1$award, 0.75, na.rm=T)] <- "mid"

rm(list = setdiff(ls(), "df1"))

df1$dependency <- df1$reddito_da_pensione_frequenza/df1$numero_contribuenti

df1$late_award <- NA
df1$late_completion <- NA
df1$late_dummy <- NA
df1$late_completion_dummy <- NA
df1$late_award_dummy <- NA

df1$award_deadline_date <- df1$data_finanziamento + df1$award_deadline
df1$completion_deadline_date <- df1$data_fine_effettiva_stipula_contratto + df1$completion_deadline 

df1$late_award <- as.numeric(difftime(df1$data_fine_effettiva_stipula_contratto, df1$award_deadline_date, units="days"))
df1$late_completion <- as.numeric(difftime(df1$data_fine_effettiva_esecuzione_fornitura, df1$completion_deadline_date, units = "days"))

df1$late_completion_dummy[df1$late_completion>0]<-1
df1$late_completion_dummy[df1$late_completion<=0]<-0
df1$late_award_dummy[df1$late_award>0]<-1
df1$late_award_dummy[df1$late_award<=0]<-0

### CUPs (project ID) - last update: 03252025 ###

file_path <- "data/raw/OpenCUP/"
cig_cup <- read.csv(paste0(file_path, "cup_csv.csv"),sep = ";")
cig_cup <- cig_cup %>%
 rename(cig=CIG, cup=CUP)

### TENDERS ###
# Add cigs from 2022

# Specify the folder path where the CSV files are located
folder_path <- "data/raw/Contracts/CIG"

# Create a vector of file names to read
file_names <- paste0("cig_csv_2022_", sprintf("%02d", 1:12), ".csv")

# Full paths of the CSV files
file_paths <- file.path(folder_path, file_names)

# Import and append all CSVs into a single data frame
cig22 <- file_paths %>%
 lapply(read.csv, sep = ";", stringsAsFactors=F) %>%   # Read each file
 bind_rows()            # Append them into a single data frame

# Add smartcigs from 2022

# Specify the folder path where the CSV files are located
folder_path <- "data/raw/Contracts/SMARTCIG"

# Create a vector of file names to read
file_names <- paste0("smartcig_csv_2022_", sprintf("%02d", 1:12), ".csv")

# Full paths of the CSV files
file_paths <- file.path(folder_path, file_names)

# Import and append all CSVs into a single data frame
smartcig22 <- file_paths %>%
 lapply(read.csv, sep = ";", stringsAsFactors=F) %>%   # Read each file
 bind_rows()            # Append them into a single data frame

# Add cigs from 2023

# Specify the folder path where the CSV files are located
folder_path <- "data/raw/Contracts/CIG"

# Create a vector of file names to read
file_names <- paste0("cig_csv_2023_", sprintf("%02d", 1:12), ".csv")

# Full paths of the CSV files
file_paths <- file.path(folder_path, file_names)

# Import and append all CSVs into a single data frame
cig23 <- file_paths %>%
 lapply(read.csv, sep = ";", stringsAsFactors=F) %>%   # Read each file
 bind_rows()            # Append them into a single data frame

# Add smartcigs from 2023

# Specify the folder path where the CSV files are located
folder_path <- "data/raw/Contracts/SMARTCIG"

# Create a vector of file names to read
file_names <- paste0("smartcig_csv_2023_", sprintf("%02d", 1:12), ".csv")

# Full paths of the CSV files
file_paths <- file.path(folder_path, file_names)

# Import and append all CSVs into a single data frame
smartcig23 <- file_paths %>%
 lapply(read.csv, sep = ";", stringsAsFactors=F) %>%   # Read each file
 bind_rows()            # Append them into a single data frame

# Add CIGs from 2024
file_names <- c("20240201-cig_csv.csv",
                "20240401-cig_csv.csv",
                "20240501-cig_csv.csv",
                "20240601-cig_csv.csv",
                "20240701-cig_csv.csv",
                "20240801-cig_csv.csv",
                "20240901-cig_csv.csv",
                "20241001-cig_csv.csv",
                "20241101-cig_csv.csv",
                "20241201-cig_csv.csv")

# Full path to the directory
folder_path <- "data/raw/Contracts/CIG/"

# Function to read each CSV file
read_agg_csv <- function(file_name) {
 read.csv(file.path(folder_path, file_name), sep=";")
}

cig24 <- do.call(rbind, lapply(file_names, read_agg_csv))

# Add SmartCIGs from 2024
file_names <- c("20240201-smartcig_csv.csv",
                "20240401-smartcig_csv.csv",
                "20240501-smartcig_csv.csv",
                "20240601-smartcig_csv.csv",
                "20240701-smartcig_csv.csv",
                "20240801-smartcig_csv.csv",
                "20240901-smartcig_csv.csv",
                "20241001-smartcig_csv.csv",
                "20241101-smartcig_csv.csv",
                "20241201-smartcig_csv.csv")

# Full path to the directory
folder_path <- "data/raw/Contracts/SMARTCIG/"

# Function to read each CSV file
read_agg_csv <- function(file_name) {
 read.csv(file.path(folder_path, file_name), sep=";")
}
smartcig24 <- do.call(rbind, lapply(file_names, read_agg_csv))

# Add CIGs from 2024
file_names <- c("20250101-cig_csv.csv",
                "20250201-cig_csv.csv",
                "20250301-cig_csv.csv",
                "20250401-cig_csv.csv")

# Full path to the directory
folder_path <- "data/raw/Contracts/CIG/"

# Function to read each CSV file
read_agg_csv <- function(file_name) {
 read.csv(file.path(folder_path, file_name), sep=";")
}

cig25 <- do.call(rbind, lapply(file_names, read_agg_csv))

# # bind cigs and smartcigs together, removing useless conflicting columns
cig22$numero_gara<-NULL
cig23$numero_gara<-NULL
cig24$numero_gara<-NULL
smartcig22$numero_gara<-NULL
smartcig23$numero_gara<-NULL
smartcig24$numero_gara<-NULL

#supercig <- bind_rows(cig22, cig23, cig24, cig25, smartcig22, smartcig23, smartcig24)

supercig22 <- merge(cig_cup, cig22, by="cig", all.x=TRUE)
rm(cig_cup, cig22)

supercig23 <- left_join(supercig22, cig23, by = "cig")
rm(supercig22, cig23)

# Identify columns with .x and .y suffixes
x_columns <- grep("\\.x$", colnames(supercig23), value = TRUE)
y_columns <- gsub("\\.x$", ".y", x_columns)

# Merge the columns and rename them
supercig23_cleaned <- supercig23 %>%
 mutate(across(all_of(x_columns), 
               ~ coalesce(.x, get(gsub("\\.x$", ".y", cur_column()))))) %>%
 select(-all_of(y_columns)) %>%
 rename_with(~ gsub("\\.x$", "", .), all_of(x_columns))

rm(supercig23)

supercig24 <- left_join(supercig23_cleaned, cig24, by = "cig")
rm(supercig23_cleaned, cig24)

# Identify columns with .x and .y suffixes
x_columns <- grep("\\.x$", colnames(supercig24), value = TRUE)
y_columns <- gsub("\\.x$", ".y", x_columns)

# Merge the columns and rename them
supercig24_cleaned <- supercig24 %>%
 mutate(across(all_of(x_columns), 
               ~ coalesce(as.character(.x), as.character(get(gsub("\\.x$", ".y", cur_column())))))) %>%
 select(-all_of(y_columns)) %>%
 rename_with(~ gsub("\\.x$", "", .), all_of(x_columns))

rm(supercig24)

#join with smartcig22
supercig22 <- left_join(supercig24_cleaned, smartcig22, by = "cig")
rm(smartcig22)

# Identify columns with .x and .y suffixes
x_columns <- grep("\\.x$", colnames(supercig22), value = TRUE)
y_columns <- gsub("\\.x$", ".y", x_columns)

# Merge the columns and rename them
supercig22_cleaned <- supercig22 %>%
 mutate(across(all_of(x_columns), 
               ~ coalesce(as.character(.x), as.character(get(gsub("\\.x$", ".y", cur_column())))))) %>%
 select(-all_of(y_columns)) %>%
 rename_with(~ gsub("\\.x$", "", .), all_of(x_columns))

rm(supercig22)

#join with smartcig23
supercig23 <- left_join(supercig22_cleaned, smartcig23, by = "cig")
rm(smartcig23)

# Identify columns with .x and .y suffixes
x_columns <- grep("\\.x$", colnames(supercig23), value = TRUE)
y_columns <- gsub("\\.x$", ".y", x_columns)

# Merge the columns and rename them
supercig23_cleaned <- supercig23 %>%
 mutate(across(all_of(x_columns), 
               ~ coalesce(as.character(.x), as.character(get(gsub("\\.x$", ".y", cur_column())))))) %>%
 select(-all_of(y_columns)) %>%
 rename_with(~ gsub("\\.x$", "", .), all_of(x_columns))

rm(supercig23)

#join with smartcig24
supercig24 <- left_join(supercig23_cleaned, smartcig24, by = "cig")
rm(smartcig24)

# Identify columns with .x and .y suffixes
x_columns <- grep("\\.x$", colnames(supercig24), value = TRUE)
y_columns <- gsub("\\.x$", ".y", x_columns)

# Merge the columns and rename them
supercig24_cleaned <- supercig24 %>%
 mutate(across(all_of(x_columns), 
               ~ coalesce(as.character(.x), as.character(get(gsub("\\.x$", ".y", cur_column())))))) %>%
 select(-all_of(y_columns)) %>%
 rename_with(~ gsub("\\.x$", "", .), all_of(x_columns))

rm(supercig24, supercig22_cleaned, supercig23_cleaned)

#join with cig25
supercig25 <- left_join(supercig24_cleaned, cig25, by = "cig")
rm(cig25)

# Identify columns with .x and .y suffixes
x_columns <- grep("\\.x$", colnames(supercig25), value = TRUE)
y_columns <- gsub("\\.x$", ".y", x_columns)

# Merge the columns and rename them
supercig25_cleaned <- supercig25 %>%
 mutate(across(all_of(x_columns), 
               ~ coalesce(as.character(.x), as.character(get(gsub("\\.x$", ".y", cur_column())))))) %>%
 select(-all_of(y_columns)) %>%
 rename_with(~ gsub("\\.x$", "", .), all_of(x_columns))

rm(supercig25, supercig22_cleaned, supercig23_cleaned)

#Combine cigs and smartcigs dates into one column
supercig25_cleaned <- supercig25_cleaned %>%
 mutate(data_pubblicazione = if_else(
  is.na(data_pubblicazione) & !is.na(data_comunicazione),
  data_comunicazione,
  data_pubblicazione
 ))

supercig25_cleaned <- supercig25_cleaned %>%
 mutate(anno_pubblicazione = if_else(
  is.na(anno_pubblicazione) & !is.na(anno_comunicazione),
  anno_comunicazione,
  anno_pubblicazione
 ))

n_distinct(supercig25_cleaned$cig)
n_distinct(supercig25_cleaned$cup)


### MERGE between PROJECT details and TENDER data ###

df1_cig <- merge(df1, supercig25_cleaned, by = "cup", all.x = T)
rm(supercig25_cleaned)

#do some cleaning
df1_cig <- df1_cig[ ,-c(33:63)]

df1_cig$importo_complessivo_gara <- as.numeric(df1_cig$importo_complessivo_gara)
df1_cig$log_importo_complessivo_gara <- NA
df1_cig$log_importo_complessivo_gara <- log(df1_cig$importo_complessivo_gara + 0.0000000001)

df1_cig <- df1_cig %>%
 mutate(regione.x = if_else(
  is.na(regione.x) & !is.na(regione.y),
  regione.y,
  regione.x
 ))

df1_cig <- df1_cig %>%
 mutate(provincia.x = if_else(
  is.na(provincia.x) & !is.na(provincia.y),
  provincia.y,
  provincia.x
 ))

df1_cig <- df1_cig %>%
 rename(regione=regione.x, regione=regione.y)

df1_cig <- df1_cig %>%
 rename(provincia=provincia.x, provincia=provincia.y)

df1_cig <- df1_cig %>%
 filter(!if_all(everything(), is.na))

df1_cig$award_status <- ifelse(!is.na(df1_cig$data_inizio_effettiva_aggiudicazione), 1, 0)
df1_cig$contractual_status <- ifelse(!is.na(df1_cig$data_fine_effettiva_stipula_contratto), 1, 0)
df1_cig$execution_status <- ifelse(!is.na(df1_cig$data_fine_effettiva_esecuzione_fornitura), 1, 0)
df1_cig$testing_status <- ifelse(!is.na(df1_cig$data_fine_effettiva_collaudo), 1, 0)
df1_cig$last_observation <- NA
df1_cig$last_observation <- as.Date("2024-12-13")
df1_cig$execution <- ifelse(df1_cig$execution_status==1, as.numeric(difftime(df1_cig$data_fine_effettiva_esecuzione_fornitura,df1_cig$data_inizio_effettiva_esecuzione_fornitura, units="days")), as.numeric(difftime(df1_cig$last_observation,df1_cig$data_fine_effettiva_stipula_contratto, units="days")))
df1_cig$contracting <- ifelse(df1_cig$contractual_status==1, as.numeric(difftime(df1_cig$data_fine_effettiva_stipula_contratto, df1_cig$data_finanziamento, units="days")), as.numeric(difftime(df1_cig$last_observation, df1_cig$data_finanziamento, units="days")))
df1_cig$award <- ifelse(df1_cig$award_status==1, as.numeric(difftime(df1_cig$data_pubblicazione, df1_cig$data_finanziamento, units="days")), as.numeric(difftime(df1_cig$last_observation, df1_cig$data_finanziamento, units="days")))
df1_cig$testing <- ifelse(df1_cig$testing_status==1, as.numeric(difftime(df1_cig$data_fine_effettiva_collaudo, df1_cig$data_inizio_effettiva_collaudo, units="days")), as.numeric(difftime(df1_cig$last_observation, df1_cig$data_inizio_effettiva_collaudo, units="days")))
df1_cig$completion <- ifelse(df1_cig$execution_status==1, as.numeric(difftime(df1_cig$data_fine_effettiva_esecuzione_fornitura, df1_cig$data_finanziamento, units="days")), as.numeric(difftime(df1_cig$last_observation, df1_cig$data_finanziamento, units="days")))

# =============================================================================
# BRIDGE: df1_cig2
#
# Everything downstream (R/02_analysis.R onward) expects an object called
# df1_cig2. The frame actually built above is called df1_cig throughout; the
# original script referenced df1_cig2 starting from the modelling section that
# used to follow this line, without ever creating it under that name. That
# rename happened once, interactively, in the console that produced the
# results, and was never captured in the saved script. Rather than silently
# renaming every variable above, or leaving a script that cannot run, the
# alias is made explicit here.
# =============================================================================
df1_cig2 <- df1_cig

cat(sprintf("df1_cig2 ready: %d rows, %d distinct CUP\n",
            nrow(df1_cig2), length(unique(df1_cig2$cup))))
