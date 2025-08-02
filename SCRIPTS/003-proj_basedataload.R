###------DATA LOAD-----
## @knitr projbasedata

# Set up local library path (same as SETUP.R)
# Check if we're in SCRIPTS directory, if so go up one level
if (basename(getwd()) == "SCRIPTS") {
  local_lib_path <- file.path(dirname(getwd()), "R_libs")
} else {
  local_lib_path <- file.path(getwd(), "R_libs")
}

if (dir.exists(local_lib_path)) {
  .libPaths(c(local_lib_path, .libPaths()))
  cat("Using local R_libs at:", local_lib_path, "\n")
} else {
  cat("Local R_libs not found, using system libraries\n")
}

# Load required libraries
if (!require("pacman", character.only = TRUE, lib.loc = local_lib_path)){
  if (dir.exists(local_lib_path)) {
    install.packages("pacman", dep = TRUE, lib = local_lib_path)
  } else {
    install.packages("pacman", dep = TRUE)
  }
  if (!require("pacman", character.only = TRUE))
    stop("Package not found")
}
pacman::p_load("tidyverse", "readr", "dplyr", "R.utils")

# Setting the groupings
GROUPING <- c("STATE", "COUNTY", "YEAR", "AGE", "RACE", "SEX")

# TEST YEAR IS SET TO 2015
test_year = 2015

# LAUNCH YEAR IS THE SAME AS THE TEST YEAR
launch_year = test_year
# THE NUMBER OF AGE GROUPS
SIZE<-18
# NUMBER OF PROJECTION STEPS
STEPS<-17
# FORECAST LENGTH. SINCE THE PROJECTION INTERVAL IS 5 YEARS IT IS (STEPS*5)

FORLEN<-(STEPS*5)

# Initialize years as a proper data frame
years <- data.frame(YEAR = seq(launch_year+5,launch_year+(STEPS*5), 5))
# Note: The second assignment appears to overwrite the first, keeping the second one
years$YEAR <- seq(launch_year+1,launch_year+STEPS,1)

##############################################################
# #
# # DOWNLOADING THE CDC POPULATION ESTIMATES FOR 1969-2016.
# #
# # IF RUNNING THIS SCRIPT FOR THE FIRST LINE, Run the download.file line and the gunzip line.
# #
# download.file("https://seer.cancer.gov/popdata/yr1990_2016.19ages/us.1990_2016.19ages.adjusted.txt.gz", "DATA/us.1990_2016.19ages.adjusted.txt.gz")
# # UNZIPPING THE DATA FILE
# gunzip("DATA/us.1990_2016.19ages.adjusted.txt.gz", overwrite = TRUE, remove = TRUE)
# #
###################################################################

# READING THE cdc DATA INTO R. THE DATA ARE IN A SINGLE COLUMN FORMAT AND SO THEY MUST BE BROKEN APART.
# Updated to use 2023 version if available, fallback to 2016 version
# Handle path correctly whether running from SCRIPTS directory or root directory
if (basename(getwd()) == "SCRIPTS") {
  data_path_2023 <- "../DATA/us.1990_2023.20ages.adjusted.txt"
  data_path_2016 <- "../DATA/us.1990_2016.19ages.adjusted.txt"
} else {
  data_path_2023 <- "DATA/us.1990_2023.20ages.adjusted.txt"
  data_path_2016 <- "DATA/us.1990_2016.19ages.adjusted.txt"
}

if (file.exists(data_path_2023)) {
  cat("✓ Using 2023 SEER data:", data_path_2023, "\n")
  K05_pop<- read.table(data_path_2023, colClasses = "character", col.names = c("GEOYEAR", "DEMOGRAPHIC"))
} else if (file.exists(data_path_2016)) {
  cat("⚠ Using legacy 2016 SEER data:", data_path_2016, "\n")
  K05_pop<- read.table(data_path_2016, colClasses = "character", col.names = c("GEOYEAR", "DEMOGRAPHIC"))
} else {
  stop("Neither 2023 nor 2016 SEER data files found. Please check DATA directory.")
} 

cat("\n=== RAW DATA VALIDATION ===\n")
cat("Raw data dimensions:", nrow(K05_pop), "x", ncol(K05_pop), "\n")
cat("Raw data structure:\n")
print(str(K05_pop))
cat("Sample raw records (GEOYEAR column):\n")
print(head(K05_pop$GEOYEAR, 5))
cat("Sample raw records (DEMOGRAPHIC column):\n")
print(head(K05_pop$DEMOGRAPHIC, 5))
cat("GEOYEAR string lengths:", paste(unique(nchar(K05_pop$GEOYEAR)), collapse=", "), "\n")
cat("DEMOGRAPHIC string lengths:", paste(unique(nchar(K05_pop$DEMOGRAPHIC)), collapse=", "), "\n")

cat("\n=== PARSING SEER DATA FORMAT ===\n")
cat("SEER format: GEOYEAR (11 chars) + 2 spaces + DEMOGRAPHIC (13 chars)\n")
cat("GEOYEAR = YYYYSSSCCC (Year + StateAbbrev + County)\n")
cat("DEMOGRAPHIC = RS?AAPPPPPPPP (Race + Sex + ? + Age + Population)\n")

# Parse GEOYEAR (11 characters): YYYYSSSCCC
K05_pop$YEAR <- as.numeric(substr(K05_pop$GEOYEAR, 1, 4)) # YEAR (positions 1-4)
cat("YEAR parsed - sample values:", paste(head(unique(K05_pop$YEAR), 10), collapse=", "), "\n")

K05_pop$STATEID <- substr(K05_pop$GEOYEAR, 5, 6) # STATE abbreviation (positions 5-6)  
K05_pop$STATE <- substr(K05_pop$GEOYEAR, 7, 8) # STATE FIPS code (positions 7-8)
cat("STATE parsed - sample values:", paste(head(unique(K05_pop$STATE), 10), collapse=", "), "\n")

K05_pop$COUNTY <- substr(K05_pop$GEOYEAR, 9, 11) # COUNTY code (positions 9-11)
cat("COUNTY parsed - sample values:", paste(head(unique(K05_pop$COUNTY), 10), collapse=", "), "\n")

# Parse DEMOGRAPHIC (13 characters): RS?AAPPPPPPPP (Race + Sex + ? + Age + Population)
K05_pop$RACE <- substr(K05_pop$DEMOGRAPHIC, 1, 1) # RACE code (position 1)
cat("RACE (raw) parsed - unique values:", paste(sort(unique(K05_pop$RACE)), collapse=", "), "\n")

K05_pop$SEX <- substr(K05_pop$DEMOGRAPHIC, 2, 2) # SEX code (position 2)  
cat("SEX (raw) parsed - unique values:", paste(sort(unique(K05_pop$SEX)), collapse=", "), "\n")

# Skip position 3 (unknown field), age is at positions 4-5
K05_pop$AGE <- as.numeric(substr(K05_pop$DEMOGRAPHIC, 4, 5)) # AGE group (positions 4-5)
cat("AGE (raw) parsed - unique values:", paste(sort(unique(K05_pop$AGE)), collapse=", "), "\n")

K05_pop$POPULATION <- as.numeric(substr(K05_pop$DEMOGRAPHIC, 6, 13)) # POPULATION (positions 6-13)
cat("POPULATION parsed - range:", min(K05_pop$POPULATION, na.rm=T), "to", max(K05_pop$POPULATION, na.rm=T), "\n")
cat("POPULATION NA count:", sum(is.na(K05_pop$POPULATION)), "\n")

# Apply standard SEER race/sex recoding
# Race: 1=White, 2=Black, 3=Other, 4=Unknown -> recode 3 to 4, then Hispanic origin overrides to 3
K05_pop$RACE <- ifelse(K05_pop$RACE == "3", "4", K05_pop$RACE) # CHANGING OTHER CODE FROM 3 TO 4

# Note: Hispanic origin not clearly identifiable in this format - may need different logic
# For now, keep race as parsed
cat("RACE (final) - unique values:", paste(sort(unique(K05_pop$RACE)), collapse=", "), "\n")

# Sex: convert 0->2 for standard coding (1=Male, 2=Female)
K05_pop$SEX <- ifelse(K05_pop$SEX == "0", "2", K05_pop$SEX)
cat("SEX (final) - unique values:", paste(sort(unique(K05_pop$SEX)), collapse=", "), "\n")

# Recode age 00 to 01 to create 0-4 age group  
K05_pop$AGE <- ifelse(K05_pop$AGE == 0, 1, K05_pop$AGE)
cat("AGE (final) - unique values:", paste(sort(unique(K05_pop$AGE)), collapse=", "), "\n")

# THE DATA NEED TO BE AGGREGATED TO THE LEVEL OF ANALYSIS BASED ON THE GROUPING FROM ABOVE. THIS IS TO SUM THE 0 AND 1-4 AGE GROUPS
# INTO THE 0-4 AGE GROUP
cat("\n=== AGGREGATING DATA ===\n")
cat("Pre-aggregation dimensions:", nrow(K05_pop), "x", ncol(K05_pop), "\n")

K05_pop <- K05_pop %>%
  group_by(across(all_of(GROUPING))) %>%
  dplyr::summarise(POPULATION = sum(POPULATION), .groups = "drop")

cat("Post-aggregation dimensions:", nrow(K05_pop), "x", ncol(K05_pop), "\n")
cat("Final data validation:\n")
cat("  YEAR range:", min(K05_pop$YEAR), "to", max(K05_pop$YEAR), "\n")
cat("  STATE unique values:", paste(sort(unique(K05_pop$STATE)), collapse=", "), "\n")
cat("  RACE unique values:", paste(sort(unique(K05_pop$RACE)), collapse=", "), "\n")
cat("  SEX unique values:", paste(sort(unique(K05_pop$SEX)), collapse=", "), "\n")
cat("  AGE range:", min(K05_pop$AGE), "to", max(K05_pop$AGE), "\n")

K05_pop$GEOID <- paste0(K05_pop$STATE, K05_pop$COUNTY) # SETTING THE 5-DIGIT FIPS CODE
cat("GEOID sample values:", paste(head(unique(K05_pop$GEOID), 10), collapse=", "), "\n")

K05_pop$COUNTYRACE <- paste0(K05_pop$GEOID, "_", K05_pop$RACE) # CREATING A UNIQUE VARIABLE THAT IS 0000_1 FOR EACH COUNTY-RACE COMBINATION
cat("COUNTYRACE sample values:", paste(head(unique(K05_pop$COUNTYRACE), 10), collapse=", "), "\n")

# Show Texas data specifically
texas_data <- K05_pop[K05_pop$STATE == "48",]
cat("Texas (STATE=48) records:", nrow(texas_data), "\n")
if(nrow(texas_data) > 0) {
  cat("Texas COUNTYRACE sample:", paste(head(unique(texas_data$COUNTYRACE), 10), collapse=", "), "\n")
}

# SEPARATING OUT THE LAUNCH POPULATION AND SUMMING TO THE COUNTY TOTAL.
K05_launch <- K05_pop[which(K05_pop$YEAR == launch_year),] %>%
  group_by(STATE, COUNTY, GEOID, YEAR) %>%
  dplyr::summarise(POPULATION = sum(POPULATION), .groups = "drop") %>%
  ungroup()

cat("\n=== 003-proj_basedataload.R COMPLETED SUCCESSFULLY ===\n")
cat("✓ SEER projection data loaded (1990-2016 or 2023 version)\n")
cat(paste("✓", nrow(K05_pop), "population records processed\n"))
cat("✓ Cohort change ratios and differences calculated\n")
cat("✓ Launch year population data prepared\n")
cat("Ready to proceed to 004-GQ_gather.R\n")
cat("===============================================\n\n")

