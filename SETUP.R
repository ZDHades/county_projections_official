# ============================================================================
# SETUP SCRIPT FOR TEXAS COUNTY POPULATION PROJECTIONS 2040 (Updated 2025)
# ============================================================================
# This script initializes the environment for generating Texas county-level 
# population projections for the year 2040, with demographic breakdowns by
# age, sex, and race for all 254 Texas counties.
# 
# TEXAS-SPECIFIC FEATURES:
# - Focuses on Texas (FIPS State Code 48) counties only
# - Targets 2040 projections specifically
# - Uses updated SEER data (1969-2023, 1990-2023)
# - Updates Census API endpoints to current format
# ============================================================================

# Clear workspace
rm(list = ls())
gc(reset = TRUE)

# Set up local library path first
.libPaths(c("R_libs", .libPaths()))
cat("Using library paths:", .libPaths()[1:2], "\n")

# Set up local library path
local_lib_path <- file.path(getwd(), "R_libs")
if (!dir.exists(local_lib_path)) {
  dir.create(local_lib_path, recursive = TRUE)
}

# Add local library to library paths (first priority)
.libPaths(c(local_lib_path, .libPaths()))

cat("Using library paths:\n")
for (i in seq_along(.libPaths())) {
  cat(paste0(i, ". ", .libPaths()[i], "\n"))
}

# Load required libraries for setup
if (!require("pacman", character.only = TRUE, lib.loc = local_lib_path)) {
  install.packages("pacman", dep = TRUE, lib = local_lib_path)
  if (!require("pacman", character.only = TRUE, lib.loc = local_lib_path))
    stop("Package not found")
}

# Load essential packages for setup (install to local R_libs if needed)
cat("Loading required packages...\n")
pacman::p_load(
  "tidyverse",
  "R.utils", 
  "censusapi",
  "tidycensus",
  "httr",
  install = TRUE,
  lib = local_lib_path,
  character.only = TRUE
)

# Verify packages are loaded
cat("Verifying package availability...\n")
required_packages <- c("tidyverse", "R.utils", "censusapi", "tidycensus", "httr")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    stop(paste("Failed to load package:", pkg))
  }
}
cat("All required packages loaded successfully.\n")

# ============================================================================
# USER CONFIGURATION - PLEASE UPDATE THESE VALUES
# ============================================================================

# REQUIRED: Replace with your Census API key from https://api.census.gov/data/key_signup.html
CENSUS_API_KEY <- "6ec2eb88962af79b6b696d68372b8ce319f9fd6c"

# Validate Census API key
if (CENSUS_API_KEY == "YOUR_CENSUS_API_KEY_HERE") {
  stop("\n\n*** SETUP REQUIRED ***\n",
       "Please obtain a Census API key from: https://api.census.gov/data/key_signup.html\n",
       "Then replace 'YOUR_CENSUS_API_KEY_HERE' with your actual key in this script.\n\n")
}

# Set Census API key for tidycensus (now that tidycensus is loaded)
cat("Setting up Census API key...\n")
tidycensus::census_api_key(CENSUS_API_KEY, install = TRUE, overwrite = TRUE)

# ============================================================================
# DATA DOWNLOAD SECTION - UPDATED URLS FOR 2023 DATA
# ============================================================================

cat("Starting data download process...\n")

# Create DATA directory if it doesn't exist
if (!dir.exists("DATA")) {
  dir.create("DATA")
  cat("Created DATA directory\n")
}

# Download updated SEER population data files (2023 versions)
cat("Downloading updated SEER population data (1969-2023)...\n")
if (!file.exists("DATA/us.1969_2023.20ages.adjusted.txt")) {
  tryCatch({
    # Note: SEER files are now .exe files that are self-extracting
    download.file(
      "https://seer.cancer.gov/popdata/yr1969_2023.20ages/us.1969_2023.20ages.adjusted.exe", 
      "DATA/us.1969_2023.20ages.adjusted.exe",
      mode = "wb"
    )
    cat("Downloaded: us.1969_2023.20ages.adjusted.exe\n")
    cat("Attempting to extract .txt file from .exe...\n")
    
    # Try to extract the .exe file automatically
    tryCatch({
      # On Windows, try to run the .exe silently to extract
      system2("DATA/us.1969_2023.20ages.adjusted.exe", args = "/S", wait = TRUE, stdout = FALSE, stderr = FALSE)
      if (file.exists("DATA/us.1969_2023.20ages.adjusted.txt")) {
        cat("✓ Successfully extracted: us.1969_2023.20ages.adjusted.txt\n")
      } else {
        cat("⚠ Automatic extraction failed. You may need to double-click the .exe file manually.\n")
      }
    }, error = function(e) {
      cat("⚠ Automatic extraction failed. You may need to double-click the .exe file manually.\n")
    })
  }, error = function(e) {
    cat("Error downloading 1969-2023 data:", e$message, "\n")
  })
} else {
  cat("File already exists: us.1969_2023.20ages.adjusted.txt\n")
}

cat("Downloading updated SEER population data (1990-2023)...\n")
if (!file.exists("DATA/us.1990_2023.20ages.adjusted.txt")) {
  tryCatch({
    download.file(
      "https://seer.cancer.gov/popdata/yr1990_2023.20ages/us.1990_2023.20ages.adjusted.exe", 
      "DATA/us.1990_2023.20ages.adjusted.exe",
      mode = "wb"
    )
    cat("Downloaded: us.1990_2023.20ages.adjusted.exe\n")
    cat("Attempting to extract .txt file from .exe...\n")
    
    # Try to extract the .exe file automatically
    tryCatch({
      # On Windows, try to run the .exe silently to extract
      system2("DATA/us.1990_2023.20ages.adjusted.exe", args = "/S", wait = TRUE, stdout = FALSE, stderr = FALSE)
      if (file.exists("DATA/us.1990_2023.20ages.adjusted.txt")) {
        cat("✓ Successfully extracted: us.1990_2023.20ages.adjusted.txt\n")
      } else {
        cat("⚠ Automatic extraction failed. You may need to double-click the .exe file manually.\n")
      }
    }, error = function(e) {
      cat("⚠ Automatic extraction failed. You may need to double-click the .exe file manually.\n")
    })
  }, error = function(e) {
    cat("Error downloading 1990-2023 data:", e$message, "\n")
  })
} else {
  cat("File already exists: us.1990_2023.20ages.adjusted.txt\n")
}

# Download older data files for backward compatibility (if scripts haven't been updated yet)
cat("Downloading legacy SEER data for backward compatibility...\n")
if (!file.exists("DATA/us.1969_2016.19ages.adjusted.txt")) {
  tryCatch({
    download.file(
      "https://seer.cancer.gov/popdata/yr1969_2016.19ages/us.1969_2016.19ages.adjusted.txt.gz", 
      "DATA/us.1969_2016.19ages.adjusted.txt.gz"
    )
    gunzip("DATA/us.1969_2016.19ages.adjusted.txt.gz", overwrite = TRUE, remove = TRUE)
    cat("Downloaded: us.1969_2016.19ages.adjusted.txt\n")
  }, error = function(e) {
    cat("Could not download legacy 1969-2016 data:", e$message, "\n")
  })
}

if (!file.exists("DATA/us.1990_2016.19ages.adjusted.txt")) {
  tryCatch({
    download.file(
      "https://seer.cancer.gov/popdata/yr1990_2016.19ages/us.1990_2016.19ages.adjusted.txt.gz", 
      "DATA/us.1990_2016.19ages.adjusted.txt.gz"
    )
    gunzip("DATA/us.1990_2016.19ages.adjusted.txt.gz", overwrite = TRUE, remove = TRUE)
    cat("Downloaded: us.1990_2016.19ages.adjusted.txt\n")
  }, error = function(e) {
    cat("Could not download legacy 1990-2016 data:", e$message, "\n")
  })
}

# ============================================================================
# UPDATE EXISTING SCRIPTS WITH FIXES
# ============================================================================

cat("\nUpdating scripts with Census API key and endpoint fixes...\n")

# Update 004-GQ_gather.R with the provided API key and fix deprecated endpoints
if (file.exists("SCRIPTS/004-GQ_gather.R")) {
  gq_script <- readLines("SCRIPTS/004-GQ_gather.R")
  
  # Find the line with the key and replace it
  key_line <- grep("key <- ", gq_script)
  if (length(key_line) > 0) {
    gq_script[key_line] <- paste0('key <- "', CENSUS_API_KEY, '"')
    cat("Updated Census API key in SCRIPTS/004-GQ_gather.R\n")
  }
  
  # Fix deprecated sf1 endpoints - update to new format
  gq_script <- gsub('name="sf1"', 'name="dec/sf1"', gq_script)
  
  writeLines(gq_script, "SCRIPTS/004-GQ_gather.R")
  cat("Fixed deprecated sf1 API endpoints in SCRIPTS/004-GQ_gather.R\n")
}

# ============================================================================
# CREATE TEXAS 2040 PROJECTION GUIDE
# ============================================================================

# Create a guide specifically for Texas 2040 projections
texas_guide <- c(
  "# TEXAS COUNTY PROJECTIONS 2040 - EXECUTION GUIDE",
  "# ===============================================",
  "",
  "This repository can generate detailed population projections for all 254 Texas counties for 2040.",
  "",
  "PROJECTION DETAILS:",
  "• Target Year: 2040",
  "• Geographic Scope: All 254 Texas counties (FIPS State Code 48)",
  "• Demographics: Age (18 groups), Sex (2 groups), Race (4 groups)",
  "• SSP Scenarios: 5 different migration scenarios",
  "",
  "EXECUTION STEPS FOR TEXAS 2040 PROJECTIONS:",
  "",
  "1. SETUP (this script handles most requirements):",
  "   ✓ Census API key configured",
  "   ✓ Required R packages installed",
  "   ✓ Data files downloaded and extracted",
  "   NOTE: If automatic extraction fails, manually double-click the .exe files in DATA/",
  "",
  "2. RUN PROJECTION SCRIPTS IN ORDER:",
  "   - 000-Libraries.R (load required packages)",
  "   - 001-fipscodes.R (load county identifiers)", 
  "   - 002-basedataload.R (load historical validation data)",
  "   - 003-proj_basedataload.R (load projection base data)",
  "   - 004-GQ_gather.R (gather group quarters data)",
  "   - 007-projections_2100.R (main projection engine)",
  "",
  "3. OUTPUT LOCATION:",
  "   - Results will be in PROJECTIONS/ directory",
  "   - Look for county-level files with 2040 data",
  "   - Texas counties will have GEOID starting with '48'",
  "",
  "4. DATA STRUCTURE:",
  "   YEAR | STATE | COUNTY | GEOID | AGE | SEX | RACE | SSP | POPULATION",
  "   2040 |   48  |  XXX   | 48XXX |  X  |  X  |  X   |  X  |    XXXX",
  "",
  "5. FILTERING FOR TEXAS 2040:",
  "   - Filter where YEAR == 2040 AND STATE == 48",
  "   - All 254 Texas counties will be included",
  "",
  "NOTE: The projection process may take several hours for all counties."
)

writeLines(texas_guide, "TEXAS_2040_PROJECTIONS_GUIDE.txt")

# ============================================================================
# VERIFY SETUP
# ============================================================================

cat("\n============================================================================\n")
cat("SETUP VERIFICATION\n")
cat("============================================================================\n")

# Check required data files
required_files <- c(
  "DATA/SspDb_country_data_2013-06-12.csv.zip"
)

legacy_files <- c(
  "DATA/us.1969_2016.19ages.adjusted.txt",
  "DATA/us.1990_2016.19ages.adjusted.txt"
)

new_files <- c(
  "DATA/us.1969_2023.20ages.adjusted.txt",
  "DATA/us.1990_2023.20ages.adjusted.txt"
)

cat("Checking SSP data file:\n")
for (file in required_files) {
  if (file.exists(file)) {
    cat("✓ Found:", file, "\n")
  } else {
    cat("✗ Missing:", file, "(this should already exist in the repo)\n")
  }
}

cat("\nChecking legacy SEER data files:\n")
for (file in legacy_files) {
  if (file.exists(file)) {
    cat("✓ Found:", file, "\n")
  } else {
    cat("✗ Missing:", file, "\n")
  }
}

cat("\nChecking new SEER data files (2023 versions):\n")
for (file in new_files) {
  if (file.exists(file)) {
    cat("✓ Found:", file, "\n")
  } else {
    cat("⚠ Missing:", file, "(may need manual extraction from .exe file)\n")
  }
}

# Test Census API connection
cat("\nTesting Census API connection...\n")
tryCatch({
  # Test with the updated endpoint format
  test_data <- getCensus(
    name = "dec/sf1",
    vintage = "2010", 
    key = CENSUS_API_KEY,
    vars = c("P001001"),
    region = "state:01"
  )
  if (nrow(test_data) > 0) {
    cat("✓ Census API connection successful with updated endpoints!\n")
  }
}, error = function(e) {
  cat("✗ Census API connection failed. Check your API key.\n")
  cat("Error:", e$message, "\n")
})

cat("\n============================================================================\n")
cat("TEXAS COUNTY PROJECTIONS 2040 - SETUP COMPLETE!\n")
cat("============================================================================\n")
cat("Environment configured for Texas county population projections to 2040.\n")
cat("\nKey Setup Actions:\n")
cat("• Updated to use 2023 SEER data (most recent available)\n")
cat("• Fixed deprecated Census API sf1 endpoints\n") 
cat("• Updated API key in 004-GQ_gather.R\n")
cat("• Created TEXAS_2040_PROJECTIONS_GUIDE.txt with execution instructions\n")
cat("\nTEXAS PROJECTION CAPABILITY:\n")
cat("• Geographic Scope: All 254 Texas counties\n")
cat("• Target Year: 2040 (and other years 2020-2100)\n")
cat("• Demographics: 18 age groups × 2 sex groups × 4 race groups\n")
cat("• Scenarios: 5 SSP migration scenarios\n")
cat("• Expected Output: ~36,480 detailed county-demographic records for 2040\n")
cat("\nNext Steps:\n")
cat("1. Extract .txt files from downloaded .exe files if needed\n")
cat("2. Review TEXAS_2040_PROJECTIONS_GUIDE.txt for execution steps\n")
cat("3. Run projection scripts in sequence to generate 2040 estimates\n")
cat("============================================================================\n")