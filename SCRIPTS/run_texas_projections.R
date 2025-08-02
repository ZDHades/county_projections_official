#!/usr/bin/env Rscript
# Texas 2040 Population Projections - Complete Setup and Run Script
# This script runs all necessary steps to generate Texas county projections

cat("=== TEXAS 2040 POPULATION PROJECTIONS ===\n")
cat("Setting up environment and running projections...\n\n")

# Check if we already have the data loaded in the current session
if (exists("K05_pop") && exists("stateid")) {
  cat("✓ Data already loaded in current R session\n")
  cat("✓ K05_pop:", nrow(K05_pop), "records\n")
  cat("✓ stateid:", length(stateid), "states\n")
} else {
  cat("Loading required data from scripts...\n")
  
  # Load FIPS codes
  cat("1/3 Loading FIPS codes...\n")
  source('001-fipscodes.R')
  
  # Load projection base data
  cat("2/3 Loading projection base data...\n") 
  source('003-proj_basedataload.R')
  
  # Set ARIMA order
  cat("3/3 Setting ARIMA parameters...\n")
  if (!exists("arima_order")) {
    arima_order <- c(0,1,1)
  }
  
  cat("✓ All data loaded successfully\n")
}

# Save workspace for future use
cat("Saving workspace for future runs...\n")
save(K05_pop, stateid, launch_year, test_year, SIZE, STEPS, FORLEN, GROUPING, arima_order, 
     file = '.RData_session')
cat("✓ Workspace saved\n\n")

# Now run the main projection script
cat("=== RUNNING TEXAS PROJECTIONS ===\n")
source('007-projections_2100.R')

cat("\n=== TEXAS PROJECTIONS COMPLETE ===\n")
cat("Check PROJECTIONS/PROJ/ directory for output files\n")
cat("Filter for YEAR=2040 and STATE=48 to get Texas 2040 data\n")