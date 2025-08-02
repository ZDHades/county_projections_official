###------COUNTY POOLING STRATEGY-----
## Group similar counties to improve projections for sparse data

# Load required libraries  
pacman::p_load("tidyverse", "readr", "dplyr")

# Classify Texas counties by population size and characteristics
classify_counties <- function() {
  
  # Read 2015 baseline population (launch year)
  baseline_pop <- K05_pop %>%
    filter(YEAR == 2015, STATE == "48") %>%
    group_by(GEOID) %>%
    summarise(total_pop_2015 = sum(POPULATION), .groups = "drop")
  
  # Classify counties
  county_classes <- baseline_pop %>%
    mutate(
      COUNTY_TYPE = case_when(
        total_pop_2015 >= 500000 ~ "MAJOR_URBAN",      # Harris, Dallas, etc.
        total_pop_2015 >= 100000 ~ "URBAN",            # Mid-size cities
        total_pop_2015 >= 25000  ~ "SUBURBAN",         # Suburban counties  
        total_pop_2015 >= 5000   ~ "SMALL_TOWN",       # Small towns
        TRUE ~ "RURAL"                                 # Rural counties
      ),
      pop_2015 = total_pop_2015
    )
  
  cat("County Classification:\n")
  print(table(county_classes$COUNTY_TYPE))
  
  return(county_classes)
}

# Strategy for using pooled estimates
pool_estimates_strategy <- function() {
  cat("\n=== POOLING STRATEGY ===\n")
  cat("1. MAJOR_URBAN (6+ counties): Use individual county estimates\n")
  cat("2. URBAN (20+ counties): Use individual estimates, smooth outliers\n") 
  cat("3. SUBURBAN (40+ counties): Pool similar counties for sparse race groups\n")
  cat("4. SMALL_TOWN (80+ counties): Pool extensively for minorities\n")
  cat("5. RURAL (100+ counties): Use regional averages for all minorities\n")
  
  cat("\nTo implement:\n")
  cat("- Run classify_counties() first\n")
  cat("- Modify projection function to use pooled data for small counties\n")
  cat("- For race groups 2&4 in RURAL/SMALL_TOWN: average across county type\n")
}

# Example usage
if(exists("K05_pop")) {
  county_classes <- classify_counties()
  pool_estimates_strategy()
} else {
  cat("Load K05_pop data first, then run this script\n")
}