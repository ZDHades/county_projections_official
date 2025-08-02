###------FILL MISSING PROJECTIONS-----
## Fill NA and zero values with reasonable estimates

# Load required libraries
pacman::p_load("tidyverse", "readr", "dplyr")

# Read the projection results
projections <- read_csv("../PROJECTIONS/PROJ/COUNTY_20152100_48.csv")

cat("=== MISSING DATA ANALYSIS ===\n")
cat("Total rows:", nrow(projections), "\n")
cat("NA values in column A:", sum(is.na(projections$A)), "\n")
cat("Zero values in B&C:", sum(projections$B == 0 & projections$C == 0), "\n")

# Analyze patterns by race
missing_by_race <- projections %>%
  mutate(RACE = str_extract(COUNTYRACE, "_([1-4])$", group = 1)) %>%
  group_by(RACE) %>%
  summarise(
    total = n(),
    na_count = sum(is.na(A)),
    zero_bc = sum(B == 0 & C == 0, na.rm = T),
    .groups = "drop"
  )
print(missing_by_race)

cat("\n=== FILLING MISSING VALUES ===\n")

# Strategy 1: Use state-level averages for missing race groups
projections_filled <- projections %>%
  mutate(
    RACE = str_extract(COUNTYRACE, "_([1-4])$", group = 1),
    COUNTY = str_extract(COUNTYRACE, "^48([0-9]{3})", group = 1)
  ) %>%
  group_by(YEAR, SEX, AGE, RACE, TYPE) %>%
  mutate(
    # Calculate state-level median for each demographic group
    state_median_A = median(A, na.rm = TRUE),
    state_median_B = median(B, na.rm = TRUE), 
    state_median_C = median(C, na.rm = TRUE),
    
    # Fill NAs with state medians
    A_filled = ifelse(is.na(A), state_median_A, A),
    B_filled = ifelse(B == 0, state_median_B, B),
    C_filled = ifelse(C == 0, state_median_C, C)
  ) %>%
  ungroup()

# Strategy 2: For still-missing values, use race group 1 (White) patterns scaled down
projections_final <- projections_filled %>%
  group_by(YEAR, SEX, AGE, COUNTY, TYPE) %>%
  mutate(
    # Get white population values for scaling 
    white_A = A_filled[RACE == "1"][1],
    
    # For minority groups with no data, use 10% of white population
    A_final = case_when(
      !is.na(A_filled) ~ A_filled,
      RACE == "2" ~ pmax(white_A * 0.1, 1), # Black: 10% of white
      RACE == "4" ~ pmax(white_A * 0.05, 1), # Other: 5% of white  
      TRUE ~ A_filled
    ),
    
    B_final = case_when(
      B_filled != 0 ~ B_filled,
      RACE == "2" ~ pmax(white_A * 0.08, 0),
      RACE == "4" ~ pmax(white_A * 0.03, 0),
      TRUE ~ B_filled
    ),
    
    C_final = case_when(
      C_filled != 0 ~ C_filled,
      RACE == "2" ~ pmax(white_A * 0.12, 1),
      RACE == "4" ~ pmax(white_A * 0.07, 1), 
      TRUE ~ C_filled
    )
  ) %>%
  ungroup() %>%
  select(-RACE, -COUNTY, -state_median_A, -state_median_B, -state_median_C, 
         -A_filled, -B_filled, -C_filled, -white_A, -A, -B, -C) %>%
  rename(A = A_final, B = B_final, C = C_final)

cat("=== RESULTS AFTER FILLING ===\n")
cat("Remaining NA values in A:", sum(is.na(projections_final$A)), "\n")
cat("Remaining zero values in B&C:", sum(projections_final$B == 0 & projections_final$C == 0), "\n")

# Save filled projections
write_csv(projections_final, "../PROJECTIONS/PROJ/COUNTY_20152100_48_FILLED.csv")
cat("✓ Filled projections saved to COUNTY_20152100_48_FILLED.csv\n")

cat("\n=== 008-fill_missing_projections.R COMPLETED ===\n")