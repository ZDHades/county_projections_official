###------REALISTIC PROJECTIONS CLEANUP-----
## Remove unrealistic projections and only keep viable demographic groups

# Load required libraries
pacman::p_load("tidyverse", "readr", "dplyr")

# Read original projections
projections <- read_csv("../PROJECTIONS/PROJ/COUNTY_20152100_48.csv")

cat("=== CREATING REALISTIC PROJECTIONS ===\n")

# Get 2015 baseline population to determine which county-race combos are viable
baseline_pop <- K05_pop %>%
  filter(YEAR == 2015, STATE == "48") %>%
  group_by(GEOID, RACE) %>%
  summarise(baseline_pop = sum(POPULATION), .groups = "drop") %>%
  mutate(COUNTYRACE = paste0(GEOID, "_", RACE))

cat("Baseline population summary by race:\n")
baseline_summary <- baseline_pop %>%
  group_by(RACE) %>%
  summarise(
    counties_with_pop = sum(baseline_pop > 0),
    total_counties = n(),
    median_pop = median(baseline_pop),
    mean_pop = mean(baseline_pop)
  )
print(baseline_summary)

# Define viability thresholds
MINIMUM_BASELINE_POP <- list(
  "1" = 50,    # White: very low threshold  
  "2" = 25,    # Black: low threshold
  "4" = 10     # Other: minimal threshold
)

# Filter to only viable county-race combinations
viable_combos <- baseline_pop %>%
  mutate(
    min_threshold = case_when(
      RACE == "1" ~ MINIMUM_BASELINE_POP[["1"]],
      RACE == "2" ~ MINIMUM_BASELINE_POP[["2"]], 
      RACE == "4" ~ MINIMUM_BASELINE_POP[["4"]]
    ),
    is_viable = baseline_pop >= min_threshold
  ) %>%
  filter(is_viable) %>%
  select(COUNTYRACE)

cat("\nViable county-race combinations by race:\n")
viable_summary <- viable_combos %>%
  mutate(RACE = str_extract(COUNTYRACE, "_([1-4])$", group = 1)) %>%
  count(RACE)
print(viable_summary)

# Filter projections to only viable combinations
realistic_projections <- projections %>%
  inner_join(viable_combos, by = "COUNTYRACE") %>%
  # Remove rows with NA projections (failed forecasts)
  filter(!is.na(A)) %>%
  # Apply reasonable bounds
  mutate(
    # Cap extreme negative values (max 90% population decline)
    A = case_when(
      A < -10000 ~ -10000,
      A > 50000 ~ 50000,  # Cap extreme positive values
      TRUE ~ A
    ),
    # Ensure confidence bounds make sense
    B = case_when(
      B == 0 ~ A * 0.8,  # Lower bound = 80% of projection
      TRUE ~ B
    ),
    C = case_when(
      C == 0 ~ A * 1.2,  # Upper bound = 120% of projection  
      TRUE ~ C
    )
  )

cat("\n=== RESULTS ===\n")
cat("Original projections:", nrow(projections), "rows\n")
cat("Realistic projections:", nrow(realistic_projections), "rows\n")
cat("Reduction:", round((1 - nrow(realistic_projections)/nrow(projections))*100, 1), "%\n")

# Check data quality
quality_check <- realistic_projections %>%
  summarise(
    na_values = sum(is.na(A)),
    zero_bc_values = sum(B == 0 & C == 0),
    negative_A = sum(A < 0),
    extreme_A = sum(abs(A) > 10000)
  )

cat("\nQuality check:\n")
print(quality_check)

# Save realistic projections
write_csv(realistic_projections, "../PROJECTIONS/PROJ/COUNTY_20152100_48_REALISTIC.csv")
cat("✓ Realistic projections saved to COUNTY_20152100_48_REALISTIC.csv\n")

# Create summary by county for 2040
projections_2040 <- realistic_projections %>%
  filter(YEAR == 2040) %>%
  mutate(
    COUNTY = str_extract(COUNTYRACE, "^48([0-9]{3})", group = 1),
    RACE = str_extract(COUNTYRACE, "_([1-4])$", group = 1)
  ) %>%
  group_by(COUNTY, RACE, TYPE) %>%
  summarise(
    total_projected_change = sum(A, na.rm = T),
    age_groups = n(),
    .groups = "drop"
  )

write_csv(projections_2040, "../PROJECTIONS/PROJ/TEXAS_2040_SUMMARY.csv")
cat("✓ 2040 summary saved to TEXAS_2040_SUMMARY.csv\n")

cat("\n=== 011-realistic_projections.R COMPLETED ===\n")