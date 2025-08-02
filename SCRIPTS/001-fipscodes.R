###------FIPSCODES-----
## @knitr fipscodes

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
pacman::p_load("tidyverse", "readr", "dplyr")

fipslist <- read_csv(file="https://www2.census.gov/geo/docs/reference/codes/files/national_county.txt", col_names = FALSE) %>%
  mutate(GEOID = paste0(X2, X3)) %>%
  dplyr::rename(state = X1,
                STATEID = X2,
                CNTYID = X3,
                NAME = X4) %>%
  filter(!STATEID %in% c("60", "66", "69", "72", "74", "78"))

# Converting the fipslist into a unique list of 2-digit state ID's #
stateid = unlist(list(unique(fipslist$STATEID)))
# Converting the fipslist into a unique list of 5-digit county ID's #
GEOID = unlist(list(unique(fipslist$GEOID)))

statenames <- group_by(fipslist, STATEID, state) %>%
  dplyr::summarise()
countynames <- group_by(fipslist, GEOID, NAME, state) %>%
  dplyr::summarise()

cat("\n=== 001-fipscodes.R COMPLETED SUCCESSFULLY ===\n")
cat("✓ FIPS codes downloaded from Census Bureau\n")
cat("✓ State and county lists created\n")
cat(paste("✓", length(stateid), "states processed\n"))
cat(paste("✓", length(GEOID), "counties processed\n"))
cat("Ready to proceed to 002-basedataload.R\n")
cat("===============================================\n\n")