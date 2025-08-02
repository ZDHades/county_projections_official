###------DATA LOAD-----
## @knitr basedataload

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
pacman::p_load("tidyverse", "readr", "dplyr", "tigris", "sp", "sf", "R.utils")

# Setting the groupings
GROUPING <- c("STATE", "COUNTY", "YEAR", "AGE", "RACE", "SEX")

# TEST YEAR IS SET TO 2000
test_year = 2000
# LAUNCH YEAR IS THE SAME AS THE TEST YEAR
launch_year = test_year
# THE NUMBER OF AGE GROUPS
SIZE<-18
# NUMBER OF PROJECTION STEPS
STEPS<-3
# FORECAST LENGTH. SINCE THE PROJECTION INTERVAL IS 5 YEARS IT IS (STEPS*5)
FORLEN<-(STEPS*5)

# Initialize years as a proper data frame
years <- data.frame(YEAR = seq(launch_year+5,launch_year+(STEPS*5), 5))
# Note: The second assignment appears to overwrite the first, keeping the second one
years$YEAR <- seq(launch_year+1,launch_year+STEPS,1)

##############################################################
#
# DOWNLOADING THE CDC POPULATION ESTIMATES FOR 1969-2016.
#
# IF RUNNING THIS SCRIPT FOR THE FIRST LINE, Run the download.file line and the gunzip line.
#
# download.file("https://seer.cancer.gov/popdata/yr1969_2016.19ages/us.1969_2016.19ages.adjusted.txt.gz", "DATA/us.1969_2016.19ages.adjusted.txt.gz")
# UNZIPPING THE DATA FILE
# gunzip("DATA/us.1969_2016.19ages.adjusted.txt.gz", overwrite = TRUE, remove = TRUE)
#
###################################################################

# READING THE cdc DATA INTO R. THE DATA ARE IN A SINGLE COLUMN FORMAT AND SO THEY MUST BE BROKEN APART.
# Updated to use 2023 version if available, fallback to 2016 version
# Handle path correctly whether running from SCRIPTS directory or root directory
if (basename(getwd()) == "SCRIPTS") {
  data_path_2023 <- "../DATA/us.1969_2023.20ages.adjusted.txt"
  data_path_2016 <- "../DATA/us.1969_2016.19ages.adjusted.txt"
} else {
  data_path_2023 <- "DATA/us.1969_2023.20ages.adjusted.txt"
  data_path_2016 <- "DATA/us.1969_2016.19ages.adjusted.txt"
}

if (file.exists(data_path_2023)) {
  cat("✓ Using 2023 SEER data:", data_path_2023, "\n")
  K05_pop<- read.table(data_path_2023)
} else if (file.exists(data_path_2016)) {
  cat("⚠ Using legacy 2016 SEER data:", data_path_2016, "\n")
  K05_pop<- read.table(data_path_2016)
} else {
  stop("Neither 2023 nor 2016 SEER data files found. Please check DATA directory.")
} 
K05_pop$V1 <- as.character(K05_pop$V1) # SETTING THE ENTIRE SINGLE VARIABLE INTO A CHARACTER
K05_pop$YEAR <- as.numeric(substr(K05_pop$V1,1,4)) # SEPARATING THE YEAR AND SETTING IT AS A NUMBER
K05_pop$STATEID <- substr(K05_pop$V1, 5,6) # SEPARATING THE 2 CHARACTER STATE ABBREVIATION
K05_pop$STATE <- substr(K05_pop$V1, 7,8) # SEPARATING THE 2-DIGIT STATE CODE
K05_pop$COUNTY <- substr(K05_pop$V1,9,11) # SEPARATING THE 3-DIGIT COUNTY CODE
K05_pop$REGISTRY <- substr(K05_pop$V1, 12,12) # REGISTRY IS A THROW AWAY VARIABLE REFERING TO ODD GEOGRAPHIES
K05_pop$RACE <- substr(K05_pop$V1, 14,14) # SEPARATING OUT THE RACE CODES.
K05_pop$ORIGIN <- substr(K05_pop$V1, 15,15) # SEPARATING OUT HISPANIC ORIGIN. THIS VARIABLE IS NOT APPLICABLE IN THE 1969-2016 DATA
K05_pop$SEX <- substr(K05_pop$V1, 16,16) # SEPARATING OUT THE SEX DATA

# SEPARATING OUT AGE CATEGORIES. THE CDC DATA CONSISTS OF 19 AGE GROUPS WHERE "00" IS CODED AS 0 YEAR OLDS AND "01" IS CODED AS 1-4 YEAR OLDS.
# I RECODE 00 TO 01 TO CREATE A 0-4 YEAR OLD AGE GROUP.
K05_pop$AGE <- as.numeric(if_else(substr(K05_pop$V1, 17, 18) == "00","01",substr(K05_pop$V1, 17, 18)))

K05_pop$POPULATION <- as.numeric(substr(K05_pop$V1, 19, 30)) # SEPARATING THE ACTUAL POPULATION ESTIMATES.

# THE DATA NEED TO BE AGGREGATED TO THE LEVEL OF ANALYSIS BASED ON THE GROUPING FROM ABOVE. THIS IS TO SUM THE 0 AND 1-4 AGE GROUPS
# INTO THE 0-4 AGE GROUP
K05_pop <- K05_pop %>%
  group_by(across(all_of(GROUPING))) %>%
  dplyr::summarise(POPULATION = sum(POPULATION), .groups = "drop")

K05_pop$GEOID <- paste0(K05_pop$STATE, K05_pop$COUNTY) # SETTING THE 5-DIGIT FIPS CODE
K05_pop$COUNTYRACE <- paste0(K05_pop$GEOID, "_", K05_pop$RACE) # CREATING A UNIQUE VARIABLE THAT IS 0000_1 FOR EACH COUNTY-RACE COMBINATION

# SEPARATING OUT THE LAUNCH POPULATION AND SUMMING TO THE COUNTY TOTAL.
K05_launch <- K05_pop[which(K05_pop$YEAR == launch_year),] %>%
  group_by(STATE, COUNTY, GEOID, YEAR) %>%
  dplyr::summarise(POPULATION = sum(POPULATION), .groups = "drop") %>%
  ungroup()

# CREATING OUTPUTS FOR EACH EVALUATION POPULATION: 2005, 2010, AND 2015.
K05_launch2 <- K05_pop[which(K05_pop$YEAR %in% c(launch_year, launch_year+5, launch_year+10, launch_year+15)),]
K05_launch2$COUNTYRACE <- paste0(K05_launch2$GEOID, "_", K05_launch2$RACE)
K05_launch2$Var1 = paste0("a", K05_launch2$AGE) # CREATING A NEW VARIABLE BASED ON THE AGE GROUP.

# DOWNLOADING A COUNTY-SHAPEFILE, CONVERTING THE MAP PROJECTION, AND THEN ELIMINATING THE OUTERLYING US TERRITORIES (GUAM, PUERTO RICO, ETC.)
counties <- counties(cb = TRUE)
# Use sf::st_transform instead of deprecated spTransform
counties <- counties %>%
  st_transform(crs = 2163) %>%  # Using EPSG:2163 directly instead of deprecated +init syntax
  subset(!(STATEFP %in% c("60", "64","66", "68", "69", "70", "74","72", "78")))
# DOWNLOAD A US STATES SHAPEFILE
states <- states(cb=TRUE)

cat("\n=== 002-basedataload.R COMPLETED SUCCESSFULLY ===\n")
cat("✓ SEER population data loaded (1969-2016 or 2023 version)\n")
cat(paste("✓", nrow(K05_pop), "population records processed\n"))
cat("✓ Population data grouped and formatted\n")
cat("✓ County and state shapefiles downloaded\n")
cat("Ready to proceed to 003-proj_basedataload.R\n")
cat("===============================================\n\n")
