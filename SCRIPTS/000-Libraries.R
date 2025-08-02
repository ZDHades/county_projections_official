###------LIBRARY SETUP-----
## @knitr libraries

# library(data.table)
# library(knitr)
# library(scales)
# library(cowplot)
# library(tmap)
# library(tmaptools)
# library(tigris)
# library(censusapi)
# library(sp)
# library(grid)
# library(tidycensus)
# library(kableExtra)
# library(tidyverse)
# library(LexisPlotR)
# library(pdftools)
# library(R.utils)
# library(forecast)

rm(list = ls()) # Remove Previous Workspace
gc(reset = TRUE) # Garbage Collection

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

# R Workspace Options
options(scipen = 12) # Scientific Notation
options(digits = 6) # Specify Digits
options(java.parameters = "-Xmx1000m") # Increase Java Heap Size


#Functions, Libraries, & Parallel Computing 
## Functions 
# Specify Number of Digits (Forward)
numb_digits_F <- function(x,y){
  numb_digits_F <- do.call("paste0", list(paste0(rep(0, y - nchar(x)), collapse = ""), x))
  numb_digits_F <- ifelse(nchar(x) < y, numb_digits_F, x)
}

# Remove Double Space 
numb_spaces <- function(x) gsub("[[:space:]]{2,}", " ", x)


# Install and load pacman for easier package loading and installation
cat("Loading pacman package...\n")
if (!require("pacman", character.only = TRUE)){
  cat("Installing pacman...\n")
  install.packages("pacman", dep = TRUE)
  if (!require("pacman", character.only = TRUE))
    stop("Package not found")
}
cat("✓ Pacman loaded successfully\n")


# Libraries - removed problematic packages that may not be available
pkgs <- c(
  "tidyverse",     # Tidyverse
  "data.table",    # Data Management/Manipulation
  "doParallel",    # Parallel Computing
  "foreach",       # Parallel Computing
  "parallel",      # Base parallel package
  "openxlsx",      # Microsoft Excel Files
  "stringi",       #Character/String Editor
  "stringr",       # Character/String Editor
  "zoo",           # Time Series
  "reshape2",      # Data Management/Manipulation
  "scales",        # Number formatting
  "cowplot",       # Plot Grids
  "tmap",          # Cartography
  "tmaptools",     # Cartographic tools
  "tigris",        # US shapefiles
  "censusapi",     # Census Data
  "sp",            # Spatial Objects
  "grid",          # Plot Grids
  "kableExtra",    # Pretty Tables
  "LexisPlotR",    # Lexis Diagrams
  "pdftools",      # Load pdfs
  "R.utils",       # Utilities
  "forecast",      # Forecasting
  "pbmcapply",     # Progress Bar Multicore Apply
  "tidycensus"     # Census Data
)

# Optional packages that may not be available - try to install separately
optional_pkgs <- c("parallelsugar", "rucm", "IDPmisc")

# Install missing packages to local library if needed
# Will only run if at least one package is missing
cat("Checking for missing packages...\n")

# Install core packages first
missing_pkgs <- pkgs[!p_isinstalled(pkgs)]
if(length(missing_pkgs) > 0){
  cat("Installing missing core packages:", paste(missing_pkgs, collapse = ", "), "\n")
  tryCatch({
    if (dir.exists(local_lib_path)) {
      cat("Installing to local library:", local_lib_path, "\n")
      p_install(
        package = missing_pkgs, 
        character.only = TRUE,
        lib = local_lib_path
      )
    } else {
      cat("Installing to system library\n")
      p_install(
        package = missing_pkgs, 
        character.only = TRUE
      )
    }
  }, error = function(e) {
    cat("Warning: Some packages failed to install:", e$message, "\n")
  })
} else {
  cat("✓ All core packages already installed\n")
}

# Try to install optional packages (don't fail if they don't work)
cat("Attempting to install optional packages...\n")
for (pkg in optional_pkgs) {
  tryCatch({
    if (!p_isinstalled(pkg)) {
      cat("Trying to install:", pkg, "\n")
      if (dir.exists(local_lib_path)) {
        p_install(pkg, character.only = TRUE, lib = local_lib_path)
      } else {
        p_install(pkg, character.only = TRUE)
      }
      cat("✓", pkg, "installed\n")
    } else {
      cat("✓", pkg, "already available\n")
    }
  }, error = function(e) {
    cat("⚠", pkg, "not available, skipping\n")
  })
}

# load the core packages
cat("Loading core packages...\n")
p_load(pkgs, character.only = TRUE)
cat("✓ Core packages loaded\n")

# Try to load optional packages
cat("Loading optional packages...\n")
for (pkg in optional_pkgs) {
  tryCatch({
    library(pkg, character.only = TRUE)
    cat("✓", pkg, "loaded\n")
  }, error = function(e) {
    cat("⚠", pkg, "not loaded (not critical)\n")
  })
}

rm(pkgs, optional_pkgs)

##Parallel Computing 
cat("Setting up parallel computing...\n")
# Establish Parallel Computing Cluster (Windows-compatible)
if (.Platform$OS.type == "windows") {
  num_cores <- min(detectCores() - 1, 4)  # Limit cores on Windows
  cat("Windows detected - using", num_cores, "cores for parallel processing\n")
  clusters <- makeCluster(num_cores, type = "PSOCK") # PSOCK cluster for Windows
} else {
  num_cores <- detectCores() - 1
  cat("Unix-like system - using", num_cores, "cores for parallel processing\n")
  clusters <- makeCluster(num_cores) # Default cluster type
}
registerDoParallel(clusters) # Register Cluster
# Parallel Computing Details
cat("✓ Parallel workers:", getDoParWorkers(), "\n") # Determine Number of Utilized Clusters
cat("✓ Backend:", getDoParName(), "\n") #  Name of the Currently Registered Parallel Computing Backend
cat("✓ Version:", getDoParVersion(), "\n") #  Version of the Currently Registered Parallel Computing Backend

arima_order <- c(0,1,1) # setting the global arima model
arma <- "ARIMA(0,1,1)"

cat("\n=== 000-Libraries.R COMPLETED SUCCESSFULLY ===\n")
cat("✓ All packages loaded\n")
cat("✓ Parallel computing cluster established\n")
cat("✓ ARIMA model configuration set\n")
cat("Ready to proceed to 001-fipscodes.R\n")
cat("===============================================\n\n")