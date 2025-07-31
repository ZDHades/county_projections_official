print('Starting 000-Libraries.R...')
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


# Set user library path to local R_libs directory
user_lib <- file.path(getwd(), "R_libs")
if (!dir.exists(user_lib)) dir.create(user_lib)
.libPaths(user_lib)

# List of required packages
pkgs <- c(
  "tidyverse",     # Tidyverse
  "data.table",    # Data Management/Manipulation
  "doParallel",    # Parallel Computing
  "foreach",       # Parallel Computing
  "stringi",       # Character/String Editor
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
  "tidycensus"     # Census Data
)

# Install missing packages
for (pkg in pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, lib = user_lib, repos = "https://cloud.r-project.org")
  }
  library(pkg, character.only = TRUE, lib.loc = user_lib)
}

##Parallel Computing 
# Establish Parallel Computing Cluster
clusters <- makeCluster(detectCores() - 1) # Create Cluster with Specified Number of Cores
registerDoParallel(clusters) # Register Cluster
# Parallel Computing Details
getDoParWorkers() # Determine Number of Utilized Clusters
getDoParName() #  Name of the Currently Registered Parallel Computing Backend
getDoParVersion() #  Version of the Currently Registered Parallel Computing Backend

arima_order <- c(0,1,1) # setting the global arima model
arma <- "ARIMA(0,1,1)"
print('Finished 000-Libraries.R.')