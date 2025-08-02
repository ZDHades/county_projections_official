# PERFORMANCE OPTIMIZATION SCRIPT FOR TEXAS PROJECTIONS
# ====================================================
# This script contains optimizations specifically for Texas county projections

# Function to optimize Census API calls with caching
optimize_census_api <- function() {
  # Set API call timeout and retry logic
  options(timeout = 300)  # 5 minute timeout
  
  # Enable parallel connections for faster API calls
  if (.Platform$OS.type == "windows") {
    options(RCurl.options = list(
      connecttimeout = 60,
      timeout = 300,
      maxconnects = 4
    ))
  }
}

# Function to process Texas counties only (instead of all states)
texas_only_optimization <- function() {
  # Filter to Texas only (FIPS 48) to reduce processing time
  texas_counties <- fipslist[fipslist$STATEID == "48", ]
  return(texas_counties$GEOID)
}

# Function to cache intermediate results
cache_intermediate_results <- function(data, filename) {
  cache_dir <- "CACHE"
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir)
  }
  
  cache_file <- file.path(cache_dir, paste0(filename, "_", Sys.Date(), ".rds"))
  saveRDS(data, cache_file)
  cat("Cached results to:", cache_file, "\n")
}

# Function to load cached results if available
load_cached_results <- function(filename) {
  cache_dir <- "CACHE"
  cache_file <- file.path(cache_dir, paste0(filename, "_", Sys.Date(), ".rds"))
  
  if (file.exists(cache_file)) {
    cat("Loading cached results from:", cache_file, "\n")
    return(readRDS(cache_file))
  }
  return(NULL)
}

cat("Performance optimization functions loaded.\n")
cat("Use optimize_census_api() to improve API performance.\n")
cat("Use texas_only_optimization() to focus on Texas counties only.\n")