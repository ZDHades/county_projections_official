rm(list=ls())
library(tidyverse)
library(tigris)
library(censusapi)
library(tidycensus)
library(data.table) # Added for rbindlist

cat("[LOG] Starting 004-GQ_gather_fixed_all.R...\n")

# ENTER YOUR CENSUS API KEY HERE
key <- "0206e3f2924a424be8722887fd0a49cea6308a7e"

fipslist <- read_csv(file="https://www2.census.gov/geo/docs/reference/codes/files/national_county.txt", col_names = FALSE) %>%
  mutate(GEOID = paste0(X2, X3)) %>%
  dplyr::rename(state = X1,
         STATEID = X2,
         CNTYID = X3,
         NAME = X4) %>%
  filter(!STATEID %in% c("60", "66", "69", "72", "74", "78"))

# Only process Texas (FIPS 48)
stateid = c("48")
cat("[LOG] Limiting processing to Texas (FIPS 48) only.\n")

getgq_2010_fixed = function(x) {
  tryCatch({
    cat("[LOG] [START] getgq_2010_fixed for state:", x, "\n")
    
    # 1. READ GEOHEADER (FIXED-WIDTH) - CORRECT
    cat("[LOG] Reading geoheader as fixed-width...\n")
    geo_file <- "DATA/2010_SF1_TX/tx2010.sf1/txgeo2010.sf1"
    # 2010 SF1 geoheader field widths (first 10 fields):
    # FILEID(6), STUSAB(2), SUMLEV(3), GEOCOMP(2), CHARITER(3), CIFSN(2), LOGRECNO(7), STATE(2), COUNTY(3), NAME(100)
    geo_widths <- c(6,2,3,2,3,2,7,2,3,100)
    geo_colnames <- c("FILEID","STUSAB","SUMLEV","GEOCOMP","CHARITER","CIFSN","LOGRECNO","STATE","COUNTY","NAME")
    geo <- read.fwf(geo_file, widths=geo_widths, col.names=geo_colnames, stringsAsFactors=FALSE)
    cat("[LOG] geo dim (before filtering):", dim(geo), "\n")
    cat("[LOG] geo SUMLEV unique values:", paste(unique(geo$SUMLEV), collapse=", "), "\n")
    cat("[LOG] geo LOGRECNO sample:", paste(head(geo$LOGRECNO, 10), collapse=", "), "\n")
    geo$SUMLEV_trim <- trimws(geo$SUMLEV)
    geo <- geo[geo$SUMLEV_trim %in% c("50", "050"), ]
    cat("[LOG] geo dim (after filtering to counties):", dim(geo), "\n")
    cat("[LOG] geo LOGRECNO sample (filtered):", paste(head(geo$LOGRECNO, 10), collapse=", "), "\n")
    cat("[LOG] Finished reading geoheader (fwf). Rows:", nrow(geo), "\n")
    cat("[LOG] Unique SUMLEV values:", paste(unique(geo$SUMLEV), collapse=", "), "\n")
    geo$SUMLEV_trim <- trimws(geo$SUMLEV)
    geo <- geo[geo$SUMLEV_trim %in% c("50", "050"), ]
    cat("[LOG] Filtered geoheader to counties. Rows:", nrow(geo), "\n")
    
    # 2. READ P012 (FIXED-WIDTH) - FIXED
    cat("[LOG] Reading P012 (total pop) as fixed-width...\n")
    p12_file <- "DATA/2010_SF1_TX/tx2010.sf1/tx000042010.sf1"
    # According to packing list: p12|4:49| means segment 4 with 49 variables
    # Header: FILEID(6), STUSAB(2), SUMLEV(3), GEOCOMP(2), CHARITER(3), CIFSN(2), LOGRECNO(7) = 25 chars
    # Variables: 49 variables of 9 characters each = 441 chars
    p12_widths <- c(6,2,3,2,3,2,7, rep(9, 49))  # Header + 49 variables
    p12_colnames <- c("FILEID","STUSAB","SUMLEV","GEOCOMP","CHARITER","CIFSN","LOGRECNO",
                      paste0("P012", sprintf("%04d", 1:49)))
    p12 <- read.fwf(p12_file, widths=p12_widths, col.names=p12_colnames, stringsAsFactors=FALSE)
    cat("[LOG] p12 dim:", dim(p12), "\n")
    cat("[LOG] p12 LOGRECNO sample:", paste(head(p12$LOGRECNO, 10), collapse=", "), "\n")
    cat("[LOG] Finished reading P012 (fwf). Rows:", nrow(p12), "\n")
    
    # 3. READ PCT013 (FIXED-WIDTH) - FIXED
    cat("[LOG] Reading PCT013 (household pop) as fixed-width...\n")
    pct13_file <- "DATA/2010_SF1_TX/tx2010.sf1/tx000182010.sf1"
    # According to packing list: pct13|18:49| means segment 18 with 49 variables
    # Header: FILEID(6), STUSAB(2), SUMLEV(3), GEOCOMP(2), CHARITER(3), CIFSN(2), LOGRECNO(7) = 25 chars
    # Variables: 49 variables of 9 characters each = 441 chars
    pct13_widths <- c(6,2,3,2,3,2,7, rep(9, 49))  # Header + 49 variables
    pct13_colnames <- c("FILEID","STUSAB","SUMLEV","GEOCOMP","CHARITER","CIFSN","LOGRECNO",
                        paste0("PCT013", sprintf("%04d", 1:49)))
    pct13 <- read.fwf(pct13_file, widths=pct13_widths, col.names=pct13_colnames, stringsAsFactors=FALSE)
    cat("[LOG] pct13 dim:", dim(pct13), "\n")
    cat("[LOG] pct13 LOGRECNO sample:", paste(head(pct13$LOGRECNO, 10), collapse=", "), "\n")
    cat("[LOG] Finished reading PCT013 (fwf). Rows:", nrow(pct13), "\n")
    
    # 4. MERGE DATA
    cat("[LOG] Merging geo and P012...\n")
    
    # Standardize LOGRECNO format across all datasets
    geo$LOGRECNO <- sprintf("%05d", as.numeric(geo$LOGRECNO))
    p12$LOGRECNO <- sprintf("%05d", as.numeric(gsub(",", "", p12$LOGRECNO)))
    pct13$LOGRECNO <- sprintf("%05d", as.numeric(gsub(",", "", pct13$LOGRECNO)))
    
    cat("[LOG] Standardized LOGRECNO format:\n")
    cat("[LOG] geo LOGRECNO sample (after):", paste(head(geo$LOGRECNO, 5), collapse=", "), "\n")
    cat("[LOG] p12 LOGRECNO sample (after):", paste(head(p12$LOGRECNO, 5), collapse=", "), "\n")
    cat("[LOG] pct13 LOGRECNO sample (after):", paste(head(pct13$LOGRECNO, 5), collapse=", "), "\n")
    
    # Merge geoheader with P012 and PCT013 (block/tract level)
    df_p12 <- merge(geo, p12, by="LOGRECNO")
    df_pct13 <- merge(geo, pct13, by="LOGRECNO")
    cat("[LOG] df_p12 dim:", dim(df_p12), "\n")
    cat("[LOG] df_pct13 dim:", dim(df_pct13), "\n")
    
    # Aggregate to county level
    cat("[LOG] Aggregating P012 and PCT013 to county level...\n")
    p12_vars <- paste0("P012", sprintf("%04d", 1:49))
    pct13_vars <- paste0("PCT013", sprintf("%04d", 1:49))
    
    # Convert to numeric for aggregation
    for (v in p12_vars) df_p12[[v]] <- as.numeric(df_p12[[v]])
    for (v in pct13_vars) df_pct13[[v]] <- as.numeric(df_pct13[[v]])
    
    # Aggregate by STATE, COUNTY
    agg_p12 <- df_p12 %>% group_by(STATE, COUNTY) %>% summarise(across(all_of(p12_vars), sum, na.rm=TRUE))
    agg_pct13 <- df_pct13 %>% group_by(STATE, COUNTY) %>% summarise(across(all_of(pct13_vars), sum, na.rm=TRUE))
    cat("[LOG] agg_p12 dim:", dim(agg_p12), "\n")
    cat("[LOG] agg_pct13 dim:", dim(agg_pct13), "\n")
    
    # Merge aggregated data
    df_merged <- merge(agg_p12, agg_pct13, by=c("STATE", "COUNTY"))
    cat("[LOG] df_merged (county-level) dim:", dim(df_merged), "\n")
    
    # Calculate total and household population
    df_merged$TOTAL <- rowSums(df_merged[, p12_vars], na.rm=TRUE)
    df_merged$HHPOP <- rowSums(df_merged[, pct13_vars], na.rm=TRUE)
    df_merged$GQ <- df_merged$TOTAL - df_merged$HHPOP
    
    # Prepare output
    out <- data.frame(
      STATE = df_merged$STATE,
      COUNTY = df_merged$COUNTY,
      NAME = NA, # Name not available after aggregation
      SEX = NA,
      RACE = NA,
      AGEGRP = NA,
      GQ = df_merged$GQ,
      YEAR = 2010
    )
    cat("[LOG] Output data frame preview:\n")
    print(head(out, 2))
    cat("[LOG] Output summary:\n")
    print(summary(out))
    cat("[LOG] [END] getgq_2010_fixed. Output rows:", nrow(out), "\n")
    return(out)
    
  }, error=function(e){
    cat("ERROR in getgq_2010_fixed:", conditionMessage(e), "\n")
    return(NULL)
  })
}

getgq_2000 = function(x){
  tryCatch({
    cat("[LOG] [START] getgq_2000 for state:", x, "\n")
    cat("[LOG] 2000 Census API calls are failing (404 errors). Skipping.\n")
    return(NULL)
  }, error=function(e){
    cat("ERROR in getgq_2000:", conditionMessage(e), "\n")
    return(NULL)
  })
}

cat("[LOG] Finished defining functions.\n")

# Process 2010 data
cat("[LOG] Processing 2010 data...\n")
dat <- lapply(stateid, getgq_2010_fixed)
GQ2010 <- rbindlist(dat, fill=TRUE)

# Process 2000 data (will be empty due to API issues)
cat("[LOG] Processing 2000 data...\n")
baseyear <- "2000"
dat <- lapply(stateid, getgq_2000)
GQ2000 <- rbindlist(dat, fill=TRUE)

# Write outputs
cat("[LOG] Writing output files...\n")
write_csv(GQ2010, "DATA-PROCESSED/gq_2010_fixed.csv")
write_csv(GQ2000, "DATA-PROCESSED/gq_2000_fixed.csv")

cat("[LOG] Script completed.\n")
cat("[LOG] 2010 output rows:", nrow(GQ2010), "\n")
cat("[LOG] 2000 output rows:", nrow(GQ2000), "\n") 