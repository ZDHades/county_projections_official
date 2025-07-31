library(readr)
library(data.table)

cat("=== VALIDATE COUNTY AGGREGATION (Optimized) ===\n\n")

# Read files
geo_file <- "DATA/2010_SF1_TX/tx2010.sf1/txgeo2010.sf1"
p12_file <- "DATA/2010_SF1_TX/tx2010.sf1/tx000042010.sf1"

geo_widths <- c(6,2,3,2,3,2,7,2,3,100)
geo_colnames <- c("FILEID","STUSAB","SUMLEV","GEOCOMP","CHARITER","CIFSN","LOGRECNO","STATE","COUNTY","NAME")
p12_widths <- c(6,2,3,2,3,2,7, rep(9, 49))
p12_colnames <- c("FILEID","STUSAB","SUMLEV","GEOCOMP","CHARITER","CIFSN","LOGRECNO",
                  paste0("P012", sprintf("%04d", 1:49)))

cat("Reading geoheader...\n")
geo <- read.fwf(geo_file, widths=geo_widths, col.names=geo_colnames, stringsAsFactors=FALSE)
cat("Geoheader rows:", nrow(geo), "\n")

cat("Reading P012...\n")
p12 <- read.fwf(p12_file, widths=p12_widths, col.names=p12_colnames, stringsAsFactors=FALSE)
cat("P012 rows:", nrow(p12), "\n")

# Standardize LOGRECNO
geo$LOGRECNO <- sprintf("%05d", as.numeric(geo$LOGRECNO))
p12$LOGRECNO <- sprintf("%05d", as.numeric(gsub(",", "", p12$LOGRECNO)))

cat("Merging geoheader and P012...\n")
df_p12 <- merge(geo, p12, by="LOGRECNO")
cat("Merged rows:", nrow(df_p12), "\n")

# Filter to Texas (STATE == 48)
df_p12_tx <- df_p12[as.numeric(df_p12$STATE) == 48, ]
cat("Rows for Texas (STATE==48):", nrow(df_p12_tx), "\n")

# Debug: show unique STATE values
cat("Unique STATE values:", paste(unique(df_p12$STATE), collapse=", "), "\n")
cat("Sample STATE values:", paste(head(df_p12$STATE, 10), collapse=", "), "\n")

# Show STATE value counts
state_counts <- table(df_p12$STATE)
cat("STATE value counts:\n")
print(state_counts)

# Check if Texas (48) exists
if ("48" %in% names(state_counts)) {
  cat("Texas (STATE=48) found with", state_counts["48"], "records\n")
} else {
  cat("Texas (STATE=48) NOT found in the data!\n")
  cat("Available states:", paste(names(state_counts), collapse=", "), "\n")
}

# Convert to data.table and set threads
dt_p12 <- as.data.table(df_p12_tx)
data.table::setDTthreads(0) # Use all available cores

# Aggregate to county
p12_vars <- paste0("P012", sprintf("%04d", 1:49))
for (v in p12_vars) dt_p12[[v]] <- as.numeric(dt_p12[[v]])
agg_p12 <- dt_p12[, lapply(.SD, sum, na.rm=TRUE), by=COUNTY, .SDcols=p12_vars]

cat("Number of unique counties:", uniqueN(agg_p12$COUNTY), "\n")
cat("Sample county codes:", paste(head(agg_p12$COUNTY, 10), collapse=", "), "\n")
cat("Sample total population by county (first 10):\n")
agg_p12$TOTAL <- rowSums(agg_p12[, ..p12_vars], na.rm=TRUE)
print(head(agg_p12[, .(COUNTY, TOTAL)], 10))
cat("\n=== VALIDATION COMPLETE ===\n") 