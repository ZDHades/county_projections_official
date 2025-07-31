library(readr)

cat("=== TESTING FIXED-WIDTH READING ===\n\n")

# Test reading PCT013 as fixed-width
pct13_file <- "DATA/2010_SF1_TX/tx2010.sf1/tx000182010.sf1"

cat("1. READING PCT013 AS FIXED-WIDTH:\n")
cat("   File:", pct13_file, "\n")

# Try different fixed-width configurations
# Based on packing list: pct13|18:49| means 49 variables
# Each variable is typically 9 characters wide

# Test 1: 49 variables of 9 characters each
cat("\n2. TESTING 49 VARIABLES (9 chars each):\n")
pct13_widths_1 <- c(6,2,3,2,3,2,7, rep(9, 49))  # Header + 49 variables
pct13_colnames_1 <- c("FILEID","STUSAB","SUMLEV","GEOCOMP","CHARITER","CIFSN","LOGRECNO",
                      paste0("PCT013", sprintf("%04d", 1:49)))

tryCatch({
  pct13_1 <- read.fwf(pct13_file, widths=pct13_widths_1, col.names=pct13_colnames_1, stringsAsFactors=FALSE, n=3)
  cat("   Success! Rows:", nrow(pct13_1), "Columns:", ncol(pct13_1), "\n")
  cat("   First row LOGRECNO:", pct13_1$LOGRECNO[1], "\n")
  cat("   First few PCT013 values:", paste(pct13_1[1, 8:12], collapse=", "), "\n")
}, error=function(e) {
  cat("   Error:", conditionMessage(e), "\n")
})

# Test 2: Different variable width (maybe 10 characters)
cat("\n3. TESTING 49 VARIABLES (10 chars each):\n")
pct13_widths_2 <- c(6,2,3,2,3,2,7, rep(10, 49))  # Header + 49 variables
pct13_colnames_2 <- c("FILEID","STUSAB","SUMLEV","GEOCOMP","CHARITER","CIFSN","LOGRECNO",
                      paste0("PCT013", sprintf("%04d", 1:49)))

tryCatch({
  pct13_2 <- read.fwf(pct13_file, widths=pct13_widths_2, col.names=pct13_colnames_2, stringsAsFactors=FALSE, n=3)
  cat("   Success! Rows:", nrow(pct13_2), "Columns:", ncol(pct13_2), "\n")
  cat("   First row LOGRECNO:", pct13_2$LOGRECNO[1], "\n")
  cat("   First few PCT013 values:", paste(pct13_2[1, 8:12], collapse=", "), "\n")
}, error=function(e) {
  cat("   Error:", conditionMessage(e), "\n")
})

cat("\n=== TESTING COMPLETE ===\n") 