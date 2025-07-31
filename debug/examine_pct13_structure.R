library(readr)

cat("=== EXAMINING PCT013 STRUCTURE ===\n\n")

# Read PCT013 file
pct13 <- read_csv("DATA/2010_SF1_TX/tx2010.sf1/tx000182010.sf1", col_names = FALSE, n_max = 5)

cat("1. PCT013 FILE STRUCTURE:\n")
cat("   Total columns:", ncol(pct13), "\n")
cat("   Total rows:", nrow(pct13), "\n\n")

cat("2. FIRST FEW ROWS (showing first 20 columns):\n")
print(pct13[, 1:20])
cat("\n")

cat("3. COLUMN ANALYSIS:\n")
# Check which columns have non-zero values
non_zero_cols <- sapply(pct13, function(x) sum(x != 0, na.rm=TRUE))
cat("   Columns with non-zero values (first 30):\n")
for(i in 1:min(30, length(non_zero_cols))) {
  cat("   X", i, ": ", non_zero_cols[i], " non-zero values\n", sep="")
}

cat("\n4. LOOKING FOR PATTERNS:\n")
# Check if there are obvious breaks in the data structure
cat("   Sample values from different column ranges:\n")
cat("   Columns 1-10 (header info):\n")
print(pct13[1, 1:10])
cat("   Columns 11-20 (likely data):\n")
print(pct13[1, 11:20])
cat("   Columns 21-30:\n")
print(pct13[1, 21:30])

cat("\n5. SEARCHING FOR HOUSEHOLD-ONLY INDICATORS:\n")
# Look for columns that might indicate household vs group quarters
# Based on Census documentation, household data typically starts after group quarters
cat("   Checking for potential household start point...\n")

# Let's look at the middle and end of the file to see patterns
mid_col <- floor(ncol(pct13)/2)
cat("   Middle column (", mid_col, ") value:", as.numeric(pct13[1, mid_col]), "\n")
cat("   Last column (", ncol(pct13), ") value:", as.numeric(pct13[1, ncol(pct13)]), "\n")

# Check if there's a pattern in the data that might indicate household vs group quarters
cat("\n6. CHECKING FOR DATA BREAKS:\n")
# Look for columns where the pattern changes significantly
for(i in seq(10, min(50, ncol(pct13)), by=10)) {
  cat("   Column ", i, " value: ", as.numeric(pct13[1, i]), "\n", sep="")
}

cat("\n=== ANALYSIS COMPLETE ===\n") 