library(readr)
library(dplyr)

cat("=== VALIDATION OF gq_2010.csv ===\n\n")

# Read the file
gq_data <- read_csv("DATA-PROCESSED/gq_2010.csv")

cat("1. BASIC FILE INFO:\n")
cat("   Rows:", nrow(gq_data), "\n")
cat("   Columns:", ncol(gq_data), "\n")
cat("   Column names:", paste(colnames(gq_data), collapse=", "), "\n\n")

cat("2. DATA TYPES:\n")
print(sapply(gq_data, class))
cat("\n")

cat("3. FIRST 10 ROWS:\n")
print(head(gq_data, 10))
cat("\n")

cat("4. SUMMARY STATISTICS:\n")
print(summary(gq_data))
cat("\n")

cat("5. NULL/NA ANALYSIS:\n")
na_counts <- sapply(gq_data, function(x) sum(is.na(x)))
cat("   NA counts per column:\n")
for(i in 1:length(na_counts)) {
  cat("   ", names(na_counts)[i], ":", na_counts[i], "\n")
}
cat("\n")

cat("6. UNIQUE VALUES ANALYSIS:\n")
cat("   Unique STATE values:", paste(unique(gq_data$STATE), collapse=", "), "\n")
cat("   Number of unique COUNTIES:", length(unique(gq_data$COUNTY)), "\n")
cat("   Unique SEX values:", paste(unique(gq_data$SEX), collapse=", "), "\n")
cat("   Unique RACE values:", paste(unique(gq_data$RACE), collapse=", "), "\n")
cat("   Unique AGEGRP values:", paste(sort(unique(gq_data$AGEGRP)), collapse=", "), "\n")
cat("   Unique YEAR values:", paste(unique(gq_data$YEAR), collapse=", "), "\n\n")

cat("7. GQ (GROUP QUARTERS) ANALYSIS:\n")
cat("   GQ mean:", mean(gq_data$GQ, na.rm=TRUE), "\n")
cat("   GQ median:", median(gq_data$GQ, na.rm=TRUE), "\n")
cat("   GQ min:", min(gq_data$GQ, na.rm=TRUE), "\n")
cat("   GQ max:", max(gq_data$GQ, na.rm=TRUE), "\n")
cat("   GQ negative values:", sum(gq_data$GQ < 0, na.rm=TRUE), "\n")
cat("   GQ zero values:", sum(gq_data$GQ == 0, na.rm=TRUE), "\n")
cat("   GQ positive values:", sum(gq_data$GQ > 0, na.rm=TRUE), "\n\n")

cat("8. COUNTY-LEVEL ANALYSIS:\n")
county_summary <- gq_data %>%
  group_by(COUNTY) %>%
  summarise(
    total_gq = sum(GQ, na.rm=TRUE),
    mean_gq = mean(GQ, na.rm=TRUE),
    min_gq = min(GQ, na.rm=TRUE),
    max_gq = max(GQ, na.rm=TRUE),
    rows = n()
  )
cat("   County summary (first 10):\n")
print(head(county_summary, 10))
cat("\n")

cat("9. POTENTIAL ISSUES:\n")
if(any(gq_data$GQ < 0, na.rm=TRUE)) {
  cat("   ⚠️  WARNING: Negative GQ values found!\n")
  cat("   Sample negative GQ values:\n")
  print(head(gq_data[gq_data$GQ < 0, ], 5))
}

if(all(is.na(gq_data$SEX))) {
  cat("   ⚠️  WARNING: All SEX values are NA!\n")
}

if(all(is.na(gq_data$RACE))) {
  cat("   ⚠️  WARNING: All RACE values are NA!\n")
}

if(all(is.na(gq_data$AGEGRP))) {
  cat("   ⚠️  WARNING: All AGEGRP values are NA!\n")
}

cat("\n=== VALIDATION COMPLETE ===\n") 