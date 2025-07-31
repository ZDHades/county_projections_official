library(readr)

# Read just the first row to examine structure
pct13 <- read_csv("DATA/2010_SF1_TX/tx2010.sf1/tx000182010.sf1", col_names = FALSE, n_max = 1)

cat("PCT013 file structure:\n")
cat("Number of columns:", ncol(pct13), "\n")
cat("First 10 column names:", paste(colnames(pct13)[1:10], collapse=", "), "\n")
cat("Last 10 column names:", paste(colnames(pct13)[(ncol(pct13)-9):ncol(pct13)], collapse=", "), "\n")

# Show a few sample values from first row
cat("\nFirst row sample values:\n")
for(i in 1:min(10, ncol(pct13))) {
  cat("Column", i, ":", pct13[1,i], "\n")
} 