library(readr)

cat("=== DEBUGGING PCT013 READING ===\n\n")

# Check what file we're currently reading
current_file <- "DATA/2010_SF1_TX/tx2010.sf1/tx000182010.sf1"
cat("1. CURRENT FILE BEING READ:\n")
cat("   File:", current_file, "\n")

# Read the file as we currently do
pct13_current <- read_csv(current_file, col_names = FALSE, n_max = 3)
cat("   Current reading method - Columns:", ncol(pct13_current), "\n")
cat("   Current reading method - Rows:", nrow(pct13_current), "\n\n")

# According to packing list, PCT013 should be in segment 18 with 49 variables
# Let me try reading it as fixed-width format instead
cat("2. ATTEMPTING FIXED-WIDTH READING:\n")

# First, let's see what the raw file looks like
raw_lines <- readLines(current_file, n = 3)
cat("   Raw file first 3 lines (first 100 chars each):\n")
for(i in 1:length(raw_lines)) {
  cat("   Line", i, ":", substr(raw_lines[i], 1, 100), "\n")
}

cat("\n3. CHECKING FILE SIZE AND STRUCTURE:\n")
file_info <- file.info(current_file)
cat("   File size:", file_info$size, "bytes\n")
cat("   Expected rows for PCT013: 65,031 (from packing list)\n")
cat("   Expected variables: 49 (from packing list)\n")

# Let me try to understand the structure by looking at the first line
first_line <- raw_lines[1]
cat("   First line length:", nchar(first_line), "\n")

# If this is fixed-width, we need to understand the field widths
# Let me check if there's a pattern
cat("\n4. ANALYZING FIRST LINE STRUCTURE:\n")
cat("   First 50 chars:", substr(first_line, 1, 50), "\n")
cat("   Chars 51-100:", substr(first_line, 51, 100), "\n")
cat("   Chars 101-150:", substr(first_line, 101, 150), "\n")

cat("\n=== DEBUGGING COMPLETE ===\n") 