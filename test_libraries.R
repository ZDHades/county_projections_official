# Simple test script to validate library loading
cat("=== TESTING R SCRIPT EXECUTION ===\n")
cat("Working directory:", getwd(), "\n")
cat("R version:", R.version.string, "\n")

# Test basic package loading
if (require(pacman, quietly = TRUE)) {
  cat("✓ Pacman available\n")
} else {
  cat("✗ Pacman not available\n")
}

cat("=== TEST COMPLETED ===\n")