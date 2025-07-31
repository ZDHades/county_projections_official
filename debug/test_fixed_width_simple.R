library(readr)

cat("=== TESTING FIXED-WIDTH READING FOR ALL FILES ===\n\n")

# Test 1: Geoheader
cat("1. TESTING GEOHEADER (txgeo2010.sf1):\n")
geo_file <- "DATA/2010_SF1_TX/tx2010.sf1/txgeo2010.sf1"
geo_widths <- c(6,2,3,2,3,2,7,2,3,100)
geo_colnames <- c("FILEID","STUSAB","SUMLEV","GEOCOMP","CHARITER","CIFSN","LOGRECNO","STATE","COUNTY","NAME")

tryCatch({
  geo <- read.fwf(geo_file, widths=geo_widths, col.names=geo_colnames, stringsAsFactors=FALSE, n=5)
  cat("   SUCCESS: Geoheader read with", ncol(geo), "columns\n")
  cat("   Sample data:\n")
  print(geo[, c("STATE", "COUNTY", "NAME")])
}, error=function(e) {
  cat("   ERROR:", conditionMessage(e), "\n")
})

# Test 2: P012
cat("\n2. TESTING P012 (tx000042010.sf1):\n")
p12_file <- "DATA/2010_SF1_TX/tx2010.sf1/tx000042010.sf1"
p12_widths <- c(6,2,3,2,3,2,7, rep(9, 49))
p12_colnames <- c("FILEID","STUSAB","SUMLEV","GEOCOMP","CHARITER","CIFSN","LOGRECNO",
                  paste0("P012", sprintf("%04d", 1:49)))

tryCatch({
  p12 <- read.fwf(p12_file, widths=p12_widths, col.names=p12_colnames, stringsAsFactors=FALSE, n=3)
  cat("   SUCCESS: P012 read with", ncol(p12), "columns\n")
  cat("   Sample P0120001 (Total):", p12$P0120001[1:3], "\n")
}, error=function(e) {
  cat("   ERROR:", conditionMessage(e), "\n")
})

# Test 3: PCT013
cat("\n3. TESTING PCT013 (tx000182010.sf1):\n")
pct13_file <- "DATA/2010_SF1_TX/tx2010.sf1/tx000182010.sf1"
pct13_widths <- c(6,2,3,2,3,2,7, rep(9, 49))
pct13_colnames <- c("FILEID","STUSAB","SUMLEV","GEOCOMP","CHARITER","CIFSN","LOGRECNO",
                    paste0("PCT013", sprintf("%04d", 1:49)))

tryCatch({
  pct13 <- read.fwf(pct13_file, widths=pct13_widths, col.names=pct13_colnames, stringsAsFactors=FALSE, n=3)
  cat("   SUCCESS: PCT013 read with", ncol(pct13), "columns\n")
  cat("   Sample PCT0130001:", pct13$PCT0130001[1:3], "\n")
}, error=function(e) {
  cat("   ERROR:", conditionMessage(e), "\n")
})

cat("\n=== TEST COMPLETED ===\n") 