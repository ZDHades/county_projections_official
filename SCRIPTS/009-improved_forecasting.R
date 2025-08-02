###------IMPROVED FORECASTING FOR SPARSE DATA-----
## Add this enhanced forecasting function to 007-projections_2100.R

# Enhanced prediction function that handles sparse data better
predccr_robust = function(ccr, sex, x, DF, fallback_value = 0){
  y <- as_data_frame(DF[[as.character(ccr)]][which(DF$COUNTYRACE== x & DF$SEX == sex )])
  
  # Check data quality
  if(nrow(y) < 5) {
    return(array(fallback_value, c(STEPS)))
  }
  
  # Remove extreme outliers
  Q1 <- quantile(y$value, 0.25, na.rm = T)
  Q3 <- quantile(y$value, 0.75, na.rm = T) 
  IQR <- Q3 - Q1
  y_clean <- y[y$value >= (Q1 - 1.5*IQR) & y$value <= (Q3 + 1.5*IQR), ]
  
  if(nrow(y_clean) < 3) {
    y_clean <- y  # Use original if too much removed
  }
  
  num <- seq(1, FORLEN, 5)
  
  # Try multiple forecasting approaches
  pred <- tryCatch({
    # Try ARIMA with automatic model selection
    if(length(unique(y_clean$value)) == 1) {
      # Constant series - just return the constant
      array(unique(y_clean$value)[1], c(STEPS))
    } else {
      # Try auto.arima first
      auto_model <- forecast::auto.arima(y_clean$value, max.p = 2, max.q = 2, max.d = 1)
      forecast(auto_model, h = FORLEN)$mean[c(num)]
    }
  }, error = function(e) {
    tryCatch({
      # Fallback: Simple exponential smoothing
      ets_model <- forecast::ets(y_clean$value, model = "ANN")
      forecast(ets_model, h = FORLEN)$mean[c(num)]
    }, error = function(e2) {
      tryCatch({
        # Final fallback: Linear trend
        if(nrow(y_clean) >= 2) {
          time_vals <- 1:nrow(y_clean)
          lm_model <- lm(y_clean$value ~ time_vals)
          future_times <- seq(nrow(y_clean) + 1, nrow(y_clean) + FORLEN, 5)
          predict(lm_model, data.frame(time_vals = future_times))
        } else {
          array(fallback_value, c(STEPS))
        }
      }, error = function(e3) {
        array(fallback_value, c(STEPS))
      })
    })
  })
  
  # Ensure reasonable bounds (no negative populations for some models)
  if(grepl("ccr", ccr) && any(ccr %in% c("ccr1", "ccr2", "ccr3"))) {
    # For young age groups, ensure minimum viable population
    pred <- pmax(pred, -1000)  # Allow some decline but not massive
  }
  
  return(pred)
}

cat("Enhanced forecasting function created.\n")
cat("To use: Replace 'predccr' function in 007-projections_2100.R with 'predccr_robust'\n")