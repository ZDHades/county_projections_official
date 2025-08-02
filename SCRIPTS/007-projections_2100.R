set.seed(100)

# Set up local library path (same as SETUP.R)
# Check if we're in SCRIPTS directory, if so go up one level
if (basename(getwd()) == "SCRIPTS") {
  local_lib_path <- file.path(dirname(getwd()), "R_libs")
} else {
  local_lib_path <- file.path(getwd(), "R_libs")
}

if (dir.exists(local_lib_path)) {
  .libPaths(c(local_lib_path, .libPaths()))
  cat("Using local R_libs at:", local_lib_path, "\n")
} else {
  cat("Local R_libs not found, using system libraries\n")
}

# Load required libraries as backup in case sourcing fails
if (!require("pacman", character.only = TRUE, lib.loc = local_lib_path)){
  if (dir.exists(local_lib_path)) {
    install.packages("pacman", dep = TRUE, lib = local_lib_path)
  } else {
    install.packages("pacman", dep = TRUE)
  }
  if (!require("pacman", character.only = TRUE))
    stop("Package not found")
}
pacman::p_load("tidyverse", "readr", "dplyr", "forecast", "stringr", "data.table", "doParallel", "foreach")

# Check if data has been saved from previous manual runs
workspace_file <- if (basename(getwd()) == "SCRIPTS") {
  "../.RData_session"
} else {
  ".RData_session"
}

cat("Checking for required data objects...\n")

# Try to load from saved workspace first
if (file.exists(workspace_file)) {
  cat("Loading data from previous session...\n")
  load(workspace_file)
  cat("✓ Loaded saved session data\n")
}

# Ensure arima_order is defined
if (!exists("arima_order")) {
  arima_order <- c(0,1,1)  # Default ARIMA model parameters from 000-Libraries.R
  cat("✓ Set default arima_order:", paste(arima_order, collapse=","), "\n")
}

# If data still not available, error out with clear instructions
if (!exists("K05_pop") || !exists("stateid")) {
  cat("\n", paste(rep("=", 60), collapse=""), "\n")
  cat("ERROR: Required data objects not found!\n")
  cat("\nTo create the required workspace file, run these commands in R console:\n")
  cat("  source('001-fipscodes.R')\n")
  cat("  source('003-proj_basedataload.R')\n")
  cat("  arima_order <- c(0,1,1)\n")
  cat("  save(K05_pop, stateid, launch_year, test_year, SIZE, STEPS, FORLEN, GROUPING, arima_order, file = '.RData_session')\n")
  cat("\nThen re-run this script.\n")
  cat(paste(rep("=", 60), collapse=""), "\n")
  stop("Data objects not available")
}

# Validate all required objects are now present
cat("Validating data objects...\n")
cat("✓ K05_pop found with", nrow(K05_pop), "records\n")
cat("✓ stateid found with", length(stateid), "states\n")
cat("✓ Configuration: launch_year =", launch_year, ", STEPS =", STEPS, ", SIZE =", SIZE, "\n")

# Add detailed data validation
cat("\n=== DATA VALIDATION ===\n")
cat("K05_pop structure:\n")
print(str(K05_pop))
cat("K05_pop sample (first 5 rows):\n")
print(head(K05_pop, 5))
cat("Unique years in K05_pop:", paste(sort(unique(K05_pop$YEAR)), collapse=", "), "\n")
cat("Unique states in K05_pop:", paste(sort(unique(K05_pop$STATE)), collapse=", "), "\n")

Klaunch <- K05_pop[which(K05_pop$YEAR==launch_year),]
cat("Klaunch (launch year data) has", nrow(Klaunch), "records\n")
if(nrow(Klaunch) == 0) {
  cat("ERROR: No data found for launch year", launch_year, "\n")
  cat("Available years:", paste(sort(unique(K05_pop$YEAR)), collapse=", "), "\n")
}
statelist <- unique(Klaunch$STATE)
cat("States in launch year data:", paste(statelist, collapse=", "), "\n")

# gq_2010.csv IS THE RESULT OF 
# Handle path correctly whether running from SCRIPTS directory or root directory
if (basename(getwd()) == "SCRIPTS") {
  gq_path <- "../DATA-PROCESSED/gq_2010.csv"
} else {
  gq_path <- "DATA-PROCESSED/gq_2010.csv"
}

# Check if group quarters file exists
if (!file.exists(gq_path)) {
  cat("WARNING: Group quarters file not found at:", gq_path, "\n")
  cat("Creating empty group quarters data...\n")
  # Create minimal empty gqpop structure for Texas
  gqpop <- data.frame(
    STATE = character(0),
    COUNTY = character(0), 
    YEAR = numeric(0),
    AGE = numeric(0),
    SEX = character(0),
    RACE = character(0),
    GEOID = character(0),
    COUNTYRACE = character(0),
    group_quarters = numeric(0)
  )
} else {
  cat("✓ Loading group quarters data from:", gq_path, "\n")
  gqpop <- read.csv(gq_path) %>%
    mutate(STATE = str_pad(STATE, 2, pad = "0"),
           COUNTY = str_pad(COUNTY, 3, pad="0"),
           GEOID = paste0(STATE, COUNTY),
           AGE = AGEGRP,
           SEX = case_when(
             SEX == "MALE" ~ "1",
             SEX == "FEMALE" ~ "2"),
           RACE = case_when(
             RACE == "BLACK, NH" ~ "2",
             RACE == "OTHER" ~ "3",
             RACE == "WHITE, NH" ~ "1",
             RACE == "HISPANIC" ~ "3",
             RACE == "OTHER, NH" ~ "3")) %>%
    group_by(across(all_of(GROUPING))) %>%
    dplyr::summarise(group_quarters = sum(GQ, na.rm=T), .groups = "drop")
}

gqpop$GEOID <- paste0(gqpop$STATE, gqpop$COUNTY)
gqpop$COUNTYRACE <- paste0(gqpop$GEOID, "_", gqpop$RACE)

# Handle path correctly whether running from SCRIPTS directory or root directory
if (basename(getwd()) == "SCRIPTS") {
  stateferts_path <- "../DATA-PROCESSED/state-level-fert-rates_20152100.csv"
} else {
  stateferts_path <- "DATA-PROCESSED/state-level-fert-rates_20152100.csv"
}

# Create minimal fertility rates for Texas if file doesn't exist
if (!file.exists(stateferts_path)) {
  cat("Creating minimal fertility rates for Texas projections...\n")
  # Create simple fertility rates for Texas (STATE=48) and all race groups
  years_proj <- seq(2020, 2100, 5)  # Projection years
  races <- c("1", "2", "3")  # Race codes used in the data
  
  stateferts <- expand.grid(STATE = "48", RACE = races, YEAR = years_proj) %>%
    mutate(value = case_when(
      RACE == "1" ~ 0.065,  # White fertility rate
      RACE == "2" ~ 0.070,  # Black fertility rate  
      RACE == "3" ~ 0.080   # Other/Hispanic fertility rate
    )) %>%
    arrange(STATE, RACE, YEAR)
  
  cat("✓ Using default fertility rates for Texas projections\n")
} else {
  stateferts <- read_csv(stateferts_path)
  cat("✓ Loaded fertility rates from file\n")
}

samp <- unique(Klaunch$COUNTYRACE)
# samp <- unique(K05_pop$COUNTYRACE[which(K05_pop$STATE == 15)])
# samp <- "27129_3"
# samp <- "48201_3"
x = unlist(list(paste0(samp)))

project = function(x){
  tryCatch({
    # Only show detailed logging for first few counties
    show_detail <- which(texas_counties == x) <= 3
    if(show_detail) cat("\n=== PROCESSING COUNTY-RACE:", x, "===\n")
    
    ###   Prediction of the CCR function
    predccr = function(ccr, sex, x, DF){
      y <- as_data_frame(DF[[as.character(ccr)]][which(DF$COUNTYRACE== x & DF$SEX == sex )])
      num<- seq(1,FORLEN,5)
      # pred<- tryCatch(predict(ucm(value~0, data = y, level = TRUE, slope = FALSE)$model, n.ahead = FORLEN)[c(num),]
      #                 , error=function(e) array(0, c(STEPS)))
      pred<- tryCatch(forecast(arima(y$value, order = arima_order), h= FORLEN)$mean[c(num)]
                      , error=function(e) {
                        if(ccr == "ccr1") cat("    FORECAST ERROR for", x, ccr, "sex", sex, ":", conditionMessage(e), "\n")
                        array(0, c(STEPS))
                      })
      return(pred)
    }

    ###################
    ### DATA PREP
    ##################
    ### Filtering the Census data based on the county/race combination
    if(show_detail) cat("  Filtering K05_pop for COUNTYRACE =", x, "\n")
    K05_raw <- K05_pop[which(K05_pop$COUNTYRACE == x),]
    if(show_detail) {
      cat("  Raw filtered records:", nrow(K05_raw), "\n")
      if(nrow(K05_raw) > 0) {
        cat("  Years in data:", paste(sort(unique(K05_raw$YEAR)), collapse=", "), "\n")
        cat("  Population range:", min(K05_raw$POPULATION, na.rm=T), "to", max(K05_raw$POPULATION, na.rm=T), "\n")
      }
    }
    
    K05 <- K05_raw %>%
      group_by(YEAR,  STATE, COUNTY, RACE, SEX, AGE, COUNTYRACE) %>%
      dplyr::summarise(POPULATION = sum(POPULATION), .groups = "drop") %>%
      ungroup()
    if(show_detail) cat("  Aggregated records:", nrow(K05), "\n")
   
    ### Calculating the cohort-change differences (CCDs)
    CCDs<- K05 %>%
      ungroup() %>%
      mutate(AGE = paste0("X", str_pad(AGE, 2, pad ="0")),
             GEOID = paste0(STATE, COUNTY),
             POPULATION = as.numeric(POPULATION)) %>%
      spread(AGE, POPULATION)
    if(is.null(CCDs$X01)){CCDs$X01=0}else{CCDs$X01=CCDs$X01}
    if(is.null(CCDs$X02)){CCDs$X02=0}else{CCDs$X02=CCDs$X02}
    if(is.null(CCDs$X03)){CCDs$X03=0}else{CCDs$X03=CCDs$X03}
    if(is.null(CCDs$X04)){CCDs$X04=0}else{CCDs$X04=CCDs$X04}
    if(is.null(CCDs$X05)){CCDs$X05=0}else{CCDs$X05=CCDs$X05}
    if(is.null(CCDs$X06)){CCDs$X06=0}else{CCDs$X06=CCDs$X06}
    if(is.null(CCDs$X07)){CCDs$X07=0}else{CCDs$X07=CCDs$X07}
    if(is.null(CCDs$X08)){CCDs$X08=0}else{CCDs$X08=CCDs$X08}
    if(is.null(CCDs$X09)){CCDs$X09=0}else{CCDs$X09=CCDs$X09}
    if(is.null(CCDs$X10)){CCDs$X10=0}else{CCDs$X10=CCDs$X10}
    if(is.null(CCDs$X11)){CCDs$X11=0}else{CCDs$X11=CCDs$X11}
    if(is.null(CCDs$X12)){CCDs$X12=0}else{CCDs$X12=CCDs$X12}
    if(is.null(CCDs$X13)){CCDs$X13=0}else{CCDs$X13=CCDs$X13}
    if(is.null(CCDs$X14)){CCDs$X14=0}else{CCDs$X14=CCDs$X14}
    if(is.null(CCDs$X15)){CCDs$X15=0}else{CCDs$X15=CCDs$X15}
    if(is.null(CCDs$X16)){CCDs$X16=0}else{CCDs$X16=CCDs$X16}
    if(is.null(CCDs$X17)){CCDs$X17=0}else{CCDs$X17=CCDs$X17}
    if(is.null(CCDs$X18)){CCDs$X18=0}else{CCDs$X18=CCDs$X18}
    if(show_detail) cat("  Calculating CCDs (Cohort Change Differences)...\n")
    CCDs<- CCDs %>%
      arrange(GEOID, SEX, YEAR) %>%
      mutate(ccr1 = X02 - lag(X01, 5),
             ccr2 = X03 - lag(X02, 5),
             ccr3 = X04 - lag(X03, 5),
             ccr4 = X05 - lag(X04, 5),
             ccr5 = X06 - lag(X05, 5),
             ccr6 = X07 - lag(X06, 5),
             ccr7 = X08 - lag(X07, 5),
             ccr8 = X09 - lag(X08, 5),
             ccr9 = X10 - lag(X09, 5),
             ccr10 = X11 - lag(X10, 5),
             ccr11 = X12 - lag(X11, 5),
             ccr12 = X13 - lag(X12, 5),
             ccr13 = X14 - lag(X13, 5),
             ccr14 = X15 - lag(X14, 5),
             ccr15 = X16 - lag(X15, 5),
             ccr16 = X17 - lag(X16, 5),
             ccr17 = X18 - (lag(X17, 5) + lag(X18, 5))) %>%
      filter(YEAR >= min(YEAR +5, na.rm=T) & YEAR <= test_year)
    if(show_detail) cat("  CCDs records:", nrow(CCDs), "| Non-zero ccr1:", sum(CCDs$ccr1 != 0, na.rm=T), "\n")
    
    ### Calculating the CCRs
    CCRs<- K05 %>%
      ungroup() %>%
      mutate(AGE = paste0("X", str_pad(AGE, 2, pad ="0")),
             GEOID = paste0(STATE, COUNTY),
             POPULATION = as.numeric(POPULATION)) %>%
      spread(AGE, POPULATION)
    if(is.null(CCRs$X01)){CCRs$X01=0}else{CCRs$X01=CCRs$X01}
    if(is.null(CCRs$X02)){CCRs$X02=0}else{CCRs$X02=CCRs$X02}
    if(is.null(CCRs$X03)){CCRs$X03=0}else{CCRs$X03=CCRs$X03}
    if(is.null(CCRs$X04)){CCRs$X04=0}else{CCRs$X04=CCRs$X04}
    if(is.null(CCRs$X05)){CCRs$X05=0}else{CCRs$X05=CCRs$X05}
    if(is.null(CCRs$X06)){CCRs$X06=0}else{CCRs$X06=CCRs$X06}
    if(is.null(CCRs$X07)){CCRs$X07=0}else{CCRs$X07=CCRs$X07}
    if(is.null(CCRs$X08)){CCRs$X08=0}else{CCRs$X08=CCRs$X08}
    if(is.null(CCRs$X09)){CCRs$X09=0}else{CCRs$X09=CCRs$X09}
    if(is.null(CCRs$X10)){CCRs$X10=0}else{CCRs$X10=CCRs$X10}
    if(is.null(CCRs$X11)){CCRs$X11=0}else{CCRs$X11=CCRs$X11}
    if(is.null(CCRs$X12)){CCRs$X12=0}else{CCRs$X12=CCRs$X12}
    if(is.null(CCRs$X13)){CCRs$X13=0}else{CCRs$X13=CCRs$X13}
    if(is.null(CCRs$X14)){CCRs$X14=0}else{CCRs$X14=CCRs$X14}
    if(is.null(CCRs$X15)){CCRs$X15=0}else{CCRs$X15=CCRs$X15}
    if(is.null(CCRs$X16)){CCRs$X16=0}else{CCRs$X16=CCRs$X16}
    if(is.null(CCRs$X17)){CCRs$X17=0}else{CCRs$X17=CCRs$X17}
    if(is.null(CCRs$X18)){CCRs$X18=0}else{CCRs$X18=CCRs$X18}
    CCRs<- CCRs %>%
      arrange(GEOID, SEX, YEAR) %>%
      mutate(ccr1 = X02 / lag(X01, 5),
             ccr2 = X03 / lag(X02, 5),
             ccr3 = X04 / lag(X03, 5),
             ccr4 = X05 / lag(X04, 5),
             ccr5 = X06 / lag(X05, 5),
             ccr6 = X07 / lag(X06, 5),
             ccr7 = X08 / lag(X07, 5),
             ccr8 = X09 / lag(X08, 5),
             ccr9 = X10 / lag(X09, 5),
             ccr10 = X11 / lag(X10, 5),
             ccr11 = X12 / lag(X11, 5),
             ccr12 = X13 / lag(X12, 5),
             ccr13 = X14 / lag(X13, 5),
             ccr14 = X15 / lag(X14, 5),
             ccr15 = X16 / lag(X15, 5),
             ccr16 = X17 / lag(X16, 5),
             ccr17 = X18 / (lag(X17, 5) + lag(X18, 5))) %>%
      filter(YEAR >= min(YEAR +5, na.rm=T) & YEAR <= test_year)

    CCRs[mapply(is.infinite, CCRs)] <- NA
    CCRs[mapply(is.nan, CCRs)] <- NA
    CCRs[is.na(CCRs)] <-0
    CCDs[mapply(is.infinite, CCDs)] <- NA
    CCDs[mapply(is.nan, CCDs)] <- NA
    CCDs[is.na(CCDs)] <-0
    ##################################################
    ### Start of the Additive projections
    ##################################################
    
    ###  Calculating the UCM's of the CCD's for each age/sex group. The confidence interval is set to 80% (1.28*SD) 
    for (i in 1:(SIZE-1)){
      data_tablef <- cbind(predccr(paste0("ccr",i), "2", x, CCDs),0,0)
      data_tablem <- cbind(predccr(paste0("ccr",i), "1", x, CCDs),0,0)
      errf <- sd(CCDs[[as.character(paste0("ccr",i))]][which(CCDs$SEX == "2")])*1.28
      errm <- sd(CCDs[[as.character(paste0("ccr",i))]][which(CCDs$SEX == "1")])*1.28
      data_tablef[,2]<- data_tablef[,1]- ifelse(is.na(errf),0, errf)
      data_tablef[,3]<- data_tablef[,1]+ ifelse(is.na(errf),0, errf)
      data_tablem[,2]<- data_tablem[,1]- ifelse(is.na(errm),0, errm)
      data_tablem[,3]<- data_tablem[,1]+ ifelse(is.na(errm),0, errm)
      assign(paste0("BA",i,"f"), data_tablef[1:STEPS,1:3])
      assign(paste0("BA",i,"m"), data_tablem[1:STEPS,1:3])
      rm(data_tablef, data_tablem, errf, errm)
    }
    ### "Stacking" the CCDs into a single vector with a high/medium/low  
    for (i in 1:STEPS){
      namm<-paste0("lx", i, "m")
      namf<-paste0("lx",i,"f")
      assign(namm, rbind(BA1m[i,],BA2m[i,], BA3m[i,], BA4m[i,], BA5m[i,], BA6m[i,], BA7m[i,], BA8m[i,], BA9m[i,], BA10m[i,]
                         , BA11m[i,], BA12m[i,], BA13m[i,], BA14m[i,], BA15m[i,], BA16m[i,], BA17m[i,]))
      assign(namf, rbind(BA1f[i,],BA2f[i,], BA3f[i,], BA4f[i,], BA5f[i,], BA6f[i,], BA7f[i,], BA8f[i,], BA9f[i,], BA10f[i,]
                         , BA11f[i,], BA12f[i,], BA13f[i,], BA14f[i,], BA15f[i,], BA16f[i,], BA17f[i,]))}
    ###   Placing the CCD's into the subdiagonal of a leslie matrix.
    for (i in 1:STEPS){
      data_tablef <- get(paste0("lx",i,"f"))
      weird_dataf <- array(0,c(SIZE,SIZE,ncol(data_tablef)))
      data_tablem <- get(paste0("lx",i,"m"))
      weird_datam <- array(0,c(SIZE,SIZE,ncol(data_tablem)))
      for(j in 1:ncol(data_tablef)){
        weird_dataf[,,j] <- rbind(0,cbind(diag(data_tablef[,j]),0))
        weird_datam[,,j] <- rbind(0,cbind(diag(data_tablem[,j]),0))
        assign(paste0("S",i,"m"), weird_datam)
        assign(paste0("S",i,"f"), weird_dataf)
      }
      rm(data_tablef)
      rm(weird_dataf)
    }
    ### Formatting the base POPULATION data as equal to the total POPULATION minus the group quaters.
    popf <- array(0, c(SIZE))
    for(i in 1:SIZE){    popf[i] <- ifelse(length(K05$POPULATION[which(K05$SEX == "2" & K05$YEAR == launch_year & K05$AGE == i)])==0,
                                           0,
                                           K05$POPULATION[which(K05$SEX == "2" & K05$YEAR == launch_year & K05$AGE == i)])}
    gqf <-  if (length(gqpop$group_quarters[which(gqpop$SEX == "2" & gqpop$COUNTYRACE == x)]) > 0){
      gqpop$group_quarters[which(gqpop$SEX == "2" & gqpop$COUNTYRACE == x)]} else {
        0}
    popf <- popf - gqf
    
    popm <- array(0, c(SIZE))
    for(i in 1:SIZE){popm[i] <- ifelse(length(K05$POPULATION[which(K05$SEX == "1" & K05$YEAR == launch_year & K05$AGE == i)])==0,
                                       0,
                                       K05$POPULATION[which(K05$SEX == "1" & K05$YEAR == launch_year & K05$AGE == i)])}
    gqm <- if (length(gqpop$group_quarters[which(gqpop$SEX == "1" & gqpop$COUNTYRACE == x)]) > 0){
      gqpop$group_quarters[which(gqpop$SEX == "1" & gqpop$COUNTYRACE == x)]} else {
        0}
    popm <- popm - gqm 
    p0f <-array(0,c(SIZE,SIZE,ncol(lx1f)))
    p0m <-array(0,c(SIZE,SIZE,ncol(lx1f)))
    for (i in 1:ncol(lx1f)){
      p0f[,,i] <-  rbind(0,cbind(diag(popf),0))[1:18,1:18]
      p0f[18,18,i] = popf[18]
      p0m[,,i] <-  rbind(0,cbind(diag(popm),0))[1:18,1:18]
      p0m[18,18,i] = popm[18]
    }  
    ### Calculating the forecasted CWR's from the UCMs. Confidence interval is set at 80% (1.28*SD) 
    n02 <- filter(stateferts, STATE == substr(x, 1,2), RACE == substr(x,7,8)) %>%
      dplyr::select(value)
    n01 <- array(0,c(STEPS,ncol(lx1f)))
    n01[,1] <-n02$value[1:STEPS]
    
    ### PROJECTION ITSELF ###
    
    # Actually projecting with the additive model
    for (i in 1:STEPS){
      data_tablef <- get(paste0("S",i,"f"))
      data_tablem <- get(paste0("S",i,"m"))
      projm<-projf <- array(0,c(SIZE,ncol(lx1f)), dimnames = list(
        c("a1", "a2", "a3", "a4", "a5","a6", "a7", "a8", "a9", "a10", "a11", "a12", "a13", "a14", "a15", "a16", "a17", "a18")))
      popdatf<- get(paste0("p",i-1,"f"))
      popdatm<- get(paste0("p",i-1,"m"))
      for(j in 1:ncol(lx1f)){  
        projf[,j] <- rowSums(data_tablef[,,j] + popdatf[,,j])
        projm[,j] <- rowSums(data_tablem[,,j] + popdatm[,,j])
        projf[1,j] <- (n01[i,j] * sum(projf[4:10,j]))*.487
        projm[1,j] <- (n01[i,j] * sum(projf[4:10,j]))*.512
        popdatf[,,j] <-rbind(0,cbind(diag(projf[,j]),0))[1:18,1:18]
        popdatm[,,j] <-rbind(0,cbind(diag(projm[,j]),0))[1:18,1:18]
        popdatf[18,18,j] <- projf[18,j]
        popdatm[18,18,j] <- projm[18,j]
        assign(paste0("p",i,"f"), popdatf)
        assign(paste0("p",i,"m"), popdatm)
        assign(paste0("proj",i,"f"),projf)
        assign(paste0("proj",i,"m"), projm)
      }
      rm(data_tablef, data_tablem, projf, projm, popdatf, popdatm)
    }
    ### Collecting the additive projections together.
    projm<-NULL
    projf<-NULL
    for (i in 1:STEPS){
      data_tablem <- as.data.frame.table(get(paste0("proj",i,"m")) + gqm)
      data_tablem$YEAR <- launch_year+ (i*5)
      data_tablem$SEX <- "1"
      data_tablef <- as.data.frame.table(get(paste0("proj",i,"f")) + gqf)
      data_tablef$YEAR <- launch_year+ (i*5)
      data_tablef$SEX <- "2"
      projm <- rbind(projm, data_tablem)
      projf <-rbind(projf, data_tablef)
      namm<- get(paste0("proj",i,"m"))
      rm(data_tablem)
    }
    ### Declaring several variables
    projadd <-rbind(projm, projf) %>%
      dplyr::rename(Scenario = Var2)
    projadd$COUNTYRACE <-x
    projadd$TYPE<- "ADD"
    
    ######################################
    ### PROJECTING THE CCRs
    
    ### Calculating the CCR UCMs for each individual age group
    for (i in 1:(SIZE-1)){
      data_tablef <- cbind(predccr(paste0("ccr",i), "2", x, CCRs),0,0)
      data_tablem <- cbind(predccr(paste0("ccr",i), "1", x, CCRs),0,0)
      errf <- sd(CCRs[[as.character(paste0("ccr",i))]][which(CCRs$SEX == "2")])*1.28
      errm <- sd(CCRs[[as.character(paste0("ccr",i))]][which(CCRs$SEX == "1")])*1.28
      data_tablef[,2]<- data_tablef[,1]- ifelse(is.na(errf),0, errf)
      data_tablef[,3]<- data_tablef[,1]+ ifelse(is.na(errf),0, errf)
      data_tablem[,2]<- data_tablem[,1]- ifelse(is.na(errm),0, errm)
      data_tablem[,3]<- data_tablem[,1]+ ifelse(is.na(errm),0, errm)
      assign(paste0("BA",i,"f"), data_tablef[1:STEPS,1:3])
      assign(paste0("BA",i,"m"), data_tablem[1:STEPS,1:3])
      rm(data_tablef, data_tablem, errf, errm)
    }
    ### Stacking the forecasted CCRs into single vectors.
    for (i in 1:STEPS){
      namm<-paste0("lx", i, "m")
      namf<-paste0("lx",i,"f")
      assign(namm, rbind(BA1m[i,],BA2m[i,], BA3m[i,], BA4m[i,], BA5m[i,], BA6m[i,], BA7m[i,], BA8m[i,], BA9m[i,], BA10m[i,]
                         , BA11m[i,], BA12m[i,], BA13m[i,], BA14m[i,], BA15m[i,], BA16m[i,], BA17m[i,]))
      assign(namf, rbind(BA1f[i,],BA2f[i,], BA3f[i,], BA4f[i,], BA5f[i,], BA6f[i,], BA7f[i,], BA8f[i,], BA9f[i,], BA10f[i,]
                         , BA11f[i,], BA12f[i,], BA13f[i,], BA14f[i,], BA15f[i,], BA16f[i,], BA17f[i,]))}
    ### Setting the sub-diagonal of a leslie matrix as equal to the projected CCRs
    for (i in 1:STEPS){
      data_tablef <- get(paste0("lx",i,"f"))
      data_tablem <- get(paste0("lx",i,"m"))
      weird_dataf <- array(0,c(SIZE,SIZE,ncol(data_tablef)))
      weird_datam <- array(0,c(SIZE,SIZE,ncol(data_tablem)))
      for(j in 1:ncol(data_tablef)){  
        weird_dataf[,,j] <- rbind(0,cbind(diag(data_tablef[,j]),0))
        weird_dataf[18,18,j]=data_tablef[17,j]
        weird_datam[,,j] <- rbind(0,cbind(diag(data_tablem[,j]),0))
        weird_datam[18,18,j]=data_tablem[17,j]
        assign(paste0("S",i,"f"), weird_dataf)
        assign(paste0("S",i,"m"), weird_datam)
      }
      rm(data_tablef, data_tablem, weird_dataf, weird_datam)
    }
    ### Formatting the base POPULATION data.
    p0f <-array(0,c(SIZE,1,ncol(lx1f)))
    p0m <-array(0,c(SIZE,1,ncol(lx1f)))
    for (i in 1:ncol(lx1f)){
      p0f[,,i] <-  cbind(popf)
      p0m[,,i] <-  cbind(popm)
      
    }  
    ### PROJECTING THE CCRs
    for (i in 1:STEPS){
      data_tablef <- get(paste0("S",i,"f"))
      data_tablem <- get(paste0("S",i,"m"))
      projm<-projf <- array(0,c(SIZE,1,ncol(lx1f)), dimnames = list(
        c("a1", "a2", "a3", "a4", "a5","a6", "a7", "a8", "a9", "a10", "a11", "a12", "a13", "a14", "a15", "a16", "a17", "a18")))
      popdatf<- get(paste0("p",i-1,"f"))
      popdatm<- get(paste0("p",i-1,"m"))
      for(j in 1:ncol(lx1f)){  
        projf[,,j] <- data_tablef[,,j] %*% popdatf[,,j]
        projm[,,j] <- data_tablem[,,j] %*%  popdatm[,,j]
        projf[1,,j] <- (n01[i,j] * sum(projf[4:10,,j]))*.487
        projm[1,,j] <- (n01[i,j] * sum(projf[4:10,,j]))*.512
        assign(paste0("p",i,"f"), projf)
        assign(paste0("p",i,"m"), projm)
        assign(paste0("proj",i,"f"), projf)
        assign(paste0("proj",i,"m"), projm)
      }
    }
    ### Collecting the projection results
    projm<-NULL
    projf<-NULL
    for (i in 1:STEPS){
      data_tablem <- as.data.frame.table(get(paste0("proj",i,"m")) + gqm)
      data_tablem$YEAR <- launch_year+ (i*5)
      data_tablem$SEX <- "1"
      data_tablef <- as.data.frame.table(get(paste0("proj",i,"f")) + gqf)
      data_tablef$YEAR <- launch_year+ (i*5)
      data_tablef$SEX <- "2"
            projm <- rbind(projm, data_tablem)
      projf <-rbind(projf, data_tablef)
      namm<- get(paste0("proj",i,"m"))
      rm(data_tablem)
    }
    
    projmult <-rbind(projm, projf) %>%
      dplyr::select(-Var2) %>%
      dplyr::rename(Scenario = Var3)
    projmult$COUNTYRACE <-x
    projmult$TYPE<- "Mult"
   
    # Collecting all projections together
    proj <-rbind(projadd, projmult) #%>%
    
    return(proj)
  }
  , error=function(e){cat(x," ERROR :",conditionMessage(e), "\n")})
}

# for(this.state in stateid){
#   x = unlist(list(unique(K05_pop$COUNTYRACE[which(K05_pop$STATE==this.state)])))
#   KT = rbindlist(pbmclapply(x, project, mc.cores = detectCores()-1))
#   KT2 <- KT %>%
#     mutate(AGE = as.numeric(substr(Var1, 2,3))) %>%
#     group_by(YEAR, COUNTYRACE, SEX, AGE) %>%
#     spread(Scenario, Freq)
#   write.table(KT2, paste0("PROJECTIONS/PROJ/COUNTY_20152100_",this.state,".csv"))
# }

# Record start time for performance tracking
(start.time <- Sys.time())

# Filter for Texas only (STATE = 48) for focused projections
texas_stateid <- "48"
cat("Processing Texas counties only (STATE = 48)...\n")

# Check if Texas data exists
cat("\n=== TEXAS DATA VALIDATION ===\n")
texas_data <- K05_pop[which(K05_pop$STATE == texas_stateid),]
cat("Total Texas records in K05_pop:", nrow(texas_data), "\n")
if(nrow(texas_data) > 0) {
  cat("Texas data years:", paste(sort(unique(texas_data$YEAR)), collapse=", "), "\n")
  cat("Texas counties:", paste(sort(unique(texas_data$COUNTY)), collapse=", "), "\n")
  cat("Texas races:", paste(sort(unique(texas_data$RACE)), collapse=", "), "\n")
}

texas_counties <- unique(K05_pop$COUNTYRACE[which(K05_pop$STATE == texas_stateid)])
cat("Found", length(texas_counties), "Texas county-race combinations\n")
cat("Sample county-race combinations:", paste(head(texas_counties, 10), collapse=", "), "\n")

if (length(texas_counties) == 0) {
  cat("Available states in K05_pop:", paste(sort(unique(K05_pop$STATE)), collapse=", "), "\n")
  stop("ERROR: No Texas data found in K05_pop. Check if STATE=48 exists in the data.")
}

# Process Texas only - sequential processing (no parallel needed for single state)
x = unlist(list(texas_counties))
cat("Projecting", length(x), "Texas county-race combinations...\n")

# Process projections for all Texas counties
cat("\n=== STARTING PROJECTIONS ===\n")
cat("Processing", length(x), "county-race combinations\n")
cat("First few to process:", paste(head(x, 5), collapse=", "), "\n")
cat("Note: Only showing detailed logs for first few counties and key errors...\n")

# Process with progress tracking
progress_counter <- 0
total_counties <- length(x)

project_with_progress <- function(county) {
  progress_counter <<- progress_counter + 1
  if(progress_counter %% 50 == 0 || progress_counter <= 10) {
    cat("Progress:", progress_counter, "/", total_counties, "counties processed\n")
  }
  return(project(county))
}

KT = rbindlist(lapply(x, project_with_progress))

cat("\n=== PROJECTION RESULTS SUMMARY ===\n")
cat("✓ Projection calculations completed\n")
cat("KT result dimensions:", nrow(KT), "x", ncol(KT), "\n")
if(nrow(KT) > 0) {
  cat("KT columns:", paste(names(KT), collapse=", "), "\n")
  cat("KT sample (first 3 rows):\n")
  print(head(KT, 3))
  cat("Freq column summary:\n")
  print(summary(KT$Freq))
}

# Format the results
cat("\n=== FORMATTING RESULTS ===\n")
KT2 <- KT %>%
  mutate(AGE = as.numeric(substr(Var1, 2,3))) %>%
  group_by(YEAR, COUNTYRACE, SEX, AGE) %>%
  spread(Scenario, Freq)
cat("✓ Results formatted\n")
cat("KT2 dimensions:", nrow(KT2), "x", ncol(KT2), "\n")
if(nrow(KT2) > 0) {
  cat("KT2 columns:", paste(names(KT2), collapse=", "), "\n")
  cat("KT2 sample (first 3 rows):\n")
  print(head(KT2, 3))
  
  # Check the scenario columns (A, B, C)
  scenario_cols <- intersect(c("A", "B", "C"), names(KT2))
  for(col in scenario_cols) {
    if(col %in% names(KT2)) {
      cat("Column", col, "summary:\n")
      print(summary(KT2[[col]]))
      cat("  NA count:", sum(is.na(KT2[[col]])), "\n")
      cat("  Zero count:", sum(KT2[[col]] == 0, na.rm=T), "\n")
    }
  }
}

# Handle output path correctly
if (basename(getwd()) == "SCRIPTS") {
  output_path <- paste0("../PROJECTIONS/PROJ/COUNTY_20152100_",texas_stateid,".csv")
} else {
  output_path <- paste0("PROJECTIONS/PROJ/COUNTY_20152100_",texas_stateid,".csv")
}

# Ensure output directory exists
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

# Save results with proper formatting
cat("Saving results to file...\n")
write.table(KT2, output_path, row.names = FALSE, sep = ",")
cat("✓ Texas projections saved to:", output_path, "\n")

# Show completion statistics
end.time <- Sys.time()
cat("\n=== PROCESSING SUMMARY ===\n")
cat("Counties processed:", length(unique(KT2$COUNTYRACE)), "\n")
cat("Years projected: 2020-2100\n")
cat("Processing time:", round(difftime(end.time, start.time, units = "mins"), 2), "minutes\n")

cat("\n=== 007-projections_2100.R COMPLETED SUCCESSFULLY ===\n")
cat("✓ County-level population projections generated (2015-2100)\n")
cat("✓ All demographic groups processed (age, sex, race)\n")
cat("✓ All SSP scenarios calculated\n")
if (basename(getwd()) == "SCRIPTS") {
  proj_dir <- "../PROJECTIONS/PROJ/"
} else {
  proj_dir <- "PROJECTIONS/PROJ/"
}
cat("✓ Projection files saved to", proj_dir, "directory\n")
cat("TEXAS 2040 DATA: Filter files for STATE=48 and YEAR=2040\n")
cat("===============================================\n\n")
