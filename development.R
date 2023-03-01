library(readxl)
library(survival)
library(survminer)
library(dplyr)
library(tidyr)

f <- function(x, y) {
  # this is just to change the format of the date.
  fout <- "%Y-%m-%d"
  # get the date as a string...
  d <- format(x, format=fout)
  # get the time as string, separate from the old date...
  t <- strsplit(as.character(y), split=' ')[[1]][[2]]
  # paste together in standard format...
  paste(d, t)
}

check_cumulative <- function(df, rep_size) {
  check <- df %>% group_by(genotype, treatment, replicate) %>%
    summarise(diff=diff(deaths), .groups='keep') %>%
    summarise(cumulative_xdiff=all(diff>=0), .groups='keep')
  cumulative_compatible <- data.frame(diff = check$cumulative_xdiff)
  check <- df %>% group_by(genotype, treatment, replicate) %>%
    summarise(sum=sum(deaths), .groups='keep') %>%
    summarise(cumulative_xsum=all(sum>rep_size), .groups='keep')
  cumulative_compatible$sum <- check$cumulative_xsum
  cumulative_compatible$cumulative <- (cumulative_compatible$diff | cumulative_compatible$sum)
  if ( all(cumulative_compatible$cumulative) ) {
    cum = TRUE
  } else {
    cum = FALSE
  }
  return( cum )
}

load_devtime_data <- function(x, sheet, rep_size, cum) {
  
  # LOAD THE DATA
  
  # check input is a character
  if (is.character(x)) {
    # check input is a path to an Excel file
    if (file.exists(x) & startsWith(format_from_signature(filepath),'xl')){
      # check `sheet` is specified
      if (!missing(sheet)){
        df <- read_excel(x, sheet=sheet)
      } else {
        try::
          df <- read_excel(x, sheet=sheet)
      }
      # check that the Excel file has a 'metadata' sheet
      if (!is.na(match("metadata", str_to_lower( (excel_sheets(filepath)) )))) {
        m <- match("metadata", str_to_lower(excel_sheets(filepath)))
        metadata <- read_excel(x, sheet=excel_sheets(filepath)[m])
      }
    }
    # if it is not a path, then check if it is a dataframe
  } else if (is.data.frame(x)) {
    df <- x
  } else {
    cat("`analyse_spreadsheet` cannot use the input data.\n")
    cat("`x` must be a suitable dataframe or a path to a suitable Excel file.")
    break }
  
  # RECONCILE ARGUMENTS AND METADATA
  
  # `rep_size`
  if (missing(rep_size)) {
    if (exists('metadata')) {
      rep_size <- metadata['Category'=='Replicate_size','Value']
      if (!is.numeric(rep_size)) {
        cat('Something went wrong when establishing replicate sizes.\n')
        cat('A default value of rep_size=20 will be used.')
        rep_size = 20
      }
    } else {
      cat("No replicate size was given so the default value of 20 will be used instead.")
      rep_size = 20 }
  }
  # `cum`
  if (missing(cum)) {
    if (exists('metadata')) {
      cum <- metadata['Category'=='Cumulative','Value']
      if (!is.logical(cum)) {
        cat('Something went wrong when establishing whether the data is recorded cumulatively.\n')
        cat('The metadata of this contains a non-logical value for `cum`.\n')
        cat('`analyse_spreadsheet` will attempt to deduce the value.\n')
        cum = check_cumulative(df, rep_size)
      }
    } else {
      cat('There is no input information about whether the data is recorded cumulatively.\n')
      cat('`analyse_spreadsheet` will attempt to deduce the value.')
      cum = check_cumulative(df, rep_size)
    }
    if (cum) {
      cat('`analyse_spreadsheet` will assume that the data was recorded CUMULATIVELY.\n')
    } else {
      cat('`analyse_spreadsheet` will assume that the data was recorded NON-CUMULATIVELY.\n')
    }
  }
  
  # STANDARDISE COLUMN NAMES
  # pending: catch naming/missing data errors here <----------------------------
  
  # make lowercase
  names(df) <- str_to_lower(names(df))
  names(df)[names(df)=='dose_units'] <- 'dose_unit'
  # make sure the 'dose' column is named correctly
  if (length( grep('dose', names(df)) )>0) {
    for (j in grep('dose', names(df)) ) {
      # create a 'date_units' if this is included in the 'dose' column name
      if (length(str_split(names(df)[j], '_'))>1 &
          length(grep('dose_units', names(df)))==0){
        df$date_units <- str_split(names(df)[j], '_')[[2]]
      }
      # if the 'dose' column incorporates units, rename as 'dose'
      if (names(df)[j]!='dose' &
          names(df)[j]!='dose_unit'){
        names(df)[j] <- 'dose'
      }
    }
  } else if (length( grep('dose', names(df)) )==0) {
    df$dose <- 'nd'
  }
  # make sure the 'censored' column is named correctly
  if (length( grep('censor', names(df)) )==1){
    names(df)[grep('censor', names(df))] <- 'censored'
  } else if (length( grep('censor', names(df)) )==0) {
    df$censored <- 0
  }
  
  # ESTABLISH TIME INTERVALS
  
  cat('establishing time intervals...\n')
  format_in <- "%d.%m.%Y"
  df$date <- as.Date(df$date, format=format_in)
  df$time2 <- apply(df[,c('date', 'time')], 1, function(w) f(w['date'], w['time']))
  df$time2 <- as.POSIXct(df$time2)
  df$hour <- signif(difftime(df$time2, df$time2[1], units = "hours"), 3)
  
  # IN CASE DATA IS RECORDED CUMULATIVELY
  
  cat('establishing individual events (non-cumulative)...\n')
  if (cum) {
    var_combos <- df %>% expand(treatment, dose, genotype, replicate)
    for (row in 1:nrow(var_combos)) {
      combo <- var_combos[row, ]
      deaths_cum <- df %>% subset(treatment==combo$treatment &
                                    dose==combo$dose &
                                    genotype==combo$genotype &
                                    replicate==combo$replicate, select=deaths)
      newdeaths <- c(0, diff(deaths_cum$deaths))
      df[df$treatment==combo$treatment &
           df$dose==combo$dose &
           df$genotype==combo$genotype &
           df$replicate==combo$replicate,]$deaths <- newdeaths
    }
  }
  
  # ESTABLISH NUMBER OF RECORDED EVENTS
  
  cat('establishing death numbers...\n')
  cat('establishing explicit censorings...\n')
  # remove rows without data 
  deaths_df<- df[ df$deaths>0, c('hour', 'deaths', 'treatment',
                                 'genotype', 'dose', 'replicate')]
  censor_df<- df[ df$censored>0, c('hour', 'censored', 'treatment',
                                   'genotype', 'dose', 'replicate')]
  # replicate rows per their number of events
  deaths_df <- deaths_df[rep(1:nrow(deaths_df), deaths_df$deaths), ]
  if (nrow(censor_df)>0) {
    censor_df <- censor_df[rep(1:nrow(censor_df), censor_df$censored), ]
  }
  # turn deaths and censored into events (0/1)
  names(deaths_df)[names(deaths_df)=='deaths'] <- 'event'
  names(censor_df)[names(censor_df)=='censored'] <- 'event'
  deaths_df$event <- 1 # code for event=death
  censor_df$event <- 0 # code for event=censoring
  # combine
  fin_df <- rbind(deaths_df, censor_df)
  fin_df$maxhour <- max(fin_df$hour)
  
  # INCLUDE NON-RECORDED CENSORSHIP
  # pending: remove recorded censorship from automated total <-----------------------
  
  cat('establishing implicit censorings...\n')
  surv_df <- aggregate(deaths ~ replicate + genotype + treatment + dose, df, sum)
  surv_df$censored <- rep_size - surv_df$deaths
  for (r in 1:nrow(surv_df)) {
    for (c in 1:surv_df$censored[r]) {
      fin_df[nrow(fin_df) + 1,] = list(max(fin_df$hour),
                                       0, # event=censoring
                                       surv_df$treatment[r],
                                       surv_df$genotype[r],
                                       surv_df$dose[r],
                                       surv_df$replicate[r],
                                       max(fin_df$maxhour))
    }
  }
  return( fin_df )
}

basic_analysis <- function(df, cph=FALSE) {
  
  cat('\nCox PH MODELLING\n')
  cat('\n----------------\n')
  if (cph) {
    cph_model <- coxph(Surv(hour, event) ~ treatment + genotype + treatment*genotype,
                       data=fin_df)
    zphfit <- cox.zph(cph_model)
    if (zphfit$table[,3]['GLOBAL']>0.05){
      cat('\tSchoenfeld test shows PH assumption is respected\n')
      cat('(null hypothesis=it is not)):\n')
      ggcoxzph(zphfit)
      cat('P-value: ', cox.zph(cph_model)$table[,3], '\n')
      cat(paste("\nlog-rank test p-value:", summary(cph_model)$logtest[3], "\n"))
      cat('\tThe variables with significant effect are:\n')
      print(cph_model)
    } else {
      cat('\tSchoenfeld test shows PH assumption is NOT respected:\n\n')
      print(cox.zph(cph_model)$table[,3]['GLOBAL'])
    }
  }

  cat('\nLOG-RANK p-value\n')
  cat('\n----------------\n')
  model <- survdiff(Surv(hour, event) ~ treatment + genotype, data=fin_df)
  cat('The log-rank test gives a p-value of ', model$pvalue)
}

col_lin_test <- function(palette, ) {
  
  # Determine explanatory variables and their reference levels
  df[explanatory_vars] <- lapply(df[explanatory_vars], factor)
  df[explanatory_vars] <- Map(relevel, df[explanatory_vars], reference_lvls)
  
  # Determine labels
  
  # Determine colour/linetype combinations
  
}

prepare_for_plotting <- function(df, explanatory_vars, reference_lvls, for_print, sep='|') {
  
  # Determine explanatory variables and their reference levels
  df[explanatory_vars] <- lapply(df[explanatory_vars], factor)
  df[explanatory_vars] <- Map(relevel, df[explanatory_vars], reference_lvls)
  
  # Determine labels
  
  # Determine colour/linetype combinations
  
}