library(readxl)
library(survival)
library(survminer)
library(dplyr)
library(tidyr)
library(stringr)

### PENDING:
# - catch naming/missing data errors when checking column names
# - implementing the use of rep_size as a table
# - remove recorded censorship from automated total !!!

# --------------------------------------------------------------------

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

# --------------------------------------------------------------------

# from https://stat.ethz.ch/pipermail/r-help/2011-September/289346.html:
is.whole <- function(x) { is.numeric(x) && floor(x)==x }

# --------------------------------------------------------------------

check_source <- function(x) {
  # check input is a character
  if (is.character(x)) {
    # check input is a path to an Excel file
    if (file.exists(x) & startsWith(format_from_signature(filepath),'xl')) {
      sour_ce <- 'excel'
    } else {
      cat("`analyse_spreadsheet` cannot use the input data.\n")
      cat("`x` must be a suitable dataframe or a path to a suitable Excel file.\n")
      break
    }
  # check input is a dataframe
  } else if (is.data.frame(x)) {
    sour_ce <- 'dataframe'
  } else {
    cat("`analyse_spreadsheet` cannot use the input data.\n")
    cat("`x` must be a suitable dataframe or a path to a suitable Excel file.\n")
    break
  }
  return(sour_ce)
}

# --------------------------------------------------------------------

load_data <- function(sour_ce, x, dsheet) {
  if (sour_ce=='excel') {
    if (!missing(dsheet)){
      df <- read_excel(x, sheet=dsheet)
      cat("`analyse_spreadsheet` will extract data from sheet `", dsheet, "`.\n", sep='')
    } else {
      if (!is.na(match("data", str_to_lower( (excel_sheets(filepath)) )))) {
        d <- match("data", str_to_lower(excel_sheets(filepath)))
        df <- read_excel(x, sheet=excel_sheets(filepath)[d])
        cat("`analyse_spreadsheet` will extract data from sheet `data` (deduced).\n")
      } else if (!is.na(match("tidy", str_to_lower( (excel_sheets(filepath)) )))) {
        # historically, previous template data files had the data in a 'tidy' sheet
        d <- match("tidy", str_to_lower(excel_sheets(filepath)))
        df <- read_excel(x, sheet=excel_sheets(filepath)[d])
        cat("`analyse_spreadsheet` will extract data from sheet `tidy` (deduced).\n")
      } else {
        df <- read_excel(x)
        message(cat('You have not provided a spreadsheet name, nor your Excel file has\n',
                    'any of the usual names for time-to-event data in the lab.\n',
                    'We will continue using whatever is in the first sheet.', sep=''))
      }
    }
  } else if (sour_ce=='dataframe') {
    df <- x
    cat("`analyse_spreadsheet` will read the data from a dataframe object.\n",
        "No checks on the data will be performed at this point.\n")
  }
  return (data)
}

# --------------------------------------------------------------------

load_metadata <- function(sour_ce, x, msheet) {
  if (!sour_ce=='excel') return (NA)
  if (!missing(msheet)){
    metadata <- read_excel(x, sheet=msheet)
    cat("`analyse_spreadsheet` will read metadata from sheet `", msheet, "`.\n", sep='')
  } else {
    if (!is.na(match("metadata", str_to_lower( (excel_sheets(filepath)) )))) {
      m <- match("metadata", str_to_lower(excel_sheets(filepath)))
      metadata <- read_excel(x, sheet=excel_sheets(filepath)[m])
      cat("`analyse_spreadsheet` will read metadata from sheet `metadata` (deduced).\n")
    } else {
      cat('You have not provided a spreadsheet name for the experiment\'s metadata,\n',
          'nor your Excel file has any of the usual names for time-to-event ',
          'metadata in the lab.\nWe will continue using default values.', sep='')
      return (NA)
    }
  }
  return (metadata)
}

# --------------------------------------------------------------------

check_rep_size <- function(rep_size) {
  
  
}

# --------------------------------------------------------------------

check_rep_size <- function(rep_size, default_rep_size) {
  # for the future case of having a table:
  if (is.data.frame(rep_size)) {
    cat('Sizes of stratum replicates are not equal.\n',
        '`analyse_spreadsheet` is not ready yet to process this data.\n',
        'A default value of rep_size = ', default_rep_size, ' will be used.\n', sep='')
    return (default_rep_size)
  # for the usual case: a single positive whole number
  } else if (is.whole(rep_size) && rep_size>0) {
    return (rep_size)
  # for negative/non-whole numbers:
  } else if (is.numeric(rep_size) && (rep_size<0 || !is.whole(rep_size))) {
    cat('You have not provided a whole, positive number for the size of stratum replicates.\n',
        'The value (', rep_size, ') will be taken as ', round(abs(rep_size)), '.\n', sep='')
    return (rep_size <- round(abs(rep_size)))
  } else {
    cat('You have not provided a valid value for the size of stratum replicates.\n',
        'A default value of rep_size = ', default_rep_size, ' will be used.\n', sep='')
    return (default_rep_size)
  }
}

# --------------------------------------------------------------------

set_rep_size <- function(data, metadata, rep_size) {
  # `rep_size`: number of individuals per replicate per stratum
  default_rep_size <- 20
  # if not argument given:
  if (missing(rep_size)) {
    # extract it from metadata
    if (!is.na(metadata)) {
      rep_size <- as.numeric(metadata[metadata$Category=='Replicate_size','Value'][[1]])
      cat("Replicate size information found in the metadata")
      # in case the rep_size is not the same for all stratum replicates:
      if (rep_size=='table'){
        rep_size <- read_excel(x, sheet='rep_size')
        names(rep_size) <- str_to_lower( names(rep_size) )
      }
    } else {
      cat("No replicate size argument given and no information found in the metadata,",
          'or metadata is not provided.',
          'A default value of rep_size = ', default_rep_size, ' will be used.\n', sep='')
      return (default_rep_size)
    }
  }
  rep_size <- check_rep_size(rep_size, default_rep_size)
  return (rep_size)
}
 
# --------------------------------------------------------------------
# --------------------------------------------------------------------
# --------------------------------------------------------------------
# --------------------------------------------------------------------

check_cumulative <- function(df, rep_size) {
  # NOT READY TO TAKE `rep_size` AS A DATAFRAME
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
    cumul = TRUE
  } else {
    cumul = FALSE
  }
  return( cumul )
}

# --------------------------------------------------------------------

analyse_spreadsheet <- function(x, dsheet, msheet, rep_size, cumul, cph=FALSE) {
  # LOAD THE DATA
  sour_ce  <- check_source(x)
  data     <- load_data(sour_ce, x, dsheet)
  metadata <- load_metadata(sour_ce, x, msheet)
  # RECONCILE ARGUMENTS AND METADATA
  rep_size <- set_rep_size(data, metadata, rep_size)
  ### For the type of event recording (`cumul`): all events (cumulative) or new events
  # case no argument given
  if (missing(cumul)) {
    # getting it from metadata
    if (exists('metadata')) {
      cumul <- metadata[metadata$Category=='cumulative','Value'][[1]]
      # check that the input is either 'TRUE' or 'FALSE' and assign a logical value
      t <- try(if (eval(parse(text=str_to_upper(cumul)))) {cumul <- TRUE }
               else if (!eval(parse(text=str_to_upper(cumul)))) { cumul <- FALSE })
      # if not, deduce `cumul` from the data
      if(inherits(t, 'try-error')) {
        cat('The metadata provided no valid information about whether events were recorded cumulatively.\n',
            '`analyse_spreadsheet` will attempt to deduce this from the data.\n', sep='')
        cumul <- check_cumulative(df, rep_size) }
    # if metadata does not exist, deduce `cumul` from the data
    } else {
      cat('There is no input information about whether events were recorded cumulatively.\n',
          '`analyse_spreadsheet` will attempt to deduce this from the data.\n', sep='')
      cumul <- check_cumulative(df, rep_size)
    }
  # case argument given
  } else {
    # and `cumul` is ok
    if(cumul == check_cumulative(df, rep_size)) {
      if (cumul) cat('User has specified that events were recorded CUMULATIVELY.\n')
      else cat('User has specified that events were NOT recorded cumulatively.\n')
    # but if data does not coincide with user's declaration...
    } else {
      if( cumul ) {
        cat('You have specified that events were recorded CUMULATIVELY.\n',
            'However, the data seem to have been recorded NON-cumulatively.\n',
            '`analyse_spreadsheet` will go ahead assuming this was an error on your part;\n',
            '---> PLEASE CHECK YOUR DATA <---', sep='')
      } else {
        cat('You have specified that events were recorded NON-cumulatively.\n',
            'However, the data seem to have been recorded CUMULATIVELY.\n',
            '`analyse_spreadsheet` will go ahead assuming this was an error on your part;\n',
            '---> PLEASE CHECK YOUR DATA <---', sep='')
      }
    }
  }

  # STANDARDISE COLUMN NAMES
  # ========================
  # make lowercase
  names(df) <- str_to_lower(names(df))
  # check that the essential variables are there:
  usual_vars <- c('genotype', 'treatment', 'sex', 'replicate', 'dose')
  all_vars <- c("date", "time", "deaths", "dead", "censorings", "censored", "treatment",
                "dose", "dose_unit", "genotype", "replicate", "sex")
  extra_vars <- setdiff(names(df), all_vars)
  confirmed_vars <- intersect(names(df), usual_vars)
  if (length(confirmed_vars)==0){
    cat('You do not seem to have any of the usual variables (genotype, treatment, dose, sex, replicate).\n')
    cat('Verify your dataset and if necessary, rename the columns.\n')
    break
  } else {
    cat(paste('You have', length(confirmed_vars), 'of the 5 usual explanatory variables:',
              paste(confirmed_vars, collapse=', ')), '\n')
  }
  if (length(extra_vars)>0){
    cat(paste('You have the additional variable(s):',
              paste(extra_vars, collapse=', ')), '\n')
    cat('This/these will be either renamed (*censor*) or passed on to the output dataframe.\n')
  }
  # take care of the columns specifying dose
  names(df)[names(df)=='dose_units'] <- 'dose_unit'
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

  # CLEAN UP NAs
  # ============
  df <- df %>%
    mutate(
      across(where(is.numeric), \(x) coalesce(x, 0)),
      across(where(is.character), \(x) coalesce(x, "NA"))
      )
  if ('sex' %in% names(df)) {
    df$sex[df$sex=='NA'] <- 'mixed'
  }
  
  # ESTABLISH TIME INTERVALS
  # ========================
  cat('establishing time intervals...\n')
  format_in <- "%d.%m.%Y"
  df$date <- as.Date(df$date, format=format_in)
  df$time2 <- apply(df[,c('date', 'time')], 1, function(w) f(w['date'], w['time']))
  df$time2 <- as.POSIXct(df$time2)
  df$hour <- signif(difftime(df$time2, df$time2[1], units = "hours"), 3)
  ### In case the data are recorded cumulatively
  cat('establishing individual events (non-cumulative)...\n')
  excluded_vars <- c('date','time','hour','time2','deaths','censored') # this may have to change depending on the project
  included_vars <- names(df)[!names(df) %in% excluded_vars]
  if (cumul) {
    var_combos <- unique(expand_grid(df[included_vars]))
    for (row in 1:nrow(var_combos)) {
      combo <- var_combos[row, ]
      combo_rows <- apply(df[included_vars],1,function(x) {all(x==combo)})
      deaths_cumul <- df[combo_rows,'deaths']
      newdeaths <- c(0, diff(deaths_cumul$deaths))
      df[combo_rows,]$deaths <- newdeaths
    }
  }
  
  # ESTABLISH NUMBER OF RECORDED EVENTS
  # ===================================
  cat('establishing death numbers...\n')
  cat('establishing explicit censorings...\n')
  # remove rows without data (deaths/censored separately)
  deaths_df<- df[ df$deaths>0, !names(df) %in% c('censored')]
  censor_df<- df[ df$censored>0, !names(df) %in% c('deaths')]
  # replicate rows per their number of deaths, then turn into event=1
  deaths_df <- deaths_df[rep(1:nrow(deaths_df), deaths_df$deaths), ]
  names(deaths_df)[names(deaths_df)=='deaths'] <- 'event'
  # same with censorings (if there are any!)
  if (length(censor_df$censored)>0) {
    censor_df <- censor_df[rep(1:nrow(censor_df), censor_df$censored), ]
    names(censor_df)[names(censor_df)=='censored'] <- 'event'
  }
  deaths_df$event <- 1 # code for event=death
  censor_df$event <- 0 # code for event=censoring
  # combine
  fin_df <- rbind(deaths_df, censor_df)
  fin_df$maxhour <- max(fin_df$hour)
  ### Include non-recorded censorship (endpoint or missed)
  cat('establishing implicit censorings...\n')
  # the period indicates ALL the variables (the included ones, that is)
  surv_df <- aggregate(deaths ~ ., df[,c(included_vars, 'deaths')], sum)
  # if not all conditions have the same size
  if (is.data.frame(rep_size)) {
    try( if (nrow(rep_size)!=nrow(surv_df)) stop("the table reporting replicate sizes in the excel file does not have the appropriate number of rows."))
    # find common colnames for surv_df and rep_size
    rep_cols <- intersect(names(surv_df), names(rep_size))
    # find the order of rep_size rows to match the variables order of surv_df
    keys <- plyr::join.keys(surv_df,rep_size,rep_cols)
    matches <- match(keys$y,keys$x,nomatch=(keys$n+1))
    # use the new order to get numbers of survivors per vial at termination
    surv_df$censored <- rep_size[order(matches),]$size - surv_df$deaths
  # if all conditions have the same size it is much simpler:
  } else if (is.numeric(rep_size)) {
    surv_df$censored <- rep_size - surv_df$deaths
  }
  for (r in 1:nrow(surv_df)) {
    # complete list of columns <--- this would need to be more generalised
    newrow <- surv_df[r,!names(surv_df) %in% c('deaths','censored')]
    newrow$date <- max(fin_df$date)
    newrow$time <- max(fin_df$time)
    newrow$event <- 0
    newrow$time2 <- max(fin_df$time2)
    newrow$hour <- max(fin_df$hour)
    newrow$maxhour <- max(fin_df$maxhour)
    newrow <- newrow[rep(1,surv_df$censored[r]),]
    fin_df <- rbind(fin_df, newrow)
  }
  
  # COLUMN CLEANUP <--------- quite ad hoc!
  # ==============
  returned_vals <- c(included_vars, 'event', 'hour', 'time2')
  fin_df <- fin_df[,returned_vals]
  fin_df <- fin_df %>% rename(time = time2)
  
  # BASIC SURVIVAL MODELLING
  # ========================
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
  # for plotting (long-rank)
  cat('\nLOG-RANK p-value\n')
  cat('\n----------------\n')
  model <- survdiff(Surv(hour, event) ~ treatment + genotype, data=fin_df)
  cat('The log-rank test gives a p-value of ', model$pvalue)
  
  return( fin_df )
}

