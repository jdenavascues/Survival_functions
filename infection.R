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
      dat <- read_excel(x, sheet=dsheet)
      cat("`analyse_spreadsheet` will extract data from sheet `", dsheet, "`.\n", sep='')
    } else {
      if (!is.na(match("data", str_to_lower( (excel_sheets(filepath)) )))) {
        d <- match("data", str_to_lower(excel_sheets(filepath)))
        dat <- read_excel(x, sheet=excel_sheets(filepath)[d])
        cat("`analyse_spreadsheet` will extract data from sheet `data` (deduced).\n")
      } else if (!is.na(match("tidy", str_to_lower( (excel_sheets(filepath)) )))) {
        # historically, previous template data files had the data in a 'tidy' sheet
        d <- match("tidy", str_to_lower(excel_sheets(filepath)))
        dat <- read_excel(x, sheet=excel_sheets(filepath)[d])
        cat("`analyse_spreadsheet` will extract data from sheet `tidy` (deduced).\n")
      } else {
        dat <- read_excel(x)
        message(cat('You have not provided a spreadsheet name, nor your Excel file has\n',
                    'any of the usual names for time-to-event data in the lab.\n',
                    'We will continue using whatever is in the first sheet.', sep=''))
      }
    }
  } else if (sour_ce=='dataframe') {
    dat <- x
    cat("`analyse_spreadsheet` will read the data from a dataframe object.\n",
        "No checks on the data will be performed at this point.\n")
  }
  names(dat) <- str_to_lower(names(dat))
  return (dat)
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
      return (NULL)
    }
  }
  return (metadata)
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

set_rep_size <- function(dat, metadata, rep_size) {
  # `rep_size`: number of individuals per replicate per stratum
  default_rep_size <- 20
  # if not argument given:
  if (missing(rep_size)) {
    # extract it from metadata
    if (!is.null(metadata)) {
      rep_size <- as.numeric(metadata[metadata$Category=='Replicate_size','Value'][[1]])
      cat("Replicate size information found in the metadata.\n")
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

check_rec_style <- function(dat, rep_size, strata_vars) {
  # this is a compatibility test - not a guarantee!
  # NOT READY TO TAKE `rep_size` AS A DATAFRAME
  # check, for all strata, which replicates are compatible with 'cumulative' recording
  # (i.e. show constant or ever-increasing event records)
  check <- dat %>%
    reframe(diff=diff(events), .by=all_of(strata_vars)) %>%
    reframe(cumulative_xdiff=all(diff>=0),  .by=all_of(strata_vars))
  cumulative_compatible <- data.frame(diff = check$cumulative_xdiff)
  # check, for all strata, that total events are compatible with 'cumulative' recording
  # (i.e. sum of all events "expressed" is not larger than rep_size)
  check <- dat %>%
    summarise(sum=sum(events), .by=all_of(strata_vars)) %>%
    summarise(cumulative_xsum=all(sum>rep_size), .by=all_of(strata_vars))
  cumulative_compatible$sum <- check$cumulative_xsum
  # if all strata replicates have records that are always increasing or
  # that add up to more than rep_size, it could be that recording is cumulative
  cumulative_compatible$cumulative <- (cumulative_compatible$diff | cumulative_compatible$sum)
  if ( all(cumulative_compatible$cumulative) ) {
    cat('`analyze_spreadsheet` finds evidence of cumulative ',
        'recording in every stratum replicate.\n',
        'It will be assumed that "cumulative" events were recorded ',
        'at each observation.\n', sep='')
    return ('cumulative')
  } else {
    cat('`analyze_spreadsheet` does not find evidence of cumulative ',
        'recording in every stratum replicate.\n',
        'It will be assumed that only "new" events were recorded ',
        'at each observation.\n', sep='')
    return ('new')
  }
}

# --------------------------------------------------------------------

set_recording_style <- function(rec_style, dat, metadata, rep_size, strata_vars) {
  # `rec_style`: whether the events have been recorded cumulatively or 'instantaneously'
  # if no argument given:
  if (missing(rec_style)) {
    # getting it from metadata
    if (!is.null(metadata)) {
      rec_style <- metadata[metadata$Category=='Recording_style','Value'][[1]]
      # if not valid, deduce `rec_style` from the data
      if(!rec_style %in% c('cumulative', 'new')) {
        cat('The metadata provided no valid information about whether events were recorded cumulatively.\n',
            '`analyse_spreadsheet` will attempt to deduce this from the data.\n', sep='')
        rec_style <- check_rec_style(dat, rep_size, strata_vars)
      }
    # if metadata does not exist, deduce `rec_style` from the data
    } else {
      cat('There is no input information about whether events were recorded cumulatively.\n',
          '`analyse_spreadsheet` will attempt to deduce this from the data.\n', sep='')
      rec_style <- check_rec_style(dat, rep_size, strata_vars)
    }
  # if argument given:
  } else {
    # `rec_style` is declared as 'new'
    if (rec_style == 'new') {
      cat('You have specified that events were NOT recorded cumulatively.\n')
      if (rec_style==check_rec_style(dat, rep_size, strata_vars)) {
        cat('The data seem to have been recorded in this way.\n')
      } else {
        cat('However, the data seem to have been recorded CUMULATIVELY.\n',
            '`analyse_spreadsheet` will go ahead assuming this was an error on your part,\n',
            'and analyse the data as if they were recorded cumulatively.\n',
            '---> PLEASE CHECK YOUR DATA <---', sep='')
        rec_style <- check_rec_style(dat, rep_size, strata_vars)
      }
    # `rec_style` is declared as 'cumulative'
    } else if (rec_style == 'cumulative') {
      cat('You have specified that events were recorded CUMULATIVELY.\n')
      if (rec_style==check_rec_style(dat, rep_size, strata_vars)) {
        cat('The data seem to have been recorded in this way.\n')
      } else {
        cat('However, the data seem to have been recorded NON-cumulatively.\n',
            '`analyse_spreadsheet` will go ahead assuming this was an error on your part,\n',
            'and analyse the data as if they were recorded non-cumulatively.\n',
            '---> PLEASE CHECK YOUR DATA <---', sep='')
        rec_style <- check_rec_style(dat, rep_size, strata_vars)
      }
    } else {
      cat('You have provided an invalid option for how the events were recorded.\n',
          '`analyse_spreadsheet` will attempt to deduce this from the data.\n', sep='')
      rec_style <- check_rec_style(dat, rep_size, strata_vars)
    }
  }
  return (rec_style)
}

# --------------------------------------------------------------------

data_cleanup <- function(dat, explanatory_vars, all_vars) {
  ### column name cleanup
  # identify columns with date data
  date_match <- grep("date|^day", names(dat))
  if (length(date_match)==0) {
    stop(
      cat('You do not seem to have any column specifying the date of the observations, ',
          'or it is NOT named in any of the usual ways ("date", "day", and derivatives).\n',
          '`analyze_spreadsheet` cannot continue without these data.\n',
          'Verify your dataset and, if necessary, rename the columns.\n', sep='')
    )
  } else if (length(date_match)>1) {
    stop(
      cat('You seem to have more than one column specifying dates, ',
          'according to the usual names for this ("date", "day", and derivatives).\n',
          '`analyze_spreadsheet` cannot continue with this ambiguity.\n',
          'Verify your dataset and, if necessary, rename the columns.\n', sep='')
    )
  } else if (length(date_match)==1) {
    names(dat)[date_match] <- 'date'
  }

  # identify columns with time data
  time_match <- grep("time|hour|hh", names(dat))
  if (length(time_match)==0) {
    strop(
      cat('You do not seem to have any column specifying the time of the day of the observations, ',
          'or it is NOT named in any of the usual ways ("time", "hour", "hh:mm", and derivatives).\n',
          '`analyze_spreadsheet` cannot continue without these data.\n',
          'Verify your dataset and, if necessary, rename the columns.\n', sep='')
    )
  } else if (length(time_match)>1) {
    stop(
      cat('You seem to have more than one column specifying time of the day, ',
          'according to the usual names for this ("time", "hour", "hh:mm", and derivatives).\n',
          '`analyze_spreadsheet` cannot continue with this ambiguity.\n',
          'Verify your dataset and, if necessary, rename the columns.\n', sep='')
    )
  } else if (length(time_match)==1) {
    names(dat)[time_match] <- 'time'
  }

  # identify columns with event data
  event_match <- grep("death|dead|event", names(dat))
  if (length(event_match)==0) {
    stop(
      cat('You do not seem to have any column with event data, ',
          'or it is NOT named in any of the usual ways ("deaths", "dead", "events", and derivatives)).\n',
          '`analyze_spreadsheet` cannot continue without these data.\n',
          'Verify your dataset and, if necessary, rename the columns.\n', sep='')
    )
  } else if (length(event_match)>1) {
    stop(
      cat('You seem to have more than one column with event data, ',
          'according to the usual names for this ("deaths", "dead", "events", and derivatives).\n',
          '`analyze_spreadsheet` cannot continue with this ambiguity.\n',
          'Verify your dataset and, if necessary, rename the columns.\n', sep='')
    )
  } else if (length(event_match)==1) {
    names(dat)[event_match] <- 'events'
  }
  
  # identify columns with censoring data
  censoring_match <- grep("censor", names(dat))
  if (length(censoring_match)==0) {
    cat('You do not seem to have any column with censoring data, ',
        'or it is NOT named in any of the usual ways ("censored", "censoring", and derivatives).\n',
        '`analyze_spreadsheet` will continue assuming that all individuals were censored ',
        'after the last observational timepoint.\n',
        'If this is not what happened, verify your dataset and, if necessary, rename the columns.\n', sep='')
  } else if (length(censoring_match)>1) {
    stop(
      cat('You seem to have more than one column with censoring data, ',
          'according to the usual names for this ("censored", "censoring", and derivatives).\n',
          '`analyze_spreadsheet` cannot continue with this ambiguity.\n',
          'Verify your dataset and, if necessary, rename the columns.\n', sep='')
    )
  } else if (length(censoring_match)==1) {
    names(dat)[censoring_match] <- 'censored'
  }
  
  # identify column with dose unit data (before identifying dose column)
  unit_match <- grep("unit", names(dat))
  if (length(unit_match)==0) {
    cat('You do not seem to have any column with specifying the units used to describe the dose of a treatment.\n',
        '`analyze_spreadsheet` will continue assuming that this is not relevant.\n',
        'If this is not the case, verify your dataset and, if necessary, rename the columns.\n', sep='')
  } else if (length(unit_match)>1) {
    cat('You seem to have more than one column expressing units (presumably of a treatment dose).\n',
        '`analyze_spreadsheet` will continue assuming that this is not relevant, and drop these data.\n',
        'If this is not the case, verify your dataset and, if necessary, rename the columns.\n', sep='')
    dat <- select(dat, -names(dat)[unit_match])
  } else if (length(unit_match)==1) {
    names(dat)[unit_match] <- 'dose_unit'
  }
  
  # identify column with dose data
  dose_match <- names(dat)[setdiff(grep("dose", names(dat)), grep("_unit", names(dat)))]
  if (length(dose_match)==0) {
    cat('You do not seem to have any column with specifying the dose of a treatment.\n',
        '`analyze_spreadsheet` will continue assuming that this is not relevant.\n',
        'If this is not the case, verify your dataset and, if necessary, rename the columns.\n', sep='')
  } else if (length(dose_match)>1) {
    cat('You seem to have more than one column expressing treatment dose.\n',
        '`analyze_spreadsheet` will continue assuming that this is not relevant, and drop these data.\n',
        'If this is not the case, verify your dataset and, if necessary, rename the columns.\n', sep='')
    dat <- select(dat, -names(dat)[dose_match])
  } else if (length(dose_match)==1) {
    names(dat)[dose_match] <- 'dose'
  }

  # check for sensible column names for basic variables
  confirmed_vars <- intersect(names(dat), explanatory_vars)
  if (length(confirmed_vars)==0){
    stop(
      cat('You do not seem to have any of the usual variables ',
          '(genotype, treatment, dose, sex, replicate).\n',
          '`analyze_spreadsheet` cannot continue without at least some of these data.\n',
          'Verify your dataset and, if necessary, rename the columns.\n', sep='')
      )
  } else if (length(confirmed_vars)==5) {
    cat('The usual explanatory variables are: ',
        '"genotype", "treatment", "dose", "sex" and "replicate".\n',
        'You seem to have all ', length(confirmed_vars), ' of them.\n', sep='')
  } else {
    cat('The usual explanatory variables are: ',
        '"genotype", "treatment", "dose", "sex" and "replicate".\n',
        'You seem to have ', length(confirmed_vars), ' of them: ',
        paste(confirmed_vars, collapse=', '), '.\n',
        '`analyze_spreadsheet` will continue using these explanatory variables only.\n',
        'If this is not correct, verify your dataset and rename columns as required.\n', sep='')
  }

  # remove un-interpretable columns
  extra_vars <- setdiff(names(dat), all_vars)
  if (length(extra_vars)>0){
    cat('You have the additional variable(s):',
        paste(extra_vars, collapse=', '), '.\n',
        '`analyze_spreadsheet` will continue assuming that these are not relevant, and drop these data.\n',
        'If this is not the case, verify your dataset and, if necessary, rename the columns.\n', sep='')
    dat <- select(dat, intersect(names(dat), all_vars))
  }

  ### clean up NAs
  dat <- dat %>%
    mutate(
      across(where(is.numeric), \(x) coalesce(x, 0)),
      across(where(is.character), \(x) coalesce(x, "NA"))
    )
  if ('sex' %in% names(dat)) dat$sex[dat$sex=='NA'] <- 'mixed'

  return (dat)
}

# --------------------------------------------------------------------

analyse_spreadsheet <- function(x, dsheet, msheet, rep_size, rec_style, cph=FALSE) {
  # LOAD THE DATA
  sour_ce <- check_source(x)
  dat <- load_data(sour_ce, x, dsheet)
  metadata <- load_metadata(sour_ce, x, msheet)
  # RECONCILE ARGUMENTS, METADATA, DATA COLUMN NAMES
  rep_size  <- set_rep_size(dat, metadata, rep_size)
  explanatory_vars <- c('genotype', 'treatment', 'sex', 'replicate', 'dose')
  all_vars <- c('genotype', 'treatment', 'sex', 'replicate', 'dose',
                'events', 'censored', 'dose_unit',
                'date', 'time')
  dat <- data_cleanup(dat, explanatory_vars, all_vars)
  strata_vars <- intersect(names(dat), explanatory_vars)
  rec_style <- set_recording_style(rec_style, dat, metadata, rep_size, strata_vars)

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # ESTABLISH TIME INTERVALS
  # ========================
  cat('establishing time intervals...\n')
  format_in <- "%d.%m.%Y"
  dat$date <- as.Date(dat$date, format=format_in)
  dat$time2 <- apply(dat[,c('date', 'time')], 1, function(w) f(w['date'], w['time']))
  dat$time2 <- as.POSIXct(dat$time2)
  dat$hour <- signif(difftime(dat$time2, dat$time2[1], units = "hours"), 3)
  ### In case the dat are recorded cumulatively
  cat('establishing individual events (non-cumulative)...\n')
  excluded_vars <- c('date','time','hour','time2','events','censored') # this may have to change depending on the project
  included_vars <- names(dat)[!names(dat) %in% excluded_vars]
  if (rec_style == 'cumulative') {
    var_combos <- unique(expand_grid(dat[included_vars]))
    for (row in 1:nrow(var_combos)) {
      combo <- var_combos[row, ]
      combo_rows <- apply(dat[included_vars],1,function(x) {all(x==combo)})
      events_rec_style <- dat[combo_rows,'events']
      new_events <- c(0, diff(events_rec_style$events))
      dat[combo_rows,]$events <- new_events
    }
  }
  
  # ESTABLISH NUMBER OF RECORDED EVENTS
  # ===================================
  cat('establishing death numbers...\n')
  cat('establishing explicit censorings...\n')
  # remove rows without data (events/censored separately)
  events_dat<- dat[ dat$events>0, !names(dat) %in% c('censored')]
  censor_dat<- dat[ dat$censored>0, !names(dat) %in% c('events')]
  # replicate rows per their number of events, then turn into event=1
  events_dat <- events_dat[rep(1:nrow(events_dat), events_dat$events), ]
  names(events_dat)[names(events_dat)=='events'] <- 'events'
  # same with censorings (if there are any!)
  if (length(censor_dat$censored)>0) {
    censor_dat <- censor_dat[rep(1:nrow(censor_dat), censor_dat$censored), ]
    names(censor_dat)[names(censor_dat)=='censored'] <- 'events'
  }
  events_dat$event <- 1 # code for event=death
  censor_dat$event <- 0 # code for event=censoring
  # combine
  fin_dat <- rbind(events_dat, censor_dat)
  fin_dat$maxhour <- max(fin_dat$hour)
  ### Include non-recorded censorship (endpoint or missed)
  cat('establishing implicit censorings...\n')
  
  
  
  
  # problems here
  
  
  
  
  
  # the period indicates ALL the variables (the included ones, that is)
  surv_dat <- aggregate(events ~ ., dat[,c(included_vars, 'events')], sum)
  # if not all conditions have the same size
  if (is.data.frame(rep_size)) {
    try( if (nrow(rep_size)!=nrow(surv_dat)) stop("the table reporting replicate sizes in the excel file does not have the appropriate number of rows."))
    # find common colnames for surv_dat and rep_size
    rep_cols <- intersect(names(surv_dat), names(rep_size))
    # find the order of rep_size rows to match the variables order of surv_dat
    keys <- plyr::join.keys(surv_dat,rep_size,rep_cols)
    matches <- match(keys$y,keys$x,nomatch=(keys$n+1))
    # use the new order to get numbers of survivors per vial at termination
    surv_dat$censored <- rep_size[order(matches),]$size - surv_dat$events
  # if all conditions have the same size it is much simpler:
  } else if (is.numeric(rep_size)) {
    surv_dat$censored <- rep_size - surv_dat$events
  }
  for (r in 1:nrow(surv_dat)) {
    # complete list of columns <--- this would need to be more generalised
    newrow <- surv_dat[r,!names(surv_dat) %in% c('events','censored')]
    newrow$date <- max(fin_dat$date)
    newrow$time <- max(fin_dat$time)
    newrow$event <- 0
    newrow$time2 <- max(fin_dat$time2)
    newrow$hour <- max(fin_dat$hour)
    newrow$maxhour <- max(fin_dat$maxhour)
    newrow <- newrow[rep(1,surv_dat$censored[r]),]
    fin_dat <- rbind(fin_dat, newrow)
  }
  
  # COLUMN CLEANUP <--------- quite ad hoc!
  # ==============
  returned_vals <- c(included_vars, 'event', 'hour', 'time2')
  fin_dat <- fin_dat[,returned_vals]
  fin_dat <- fin_dat %>% rename(time = time2)
  
  # BASIC SURVIVAL MODELLING
  # ========================
  cat('\nCox PH MODELLING\n')
  cat('\n----------------\n')
  if (cph) {
    cph_model <- coxph(Surv(hour, event) ~ treatment + genotype + treatment*genotype,
                       data=fin_dat)
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
  model <- survdiff(Surv(hour, event) ~ treatment + genotype, data=fin_dat)
  cat('The log-rank test gives a p-value of ', model$pvalue)
  
  return( fin_dat )
}

