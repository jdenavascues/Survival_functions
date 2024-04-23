library(readxl)
library(survival)
library(survminer)
library(dplyr)
library(tidyr)
library(stringr)

### PENDING:
# - catch naming/missing data errors when checking column names
# - implementing the use of rep_size as a table
#   - cleanup column names as in data sheet, so then they coincide with the aggregate of events
# - remove recorded censorship from automated total !!!
# change perspective: first try to get all the info available in the Excel file (metadata, rep_file, etc)
# once this is done, try to fill in the info

check_source <- function(x) {
  # check input is a character
  if (is.character(x)) {
    # check input is a path to an Excel file
    if (file.exists(x) & startsWith(format_from_signature(filepath),'xl')) {
      sour_ce <- 'excel'
    } else {
      stop(
        cat('\n---\n', '`analyse_spreadsheet` cannot use the input data.\n',
            '`x` must be a suitable dataframe or a path to a suitable Excel file.\n',
            sep='')
      )
    }
  # check input is a dataframe
  } else if (is.data.frame(x)) {
    sour_ce <- 'dataframe'
  } else {
    stop(
      cat('\n---\n', '`analyse_spreadsheet` cannot use the input data.\n',
          "`x` must be a suitable dataframe or a path to a suitable Excel file.\n",
          sep='')
    )
  }
  return(sour_ce)
}

# --------------------------------------------------------------------

load_data <- function(sour_ce, x, dsheet) {
  cat('\n---\n', 'Loading event data...\n', sep='')
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
        message(cat('\nYou have not provided a spreadsheet name, nor your Excel file has\n',
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
  cat('\n---\n', 'Loading experiment metadata...\n', sep='')
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
      cat('\nYou have not provided a spreadsheet name for the experiment\'s metadata,\n',
          'nor your Excel file has any of the usual names for time-to-event ',
          'metadata in the lab.\nWe will continue using default values.', sep='')
      return (NULL)
    }
  }
  return (metadata)
}

# --------------------------------------------------------------------

# from https://stat.ethz.ch/pipermail/r-help/2011-September/289346.html:
is.whole <- function(x) { is.numeric(x) && floor(x)==x }

# --------------------------------------------------------------------

check_rep_size <- function(dat, rep_size, default_rep_size, explanatory_vars) {
  
  # for the usual case: a single positive whole number
  if (is.whole(rep_size) && rep_size>0) {
    return (rep_size)
  
  # for negative/non-whole numbers, abs value and round up:
  } else if (is.numeric(rep_size) && (rep_size<0 || !is.whole(rep_size))) {
    cat('\nYou have not provided a whole, positive number for the size of stratum replicates.\n',
        'The value (', rep_size, ') will be taken as ', round(abs(rep_size)), '.\n', sep='')
    return (rep_size <- round(abs(rep_size)))
  
  # for different numbers per stratum replicate
  } else if (is.data.frame(rep_size)) {
    experiment_vars <- intersect(names(dat), explanatory_vars)
    rep_size <- repsize_cleanup(rep_size, dat, explanatory_vars, experiment_vars)
    return(rep_size)
  
  # no workable information -> set default
  } else {
    cat('\nYou have not provided a valid value for the size of stratum replicates.\n',
        'A default value of rep_size = ', default_rep_size, ' will be used.\n', sep='')
    return (default_rep_size)
  }
}

# --------------------------------------------------------------------

repsize_cleanup <- function(rep_size, dat, explanatory_vars, experiment_vars) {
  
  cat('\n\tEstablishing variable size of replicates...\n')
  
  # enforce sensible column naming for basic variables
  names(rep_size) <- gsub('s$', '', str_to_lower( names(rep_size) ))
  
  # check rep_size colnames include `size`
  if (!'size' %in% names(rep_size)) {
    stop(
      cat('\n\tYour input data for variable replicate sizes do NOT seem to have a `size` column.\n',
           '\t`analyze_spreadsheet` cannot make sense of the input without this column.\n',
           '\tVerify your dataset and, if necessary, rename the columns in the `rep_size` datasheet.\n', sep='')
      )
  }
  
  # check rep_size colnames include experimental variables
  confirmed_vars <- intersect(names(rep_size), explanatory_vars)
  # if none included
  if (length(confirmed_vars)==0){
    cat('\n\tYour replicate sizes do NOT seem to be organised according to the usual\n',
        '\texplanatory variables (genotype, treatment, dose, sex, and replicate).\n',
        '\t`analyze_spreadsheet` will continue taking the MODE of the `size` column.\n',
        '\tVerify your dataset and, if necessary, rename the columns in the `rep_size` datasheet.\n', sep='')
    return (mode(rep_size$size))
  }
  # if more variables in rep_size than in event data
  if (length(confirmed_vars)>length(experiment_vars)) {
    # check actual unique rows are the same
    if ( nrow( unique(expand_grid(dat[experiment_vars])) ) == nrow( unique(expand_grid(rep_size[confirmed_vars])) ) &
      all( unique(expand_grid(dat[confirmed_vars])) == unique(expand_grid(rep_size[confirmed_vars])) ) ) {
      # if so, assign as it is:
      return ( unique(expand_grid(rep_size[c(experiment_vars, 'size')])) )
    } else {
      stop(
        cat('\n\tYour replicate sizes seem to depend on MORE variables than the event data.\n',
            '\t`analyze_spreadsheet` cannot make sense of the input.\n',
            '\tVerify your dataset and, if necessary, rename the columns in the `rep_size` datasheet.\n', sep='')
      )
    }
  # if less variables in rep_size than in event data
  } else if (length(confirmed_vars)<length(experiment_vars)) {
    # assume the variables not included do not change the size in the strata they define
    cat('\n\tYour replicate sizes depend on less variables than the event data.\n',
        '\t`analyze_spreadsheet` will assume that replicate sizes are the same within the unspecified strata.\n',
        '\tVerify your dataset and, if necessary, rename the columns in the `rep_size` datasheet.\n', sep='')
    dropped_vars <- experiment_vars[!experiment_vars %in% confirmed_vars]
    interim_size <- unique(expand_grid(dat[experiment_vars]))
    # pivot wider until row numbers are the same
    for (var in dropped_vars) {
      interim_size <- pivot_wider(interim_size,
                                  names_from = {var},
                                  names_prefix = paste0(var, "-"),
                                  values_from = {var})
    }
    # add size data
    interim_size$size <- rep_size$size
    # now pivot longer to repeat size data for all variables not included in `rep_size`
    for (var in dropped_vars) {
      interim_size <- pivot_longer(interim_size,
                                   cols = starts_with(var),
                                   names_to = paste0('dummy-', var),
                                   values_to = {var})
    }
    rep_size <- select(interim_size, !starts_with('dummy'))
    rep_size <- relocate(rep_size, size, .after = last_col())
  }
  return (rep_size)
}

# --------------------------------------------------------------------

set_rep_size <- function(dat, metadata, rep_size, explanatory_vars) {
  
  cat('\n---\n', 'Establishing replicates size...\n', sep='')
  
  # `rep_size`: number of individuals per replicate per stratum
  default_rep_size <- 20
  
  # if not argument given, extract it from metadata or make assumption:
  if ( missing(rep_size) ) {
    if ( !is.null(metadata) && !is.na(metadata) ) {
      rep_size <- as.numeric(metadata[metadata$Category=='Replicate_size','Value'][[1]])
      cat("Replicate size information found in the metadata.\n")
    } else {
      cat("No replicate size argument given and no information found in the metadata,",
          'or metadata is not provided.\n',
          'A default value of rep_size = ', default_rep_size, ' will be used.\n', sep='')
      return (default_rep_size)
    }
  }
  
  # now check what we have and enforce it being workable
  rep_size <- check_rep_size(dat, rep_size, default_rep_size, explanatory_vars)
  
  # now inform the user
  # if same number for all strata and replicates
  if (length(rep_size)==1 && !is.data.frame(rep_size)) {
    cat('\n\tReplicate size is determined as: \n\t', rep_size, '.\n', sep='')
    
  # if different numbers for strata/replicates
  } else if (is.data.frame(rep_size)) {
    cat('\n\tReplicate size is variable across samples, ranging from ',
        min(rep_size$size), ' to ', max(rep_size$size), '.\n', sep='')
  }

  return (rep_size)
}

# --------------------------------------------------------------------

check_rec_style <- function(dat, rep_size, strata_vars, rec_style) {
  # this is a compatibility test - not a guarantee!

  # check, for all strata, which replicates are compatible with 'cumulative' recording
  # (i.e. show constant or ever-increasing event records)
  check <- dat %>%
    reframe(diff=diff(events), .by=all_of(strata_vars)) %>%
    reframe(cumulative_xdiff=all(diff>=0),  .by=all_of(strata_vars))
  cumulative_compatible <- data.frame(diff = check$cumulative_xdiff)
  # check, for all strata, that total events are compatible with 'new' recording
  # (i.e. sum of all events "expressed" is not larger than rep_size)
  if (is.whole(rep_size)) {
    check <- dat %>%
      summarise(sum=sum(events), .by=all_of(strata_vars)) %>%
      summarise(cumulative_xsum=all(sum>rep_size), .by=all_of(strata_vars))
    cumulative_compatible$sum <- check$cumulative_xsum
  } else if (is.data.frame(rep_size)) {
    check <- dat %>%
      summarise(sum=sum(events), .by=all_of(strata_vars)) %>%
      summarise(cumulative_xsum=all(sum>rep_size$size), .by=all_of(strata_vars))
    cumulative_compatible$sum <- check$cumulative_xsum
  }

  # if all strata replicates have records that are always increasing or
  # that add up to more than rep_size, it could be that recording is cumulative
  cumulative_compatible$cumulative <- (cumulative_compatible$diff | cumulative_compatible$sum)
  cumul_rate <- sum(cumulative_compatible$cumulative) / nrow(cumulative_compatible)
  
  # if all strata/replicates show cumulative behaviour (in pattern or sum)
  if ( cumul_rate==1 ) {
    cat('\n`analyze_spreadsheet` finds strong evidence of cumulative ',
        'recording in every stratum replicate.\n',
        'It will be assumed that "cumulative" events were recorded ',
        'at each observation.\n', sep='')
    return ('cumulative')
    
  # if more than half the strata/replicates show cumulative behaviour
  } else if (cumul_rate > 0.5) {
    cat('\n`analyze_spreadsheet` finds evidence of cumulative ',
        'recording in a majority (~', round(cumul_rate*100), '%) of strata replicates.\n',
        'It will be assumed that "cumulative" events were recorded at each observation',
        'and `analyze_spreadsheet` will attempt to repair the data.\n', sep='')
    # repair "lost corpses"
    dat <<- dat %>%
      # find the diff(events) with 0 as first element using lag, by strata
      mutate(dif = events - lag(events, default=first(events)), .by=all_of(strata_vars)) %>%
      # keep only the negative values (corpses disappear)
      mutate(steps=ifelse(dif<0, abs(dif), 0)) %>%
      # get the cumsum of the absolute value and add to the events, by strata
      mutate(cumdif = cumsum(abs(steps)), .by=all_of(strata_vars)) %>%
      mutate(events = events+cumdif) %>%
      # clean up
      select(!c(dif, steps, cumdif))
    return ('cumulative')
  
  # if less than half the strata/replicates show cumulative behaviour:
  } else {
    cat('\n`analyze_spreadsheet` finds little evidence of cumulative ',
        'recording: only in ~', round(cumul_rate*100), '% of strata replicates.\n',
        'It will be assumed that only "new" events were recorded ',
        'at each observation.\n', sep='')
    return ('new')
  }
}

# --------------------------------------------------------------------

set_recording_style <- function(dat, metadata, rep_size, strata_vars, rec_style) {
  
  # `rec_style`: whether the events have been recorded cumulatively or 'instantaneously'
  cat('\n---\n', 'Checking data recording mode (new/cumulative)...\n', sep='')

  # if no argument given:
  if (missing(rec_style)) {
    cat('\nYou have not provided information about whether events were recorded cumulatively.\n',
        '`analyse_spreadsheet` will attempt to deduce this from the data/metadata.\n\n', sep='')
    
    # getting it from metadata
    if ( !is.null(metadata) && !is.na(metadata) ) {
      rec_style <- metadata[metadata$Category=='Recording_style','Value'][[1]]
      # if not valid, deduce `rec_style` from the data
      if(!rec_style %in% c('cumulative', 'new')) {
        cat('\nThe metadata provided no valid information about whether events were recorded cumulatively.\n',
            '`analyse_spreadsheet` will attempt to deduce this from the data.\n\n', sep='')
        rec_style <- check_rec_style(dat, rep_size, strata_vars, rec_style)
      }
    
    # if metadata does not exist, deduce `rec_style` from the data
    } else {
      cat('\nThere is no input information about whether events were recorded cumulatively.\n',
          '`analyse_spreadsheet` will attempt to deduce this from the data.\n\n', sep='')
      rec_style <- check_rec_style(dat, rep_size, strata_vars, rec_style)
    }

  # if argument given:
  } else {
    
    # `rec_style` is declared as 'new'
    if (rec_style == 'new') {
      cat('\nYou have specified that events were NOT recorded cumulatively.\n')
      if (rec_style == check_rec_style(dat, rep_size, strata_vars, rec_style)) {}
      else {
        cat('\nHowever, the data seem to have been recorded CUMULATIVELY.\n',
            '`analyse_spreadsheet` will go ahead assuming this was an error on your part,\n',
            'and analyse the data as if they were recorded cumulatively.\n',
            '---> PLEASE CHECK YOUR DATA <---', sep='')
        rec_style <- check_rec_style(dat, rep_size, strata_vars, rec_style)
      }
    
    # `rec_style` is declared as 'cumulative'
    } else if ( grepl('cum', rec_style) ) {
      cat('\nYou have specified that events were recorded CUMULATIVELY.\n')
      if (rec_style==check_rec_style(dat, rep_size, strata_vars)) {}
      else {
        cat('\nHowever, the data seem to have been recorded NON-cumulatively.\n',
            '`analyse_spreadsheet` will go ahead assuming this was an error on your part,\n',
            'and analyse the data as if they were recorded non-cumulatively.\n',
            '---> PLEASE CHECK YOUR DATA <---', sep='')
        rec_style <- check_rec_style(dat, rep_size, strata_vars, rec_style)
      }
    
    # neither 'new' nor 'cumulative'
    } else {
      cat('\nYou have provided an invalid option for how the events were recorded.\n',
          '`analyse_spreadsheet` will attempt to deduce this from the data.\n', sep='')
      rec_style <- check_rec_style(dat, rep_size, strata_vars, rec_style)
    }
  }
  cat('\n\tRecording mode established as "',
      ifelse(is.data.frame(rec_style), 'a `data.frame`', rec_style),
      '".\n', sep='')
  
  return (rec_style)
}

# --------------------------------------------------------------------

data_cleanup <- function(dat, explanatory_vars, all_vars) {
  cat('\n---\n', 'Harmonising data-variable names and cleaning up data...\n', sep='')
  ### column name cleanup
  # identify columns with date data
  date_match <- grep("date|^day", names(dat))
  if (length(date_match)==0) {
    stop(
      cat('\nYou do not seem to have any column specifying the date of the observations, ',
          'or it is NOT named in any of the usual ways ("date", "day", and derivatives).\n',
          '`analyze_spreadsheet` cannot continue without these data.\n',
          'Verify your dataset and, if necessary, rename the columns.\n', sep='')
    )
  } else if (length(date_match)>1) {
    stop(
      cat('\nYou seem to have more than one column specifying dates, ',
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
    stop(
      cat('\nYou do not seem to have any column specifying the time of the day of the observations, ',
          'or it is NOT named in any of the usual ways ("time", "hour", "hh:mm", and derivatives).\n',
          '`analyze_spreadsheet` cannot continue without these data.\n',
          'Verify your dataset and, if necessary, rename the columns.\n', sep='')
    )
  } else if (length(time_match)>1) {
    stop(
      cat('\nYou seem to have more than one column specifying time of the day, ',
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
      cat('\nYou do not seem to have any column with event data, ',
          'or it is NOT named in any of the usual ways ("deaths", "dead", "events", and derivatives)).\n',
          '`analyze_spreadsheet` cannot continue without these data.\n',
          'Verify your dataset and, if necessary, rename the columns.\n', sep='')
    )
  } else if (length(event_match)>1) {
    stop(
      cat('\nYou seem to have more than one column with event data, ',
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
    cat('\nYou do not seem to have any column with censoring data, ',
        'or it is NOT named in any of the usual ways ("censored", "censoring", and derivatives).\n',
        '`analyze_spreadsheet` will continue assuming that all individuals were censored ',
        'after the last observational timepoint.\n',
        'If this is not what happened, verify your dataset and, if necessary, rename the columns.\n', sep='')
  } else if (length(censoring_match)>1) {
    stop(
      cat('\nYou seem to have more than one column with censoring data, ',
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
    cat('\nYou do not seem to have any column with specifying the units used to describe the dose of a treatment.\n',
        '`analyze_spreadsheet` will continue assuming that this is not relevant.\n',
        'If this is not the case, verify your dataset and, if necessary, rename the columns.\n', sep='')
  } else if (length(unit_match)>1) {
    cat('\nYou seem to have more than one column expressing units (presumably of a treatment dose).\n',
        '`analyze_spreadsheet` will continue assuming that this is not relevant, and drop these data.\n',
        'If this is not the case, verify your dataset and, if necessary, rename the columns.\n', sep='')
    dat <- select(dat, -names(dat)[unit_match])
  } else if (length(unit_match)==1) {
    names(dat)[unit_match] <- 'dose_unit'
  }
  
  # identify column with dose data
  dose_match <- names(dat)[setdiff(grep("dose", names(dat)), grep("_unit", names(dat)))]
  if (length(dose_match)==0) {
    cat('\nYou do not seem to have any column with specifying the dose of a treatment.\n',
        '`analyze_spreadsheet` will continue assuming that this is not relevant.\n',
        'If this is not the case, verify your dataset and, if necessary, rename the columns.\n', sep='')
  } else if (length(dose_match)>1) {
    cat('\nYou seem to have more than one column expressing treatment dose.\n',
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
      cat('\nYou do not seem to have any of the usual variables ',
          '(genotype, treatment(s), dose, sex, replicate).\n',
          '`analyze_spreadsheet` cannot continue without at least some of these data.\n',
          'Verify your dataset and, if necessary, rename the columns.\n', sep='')
      )
  } else if (length(confirmed_vars)==5) {
    cat('\nThe usual explanatory variables are: ',
        '"genotype", "treatment(s)", "dose", "sex" and "replicate".\n',
        'You seem to have all ', length(confirmed_vars), ' of them.\n', sep='')
  } else {
    cat('\nThe usual explanatory variables are: ',
        '"genotype", "treatment(s)", "dose", "sex" and "replicate".\n',
        'You seem to have ', length(confirmed_vars), ' of them: ',
        paste(confirmed_vars, collapse=', '), '.\n',
        '`analyze_spreadsheet` will continue using these explanatory variables only.\n',
        'If this is not correct, verify your dataset and rename columns as required.\n', sep='')
  }

  # remove un-interpretable columns
  extra_vars <- setdiff(names(dat), all_vars)
  if (length(extra_vars)>0){
    cat('\nYou have the additional variable(s):',
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

find_time_intervals <- function(dat, explanatory_vars, rec_style, time_unit) {
  cat('\n---\n', 'Establishing time-to-event intervals...\n', sep='')
  
  # standardise time_unit name (case, plural, valid options)
  time_unit <- str_to_lower(time_unit)
  if (!time_unit=='auto' & !endsWith(time_unit, 's')) time_unit <- paste0(time_unit, 's')
  time_unit_list <- c("seconds", "secs", "minutes", "mins",
                      "auto", "hours", "days", "weeks") # options for `difftime`
  if (!time_unit %in% time_unit_list) {
    cat('\nYou have not specified a valid time unit for expressing time-to-event intervals.\n',
        '`analyze_spreadsheet` will pick a time unit automatically.\n', sep='')
    time_unit <- 'auto'
  }
  if (time_unit=='seconds') time_unit <- 'secs'
  if (time_unit=='minutes') time_unit <- 'mins'
  
  # standardise time format and obtain interval
  format_in <- "%d.%m.%Y"
  dat %<>% 
    mutate( date = as.Date(date, format=format_in)) %>%
    mutate( time2 = as.POSIXct(paste( date, time ), format="%Y-%m-%d %H:%M:%S")) %>%
    mutate( time_to_event = signif(difftime( time2, time2[1], units = time_unit ), 3))
  
  # turn cumulative events into new ones-only
  cat('\nEstablishing individual events (non-cumulative)...\n')
  included_vars <- intersect(names(dat), explanatory_vars)
  # this part is very slow -- try to use vectorized functions
  if  (rec_style == 'cumulative') {
    var_combos <- unique(expand_grid(dat[included_vars]))
    for (row in 1:nrow(var_combos)) {
      combo <- var_combos[row, ]
      combo_rows <- apply(dat[included_vars],1,function(x) {all(x==combo)})
      events_rec_style <- dat[combo_rows,'events']
      new_events <- c(0, diff(events_rec_style$events))
      dat[combo_rows,]$events <- new_events
    }
  }
  return (dat)
}

# --------------------------------------------------------------------

rowtime_to_rowevent <- function(dat, explanatory_vars) {
  cat('\n---\n', 'Converting data from time-based to event-based...\n', sep='')
  
  # store strata for later
  experiment_vars <- intersect(names(dat), explanatory_vars)
  original_grid <- unique(expand_grid(dat[experiment_vars]))
  
  # remove rows without data (events/censored separately)
  events_dat <- dat[ dat$events > 0, !names(dat) %in% c('censored')]
  if ( any(grepl('censor', names(dat))) ) {
    censor_dat <- dat[ dat$censored>0, !names(dat) %in% c('events')]
  } else {
    censor_dat <- filter(dat, events<0) %>% # empty
      rename(censored=events)               # change events for censored
  }
  
  # replicate rows per their number of events, then turn into event=1
  events_dat <- events_dat[rep(1:nrow(events_dat), events_dat$events), ]
  # same with explicit censorings (if there are any!) and turn them into events
  if (nrow(censor_dat)>0) {
    censor_dat <- censor_dat[rep(1:nrow(censor_dat), censor_dat$censored), ]
    names(censor_dat)[names(censor_dat)=='censored'] <- 'events'
  }
  # new col with _type_ (not number) of event
  events_dat$event_type <- 1 # code for event=death
  censor_dat$event_type <- 0 # code for event=censoring
  
  # combine deaths and censorings, remove unwanted cols
  dat <- rbind(events_dat, censor_dat) # using 'events' as binder
  keeper_cols <- c(experiment_vars, "time_to_event", "event_type")
  dat <- select(dat, all_of( intersect(keeper_cols, names(dat)) ))
  
  # make sure strata with no events recorded are not lost
  # compare strata/grids:
  interim_grid <- unique(expand_grid(dat[experiment_vars]))
  missing_strata <- anti_join(original_grid, interim_grid,
                              by = join_by( !!!experiment_vars ))
  # restore strata without any event/censoring recorded
  for (r in 1:nrow(missing_strata)) {
    newrow <- missing_strata[r,]
    if ('dose_unit' %in% experiment_vars) {
      newrow$dose_unit <- unique(dat$dose_unit)[[1]] # just in case - to be generalised
    }
    newrow$event_type <- 0
    newrow$time_to_event <- max(dat$time_to_event)
    dat <- rbind(dat, newrow)
  }
  
  return (dat)
}

# --------------------------------------------------------------------

implicit_censoring <- function(dat, explanatory_vars, rep_size) {
  
  cat('\n---\n', 'Adding implicit (endpoint) or missed censoring events...\n', sep='')
  
  maxtime <- max(dat$time_to_event) # endpoint time for implicit censoring
  
  # find out total numbers of 'accounted for' individuals
  experiment_vars <- intersect(names(dat), explanatory_vars)
  surv_dat <- aggregate(event_type ~ ., # period indicates ALL variables
                        dat[,c(experiment_vars, 'event_type')],
                        sum)
  
  # calculate remainder of individuals in each replicate per stratum
  # if not all conditions have the same size
  if (is.data.frame(rep_size)) {
    try( if (nrow(rep_size)!=nrow(surv_dat)) {
      stop(
        cat('\nThe table reporting replicate sizes in the excel file ',
            'does not have the appropriate number of rows.\n', sep='')
    )})
    # find common colnames for surv_dat and rep_size
    rep_cols <- intersect(names(surv_dat), names(rep_size))
    # find the order of rep_size rows to match the variables order of surv_dat
    keys <- plyr::join.keys(surv_dat, rep_size, rep_cols)
    matches <- match(keys$y, keys$x, nomatch=(keys$n+1))
    # use the new order to get numbers of survivors per vial at termination
    surv_dat$censored <- rep_size[order(matches),]$size - surv_dat$event
  
  # calculate remainder of individuals in each replicate per stratum
  # if all conditions do have the same size:
  } else if (is.numeric(rep_size)) {
    surv_dat$censored <- rep_size - surv_dat$event
  }
  
  # create data rows per missed or endpoint censoring
  for (r in 1:nrow(surv_dat)) {
    # complete list of columns <--- this would need to be more generalised
    newrow <- surv_dat[r,experiment_vars]
    if ('dose_unit' %in% experiment_vars) {
      newrow$dose_unit <- unique(dat$dose_unit)[[1]] # just in case - to be generalised
    }
    newrow$event_type <- 0
    newrow$time_to_event <- maxtime
    newrow <- newrow[rep(1,surv_dat$censored[r]),]
    dat <- rbind(dat, newrow)
  }
  return (dat)
}

# --------------------------------------------------------------------

basic_PH_test <- function(dat, cph, explanatory_vars) {
  if (missing(cph)) cph <- FALSE
  if (cph) {
    cat('\n---\n', "Quick'n'dirty evaluation of Proportional Hazards in the data...\n", sep='')
    experiment_vars <- intersect(names(dat), explanatory_vars)
    experiment_vars <- experiment_vars[!experiment_vars=='replicate']
    fm <- formula(paste0('Surv(time_to_event, event_type) ~ ',
                         paste(experiment_vars, collapse=' + ')))
    cph_model <- coxph(fm, data=dat)
    zphfit <- cox.zph(cph_model)
    if (zphfit$table[,3]['GLOBAL']>0.05){
      cat('\n\tSchoenfeld test shows PH assumption is respected\n')
      cat('\n\t(null hypothesis=it is not)):\n')
      ggcoxzph(zphfit)
      cat('\n\tP-value: ', cox.zph(cph_model)$table[,3], '\n')
      cat(paste("\n\tlog-rank test p-value:", summary(cph_model)$logtest[3], "\n"))
      cat('\n\tThe variables with significant effect are:\n')
      print(cph_model)
    } else {
      cat('\n\tSchoenfeld test shows PH assumption is NOT respected:\n\n')
      print(cox.zph(cph_model)$table[,3]['GLOBAL'])
    }
  } else {
    cat('\n---\n', "Evaluation of Proportional Hazards has not been requested.\n", sep='')
  }
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

analyse_spreadsheet <- function(
  x, dsheet, msheet, rep_size, rec_style, time_unit='hour', cph) {

  # LOAD THE DATA
  # determine whether source is an excel file or an existing df
  sour_ce <- check_source(x)
  # read the event data from the appropriate sheet
  dat <- load_data(sour_ce, x, dsheet)
  # read the metadata if available
  metadata <- load_metadata(sour_ce, x, msheet)
  
  # RECONCILE ARGUMENTS, METADATA, DATA COLUMN NAMES
  # determine the size of the strata replicates &
  # harmonise column names and very basic data cleanup (NAs...)
  explanatory_vars <- c('genotype', 'treatment_1', 'treatment_2', 'sex', 'replicate', 'dose')
  all_vars <- c('genotype', 'treatment_1', 'treatment_2', 'sex', 'replicate', 'dose',
                'events', 'censored', 'dose_unit',
                'date', 'time')
  rep_size  <- set_rep_size(dat, metadata, rep_size, explanatory_vars)
  dat <- data_cleanup(dat, explanatory_vars, all_vars)
  # determine whether events were recorded new or cumulative
  strata_vars <- intersect(names(dat), explanatory_vars)
  rec_style <- set_recording_style(dat, metadata, rep_size, strata_vars, rec_style)
  
  # TRANSFORM DATA TO TIME-TO-EVENT FORMAT
  # express times as intervals since start of experiment
  dat <- find_time_intervals(dat, explanatory_vars, rec_style, time_unit)
  # move from rows-as-time intervals to rows-as-events
  dat <- rowtime_to_rowevent(dat, explanatory_vars)
  # include implicit censoring
  dat <- implicit_censoring(dat, explanatory_vars, rep_size)

  # BASIC SURVIVAL MODELLING
  basic_PH_test(dat, cph, explanatory_vars)
  
  return( dat )
}