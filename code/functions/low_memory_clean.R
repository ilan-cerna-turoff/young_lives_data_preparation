# File: low_memory_clean.R
# Date: 05/05/2022
# Author: Lawrence Chillrud 

#-------------------------#
#### TABLE OF CONTENTS ####
#-------------------------#
# N. Notes
# F. Function
# 0. Error handling
# 1. Set up data
# 2. Read & clean data
# 3. Data dictionary set up
# 4. Variable selection 
# 5. Merge all data

#--------------------------#
####      N. NOTES      ####
#--------------------------#
# This script contains the function low_memory_clean() for merging and cleaning young_lives_data_preparation raw data files when R's memory is superseded.
# low_memory_clean() does the following: 
#     (1) applies previously coded labels to variables automatically; 
#     (2) handles all missing values that we want reported as NA;
#     (3) allows for variable selection; 
#     (4) completes necessary full_joins of all .dta files; 
#     (5) curates useful metadata.
# 
# 
# INPUTS: 
#     survey: can be one of "com_main", "cvd_och", "cvd_ych"
#     round: can be one of "r1", "r2", ..., "r5"
#     country: can be one of "et", "in", "pe", "vt"
#     vars: character vector of variable names to extract
#     linkage_vars: a character vector of variable names to join by
#     missing_keys: (optional) a character vector of values that should be treated as missing whenever encountered in the data. by default = the object 'default_missing_keys'
#     case_sensitive: (optional) logical indicating if variables names are case sensitive. by default = FALSE.
#     compression: (optional) a character indicating the process to follow when compressing data. must be one of "none", "chop", or "nest". by default = "none".
# 
# OUTPUTS: 
#   (1) data_list: A list of data.frames comprising the data requested.
#   (2) data_dictionary: A tibble containing useful metadata on all variables in df.
#   (3) discarded_data_dictionary: A tibble containing useful metadata on all variables that were discarded 
#       (not selected because they were not passed via the "vars" argument) during cleaning.
#   (4) vars_not_found: A character vector containing the variables that were not found at all during cleaning.
#
# WARNINGS: package requirements: haven, here, tidyverse must all be installed for low_memory_clean() to run.
#           Additionally, low_memory_clean() expects that data files (both raw & intermediate) are named according 
#           to the conventions laid out in the various markdown files.

#--------------------------#
####     F. FUNCTION    ####
#--------------------------#
low_memory_clean <- function(survey, round, country, vars, linkage_vars, missing_keys = default_missing_keys, case_sensitive = FALSE, compression = "none") {

#--------------------------#
####  0. ERROR HANDLING ####
#--------------------------#
  # 0a. Standard error handling (ensuring inputs are correct)
  if (!(survey %in% c("com_main", "cvd_och", "cvd_ych"))) stop('Invalid "survey" passed. Must be one of: {"com_main", "cvd_och", "cvd_ych"}')
  if (!(round %in% paste0("r", 1:5))) stop('Invalid "round" passed. Must be one of: {"r1", "r2", ..., "r5"}')
  if (!(country %in% c("et", "in", "pe", "vt"))) stop('Invalid "country" passed. Must be one of: {"et", "in", "pe", "vt"}')
  if (!is.logical(case_sensitive)) stop('Invalid "case_sensitive" passed. Must be a logical (TRUE or FALSE).')
  if (!is.character(vars)) stop('Invalid "vars" passed. Must be a character vector.')
  if (!is.character(linkage_vars)) stop('Invalid "linkage_vars" passed. Must be a character vector.')
  if (!(compression %in% c("none", "chop", "nest"))) stop('Invalid "compression" passed. Must be one of: {"none", "chop", "nest"}')

  # 0b. Ensure the linkage_vars are also present in vars:
  if (is.character(linkage_vars)) vars <- unique(c(linkage_vars, vars))
  
  # 0c. Message letting us know we have begun cleaning:
  starting_message <- c(paste0(rep('#', 20), collapse = ""), 
                        paste0(" CLEANING: ", survey, ", ", round, ", ", country, " "),
                        paste0(rep('#', 20), collapse = ""))
  message(starting_message)
    
#--------------------------#
####   1. SET UP DATA   ####
#--------------------------#
  # 1a. Setting up data dirs
  raw_data_dir <- here("data", "raw", survey)
  
  # 1b. Collecting filepaths and filenames we care about:
  raw_data_filepaths <- list.files(raw_data_dir, pattern = paste(round, country, "*", sep = "_"), full.names = T)
  if (length(raw_data_filepaths) == 0) stop(paste0("No files were found for the input [survey, round, country] combination. Please double check the existence of files in the format: data/raw/", survey, "/", round, "_", country, "_*.dta"))
  
  raw_data_filenames <- raw_data_filepaths %>% 
    str_replace(here(raw_data_dir, paste(round, paste0(country, "_"), sep = "_")), "") %>%
    str_replace(".dta", "")
  
  # 1c. Standardising case of vars input if we are NOT being case-sensitive:
  if (!case_sensitive) vars <- tolower(vars)
  if (!case_sensitive) linkage_vars <- tolower(linkage_vars)
  
#----------------------------#
#### 2. READ & CLEAN DATA ####
#----------------------------#
  # 2a. Reading in all the data for (survey, round, country) combination specified.
  # Note that the first call to "mutate" here does *most* variable coding automatically.
  # Second call to "mutate" is to make dates be in the appropriate format when hunting for missing values a few lines below.
  # Third & fourth mutates convert the entire dataframe to lowercase.
  # If we are NOT being case sensitive, we then standardise the case of all variable names.
  # Next, we encode any missing value as NA.
  data <- map(.x = raw_data_filepaths, .f = 
    function(x) {
      df <- read_dta(x) %>% 
        mutate(across(where(is.labelled), as_factor)) %>%
        mutate(across(where(~ lubridate::is.Date(.x) || lubridate::is.POSIXct(.x) || lubridate::is.POSIXt(.x) || lubridate::is.POSIXlt(.x)), as.character)) %>%
        mutate(across(where(is.factor), ~as_factor(tolower(.)))) %>%
        mutate(across(where(is.character), tolower))
      if (!case_sensitive) colnames(df) <- tolower(colnames(df))
      factor_cols <- sapply(df, is.factor)
      df[, factor_cols] <- df[, factor_cols] %>% mutate(across(everything(), as.character))
      for (i in 1:length(missing_keys)) df[df == missing_keys[i]] <- NA
      df[, factor_cols] <- df[, factor_cols] %>% mutate(across(everything(), as.factor))
      df
    }
  )

  # 2b. Set the names of each dataset:
  names(data) <- raw_data_filenames

#--------------------------------#
#### 3. DATA DICTIONARY SET UP####
#--------------------------------#
  # 3a. Small helper function to create a data dictionary from a dataframe
  get_dict <- function(cdf, cdf_name) { 
    missing_stats <- map_dfc(cdf, ~sum(is.na(.x))) %>% 
      pivot_longer(everything(), names_to = "var_name", values_to = "total_missing") %>%
      mutate(perc_missing = total_missing / nrow(cdf) * 100, df_name = cdf_name)

    definitions <- tolower(as.character(sapply(cdf, attr, which = "label")))
    data_types <- as.character(sapply(cdf, class))
    dd <- data.frame(df_name = cdf_name, var_name = colnames(cdf), var_definition = definitions, var_type = data_types)
    left_join(dd, missing_stats, by = c("df_name", "var_name"))
  }

  # 3b. Create a data dictionary for the entire survey, round, country:
  all_data_dict <- map2_dfr(.x = data, .y = names(data), ~get_dict(cdf = .x, cdf_name = .y)) %>%
    mutate(var_needs_manual_check = ifelse((var_type == "factor" | str_detect(var_definition, "id") | str_detect(var_definition, "number") | str_detect(var_definition, "index") | str_detect(var_definition, "age") | str_detect(var_definition, "size")), F, T)) %>%
    as_tibble()

#--------------------------#
####  4. VAR SELECTION  ####
#--------------------------#
  # 4a. select all the variables we care about (if they are present in the data).
  # We also make a call to "distinct" here to ensure extraneous info is not kept when truncating some dfs.
  # There is also a check to help flag when there was actually nothing of interest from a df. 
  # We remove these non-dfs in step 4b below. 
  data <- data %>% 
    map(function(df) {
      df <- df %>% select(any_of(vars)) %>% distinct()
      if (ncol(df) == 0 || (all(colnames(df) %in% linkage_vars))) {
        return(NULL)
      } else {
        return(df)
      }
    })
  
  # 4b. Remove any dataframes from the data list that came back as NULL (ie. because they had no variables we were asking for):
  null_dfs <- which(sapply(data, is.null))
  if (length(null_dfs) > 0) data <- data[-null_dfs]
  if (length(data) == 0) stop('None of the variables in "vars" could be found in the data/raw/[survey]/[round]_[country]_*.dta files you pointed to.')
  
  # 4c. Create information table on variables that may be possible linkage variables for that round.
  possible_linkage_vars <- data.frame(table(reduce(map(data, colnames), .f=c))) %>% filter(Freq > 1) %>%
    mutate(total_files = length(data), perc = paste0(round(Freq / total_files * 100), "%"))
  colnames(possible_linkage_vars) <- c("var_name", "files_appears_in", "total_files", "perc_appears_in")

  # 4d. Rename ambiguous variables if linkage_vars is not NULL to prevent conflicts during the join. Print a warning letting the user know.
  vars_to_rename <- data.frame(var_name = setdiff(possible_linkage_vars$var_name, linkage_vars)) %>%
    left_join(., all_data_dict, by = "var_name") %>%
    select(var_name, df_name) %>% 
    mutate(new_name = paste(var_name, df_name, sep = "_"))
  
  if (nrow(vars_to_rename) > 0) {
    rename_mssg <- c("The following variables are being renamed to prevent conflicts later down the line during a potential join:\n")
    for (i in 1:nrow(vars_to_rename)) {
      rename_mssg <- c(rename_mssg, paste0("\t", vars_to_rename$var_name[i], " (from the file ", vars_to_rename$df_name[i], ") --> ", vars_to_rename$new_name[i], "\n"))
      data[[vars_to_rename$df_name[i]]] <- data[[vars_to_rename$df_name[i]]] %>% rename_with(~ vars_to_rename$new_name[i], all_of(vars_to_rename$var_name[i]))
    }
    warning(rename_mssg)
  }
  
  # 4e. Provide information on variables not found:
  vars_found <- unique(unlist(map(data, colnames)))
  vars_not_found <- setdiff(setdiff(vars, vars_found), unique(vars_to_rename$var_name))
  options(useFancyQuotes = FALSE) # for the below warning message:
  if (length(vars_not_found) > 0) {
    warning(call. = F, length(vars_not_found), ' variable(s) you asked for in "vars" were not found when cleaning the data. Subsequently, they will not be present in the ouput data_list. They were: ', paste(dQuote(vars_not_found), collapse = ", "))
  } else {
    vars_not_found <- NULL
  }
  
  # 4f. Create a data dictionary for the variables we do have:
  data_dict <- map2_dfr(.x = data, .y = names(data), ~get_dict(cdf = .x, cdf_name = .y)) %>%
    mutate(var_needs_manual_check = ifelse((var_type == "factor" | str_detect(var_definition, "id") | str_detect(var_definition, "number") | str_detect(var_definition, "index") | str_detect(var_definition, "age") | str_detect(var_definition, "size")), F, T)) %>%
    as_tibble()

  # 4g. Create a data dictionary for the variables we DO NOT have:
  discard_dict <- all_data_dict %>% 
    filter(var_name %in% setdiff(setdiff(unique(all_data_dict$var_name), unique(data_dict$var_name)), unique(vars_to_rename$var_name))) %>% 
    as_tibble()

  # 4h. Specifies different options for the structure of merged data:
  if (compression != "none") {
    if (compression == "chop") {
      data <- map(data, ~chop(.x, -all_of(linkage_vars)))
    } else {
      data <- map2(data, names(data), function(df, df_name) {
        df <- df %>% group_by(all_of(linkage_vars)) %>% nest()
        colnames(df) <- c(linkage_vars, df_name)
        df
      })
    }
    data <- reduce(data, full_join)
  }

  # Final object to return:
  list(data = data, data_dictionary = data_dict, discarded_data_dictionary = discard_dict, vars_not_found = vars_not_found)
    
}

#-------------------------#
####  5. MISSING KEYS  ####
#-------------------------#
  # 5. List of responses within variables that should be set to NA.
  # No values, such as "99", which could be real values for some question in the survey 
  # should be included in the initial automation
  default_missing_keys <- c("missing", "nk", "na", "n/a", "not applicable",
                            "don't know", "not mentioned", "not known", 
                            "dk","refused to answer", "you don't know",
                            "nqc", "ns", "rta","unknown", 
                            "not known _____________________","", 
                            "-9974", "-9969","-888", "-99", "-97", "-88", 
                            "-79", "-77", "-66", "-60", "-45", "-44", "-40", 
                            "-30", "-20","-15", "-9", "-8.80000019073486",
                            "-8", "-7", "-5", "-3", "-2")