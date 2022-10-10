# File: clean.R
# Date: 09/04/2022
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
# 6. Missing keys

#--------------------------#
####      N. NOTES      ####
#--------------------------#
# This script contains the function clean() for cleaning the young_lives_data_preparation raw data files automatically, and all at once:
# clean() does the following: 
#     (1) applies previously coded labels to variables automatically; 
#     (2) handles all missing values that we want reported as NA;
#     (3) allows for variable selection; 
#     (4) completes necessary full_joins of all .dta files; 
#     (5) curates useful metadata.
# 
# INPUTS: 
#     survey: can be one of "com_main", "cvd_och", "cvd_ych"
#     round: can be one of "r1", "r2", "r3", "r4", "r5"
#     country: can be one of "et", "in", "pe", "vt"
#     missing_keys: (optional) a character vector of values that should be treated as missing whenever encountered in the data. by default = the object 'default_missing_keys'
#     case_sensitive: (optional) logical indicating if variables names are case sensitive. by default = FALSE.
#     vars: (optional) character vector of variable names to extract. by default = NULL.
#     linkage_vars: (optional) either (1) a character vector of variable names to join by; or (2) NULL (default) meaning "join" should decide how the join should be performed (for backwards compatibility).
# 
# OUTPUTS: 
#   if the vars passed to clean() are character vectors: 
#     (1) df: A tibble comprising the final clean dataframe.
#     (2) data_dictionary: A tibble containing useful metadata on all variables in df.
#     (3) discarded_data_dictionary: A tibble containing useful metadata on all variables that were discarded.
#         (not selected because they were not passed via the "vars" argument) during cleaning.
#     (4) vars_not_found: A character vector containing the variables that were not found at all during cleaning.
#
#   if the vars passed to clean() is NULL:
#     (1) data_list: A list of all the raw data from the current specified survey, round, & country.
#     (2) data_dictionary: A tibble containing useful metadata on all variables from the current round of cleaning (specific to current survey & country).
#     (3) prev_vars_not_found: A tibble containing useful metadata on all variables that appear in previous 
#         rounds' intermediate (cleaned) data but were not found in the current round of cleaning. (Specific to current survey & country).
#     (4) possible_linkage_vars: A dataframe giving information on possible linkage variables that can be used.
#
# WARNINGS: package requirements: haven, here, tidyverse must all be installed for clean() to run.
#           additionally, clean() expects that data files (both raw & intermediate) are named according 
#           to the conventions laid out in the various markdown files.

#--------------------------#
####     F. FUNCTION    ####
#--------------------------#
clean <- function(survey, round, country, missing_keys = default_missing_keys, case_sensitive = FALSE, vars = NULL, linkage_vars = NULL) {
  
#--------------------------#
####  0. ERROR HANDLING ####
#--------------------------#
  # 0a. Standard error handling (ensuring inputs are correct)
  if (!(survey %in% c("com_main", "cvd_och", "cvd_ych"))) stop('Invalid "survey" passed. Must be one of: {"com_main", "cvd_och", "cvd_ych"}')
  if (!(round %in% paste0("r", 1:5))) stop('Invalid "round" passed. Must be one of: {"r1", "r2", ..., "r5"}')
  if (!(country %in% c("et", "in", "pe", "vt"))) stop('Invalid "country" passed. Must be one of: {"et", "in", "pe", "vt"}')
  if (!is.logical(case_sensitive)) stop('Invalid "case_sensitive" passed. Must be a logical (TRUE or FALSE).')
  if (!(is.character(vars) || is.null(vars))) stop('Invalid "vars" passed. Must either be a character vector or NULL.')
  if (!(is.character(linkage_vars) || is.null(linkage_vars))) stop('Invalid "linkage_vars" passed. Must either be a character vector or NULL.')
  if (is.character(linkage_vars) && is.null(vars)) stop('You cannot provide "linkage_vars" without providing "vars". Please either set "linkage_vars" to NULL or pass a character vector to "vars".')
  
  # 0b. Ensure the linkage_vars are also present in vars:
  if (is.character(linkage_vars)) vars <- unique(c(linkage_vars, vars))
  
  # 0c. Message letting us know we've begun cleaning:
  starting_message <- c(paste0(rep('#', 20), collapse = ""), 
                        paste0(" CLEANING: ", survey, ", ", round, ", ", country, " "),
                        paste0(rep('#', 20), collapse = ""))
  message(starting_message)
  
  # 0d. Warning about not using linkage_vars..!
  if (is.character(vars) && is.null(linkage_vars)) warning("NOTE: You have passed vars without specifiying linkage_vars for the join. This is STRONGLY DISCOURAGED. The function has run for the sake of backwards compatibility, but you should consider explicitly passing linkage_vars and re-running.")
  
#--------------------------#
####   1. SET UP DATA   ####
#--------------------------#
  # 1a. Setting up data directories
  raw_data_dir <- here("data", "raw", survey)
  int_data_dir <- here("data", "intermediate", survey)
  
  # 1b. Collecting filepaths and filenames:
  raw_data_filepaths <- list.files(raw_data_dir, pattern = paste(round, country, "*", sep = "_"), full.names = T)
  if (length(raw_data_filepaths) == 0) stop(paste0("No files were found for the input [survey, round, country] combination. Please double check the existence of files in the format: data/raw/", survey, "/", round, "_", country, "_*.dta"))
  
  raw_data_filenames <- raw_data_filepaths %>% 
    str_replace(here(raw_data_dir, paste(round, paste0(country, "_"), sep = "_")), "") %>%
    str_replace(".dta", "")
  
  # 1c. Standardizing case of vars input if we are NOT being case-sensitive:
  if (!case_sensitive && is.character(vars)) vars <- tolower(vars)
  if (!case_sensitive && is.character(linkage_vars)) linkage_vars <- tolower(linkage_vars)
  
#----------------------------#
#### 2. READ & CLEAN DATA ####
#----------------------------#
  # 2a. Reading in all the data for (survey, round, country) combination specified.
  # Note that the first call to "mutate" here does *most* variable coding automatically.
  # Second call to "mutate" is to make dates be in the appropriate format when hunting for missing values a few lines below.
  # Third & fourth mutates convert the entire dataframe to lowercase.
  # If we are NOT being case sensitive, we then standardise the case of all variable names.
  # Next, we encode missing values as NA.
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

#---------------------------------#
#### 3. DATA DICTIONARY SET UP ####
#---------------------------------#
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
  # 4a. If vars is not NULL, we then select all the variables we care about (if they are present in the data).
  # We also make a call to "distinct" here to ensure extraneous info is not kept when truncating some dfs.
  # There is also a check to help flag when there was actually nothing of interest from a df. 
  # We remove these non-dfs in step 4b below. 
  if (!is.null(vars)) {
    data <- data %>% 
      map(function(df) {
        df <- df %>% select(any_of(vars)) %>% distinct()
        if (ncol(df) == 0 || (is.character(linkage_vars) && (all(colnames(df) %in% linkage_vars)))) {
          return(NULL)
        } else {
          return(df)
        }
      })
  } 
  
  # 4b. Remove any dataframes from the data list that came back as NULL (ie. because they had no variables we were asking for):
  null_dfs <- which(sapply(data, is.null))
  if (length(null_dfs) > 0) data <- data[-null_dfs]
  if (length(data) == 0) stop('None of the variables in "vars" could be found in the data/raw/[survey]/[round]_[country]_*.dta files you pointed to.')
  
  # 4c. Create information table on variables that may be possible linkage variables for that round.
  possible_linkage_vars <- data.frame(table(reduce(map(data, colnames), .f=c))) %>% filter(Freq > 1) %>%
    mutate(total_files = length(data), perc = paste0(round(Freq / total_files * 100), "%"))
  colnames(possible_linkage_vars) <- c("var_name", "files_appears_in", "total_files", "perc_appears_in")
  
  # 4d. If the user does not know which vars they are after, report the possible linkage vars information from above step 4b (& return information in dataframe later)
  if (is.null(vars)) {
    linkage_mssg <- c("Possible linkage variables include:\n")
    for (i in 1:nrow(possible_linkage_vars)) linkage_mssg <- c(linkage_mssg, paste0("\t", possible_linkage_vars$var_name[i], ": appears in ", possible_linkage_vars$files_appears_in[i], " / ", possible_linkage_vars$total_files[i], "  files (", possible_linkage_vars$perc_appears_in[i], ")\n"))
    message(linkage_mssg)
  }
  
  # 4e. Rename ambiguous variables if linkage_vars is not NULL to prevent conflicts during the join. Print a warning letting the user know.
  if (is.character(linkage_vars)) {
    vars_to_rename <- data.frame(var_name = setdiff(possible_linkage_vars$var_name, linkage_vars)) %>%
      left_join(., all_data_dict, by = "var_name") %>%
      select(var_name, df_name) %>% 
      mutate(new_name = paste(var_name, df_name, sep = "_"))
    if (nrow(vars_to_rename) > 0) {
      rename_mssg <- c("The following variables are being renamed to prevent conflicts during the join:\n")
      for (i in 1:nrow(vars_to_rename)) {
        rename_mssg <- c(rename_mssg, paste0("\t", vars_to_rename$var_name[i], " (from the file ", vars_to_rename$df_name[i], ") --> ", vars_to_rename$new_name[i], "\n"))
        data[[vars_to_rename$df_name[i]]] <- data[[vars_to_rename$df_name[i]]] %>% rename_with(~ vars_to_rename$new_name[i], all_of(vars_to_rename$var_name[i]))
      }
      warning(rename_mssg)
    }
  } else {
    vars_to_rename <- data.frame(var_name = character())
  }
  
#--------------------------#
####  5. MERGE ALL DATA ####
#--------------------------#
  if (!is.null(vars)) {
    
#--------------------------#
#### 5i. VARS IS PASSED ####
#--------------------------#
    # 5i.a. We only want to merge all the data if vars is not NULL. 
    # This is because we assume that we are almost done with data set up. 
    final_df <- reduce(.x = data, .f = full_join)
    
    # 5i.b. Provide information on variables not found:
    vars_not_found <- setdiff(vars, colnames(final_df))
    if (is.character(linkage_vars)) vars_not_found <- setdiff(vars_not_found, unique(vars_to_rename$var_name))
    options(useFancyQuotes = FALSE) # for the below warning message:
    if (length(vars_not_found) > 0) {
      warning(call. = F, length(vars_not_found), ' variable(s) you asked for in "vars" were not found when cleaning the data. Subsequently, they will not be present in the ouput dataframe. They were: ', paste(dQuote(vars_not_found), collapse = ", "))
    } else {
      vars_not_found <- NULL
    }
    
    # 5i.c. Create a data dictionary for the variables we do have:
    data_dict <- get_dict(final_df, "final_df") %>%
      select(-df_name) %>%
      mutate(var_needs_manual_check = ifelse((var_type == "factor" | str_detect(var_definition, "id") | str_detect(var_definition, "number") | str_detect(var_definition, "index") | str_detect(var_definition, "age") | str_detect(var_definition, "size")), F, T))

    # 5i.d. Append to the data dictionary where variables come from:
    vars_origin_key <- map_dfr(data_dict$var_name, function(v) {
      origins_df <- all_data_dict %>% filter(var_name == v)
      files_var_appears <- unique(origins_df$df_name)
      data.frame(var_name = v, files_var_found_in = paste(files_var_appears, collapse = ", "), n_files_var_found_in = length(files_var_appears))
    })
    data_dict <- full_join(data_dict, vars_origin_key, by = "var_name") %>% 
      as_tibble()

    # 5i.e. Create a data dictionary for the variables we DON'T have:
    discard_dict <- all_data_dict %>% 
      filter(var_name %in% setdiff(setdiff(unique(all_data_dict$var_name), unique(data_dict$var_name)), unique(vars_to_rename$var_name))) %>% 
      as_tibble()

    # 5i.f. Wrapping up all the information we would like to return:
    clean_data <- list(df = final_df, data_dictionary = data_dict, discarded_data_dictionary = discard_dict, vars_not_found = vars_not_found)
    
  } else {
    
#--------------------------#
####  5ii. VARS = NULL  ####
#--------------------------#
    # 5ii.a. if vars is NULL, then it is too expensive (computationally & memory-wise) to risk a merge. 
    # It also implies that we do not know what vars we want yet, so instead, some helpful metadata should be 
    # returned, along with the data list. We get a list of variables from previous rounds' 
    # intermediate cleaned data (if available) and append that to the data dict made in step 3b:
    prev_rounds <- paste0("r", 1:(as.numeric(str_sub(round, 2, 2)) - 1))
    prev_filepaths <- here(int_data_dir, paste0(prev_rounds, "_", country, ".dta"))
    existing_filepaths <- prev_filepaths[file.exists(prev_filepaths)]
    if (length(existing_filepaths) > 0) {
      prev_data <- map(existing_filepaths, 
        ~read_dta(.) %>% 
        slice_head %>% 
        mutate(across(where(is.labelled), as_factor)) %>%
        mutate(across(where(lubridate::is.Date), as.character))) 
      prev_dict <- map2_dfr(.x = prev_data, .y = prev_rounds[file.exists(prev_filepaths)], ~get_dict(cdf = .x, cdf_name = .y)) %>%
        mutate(var_name = str_remove(var_name, paste0("r(.)_", country, "_")))

      all_data_dict <- all_data_dict %>%
        mutate(var_appears_in_prev_rounds = ifelse(var_name %in% unique(prev_dict$var_name), T, F))
      
      prev_vars_not_found_vec <- setdiff(unique(prev_dict$var_name), unique(all_data_dict$var_name))

      prev_vars_not_found_df <- prev_dict %>% 
        filter(var_name %in% prev_vars_not_found_vec) %>%
        as_tibble()
    
    } else {      
      
      # 5ii.b. in case there are no existing filepaths for previous rounds.
      prev_vars_not_found_df <- NULL
    }

    # 5ii.c. highlight possible linkage variables:
    all_data_dict <- all_data_dict %>% mutate(possible_linkage_var = ifelse(var_name %in% possible_linkage_vars$var_name, TRUE, FALSE))
    poss_linkage_vars_to_return <- right_join(all_data_dict, possible_linkage_vars, by = "var_name") %>%
      select(var_name, var_definition, var_type, files_appears_in, total_files, perc_appears_in, df_name) %>%
      arrange(desc(files_appears_in), var_name, df_name)

    # 5ii.f. Wrapping up all the information we would like to return:
    clean_data <- list(data_list = data, data_dictionary = all_data_dict, prev_vars_not_found = prev_vars_not_found_df, possible_linkage_vars = poss_linkage_vars_to_return)
  }
  # Final object to return:
  return(clean_data)
}

#-------------------------#
####  6. MISSING KEYS  ####
#-------------------------#
    # 6. List of responses within variables that should be set to NA.
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