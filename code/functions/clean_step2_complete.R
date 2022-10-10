# File: clean_step2_complete.R
# Date: 09/04/2022
# Author: Lawrence Chillrud 

#-------------------------#
#### TABLE OF CONTENTS ####
#-------------------------#
# N. Notes
# F. Function
# 0. Error handling
# 1. Clean data
# 2. Data dictionary set up
# 3. Variable selection 
# 4. Merge all data
# 5. Missing keys

#--------------------------#
####      N. NOTES      ####
#--------------------------#
# This script contains the function clean_step2_complete() for cleaning the young_lives_data_preparation raw data files automatically, and all at once after the linkage variable has been identified and corrected:
# clean_step2_complete() does the following: 
#     (1) applies previously coded labels to variables automatically; 
#     (2) handles all missing values that we want reported as NA;
#     (3) allows for variable selection; 
#     (4) completes necessary full_joins of all .dta files; 
#     (5) curates useful metadata.
# 
# 
# INPUTS: 
#     data: A list of the raw data that needs to be cleaned.
#     vars: Character vector of variable names to extract. by default = NULL.
#     linkage_vars: Either (1) a character vector of variable names to join by; or (2) NULL (default) meaning "join" should decide how the join should be performed (for backwards compatibility)
#     missing_keys: (Optional) a character vector of values that should be treated as missing whenever encountered in the data. by default = the object 'default_missing_keys'
#     case_sensitive: (Optional) logical indicating if variables names are case sensitive. by default = FALSE.
# 
# OUTPUTS: 
#     (1) df: A tibble comprising the final clean dataframe.
#     (2) data_dictionary: A tibble containing useful metadata on all variables in df.
#     (3) discarded_data_dictionary: A tibble containing useful metadata on all variables that were discarded 
#         (not selected because they were not passed via the "vars" argument) during cleaning.
#     (4) vars_not_found: A character vector containing the variables that were not found at all during cleaning.
#
# WARNINGS: package requirements: haven, here, tidyverse must all be installed for clean() to run.
#           additionally, clean_step2_complete() expects that data files (both raw & intermediate) are named according 
#           to the conventions laid out in the various markdown files.

#--------------------------#
####     F. FUNCTION    ####
#--------------------------#
clean_step2_complete <- function(data, vars, linkage_vars, missing_keys = default_missing_keys, case_sensitive = FALSE) {

#--------------------------#
####  0. ERROR HANDLING ####
#--------------------------#
  # 0a. Standard error handling (ensuring inputs are correct)
  if (!is.logical(case_sensitive)) stop('Invalid "case_sensitive" passed. Must be a logical (TRUE or FALSE).')
  
  # 0b. Ensure the linkage_vars are also present in vars
  vars <- unique(c(linkage_vars, vars))
  
  # 0c. Message letting us know that we have begun cleaning:
  message("\nCompleting cleaning...\n")
  
#---------------------#
#### 1. CLEAN DATA ####
#---------------------#
  # 1a. Standardise case of variables
  if (!case_sensitive) vars <- tolower(vars)
  if (!case_sensitive) linkage_vars <- tolower(linkage_vars)
  
  # 1b. Save the names of the data list
  data_names <- names(data)

  # 1c. Clean the data. The first call to "mutate" here does *most* variable coding automatically.
  # Second call to "mutate" is to make dates be in the appropriate format when hunting for missing values a few lines below.
  # Third & fourth mutates convert the entire dataframe to lowercase.
  # If we are NOT being case sensitive, we then standardise the case of all variable names.
  # Next, we encode missing values as NA.
  data <- map(.x = data, .f = 
    function(df) {
      df <- df %>% 
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

  # 1d. Set the names of each dataset:
  names(data) <- data_names

#--------------------------------#
#### 2. DATA DICTIONARY SET UP####
#--------------------------------#
  # 2a. Small helper function to create a data dictionary from a dataframe
  get_dict <- function(cdf, cdf_name) { 
    missing_stats <- map_dfc(cdf, ~sum(is.na(.x))) %>% 
      pivot_longer(everything(), names_to = "var_name", values_to = "total_missing") %>%
      mutate(perc_missing = total_missing / nrow(cdf) * 100, df_name = cdf_name)

    definitions <- tolower(as.character(sapply(cdf, attr, which = "label")))
    data_types <- as.character(sapply(cdf, class))
    dd <- data.frame(df_name = cdf_name, var_name = colnames(cdf), var_definition = definitions, var_type = data_types)
    left_join(dd, missing_stats, by = c("df_name", "var_name"))
  }

  # 2b. Create a data dictionary for the entire survey, round, country:
  all_data_dict <- map2_dfr(.x = data, .y = names(data), ~get_dict(cdf = .x, cdf_name = .y)) %>%
    mutate(var_needs_manual_check = ifelse((var_type == "factor" | str_detect(var_definition, "id") | str_detect(var_definition, "number") | str_detect(var_definition, "index") | str_detect(var_definition, "age") | str_detect(var_definition, "size")), F, T)) %>%
    as_tibble()

#--------------------------#
####  3. VAR SELECTION  ####
#--------------------------#
  # 3a. Select all the variables we care about (if they are present in the data).
  # We also make a call to "distinct" here to ensure extraneous info is not kept when truncating some dfs.
  # There is also a check to help flag when there was actually nothing of interest from a df. 
  # We remove these non-dfs in step 3b below. 
  data <- data %>% 
    map(function(df) {
      df <- df %>% select(any_of(vars)) %>% distinct()
      if (ncol(df) == 0 || ((all(colnames(df) %in% linkage_vars)))) {
        return(NULL)
      } else {
        return(df)
      }
    })
  
  # 3b. Remove any dataframes from the data list that came back as NULL (ie. because they had no variables we were asking for):
  null_dfs <- which(sapply(data, is.null))
  if (length(null_dfs) > 0) data <- data[-null_dfs]
  if (length(data) == 0) stop('None of the variables in "vars" could be found in the data/raw/[survey]/[round]_[country]_*.dta files you pointed to.')
  
  # 3c. Create information table on variables that may be possible linkage variables for that round.
  possible_linkage_vars <- data.frame(table(reduce(map(data, colnames), .f=c))) %>% filter(Freq > 1) %>%
    mutate(total_files = length(data), perc = paste0(round(Freq / total_files * 100), "%"))
  colnames(possible_linkage_vars) <- c("var_name", "files_appears_in", "total_files", "perc_appears_in")
  
  # 3d. Rename ambiguous variables to prevent conflicts during the join. Print a warning letting the user know.
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
  
#--------------------------#
####  4. MERGE ALL DATA ####
#--------------------------#
  # 4a. Merge all the data
  final_df <- reduce(.x = data, .f = full_join)
  
  # 4b. Provide information on variables not found:
  vars_not_found <- setdiff(vars, colnames(final_df))
  if (is.character(linkage_vars)) vars_not_found <- setdiff(vars_not_found, unique(vars_to_rename$var_name))
  options(useFancyQuotes = FALSE) # for the below warning message:
  if (length(vars_not_found) > 0) {
    warning(call. = F, length(vars_not_found), ' variable(s) you asked for in "vars" were not found when cleaning the data. Subsequently, they will not be present in the ouput dataframe. They were: ', paste(dQuote(vars_not_found), collapse = ", "))
  } else {
    vars_not_found <- NULL
  }
  
  # 4c. Create a data dictionary for the variables we do have:
  data_dict <- get_dict(final_df, "final_df") %>%
    select(-df_name) %>%
    mutate(var_needs_manual_check = ifelse((var_type == "factor" | str_detect(var_definition, "id") | str_detect(var_definition, "number") | str_detect(var_definition, "index") | str_detect(var_definition, "age") | str_detect(var_definition, "size")), F, T))

  # 4d. Append to the data dictionary where variables come from
  vars_origin_key <- map_dfr(data_dict$var_name, function(v) {
    origins_df <- all_data_dict %>% filter(var_name == v)
    files_var_appears <- unique(origins_df$df_name)
    data.frame(var_name = v, files_var_found_in = paste(files_var_appears, collapse = ", "), n_files_var_found_in = length(files_var_appears))
  })
  data_dict <- full_join(data_dict, vars_origin_key, by = "var_name") %>% 
    as_tibble()

  # 4e. Create a data dictionary for the variables we DO NOT have:
  discard_dict <- all_data_dict %>% 
    filter(var_name %in% setdiff(setdiff(unique(all_data_dict$var_name), unique(data_dict$var_name)), unique(vars_to_rename$var_name))) %>% 
    as_tibble()
  
  # 4f. Final object to return:
  return(list(df = final_df, data_dictionary = data_dict, discarded_data_dictionary = discard_dict, vars_not_found = vars_not_found))
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