# File: clean_step1_label_error.R
# Date: 05/04/2022
# Author: Lawrence Chillrud 

#-------------------------#
#### TABLE OF CONTENTS ####
#-------------------------#
# N. Notes
# F. Function
# 0. Error handling
# 1. Set up data
# 2. Read & clean data

#--------------------------#
####      N. NOTES      ####
#--------------------------#
# This script contains the function clean_step1_label_error() for identifying when linkage variables have not been cast.
# This only works when R's memory capacity has not been superseded. After completion of this function, clean_step2_complete()
# should be run.
# 
# INPUTS: 
#     survey: can be one of "com_main", "cvd_och", "cvd_ych"
#     round: can be one of "r1", "r2", ..., "r5"
#     country: can be one of "et", "in", "pe", "vt"
#     case_sensitive: (optional) logical indicating if variables names are case sensitive. by default = FALSE.
# 
# OUTPUTS: 
#   (1) data_list: A list of all the raw data from the current specified (survey, round, country).
#   (2) data_dictionary: A tibble containing useful metadata on all variables from the current round of cleaning (specific to current survey & country).
#   (3) possible_linkage_vars: A dataframe giving information on possible linkage variables that can be used.
#
# WARNINGS: package requirements: haven, here, tidyverse must all be installed for clean_step1_label_error() to run.
#           additionally, clean_step1_label_error() expects that data files (both raw & intermediate) are named according 
#           to the conventions laid out in the various markdown files.

#--------------------------#
####     F. FUNCTION    ####
#--------------------------#
clean_step1_label_error <- function(survey, round, country, case_sensitive = FALSE) {
  
#--------------------------#
####  0. ERROR HANDLING ####
#--------------------------#
  # 0a. Standard error handling (ensuring inputs are correct)
  if (!(survey %in% c("com_main", "cvd_och", "cvd_ych"))) stop('Invalid "survey" passed. Must be one of: {"com_main", "cvd_och", "cvd_ych"}')
  if (!(round %in% paste0("r", 1:5))) stop('Invalid "round" passed. Must be one of: {"r1", "r2", ..., "r5"}')
  if (!(country %in% c("et", "in", "pe", "vt"))) stop('Invalid "country" passed. Must be one of: {"et", "in", "pe", "vt"}')
    
  # 0c. Message letting us know we have begun cleaning:
  starting_message <- c(paste0(rep('#', 20), collapse = ""), 
                        paste0(" READING IN: ", survey, ", ", round, ", ", country, " "),
                        paste0(rep('#', 20), collapse = ""))
  message(starting_message)
    
#--------------------------#
####   1. SET UP DATA   ####
#--------------------------#
  # 1a. Setting up data directories
  raw_data_dir <- here("data", "raw", survey)
  
  # 1b. Collecting filepaths and filenames:
  raw_data_filepaths <- list.files(raw_data_dir, pattern = paste(round, country, "*", sep = "_"), full.names = T)
  if (length(raw_data_filepaths) == 0) stop(paste0("No files were found for the input [survey, round, country] combination. Please double check the existence of files in the format: data/raw/", survey, "/", round, "_", country, "_*.dta"))
  
  raw_data_filenames <- raw_data_filepaths %>% 
    str_replace(here(raw_data_dir, paste(round, paste0(country, "_"), sep = "_")), "") %>%
    str_replace(".dta", "")
    
#----------------------------#
#### 2. READ & CLEAN DATA ####
#----------------------------#
  # 2a. Reading in all the data for (survey, round, country) combination specified.
  data <- map(.x = raw_data_filepaths, .f = function(x) {
    df <- read_dta(x)
    if (!case_sensitive) colnames(df) <- tolower(colnames(df))
    df
  })

  # 2b. Set the names of each dataset:
  names(data) <- raw_data_filenames

#--------------------------------#
#### 3. DATA DICTIONARY SET UP####
#--------------------------------#
  # 3a. Small helper function to create a data dictionary from a dataframe
  get_dict <- function(cdf, cdf_name) { 
    definitions <- tolower(as.character(sapply(cdf, attr, which = "label")))
    data_types <- as.character(sapply(cdf, class))
    data.frame(df_name = cdf_name, var_name = colnames(cdf), var_definition = definitions, var_type = data_types)
  }

  # 3b. Create a data dictionary for the entire survey, round, & country:
  all_data_dict <- map2_dfr(.x = data, .y = names(data), ~get_dict(cdf = .x, cdf_name = .y)) %>%
    mutate(var_needs_manual_check = ifelse((var_type == "factor" | str_detect(var_definition, "id") | str_detect(var_definition, "number") | str_detect(var_definition, "index") | str_detect(var_definition, "age") | str_detect(var_definition, "size")), F, T)) %>%
    as_tibble()

#--------------------------#
####  4. LINKAGE VARS   ####
#--------------------------#
  # 4a. Create information table on variables that may be possible linkage variables for that round.
  possible_linkage_vars <- data.frame(table(reduce(map(data, colnames), .f=c))) %>% filter(Freq > 1) %>%
    mutate(total_files = length(data), perc = paste0(round(Freq / total_files * 100), "%"))
  colnames(possible_linkage_vars) <- c("var_name", "files_appears_in", "total_files", "perc_appears_in")
  
  # 4b. Report the possible linkage vars information from above step 4b (& return information in dataframe later)
  linkage_mssg <- c("Possible linkage variables include:\n")
  for (i in 1:nrow(possible_linkage_vars)) linkage_mssg <- c(linkage_mssg, paste0("\t", possible_linkage_vars$var_name[i], ": appears in ", possible_linkage_vars$files_appears_in[i], " / ", possible_linkage_vars$total_files[i], "  files (", possible_linkage_vars$perc_appears_in[i], ")\n"))
  message(linkage_mssg)
  
  # 4c. Combine with information from data dictionary
  all_data_dict <- all_data_dict %>% mutate(possible_linkage_var = ifelse(var_name %in% possible_linkage_vars$var_name, TRUE, FALSE))
  poss_linkage_vars_to_return <- right_join(all_data_dict, possible_linkage_vars, by = "var_name") %>%
    select(var_name, var_definition, var_type, files_appears_in, total_files, perc_appears_in, df_name) %>%
    arrange(desc(files_appears_in), var_name, df_name)

  # Final object to return:
  return(list(data_list = data, data_dictionary = all_data_dict, possible_linkage_vars = poss_linkage_vars_to_return))

}