# File: merge_intermediate.R
# Date: 27/06/2022
# Author: Lawrence Chillrud 

#-------------------------#
#### TABLE OF CONTENTS ####
#-------------------------#
# N. Notes
# 0. Function definition

#--------------------------#
####      N. NOTES      ####
#--------------------------#
# This script provides the code for the merge_intermediate() function. This creates a final merged longitudinal 
# structure for each country and survey type.

#--------------------------------#
####  0. FUNCTION DEFINITION  ####
#--------------------------------#
#' merge_intermediate is a function that merges all int .rds data files
#' within a given survey and country combination. merge_intermediate() expects that all relevant 
#' intermediate data files are in .rds format and in the proper intermediate/survey folder,
#' named with the proper conventions (data/intermediate/[survey]/[round]_[country].rds).
#' 
#' @param survey The desired survey. Must be one of {com_main, cvd_och, cvd_ych}
#' @param country The desired country. Must be one of {et, in, pe, vt}
#' @param save_as (Optional) The filepath to save the resulting data to. Must end in ".rds"; OR = NULL if no save 
#' should be performed; OR = "default" for the default option. 
#' If save_as = "default", the data will be saved to: "data/final/[survey]/[country].rds"
#' 
#' @return Dataframe with all rounds from the given survey-country combination merged

merge_intermediate <- function(survey, country, save_as = "default") {
  
  # 0a. Standard error handling (ensuring inputs are correct)
  if (!(survey %in% c("com_main", "cvd_och", "cvd_ych"))) stop('Invalid "survey" passed. Must be one of: {"com_main", "cvd_och", "cvd_ych"}')
  if (!(country %in% c("et", "in", "pe", "vt"))) stop('Invalid "country" passed. Must be one of: {"et", "in", "pe", "vt"}')
  if (!(save_as == "default" || is.null(save_as) || endsWith(tolower(save_as), ".rds"))) stop('Invalid "save_as" passed. Must be one of {NULL, "default", or ends in ".rds"}')
    
  # 0b. File paths set up:
  int_dir <- here("data", "intermediate", survey)
  fin_dir <- here("data", "final", survey)
  int_filepaths <- list.files(int_dir, pattern = paste0("*_", country, ".rds"))
  rounds <- as.numeric(str_extract(int_filepaths, "\\d"))
  
  # 0c. Read in data:
  data <- map2(here(int_dir, int_filepaths), rounds, function(f, r) {
    read_rds(f) %>% mutate(round = r, across(where(is.ordered), as.character))
  })
  names(data) <- rounds
  
  # 0d. Merge data:
  final_df <- bind_rows(data) %>% select(round, everything())
  
  # 0e. Data dictionary function:
  get_dict <- function(cdf, cdf_name) { 
    missing_stats <- map_dfc(cdf, ~sum(is.na(.x))) %>% 
      pivot_longer(everything(), names_to = "var_name", values_to = "total_missing") %>%
      mutate(perc_missing = total_missing / nrow(cdf) * 100, df_name = cdf_name)
    
    definitions <- tolower(as.character(sapply(cdf, attr, which = "label")))
    data_types <- as.character(sapply(cdf, class))
    dd <- data.frame(df_name = cdf_name, var_name = colnames(cdf), var_definition = definitions, var_type = data_types)
    left_join(dd, missing_stats, by = c("df_name", "var_name"))
  }
  
  # 0f. Get info on which rounds the variables in the final dataframe appear in:
  rounds_info <- map2_dfr(.x = data, .y = paste0("r", names(data)), ~get_dict(cdf = .x, cdf_name = .y)) %>%
    group_by(var_name) %>% 
    summarise(
      var_definition = paste(unique(var_definition), collapse = ", "),
      rounds_appears_in = paste(df_name, collapse = ", "),
      num_rounds = n(),
      perc_rounds = round(n() / length(rounds) * 100, 2),
    ) %>%
    mutate(
      var_definition = str_remove(var_definition, "\\s?[, ]?\\s?null\\s?[, ]?\\s?"),
      var_definition = ifelse(var_definition == "", NA, var_definition)
    )
  
  # 0g. Get missingness info across the overall final dataframe:
  missingness_info <- get_dict(final_df, "final_df") %>% 
    select(var_name, ends_with("missing")) %>%
    rename(overall_perc_missing = perc_missing)
  
  # 0h. Get in depth missingness info at by-round resolution:
  extra_missingness_info <- map_dfr(colnames(final_df), function(v) {
    present <- final_df %>% 
      select(round, !!sym(v)) %>% 
      na.omit() %>%
      count(round, name = "present")
    
    totals <- final_df %>% count(round, name = "total")
    
    full_join(present, totals, by = "round") %>%
      mutate(
        perc_missing = round((total - present) / total * 100, 2),
        var_name = v
      ) %>%
      select(-present, -total) %>%
      pivot_wider(
        names_from = round, 
        values_from = perc_missing, 
        names_glue = "r{round}_{.value}"
      ) %>%
      mutate(across(contains("missing"), ~ifelse(is.na(.), 100.00, .)))
  })
  
  # 0j. Join all the dictionary info together:
  final_dict <- full_join(rounds_info, missingness_info, by = "var_name") %>%
    left_join(., extra_missingness_info, by = "var_name") %>%
    arrange(desc(perc_rounds), overall_perc_missing) %>%
    select(-c(num_rounds, total_missing))
  
  # Save & return:
  ret <- list(df = final_df, dict = final_dict)
  if (!is.null(save_as)) {
    if (save_as == "default") {
      output_fp <- here(fin_dir, paste0(country, ".rds"))
    } else {
      output_fp <- save_as
    }
    dir.create(fin_dir, showWarnings = F)
    write_rds(ret, output_fp)
  }
  
  ret
  
}
