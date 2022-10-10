# Filename: fuzzy_match.R
# Date: 27/05/2022
# Author: Lawrence Chillrud 

#-------------------------#
#### TABLE OF CONTENTS ####
#-------------------------#
# N. Notes
# 0. Define Function
# 1. Run tests

#--------------------------#
####      N. NOTES      ####
#--------------------------#
# This script is meant to provide a function to perform fuzzy matching of 
# variable names across rounds of a survey-country combination. It is useful when
# variables do not have similar but not the exact same name across rounds.

#--------------------------#
#### 0. DEFINE FUNCTION ####
#--------------------------#
fuzzy_match <- function(survey, country, data_dir = "raw", traits = c("var", "definition"), n = 1) {
  
  # 0a. error handling:
  if (!(survey %in% c("com_main", "cvd_och", "cvd_ych"))) stop('Invalid "survey" passed. Must be one of: {"com_main", "cvd_och", "cvd_ych"}')
  if (!(country %in% c("et", "in", "pe", "vt"))) stop('Invalid "country" passed. Must be one of: {"et", "in", "pe", "vt"}')
  if (!(data_dir %in% c("raw", "intermediate"))) stop('Invalid "data_dir" passed. Must be one of: {"raw", "intermediate"}')
  
  # 0b. file setup:
  dir <- here("data", data_dir, survey)
  rounds <- sort(as.numeric(unique(str_extract(list.files(dir, pattern = paste("*", country, "*", sep = "_"), full.names = F), "\\d"))))
  
  # 0c. read in data & create by-round dictionary to return to user:
  data <- map(rounds, function(r) {
    filepaths <- list.files(dir, pattern = paste(paste0("r", r), country, "*", sep = "_"), full.names = T)
    filenames <- filepaths %>% str_replace(here(dir, paste(paste0("r", r), paste0(country, "_"), sep = "_")), "") %>% str_replace(".dta", "")
    map2_dfr(filepaths, filenames, function(p, n) {
      d <- read_dta(p)
      var <- tolower(colnames(d))
      definition <- tolower(as.character(sapply(d, attr, which = "label")))
      data.frame(round = r, df_name = n, var, definition)
    }) %>% 
      distinct(across(all_of(traits)), .keep_all = T) %>% 
      unite(string, all_of(traits), sep = "; ")
  })
  names(data) <- paste0("r", rounds)
  
  # 0d. perform fuzzy matching of col names (round by round):
  matches <- map(names(data), function(i) {
    cat(paste0("Fuzzing matching variables in round ", i, " against those variables in every other round...\n"))
    map_dfc(names(data), function (j) {
      if (i != j) {
        expand.grid(V1 = unique(data[[i]]$string), V2 = unique(data[[j]]$string)) %>%
          tidy_stringdist(method = c("cosine", "jaccard", "jw")) %>%
          group_by(V1) %>%
          mutate(avg = (cosine + jaccard + jw)/3) %>%
          slice_min(avg, n = n, with_ties = F) %>%
          select(V1, V2) %>%
          rename(!!sym(j) := V2)
      }
    }) %>% 
      select(any_of(c("V1...1", paste0("r", rounds)))) %>% 
      rename(!!sym(paste0("cur_", i)) := `V1...1`)
  })
  names(matches) <- names(data)
  
  list(matches = matches, dict = data)
}

#--------------------------#
#### 1. RUN TESTS ####
#--------------------------#
# test1 <- fuzzy_match(survey = "com_main", country = "vt")
# test2 <- fuzzy_match(survey = "com_main", country = "vt", traits = "var")
# test3 <- fuzzy_match(survey = "com_main", country = "vt", traits = "var", n = 5)
