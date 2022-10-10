# File: India covid survey with older children round 1
# Date: 28/03/2022
# Author: Ilan Cerna-Turoff

#--------------------------#
#### 0. PACKAGE IMPORTS ####
#--------------------------#
# 0a. Run the 0_requirements.R file if you have not already:
if (!exists("ran_0_requirements")) {
  here::i_am("README.md")
  source(here::here("code", "functions", "0_requirements.R"))
}
# Loads in the necessary packages for the cleaning functions. Cleaning functions rely on the haven(), here(), and tidyverse() packages

#------------------------------------------------------------------------------
#### 1. LOAD CLEAN AND DATA  ####
#------------------------------------------------------------------------------
source(here("code", "functions", "clean.R")) # loading in the clean() function, which relies on the above packages^

survey <- "cvd_och"
round <- "r1"
country <- "in"
linkage_var <- "childcode"

vars_i_want <- c("havphncov1","hep_group","hhun192cov1","hhun193cov1","typesite_fc")

cvd_r1_in <- clean(survey = survey, round = round, country = country, vars = vars_i_want,
                   linkage_var = linkage_var) 

#------------------------------------------------------------------------------
#### 2. PERFORMING A MANUAL CHECK ####
#------------------------------------------------------------------------------
summary(cvd_r1_in$df) # a summary of the final clean data
cvd_r1_in$vars_needing_careful_manual_check
cvd_r1_in$vars_not_found
dd <- cvd_r1_in$data_dictionary # vars included above as vars_i_want
dd_discards <- cvd_r1_in$discarded_data_dictionary # vars not included

#------------------------------------------------------------------------------
#### 3. CONVERT TO TIBBLE FOR DATA CLEANING/MANIPULATION    ####
#------------------------------------------------------------------------------
df1 <- as_tibble(cvd_r1_in$df)
rm(cvd_r1_in) #clean environment

#------------------------------------------------------------------------------
#### 4. CLEAN/MANIPULATE REST OF VARS                    ####
#------------------------------------------------------------------------------
## Clean labels ##
df1 <- df1 %>%
  mutate(across(c("havphncov1","hep_group","hhun192cov1","hhun193cov1","typesite_fc"), as.character)) %>%    
  replace_na(list(hhun193cov1 = "missing", hhun192cov1 = "missing")) %>%
  mutate(across(c("havphncov1","hep_group","hhun192cov1","hhun193cov1","typesite_fc"), as.factor))

#------------------------------------------------------------------------------
#### 5. RENAME FOR DIFFERENCES BETWEEN ROUNDS              ####
#------------------------------------------------------------------------------
df1 <- df1 %>%
  rename(typesite = typesite_fc,
         havphncov = havphncov1,
         cureducov = hhun192cov1,
         ened02cov = hhun193cov1) 

#------------------------------------------------------------------------------
#### 6. WRITE INTERMEDIATE FILE              ####
#------------------------------------------------------------------------------
saveRDS(df1, here("data", "intermediate", survey, paste0(round, "_", country, ".rds")))
# RDS maintains factor labeling


