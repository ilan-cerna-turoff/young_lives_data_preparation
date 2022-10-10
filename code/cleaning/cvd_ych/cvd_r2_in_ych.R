# File: India covid survey with younger children round 2
# Date: 04/04/2022
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

survey <- "cvd_ych"
round <- "r2"
country <- "in"
linkage_var <- "childcode"

vars_i_want <- c("havphncov2","hep_group_sc","cureducov2","ened02cov2","typesite_sc")

cvd_r2_in <- clean(survey = survey, round = round, country = country, vars = vars_i_want,
                   linkage_var = linkage_var) 

#------------------------------------------------------------------------------
#### 2. PERFORMING A MANUAL CHECK ####
#------------------------------------------------------------------------------
summary(cvd_r2_in$df) # a summary of the final clean data
cvd_r2_in$vars_needing_careful_manual_check
cvd_r2_in$vars_not_found
dd <- cvd_r2_in$data_dictionary # vars included above as vars_i_want
dd_discards <- cvd_r2_in$discarded_data_dictionary # vars not included

#------------------------------------------------------------------------------
#### 3. CONVERT TO TIBBLE FOR DATA CLEANING/MANIPULATION    ####
#------------------------------------------------------------------------------
df1 <- as_tibble(cvd_r2_in$df)
rm(cvd_r2_in) #clean environment

#------------------------------------------------------------------------------
#### 4. CLEAN/MANIPULATE REST OF VARS    ####
#------------------------------------------------------------------------------
## Clean labels ##
df1 <- df1 %>%
  mutate(across(c("havphncov2","hep_group_sc","cureducov2","ened02cov2","typesite_sc"), as.character),
         ened02cov2 = ifelse(ened02cov2 == "y", "yes","no"),
         cureducov2 = str_trim(cureducov2, side = "both"), 
         cureducov2 = ifelse(cureducov2 == "no", "no", #not enrolled in school currently due to covid
                             ifelse(cureducov2 == "never attended", "not applicable", "yes"))) %>%  
  replace_na(list(hep_group_sc = "missing", typesite_sc = "missing", ened02cov2 = "missing")) %>%
  mutate(across(c("havphncov2","hep_group_sc","cureducov2","ened02cov2","typesite_sc"), as.factor))

#------------------------------------------------------------------------------
#### 5. RENAME FOR DIFFERENCES BETWEEN ROUNDS              ####
#------------------------------------------------------------------------------
df1 <- df1 %>%
  rename(typesite = typesite_sc,
         hep_group = hep_group_sc,
         havphncov = havphncov2,
         cureducov = cureducov2,
         ened02cov = ened02cov2) 

#------------------------------------------------------------------------------
#### 6. WRITE INTERMEDIATE FILE              ####
#------------------------------------------------------------------------------
saveRDS(df1, here("data", "intermediate", survey, paste0(round, "_", country, ".rds")))
# RDS maintains factor labeling