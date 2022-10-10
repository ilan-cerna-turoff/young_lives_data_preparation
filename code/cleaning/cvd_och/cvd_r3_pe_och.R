# File: Peru covid survey with older children round 3
# Date: 18/04/2022
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
round <- "r3"
country <- "pe"
linkage_var <- "childcode"

vars_i_want <- c("havphncov3","hep_group_tc","typesite_tc","stpattcov3","cureducov3")

cvd_r3_pe <- clean(survey = survey, round = round, country = country, vars = vars_i_want,
                   linkage_var = linkage_var) 

# Note: ened02cov3 does not exist for Peru but can be constructed

#------------------------------------------------------------------------------
#### 2. PERFORMING A MANUAL CHECK ####
#------------------------------------------------------------------------------
summary(cvd_r3_pe$df) # a summary of the final clean data
cvd_r3_pe$vars_needing_careful_manual_check
cvd_r3_pe$vars_not_found
dd <- cvd_r3_pe$data_dictionary # vars included above as vars_i_want
dd_discards <- cvd_r3_pe$discarded_data_dictionary # vars not included

#------------------------------------------------------------------------------
#### 3. CONVERT TO TIBBLE FOR DATA CLEANING/MANIPULATION    ####
#------------------------------------------------------------------------------
df1 <- as_tibble(cvd_r3_pe$df)
rm(cvd_r3_pe) #clean environment

#------------------------------------------------------------------------------
#### 4. CLEAN/MANIPULATE REST OF VARS    ####
#------------------------------------------------------------------------------
## Clean labels ##
df1 <- df1 %>%
  mutate(across(c("havphncov3","hep_group_tc","cureducov3","stpattcov3","typesite_tc"), as.character),
         stpattcov3 = ifelse(str_detect(stpattcov3, "(.*)virtual(.*)"), "yes", "no"),
         cureducov3 = str_trim(cureducov3, side = "both"), 
         cureducov3 = ifelse(cureducov3 == "no", "no", "yes")) %>% #not enrolled in school currently due to covid
  replace_na(list(stpattcov3 = "missing", cureducov3 = "missing", hep_group_tc = "missing")) %>%
  mutate(across(c("havphncov3","hep_group_tc","cureducov3","stpattcov3","typesite_tc"), as.factor))

#------------------------------------------------------------------------------
#### 5. RENAME FOR DIFFERENCES BETWEEN ROUNDS              ####
#------------------------------------------------------------------------------
df1 <- df1 %>%
  rename(typesite = typesite_tc,
         hep_group = hep_group_tc,
         havphncov = havphncov3,
         cureducov = cureducov3,
         ened02cov = stpattcov3) 

#------------------------------------------------------------------------------
#### 6. WRITE INTERMEDIATE FILE              ####
#------------------------------------------------------------------------------
saveRDS(df1, here("data", "intermediate", survey, paste0(round, "_", country, ".rds")))
# RDS maintains factor labeling
