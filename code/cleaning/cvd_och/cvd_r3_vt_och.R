# File: Vietnam covid survey with older children round 3
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
country <- "vt"
linkage_var <- "childcode"

vars_i_want <- c("havphncov3","cureducov3","ened02cov3")

cvd_r3_vt <- clean(survey = survey, round = round, country = country, vars = vars_i_want,
                   linkage_var = linkage_var) 

#------------------------------------------------------------------------------
#### 2. PERFORMING A MANUAL CHECK ####
#------------------------------------------------------------------------------
summary(cvd_r3_vt$df) # a summary of the final clean data
cvd_r3_vt$vars_needing_careful_manual_check
cvd_r3_vt$vars_not_found
dd <- cvd_r3_vt$data_dictionary # vars included above as vars_i_want
dd_discards <- cvd_r3_vt$discarded_data_dictionary # vars not included

#------------------------------------------------------------------------------
#### 3. CONVERT TO TIBBLE FOR DATA CLEANING/MANIPULATION    ####
#------------------------------------------------------------------------------
df1 <- as_tibble(cvd_r3_vt$df)
rm(cvd_r3_vt) #clean environment

#------------------------------------------------------------------------------
#### 4. CLEAN/MANIPULATE REST OF VARS    ####
#------------------------------------------------------------------------------
## Clean labels ##
df1 <- df1 %>%
  mutate(across(c("havphncov3","cureducov3","ened02cov3"), as.character),
         ened02cov3 = ifelse(ened02cov3 == "y", "yes","no"),
         cureducov3 = str_trim(cureducov3, side = "both"), 
         cureducov3 = ifelse(cureducov3 == "no", "no", #not enrolled in school currently due to covid
                             ifelse(cureducov3 == "never attended", "not applicable", "yes"))) %>%  
  replace_na(list(ened02cov3 = "missing")) %>%
  mutate(across(c("havphncov3","cureducov3","ened02cov3"), as.factor))

#------------------------------------------------------------------------------
#### 5. RENAME FOR DIFFERENCES BETWEEN ROUNDS              ####
#------------------------------------------------------------------------------
df1 <- df1 %>%
  rename(havphncov = havphncov3,
         cureducov = cureducov3,
         ened02cov = ened02cov3) 

#------------------------------------------------------------------------------
#### 6. WRITE INTERMEDIATE FILE              ####
#------------------------------------------------------------------------------
saveRDS(df1, here("data", "intermediate", survey, paste0(round, "_", country, ".rds")))
# RDS maintains factor labeling

