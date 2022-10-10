# File: Ethiopia community survey round 1
# Date: 01/03/2022
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

survey <- "com_main"
round <- "r1"
country <- "et"
linkage_vars <- "commid"

vars_i_want <- c("anydis","disrel","subzone","pop","pubtran1","thfcrm",
                 "violcrm","yuthcrm","proscrm","forcrm","city","sochel",
                 "adultlit","movwork","region",paste0("eth", 2:9),
                 paste0("rel", 1:4))

com_r1_et <- clean(survey = survey, round = round, country = country, vars = vars_i_want,
                   linkage_vars = linkage_vars)

#------------------------------------------------------------------------------
#### 2. PERFORMING A MANUAL CHECK ####
#------------------------------------------------------------------------------
summary(com_r1_et$df) # a summary of the final clean data
com_r1_et$vars_needing_careful_manual_check
com_r1_et$vars_not_found
dd <- com_r1_et$data_dictionary # vars included above as vars_i_want
dd_discards <- com_r1_et$discarded_data_dictionary # vars not included

#------------------------------------------------------------------------------
#### 3. CONVERT TO TIBBLE FOR DATA CLEANING/MANIPULATION    ####
#------------------------------------------------------------------------------
df1 <- as_tibble(com_r1_et$df)
rm(com_r1_et) #clean environment

#------------------------------------------------------------------------------
#### 4. CLEAN/MANIPULATE REST OF VARS    ####
#------------------------------------------------------------------------------
numberfunction <- function(x){ifelse(x=="yes", 1,0)} # makes yes and no into 1,0


# Consistent naming across rounds/countries
df1 <- rename(df1, geographic = region) %>%
  mutate(geographic = recode(geographic, "south nations" = "snnp"))


# Create natural disaster exposure and cleaning labels/skips
df1 <- df1 %>% 
  mutate(across(c("anydis","disrel","movwork"), as.character),
         anydis = ifelse(is.na(anydis), "none", anydis), # creates the no disaster category
         anydis = ifelse(anydis == "flooding", "flood", anydis), # clean label for consistency
         disrel = ifelse(anydis == "none", NA, disrel),
         disrel = ifelse(commid == "et2041", "yes", disrel), #manual re-coding of errors
         disrel = ifelse(commid == "et2061", "yes", disrel),
         disrel = ifelse(commid == "et2071", "yes", disrel),
         disrel = ifelse(commid == "et5171", "yes", disrel),
         disrel = ifelse(commid == "et5201", "yes", disrel),
         disrel = ifelse(commid == "et5202", "yes", disrel),
         movwork = ifelse(commid == "et5201", "yes", movwork),
         movwork = ifelse(commid == "et5191", "yes", movwork),
         movwork = ifelse(commid == "et5181", "yes", movwork),
         movwork = ifelse(commid == "et5171", "yes", movwork),
         movwork = ifelse(commid == "et4161", "yes", movwork),
         movwork = ifelse(commid == "et4151", "yes", movwork),
         movwork = ifelse(commid == "et4131", "yes", movwork),
         movwork = ifelse(commid == "et4121", "yes", movwork),
         movwork = ifelse(commid == "et3112", "yes", movwork),
         movwork = ifelse(commid == "et3111", "yes", movwork),
         movwork = ifelse(commid == "et3101", "yes", movwork),
         movwork = ifelse(commid == "et3081", "yes", movwork),
         movwork = ifelse(commid == "et2071", "yes", movwork),
         movwork = ifelse(commid == "et2061", "yes", movwork),
         movwork = ifelse(commid == "et2051", "yes", movwork),
         movwork = ifelse(commid == "et2041", "yes", movwork),
         movwork = ifelse(commid == "et1032", "yes", movwork),
         movwork = ifelse(commid == "et1031", "yes", movwork),
         movwork = ifelse(commid == "et1022", "yes", movwork),
         movwork = ifelse(commid == "et1021", "yes", movwork),
         movwork = ifelse(commid == "et1011", "yes", movwork)) %>%
  mutate(across(c("anydis","disrel","movwork"), as.factor)) 


# Count of religious groups in community
df1 <- df1 %>% 
  mutate(across(c("rel1_main","rel2_main","rel3_main","rel4_main"), numberfunction)) %>% 
  rowwise() %>% 
  mutate(relcomp = sum(rel1_main,rel2_main,rel3_main,rel4_main)) %>%
  replace_with_na(replace = list(relcomp = c(0)))  #impossible that 0 ethnicities

df1 <- select(df1, -c(rel1_main,rel2_main,rel3_main,rel4_main,rel1_subnaturaldisasters,
                      rel2_subnaturaldisasters,rel3_subnaturaldisasters,rel4_subnaturaldisasters))

# Note: rel1...4 var names used for types of disaster relief and religious groups in the same survey.
# We are only using religious groups variables so removing relief related variables


# Count of ethnic groups in community
df1 <- df1 %>% 
  mutate(across(c("eth2","eth3","eth4","eth5","eth6","eth7","eth8","eth9"), numberfunction)) %>% 
  rowwise() %>% 
  mutate(ethcomp = sum(eth2,eth3,eth4,eth5,eth6,eth7,eth8,eth9)) %>%
  replace_with_na(replace = list(ethcomp = c(0)))  #impossible that 0 ethnicities

df1 <- select(df1, -c(eth2,eth3,eth4,eth5,eth6,eth7,eth8,eth9))

#------------------------------------------------------------------------------
#### 5. WRITE INTERMEDIATE FILE              ####
#------------------------------------------------------------------------------
saveRDS(df1, here("data", "intermediate", survey, paste0(round, "_", country, ".rds")))
# RDS maintains factor labeling