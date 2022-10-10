# File: India community survey round 1
# Date: 02/03/2022
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
country <- "in"
linkage_vars <- "COMMID"

vars_i_want <- c("DISTRID","anydis","disrel",paste0("rel", 1:5),"pop","city",
                 "subzone",paste0("eth", 1:4),"thfcrm","violcrm","yuthcrm",
                 "proscrm","pubtran1","sochel","adultlit","frqpass")

com_r1_in <- clean(survey = survey, round = round, country = country, vars = vars_i_want,
                   linkage_vars = linkage_vars) 

#------------------------------------------------------------------------------
#### 2. PERFORMING A MANUAL CHECK ####
#------------------------------------------------------------------------------
summary(com_r1_in$df) # a summary of the final clean data
com_r1_in$vars_needing_careful_manual_check
com_r1_in$vars_not_found
dd <- com_r1_in$data_dictionary # vars included above as vars_i_want
dd_discards <- com_r1_in$discarded_data_dictionary # vars not included

#------------------------------------------------------------------------------
#### 3. CONVERT TO TIBBLE FOR DATA CLEANING/MANIPULATION    ####
#------------------------------------------------------------------------------
df1 <- as_tibble(com_r1_in$df)
rm(com_r1_in) #clean environment

#------------------------------------------------------------------------------
#### 4. CLEAN/MANIPULATE REST OF VARS    ####
#------------------------------------------------------------------------------
numberfunction <- function(x){ifelse(x=="yes", 1,0)} # makes yes and no into 1,0


# Consistent naming across rounds/countries
df1 <- rename(df1, geographic = distrid)


# Create natural disaster exposure and skips
df1 <- df1 %>% 
  mutate(across(c("anydis","disrel"), as.character),
         anydis = ifelse(is.na(anydis), "none", anydis), # creates the no disaster category
         anydis = ifelse(anydis == "flooding", "flood", anydis),
         disrel = ifelse(anydis == "none", NA, disrel),
         across(c("anydis","disrel"), as.factor))


# Count of ethnic groups in community
df1 <- df1 %>% 
  mutate(across(c("eth1","eth2","eth3","eth4"), numberfunction)) %>% 
  rowwise() %>% 
  mutate(ethcomp = sum(eth1,eth2,eth3,eth4)) %>%
  replace_with_na(replace = list(ethcomp = c(0)))  #impossible that 0 ethnicities
           
df1 <- select(df1, -c(eth1,eth2,eth3,eth4))


# Count of religious groups in community
df1 <- df1 %>% 
  mutate(across(c("rel1_community","rel2_community",
                  "rel3_community","rel4_community","rel5"), numberfunction)) %>% 
  rowwise() %>% 
  mutate(relcomp = sum(rel1_community,rel2_community,rel3_community,
                         rel4_community,rel5)) %>%
  replace_with_na(replace = list(relcomp = c(0)))  #impossible that 0 religions

df1 <- select(df1, -c(rel1_community,rel2_community,rel3_community,rel4_community,rel5,
                      rel1_csec1disasters,rel2_csec1disasters,rel3_csec1disasters,rel4_csec1disasters))

# Note: rel1...4 var names used for types of disaster relief and religious groups in the same survey.
# We are only using religious groups variables so removing relief related variables. rel5 is unique
# to the religion variable and therefore, does not include the snake case label to distinguish between lists

#------------------------------------------------------------------------------
#### 5. WRITE INTERMEDIATE FILE              ####
#------------------------------------------------------------------------------
saveRDS(df1, here("data", "intermediate", survey, paste0(round, "_", country, ".rds")))
# RDS maintains factor labeling
