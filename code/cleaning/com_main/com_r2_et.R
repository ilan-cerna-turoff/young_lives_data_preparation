# File: Ethiopia community survey round 2
# Date: 04/03/2022
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
round <- "r2"
country <- "et"
linkage_vars <- "placeid"

vars_i_want <- c("disaster","popsize","migrate",paste0("trans", 1:3),"robbery",
                 "cttlthft",paste0("othrprb", 1:2),"vlntcrme","gangs","prstitn",
                 "recvhelp","socwrkr","ltrcycmp")

com_r2_et <- clean(survey = survey, round = round, country = country, vars = vars_i_want,
                   linkage_vars = linkage_vars) 

#------------------------------------------------------------------------------
#### 2. PERFORMING A MANUAL CHECK ####
#------------------------------------------------------------------------------
summary(com_r2_et$df) # a summary of the final clean data
com_r2_et$vars_needing_careful_manual_check
com_r2_et$vars_not_found
dd <- com_r2_et$data_dictionary # vars included above as vars_i_want
dd_discards <- com_r2_et$discarded_data_dictionary # vars not included

#------------------------------------------------------------------------------
#### 3. CONVERT TO TIBBLE FOR DATA CLEANING/MANIPULATION    ####
#------------------------------------------------------------------------------
df1 <- as_tibble(com_r2_et$df)
rm(com_r2_et) #clean environment

#------------------------------------------------------------------------------
#### 4. VARS NAMES TO AGREE WITH PAST ROUND     ####
#------------------------------------------------------------------------------
df1 <- df1 %>%
  rename(commid = placeid,
         anydis = disaster,
         disrel = recvhelp,
         pop = popsize,
         violcrm = vlntcrme,
         yuthcrm = gangs,
         proscrm = prstitn,
         movwork = migrate,
         sochel = socwrkr,
         adultlit = ltrcycmp)

#------------------------------------------------------------------------------
#### 5. CLEAN/MANIPULATE REST OF VARS    ####
#------------------------------------------------------------------------------
# Create natural disaster exposure and cleaning labels/skips
df1 <- df1 %>%
  mutate(anydis = as.character(anydis),
         anydis = ifelse(anydis == "mud avalanche/slide", "avalanche/mud slide", anydis)) %>% # consistent labeling across rounds
  replace_with_na(replace = list(anydis = c("disease epidemics affecting humans", # removes non-natural disasters from question
                                            "crop failure due to pest/disease",
                                            "disease epidemics affecting animals"))) %>%
  mutate(anydis = ifelse(is.na(anydis), "none", anydis), # creates the no disaster category
         anydis = ifelse(commid == "et2051" & anydis == "none", NA, anydis), #manual re-coding of errors
         anydis = ifelse(commid == "et2071" & anydis == "none", NA, anydis),
         anydis = ifelse(commid == "et3101" & anydis == "none", NA, anydis),
         anydis = ifelse(commid == "et3111" & anydis == "none", NA, anydis),
         anydis = ifelse(commid == "et4131" & anydis == "none", NA, anydis),
         anydis = ifelse(commid == "et4141" & anydis == "none", NA, anydis),
         anydis = ifelse(commid == "et4151" & anydis == "none", NA, anydis),
         anydis = ifelse(commid == "et4161" & anydis == "none", NA, anydis),
         anydis = ifelse(commid == "et4181" & anydis == "none", NA, anydis),
         anydis = ifelse(commid == "et5181" & anydis == "none", NA, anydis),
         anydis = as.factor(anydis))


# General question on if relief was received
df1 <- df1 %>% 
  mutate(disrel = as.character(disrel),
         disrel = ifelse(anydis == "none", NA, disrel), #corresponding NA if no disaster occurred
         disrel = ifelse(disrel == "no", "no", "yes"),
         disrel = ifelse(commid == "et3101", "yes", disrel), #manual re-coding of errors
         disrel = ifelse(commid == "et3111", "yes", disrel),
         disrel = ifelse(commid == "et4151", "yes", disrel),
         disrel = ifelse(commid == "et2051", "no", disrel),
         disrel = ifelse(commid == "et2071", "yes", disrel),
         disrel = ifelse(commid == "et4131", "no", disrel),
         disrel = ifelse(commid == "et4141", "yes", disrel),
         disrel = ifelse(commid == "et4161", "no", disrel),
         disrel = ifelse(commid == "et5171", "yes", disrel),
         disrel = ifelse(commid == "et5181", "yes", disrel),
         disrel = as.factor(disrel)) 
  

# Create comparable categories across rounds 
df1 <- df1 %>%
  mutate(across(c("robbery","cttlthft","othrprb1","othrprb2"), as.character),
         thfcrm = ifelse((robbery == "no" & cttlthft == "no"), "no", "yes"), 
         forcrm = ifelse((othrprb1 == "88" & othrprb2 == "88"), "no", "yes"), # only options are yes and 88
         across(c("thfcrm","forcrm"), as.factor))
# Note: cttlthft is completely nested within robbery 

df1 <- select(df1, -c(robbery,cttlthft,othrprb1,othrprb2))


# Public transportation (creating something similar to pubtran1)
df1 <- df1 %>% 
  mutate(across(c("trans1","trans2","trans3"), as.character),
         across(c("trans1","trans2","trans3"), ~replace(., . %in% c("bus","micro/combi","mototaxi"), "yes")),
         across(c("trans1","trans2","trans3"), ~replace(., . %in% c("by foot","car","other","animal (horse/donkey)",
                                                                    "bicycle"), "no"))) %>%
  replace_na(list(trans1 = "missing", trans2 = "missing", trans3 = "missing")) %>%
  mutate(pubtran1 = ifelse(c((trans1 == "no" | trans2 == "no" | trans3 == "no") &
                               (trans1 != "yes" | trans2 != "yes" | trans3 != "yes")), "no", "yes"),
         pubtran1 = ifelse((trans1 == "missing" & trans2 == "missing" & trans3 == "missing"), "missing", pubtran1),
         pubtran1 = as.factor(pubtran1))
  
df1 <- select(df1, -c(trans1,trans2,trans3))

#------------------------------------------------------------------------------
#### 6. WRITE INTERMEDIATE FILE              ####
#------------------------------------------------------------------------------
saveRDS(df1, here("data", "intermediate", survey, paste0(round, "_", country, ".rds")))
# RDS maintains factor labeling
