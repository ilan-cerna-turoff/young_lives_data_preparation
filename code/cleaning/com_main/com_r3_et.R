# File: Ethiopia community survey round 3
# Date: 18/03/2022
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
round <- "r3"
country <- "et"
linkage_vars <- "placeid"

vars_i_want <- c("disaster",paste0("recvhlp", 1:3),"popsize",paste0("trans", 1:3),
                 "robbery","cttlthft","vlntcrme","gangs","prstitn",paste0("othrprb", 1:2),
                 "socwrkr","commigr","ltrcycmp")

com_r3_et <- clean(survey = survey, round = round, country = country, vars = vars_i_want,
                   linkage_vars = linkage_vars) 

#------------------------------------------------------------------------------
#### 2. PERFORMING A MANUAL CHECK ####
#------------------------------------------------------------------------------
summary(com_r3_et$df) # a summary of the final clean data
com_r3_et$vars_needing_careful_manual_check
com_r3_et$vars_not_found
dd <- com_r3_et$data_dictionary # vars included above as vars_i_want
dd_discards <- com_r3_et$discarded_data_dictionary # vars not included

#------------------------------------------------------------------------------
#### 3. CONVERT TO TIBBLE FOR DATA CLEANING/MANIPULATION    ####
#------------------------------------------------------------------------------
df1 <- as_tibble(com_r3_et$df)
rm(com_r3_et) #clean environment

#------------------------------------------------------------------------------
#### 4. VARS NAMES TO AGREE WITH PAST ROUND     ####
#------------------------------------------------------------------------------
df1 <- df1 %>%
  rename(commid = placeid,
         anydis = disaster,
         disrel1 = recvhlp1,
         disrel2 = recvhlp2,
         disrel3 = recvhlp3,
         pop = popsize,
         violcrm = vlntcrme,
         yuthcrm = gangs,
         proscrm = prstitn,
         movwork = commigr,
         sochel = socwrkr,
         adultlit = ltrcycmp)

#------------------------------------------------------------------------------
#### 5. CLEAN/MANIPULATE REST OF VARS    ####
#------------------------------------------------------------------------------
# Clean labels and remove skips
df1 <- df1 %>% 
  mutate(anydis = as.character(anydis)) %>%
  replace_with_na(replace = list(anydis = c("88","disease epidemics affecting humans", # remove skip and non-natural disasters from question
                                            "crop failure due to pest/disease",
                                            "disease epidemics affecting animals"))) %>%
  mutate(anydis = ifelse(anydis == "mud avalanche/slide", "avalanche/mud slide", anydis), # consistent labeling across rounds
         anydis = ifelse(anydis == "no disaster occured", "none", anydis),
         anydis = as.factor(anydis))


# Disaster relief received
df1 <- df1 %>% 
  mutate(across(c("disrel1","disrel2","disrel3"), as.character),
         across(c("disrel1","disrel2","disrel3"), ~replace(., . %in% c("other institutions","yes, friends and family",
                                                                       "yes, the govt","yes, ngos working in the locality"), "yes"))) %>%
  replace_na(list(disrel1 = "missing", disrel2 = "missing", disrel3 = "missing")) %>%
  mutate(disrel = ifelse(c((disrel1 == "no" | disrel2 == "no" | disrel3 == "no") &
                             (disrel1 != "yes" | disrel2 != "yes" | disrel3 != "yes")), "no", "yes"),
         disrel = ifelse((disrel1 == "missing" & disrel2 == "missing" & disrel3 == "missing"), "missing", disrel),
         disrel = ifelse(anydis == "none", NA, disrel), #NA to properly correspond if no disaster
         disrel = ifelse(commid == "et1021", "yes", disrel), #manual re-coding of errors
         disrel = ifelse(commid == "et2041", "yes", disrel),
         disrel = ifelse(commid == "et2051", "no", disrel),
         disrel = ifelse(commid == "et4121", "no", disrel),
         disrel = ifelse(commid == "et4131", "yes", disrel),
         disrel = ifelse(commid == "et4141", "yes", disrel),
         disrel = ifelse(commid == "et4151", "yes", disrel),
         disrel = ifelse(commid == "et4161", "no", disrel),
         disrel = ifelse(commid == "et4162", "no", disrel),
         disrel = ifelse(commid == "et5181", "yes", disrel),
         disrel = ifelse(commid == "et5191", "yes", disrel),
         disrel = ifelse(commid == "et5201", "no", disrel),
         disrel = as.factor(disrel))
        
df1 <- select(df1, -c(disrel1,disrel2,disrel3))


# Create comparable category of crime types and groups against crime
df1 <- df1 %>%
  mutate(across(c("robbery","cttlthft","othrprb1","othrprb2"), as.character),
         thfcrm = ifelse((robbery == "no" & cttlthft == "no"), "no", "yes")) %>%
  replace_na(list(othrprb1 = "missing", othrprb2 = "missing")) %>%
  mutate(forcrm = ifelse(c((othrprb1 == "no" | othrprb2 == "no") &
                             (othrprb1 != "yes" | othrprb2 != "yes")), "no", "yes"),
         forcrm = ifelse((othrprb1 == "missing" & othrprb2 == "missing"), "missing", forcrm),
         across(c("thfcrm","forcrm"), as.factor))

df1 <- select(df1, -c(robbery,cttlthft,othrprb1,othrprb2))


# Public transportation (creating something similar to pubtran1)
df1 <- df1 %>% 
  mutate(across(c("trans1","trans2","trans3"), as.character),
         across(c("trans1","trans2","trans3"), ~replace(., . %in% c("bus","micro/combi","mototaxi"), "yes")),
         across(c("trans1","trans2","trans3"), ~replace(., . %in% c("by foot","car","other",
                                                                    "animal (horse/donkey)","bicycle"), "no"))) %>%
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
