# File: India community survey round 3
# Date: 19/03/2022
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
country <- "in"
linkage_vars <- "placeid"

vars_i_want <- c("disaster","recvhelp",paste0("rechelp", 2:3),"popsize","robbery",
                 "cttlthft","vlntcrme","gangs","prstitn","janacc","febacc","maracc",
                 "apracc","mayacc","junacc","julacc","augacc","sepacc","octacc",
                 "novacc","decacc","socwrkr","ltrcycmp",paste0("trans", 1:3))

com_r3_in <- clean(survey = survey, round = round, country = country, vars = vars_i_want,
                   linkage_vars = linkage_vars) 

#------------------------------------------------------------------------------
#### 2. PERFORMING A MANUAL CHECK ####
#------------------------------------------------------------------------------
summary(com_r3_in$df) # a summary of the final clean data
com_r3_in$vars_needing_careful_manual_check
com_r3_in$vars_not_found
dd <- com_r3_in$data_dictionary # vars included above as vars_i_want
dd_discards <- com_r3_in$discarded_data_dictionary # vars not included

#------------------------------------------------------------------------------
#### 3. CONVERT TO TIBBLE FOR DATA CLEANING/MANIPULATION    ####
#------------------------------------------------------------------------------
df1 <- as_tibble(com_r3_in$df)
rm(com_r3_in) #clean environment

#------------------------------------------------------------------------------
#### 4. VARS NAMES TO AGREE WITH PAST ROUND     ####
#------------------------------------------------------------------------------
df1 <- df1 %>%
  rename(commid = placeid,
         anydis = disaster,
         disrel1 = recvhelp,
         disrel2 = rechelp2,
         disrel3 = rechelp3,
         pop = popsize,
         violcrm = vlntcrme,
         yuthcrm = gangs,
         proscrm = prstitn,
         sochel = socwrkr,
         adultlit = ltrcycmp)

#------------------------------------------------------------------------------
#### 5. CLEAN/MANIPULATE REST OF VARS    ####
#------------------------------------------------------------------------------
# Clean labels and remove skips
df1 <- df1 %>% 
  mutate(anydis = as.character(anydis)) %>% 
  replace_with_na(replace = list(anydis = c("disease epidemics affecting humans", 
                                            "crop failure due to pest/disease",
                                            "disease epidemics affecting animals"))) %>%
  mutate(anydis = ifelse(anydis == "no disaster occured", "none", anydis), #clean label for consistency
         anydis = ifelse(commid == "in072" & anydis == "none", NA, anydis), #manual re-coding of errors
         anydis = ifelse(commid == "in074" & anydis == "none", NA, anydis),
         anydis = ifelse(commid == "in075" & anydis == "none", NA, anydis),
         anydis = ifelse(commid == "in083" & anydis == "none", NA, anydis),
         anydis = ifelse(commid == "in086" & anydis == "none", NA, anydis),
         anydis = ifelse(commid == "in087" & anydis == "none", NA, anydis),
         anydis = ifelse(commid == "in093" & anydis == "none", NA, anydis),
         anydis = ifelse(commid == "in099" & anydis == "none", NA, anydis),
         anydis = ifelse(commid == "in084" & anydis == "none", NA, anydis), 
         anydis = as.factor(anydis))


# General disaster relief received
df1 <- df1 %>% 
  mutate(across(c("disrel1","disrel2","disrel3"), as.character),
         across(c("disrel1","disrel2","disrel3"), ~replace(., . %in% c("other institutions","yes, friends and family",
                                                                       "yes, the govt","yes, ngos working in the locality"), "yes"))) %>%
  replace_na(list(disrel1 = "missing", disrel2 = "missing", disrel3 = "missing")) %>%
  mutate(disrel = ifelse(c((disrel1 == "no" | disrel2 == "no" | disrel3 == "no") &
                             (disrel1 != "yes" | disrel2 != "yes" | disrel3 != "yes")), "no", "yes"),
         disrel = ifelse((disrel1 == "missing" & disrel2 == "missing" & disrel3 == "missing"), "missing", disrel),
         disrel = ifelse(anydis == "none", NA, disrel), #NA to properly correspond if no disaster
         disrel = ifelse(commid == "in084", "yes", disrel), #manual re-coding of errors
         disrel = ifelse(commid == "in072", "no", disrel),
         disrel = ifelse(commid == "in074", "no", disrel),
         disrel = ifelse(commid == "in075", "no", disrel),
         disrel = ifelse(commid == "in083", "yes", disrel),
         disrel = ifelse(commid == "in086", "no", disrel),
         disrel = ifelse(commid == "in087", "yes", disrel),
         disrel = ifelse(commid == "in093", "no", disrel),
         disrel = ifelse(commid == "in099", "yes", disrel),
         disrel = as.factor(disrel))

df1 <- select(df1, -c(disrel1,disrel2,disrel3))


# Create comparable category of crime types and groups against crime
df1 <- df1 %>%
  mutate(across(c("robbery","cttlthft"), as.character),
         thfcrm = ifelse((robbery == "no" & cttlthft == "no"), "no", "yes"),
         thfcrm = as.factor(thfcrm)) 

df1 <- select(df1, -c(robbery,cttlthft))


# Number of months per year inaccessible by vehicle
months_year <- c("janacc","febacc","maracc","apracc","mayacc","junacc","julacc",
                 "augacc","sepacc","octacc","novacc","decacc")

df1 <- df1 %>% 
  mutate(across(all_of(months_year), as.character),
         across(all_of(months_year), ~replace(., . %in% c("accessible"), "1")),
         across(all_of(months_year), ~replace(., . %in% c("not accessible"), "0")),
         across(all_of(months_year), as.numeric)) %>% 
  rowwise() %>% 
  mutate(frqpass = sum(janacc,febacc,maracc,apracc,mayacc,junacc,julacc,augacc,
                       sepacc,octacc,novacc,decacc))

df1 <- select(df1, -c(janacc,febacc,maracc,apracc,mayacc,junacc,julacc,augacc,
                      sepacc,octacc,novacc,decacc))


# Public transportation (creating something similar to pubtran1)
df1 <- df1 %>% 
  mutate(across(c("trans1","trans2","trans3"), as.character),
         across(c("trans1","trans2","trans3"), ~replace(., . %in% c("bus","mototaxi (any auto)","rail","micro/combi"), "yes")),
         across(c("trans1","trans2","trans3"), ~replace(., . %in% c("by foot","more than one mean of transportation used","truck",
                                                                    "motorcycle","car/jeep","bicycle"), "no"))) %>%
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
