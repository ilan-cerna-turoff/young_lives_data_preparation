# File: Peru community survey round 2
# Date: 07/03/2022
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
country <- "pe"
linkage_vars <- "placeid"

vars_i_want <- c("disaster","recvhelp","popsize","robbery","cttlthft","vlntcrme",
                 "gangs","prstitn","janacc","febacc","maracc","apracc","mayacc",
                 "junacc","julacc","augacc","sepacc","octacc","novacc","decacc",
                 paste0("trans", 1:2),"socwrkr","ltrcycmp")
      
com_r2_pe <- clean(survey = survey, round = round, country = country, vars = vars_i_want,
                   linkage_vars = linkage_vars) 
                    
#------------------------------------------------------------------------------
#### 2. PERFORMING A MANUAL CHECK ####
#------------------------------------------------------------------------------
summary(com_r2_pe$df) # a summary of the final clean data
com_r2_pe$vars_needing_careful_manual_check
com_r2_pe$vars_not_found
dd <- com_r2_pe$data_dictionary # vars included above as vars_i_want
dd_discards <- com_r2_pe$discarded_data_dictionary # vars not included

#------------------------------------------------------------------------------
#### 3. CONVERT TO TIBBLE FOR DATA CLEANING/MANIPULATION    ####
#------------------------------------------------------------------------------
df1 <- as_tibble(com_r2_pe$df)
rm(com_r2_pe) #clean environment

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
         sochel = socwrkr,
         adultlit = ltrcycmp)

#------------------------------------------------------------------------------
#### 5. CLEAN/MANIPULATE REST OF VARS    ####
#------------------------------------------------------------------------------
# Clean labels and remove skips
df1 <- df1 %>% 
  mutate(across(c("anydis","disrel"), as.character)) %>% 
  replace_with_na(replace = list(anydis = c("crop failure due to pest/disease (other than drought)","88"), #remove skips and not natural disasters
                                 pop = "1")) %>% # remove skips
  mutate(anydis = ifelse(anydis == "no disaster occured", "none", anydis), 
         anydis = ifelse(anydis == "mud avalanche/slide", "avalanche/mud slide", anydis), 
         across(c("disrel"), ~replace(., . %in% c("yes, to friends and family", # var = disaster relief (yes/no)
                                                  "yes, to the govt"), "yes")),
         disrel = ifelse(anydis == "none", NA, disrel), #NA to properly correspond if no disaster
         disrel = ifelse(commid == "pe04c01", "yes", disrel), #manual re-coding of errors
         disrel = ifelse(commid == "pe04c06", "yes", disrel), 
         disrel = ifelse(commid == "pe15c02", "yes", disrel), 
         disrel = ifelse(commid == "pe04c02", "yes", disrel), 
         disrel = ifelse(commid == "pe04c03", "yes", disrel), 
         disrel = ifelse(commid == "pe04c04", "no", disrel), 
         disrel = ifelse(commid == "pe04c05", "yes", disrel), 
         disrel = ifelse(commid == "pe04c08", "no", disrel), 
         disrel = ifelse(commid == "pe04c10", "no", disrel), 
         disrel = ifelse(commid == "pe05c08", "yes", disrel), 
         disrel = ifelse(commid == "pe09c01", "no", disrel), 
         disrel = ifelse(commid == "pe11c05", "yes", disrel), 
         disrel = ifelse(commid == "pe11c06", "yes", disrel), 
         disrel = ifelse(commid == "pe15c03", "yes", disrel), 
         disrel = ifelse(commid == "pe15c07", "yes", disrel), 
         disrel = ifelse(commid == "pe17c03", "yes", disrel), 
         disrel = ifelse(commid == "pe18c04", "no", disrel), 
         disrel = ifelse(commid == "pe20c02", "no", disrel), 
         disrel = ifelse(commid == "pe80c05", "yes", disrel), 
         across(c("anydis","disrel"), as.factor))


# Create comparable category of crime types and groups against crime
df1 <- df1 %>%
  mutate(across(c("robbery", "cttlthft"=), as.character),
         thfcrm = ifelse((robbery == "no" & cttlthft == "no"), "no", "yes"),
         thfrcrm = as.factor(thfcrm))

df1 <- select(df1, -c(robbery,cttlthft))


# Number of months per year inaccessible by vehicle
months_year <- c("janacc","febacc","maracc","apracc","mayacc","junacc","julacc",
                 "augacc","sepacc","octacc","novacc","decacc")

df1 <- df1 %>% 
  mutate(across(all_of(months_year), as.character), 
         across(all_of(months_year), ~replace(., . %in% c("month in which it was accessible"), "1")),
         across(all_of(months_year), ~replace(., . %in% c("month in which it was not accessible"), "0")),
         across(all_of(months_year), as.numeric)) %>% 
  rowwise() %>% 
  mutate(frqpass = sum(janacc,febacc,maracc,apracc,mayacc,junacc,julacc,augacc,
                       sepacc,octacc,novacc,decacc))

df1 <- select(df1, -c(janacc,febacc,maracc,apracc,mayacc,junacc,julacc,augacc,
                      sepacc,octacc,novacc,decacc))


# Public transportation (creating something similar to pubtran1)
df1 <- df1 %>% 
  mutate(across(c("trans1","trans2"), as.character),
         across(c("trans1","trans2"), ~replace(., . %in% c("bus","micro/combi","mototaxi"), "yes")),
         across(c("trans1","trans2"), ~replace(., . %in% c("animal (horse/donkey)","bicycle","boat",
                                                           "by foot","car","other"), "no"))) %>%
  replace_na(list(trans1 = "missing", trans2 = "missing")) %>%
  mutate(pubtran1 = ifelse(c((trans1 == "no" | trans2 == "no") & (trans1 != "yes" | trans2 != "yes")), "no", "yes"),
         pubtran1 = ifelse((trans1 == "missing" & trans2 == "missing"), "missing", pubtran1),
         pubtran1 = as.factor(pubtran1))

df1 <- select(df1, -c(trans1,trans2))

#------------------------------------------------------------------------------
#### 6. FIX COMMID             ####
#------------------------------------------------------------------------------
df1 <- df1 %>%
  mutate(commid = as.character(commid)) #to avoid error when joining across rounds

#------------------------------------------------------------------------------
#### 7. WRITE INTERMEDIATE FILE              ####
#------------------------------------------------------------------------------
saveRDS(df1, here("data", "intermediate", survey, paste0(round, "_", country, ".rds")))
# RDS maintains factor labeling
