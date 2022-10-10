# File: Peru community survey round 3
# Date: 22/03/2022
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
country <- "pe"
linkage_vars <- "placeid"

vars_i_want <- c("disaster","ntrldist",paste0("recvhlp", 1:3),"popsize",
                 "robbery","cttlthft","vlntcrme","gangs","prstitn","janacc",
                 "febacc","maracc","apracc","mayacc","junacc","julacc",
                 "augacc","sepacc","octacc","novacc","decacc","socwrkr",
                 "ltrcycmp",paste0("trans", 1:2))

com_r3_pe <- clean(survey = survey, round = round, country = country, vars = vars_i_want,
                   linkage_vars = linkage_vars) 

#------------------------------------------------------------------------------
#### 2. PERFORMING A MANUAL CHECK ####
#------------------------------------------------------------------------------
summary(com_r3_pe$df) # a summary of the final clean data
com_r3_pe$vars_needing_careful_manual_check
com_r3_pe$vars_not_found
dd <- com_r3_pe$data_dictionary # vars included above as vars_i_want
dd_discards <- com_r3_pe$discarded_data_dictionary # vars not included

#------------------------------------------------------------------------------
#### 3. CONVERT TO TIBBLE FOR DATA CLEANING/MANIPULATION    ####
#------------------------------------------------------------------------------
df1 <- as_tibble(com_r3_pe$df)
rm(com_r3_pe) #clean environment

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
         sochel = socwrkr,
         adultlit = ltrcycmp)

#------------------------------------------------------------------------------
#### 5. CLEAN/MANIPULATE REST OF VARS    ####
#------------------------------------------------------------------------------
# Clean labels and remove skips
df1 <- df1 %>%
  mutate(across(c("anydis","ntrldist"), as.character),
         anydis = ifelse(ntrldist == "0", "none", anydis), #creates no disaster
         anydis = ifelse(ntrldist == "1" & is.na(anydis), "unknown", anydis), #fills in missing values
         anydis = ifelse(anydis == "other specify", "other", anydis),
         anydis = ifelse(anydis == "mud avalanche/slide", "avalanche/mud slide", anydis),
         anydis = as.factor(anydis))

df1 <- select(df1, -c(ntrldist))


# Disaster relief received
df1 <- df1 %>% 
  mutate(across(c("disrel1","disrel2","disrel3"), as.character),
                  across(c("disrel1","disrel2","disrel3"), ~replace(., . %in% c("other institutions","yes, to friends and family",
                                                                                "yes, to the govt"), "yes"))) %>%
           replace_na(list(disrel1 = "missing", disrel2 = "missing", disrel3 = "missing")) %>%
           mutate(disrel = ifelse(c((disrel1 == "no" | disrel2 == "no" | disrel3 == "no") &
                                      (disrel1 != "yes" | disrel2 != "yes" | disrel3 != "yes")), "no", "yes"),
                  disrel = ifelse((disrel1 == "missing" & disrel2 == "missing" & disrel3 == "missing"), "missing", disrel),
                  disrel = ifelse(anydis == "none", NA, disrel), #NA to properly correspond if no disaster
                  disrel = ifelse(commid == "pe18c02", "yes", disrel), #manual re-coding of errors
                  disrel = ifelse(commid == "pe18c06", "yes", disrel), 
                  disrel = as.factor(disrel))

df1 <- select(df1, -c(disrel1,disrel2,disrel3))


# Create comparable category across rounds (thfcrm)
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
         across(all_of(months_year),~replace(., . %in% c("99"), NA)),
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
         across(c("trans1","trans2"), ~replace(., . %in% c("bus","mototaxi","micro/combi"), "yes")),
         across(c("trans1","trans2"), ~replace(., . %in% c("by foot","truck",
                                                           "motorcycle","car"), "no"))) %>%
  replace_na(list(trans1 = "missing", trans2 = "missing")) %>%
  mutate(pubtran1 = ifelse(c((trans1 == "no" | trans2 == "no") & (trans1 != "yes" | trans2 != "yes")), "no", "yes"),
         pubtran1 = ifelse((trans1 == "missing" & trans2 == "missing"), "missing", pubtran1),
         pubtran1 = as.factor(pubtran1))
         
df1 <- select(df1, -c(trans1,trans2))

#------------------------------------------------------------------------------
#### 7. FIX COMMID             ####
#------------------------------------------------------------------------------
df1 <- df1 %>%
  mutate(commid = as.character(commid)) #to avoid error when joining across rounds

#------------------------------------------------------------------------------
#### 8. WRITE INTERMEDIATE FILE              ####
#------------------------------------------------------------------------------
saveRDS(df1, here("data", "intermediate", survey, paste0(round, "_", country, ".rds")))
# RDS maintains factor labeling
