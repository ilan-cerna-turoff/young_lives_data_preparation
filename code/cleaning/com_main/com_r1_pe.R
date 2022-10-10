# File: Peru community survey round 1
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
country <- "pe"
linkage_vars <- "comnro"

vars_i_want <- c("commid","clustid","anydis1","disrel","pop","subzone",
                 paste0("plang", 1:5),"thfcrm","violcrm","yuthcrm",
                 "proscrm",paste0("plang", 6:7),"frqpass","ppubtran1",
                 "sochel","adultlit","prel1",paste0("prel", 5:10))

com_r1_pe <- clean(survey = survey, round = round, country = country, vars = vars_i_want,
                   linkage_vars = linkage_vars) 

# Note: only comnro is common across datafiles, but commid is needed for linkage
# to other rounds

#------------------------------------------------------------------------------
#### 2. PERFORMING A MANUAL CHECK ####
#------------------------------------------------------------------------------
summary(com_r1_pe$df) # a summary of the final clean data
com_r1_pe$vars_needing_careful_manual_check
com_r1_pe$vars_not_found
dd <- com_r1_pe$data_dictionary # vars included above as vars_i_want
dd_discards <- com_r1_pe$discarded_data_dictionary # vars not included

#------------------------------------------------------------------------------
#### 3. CONVERT TO TIBBLE FOR DATA CLEANING/MANIPULATION    ####
#------------------------------------------------------------------------------
df1 <- as_tibble(com_r1_pe$df)
rm(com_r1_pe) #clean environment

#------------------------------------------------------------------------------
#### 4. RENAME VARS FOR MERGING WITH FUTURE ROUNDS     ####
#------------------------------------------------------------------------------
df1 <- df1 %>%
  rename(geographic = clustid,
         anydis = anydis1,
         pubtran1 = ppubtran1)

#------------------------------------------------------------------------------
#### 5. CLEAN/MANIPULATE REST OF VARS    ####
#------------------------------------------------------------------------------
numberfunction <- function(x){ifelse(x=="yes", 1,0)} #makes yes and no into 1,0


# Clean labels and skips 
df1 <- df1 %>% 
  mutate(across(c("anydis","disrel"), as.character),
         anydis = ifelse(anydis == "no disaster", "none", anydis), # creates the no disaster category
         anydis = ifelse(anydis == "avalanche, rock fall, mudslide", "avalanche/mud slide", anydis), # make labels comparable across rounds
         anydis = ifelse(anydis == "cyclone, tornado, hurricane", "cyclone/tornado/hurricane", anydis), 
         anydis = ifelse(anydis == "floods", "flood", anydis), 
         anydis = ifelse(anydis == "snow/ice/extreme cold", "frost/cold front", anydis)) %>%
  replace_with_na(replace = list(disrel = c("7"))) %>% 
  mutate(disrel = ifelse(anydis == "none", NA, disrel), #NA to properly correspond if no disaster
         disrel = ifelse(commid == "pe17c02", "yes", disrel), #manual re-coding of errors
         disrel = ifelse(commid == "pe03c08", "yes", disrel), 
         disrel = ifelse(commid == "pe19c04", "yes", disrel), 
         disrel = ifelse(commid == "pe19c03", "yes", disrel), 
         disrel = ifelse(commid == "pe19c02", "yes", disrel), 
         disrel = ifelse(commid == "pe19c01", "yes", disrel), 
         disrel = ifelse(commid == "pe18c01", "no", disrel), 
         disrel = ifelse(commid == "pe17c08", "no", disrel), 
         disrel = ifelse(commid == "pe17c07", "no", disrel), 
         disrel = ifelse(commid == "pe17c04", "yes", disrel), 
         disrel = ifelse(commid == "pe17c03", "yes", disrel), 
         disrel = ifelse(commid == "pe16c06", "no", disrel), 
         disrel = ifelse(commid == "pe16c04", "yes", disrel), 
         disrel = ifelse(commid == "pe16c03", "no", disrel), 
         disrel = ifelse(commid == "pe15c05", "no", disrel), 
         disrel = ifelse(commid == "pe13c01", "yes", disrel), 
         disrel = ifelse(commid == "pe05c08", "yes", disrel), 
         disrel = ifelse(commid == "pe05c06", "yes", disrel), 
         disrel = ifelse(commid == "pe05c03", "no", disrel), 
         disrel = ifelse(commid == "pe04c06", "yes", disrel), 
         disrel = ifelse(commid == "pe04c01", "no", disrel), 
         disrel = ifelse(commid == "pe03c07", "no", disrel), 
         disrel = ifelse(commid == "pe03c06", "yes", disrel), 
         disrel = ifelse(commid == "pe03c05", "no", disrel), 
         disrel = ifelse(commid == "pe03c04", "no", disrel), 
         disrel = ifelse(commid == "pe03c03", "no", disrel), 
         disrel = ifelse(commid == "pe03c01", "no", disrel), 
         disrel = ifelse(commid == "pe02c04", "no", disrel), 
         disrel = ifelse(commid == "pe02c03", "yes", disrel), 
         disrel = ifelse(commid == "pe02c01", "yes", disrel), 
         disrel = ifelse(commid == "pe01c01", "yes", disrel), 
         across(c("anydis","disrel"), as.factor))


# Count of religious groups in community
df1 <- df1 %>% 
  mutate(across(c("prel1","prel5","prel6","prel7","prel8","prel9","prel10"), as.character),
         across(c("prel1","prel5","prel6","prel7","prel8","prel9","prel10"), ~replace(., . %in% c("the most practiced",
                                                                                                  "the second most practiced","the third",
                                                                                                  "the fourth","very little/very few people"), "yes")),
         across(c("prel1","prel5","prel6","prel7","prel8","prel9","prel10"), ~replace(., . %in% c("no one practices this religion"), "no")),
         across(c("prel1","prel5","prel6","prel7","prel8","prel9","prel10"), numberfunction),
         across(c("prel1","prel5","prel6","prel7","prel8","prel9","prel10"), ~replace(., . %in% c(NA), "0")), #makes sure count rows where there is NAs
         across(c("prel1","prel5","prel6","prel7","prel8","prel9","prel10"), as.numeric)) %>% 
  rowwise() %>% 
  mutate(relcomp = sum(prel1,prel5,prel6,prel7,prel8,prel9,prel10)) %>%
  replace_with_na(replace = list(relcomp = c(0))) #0s are actually rows with all NAs since mutated above for the count

# Note: prel2...4 are 100% no

df1 <- select(df1, -c(prel1,prel5,prel6,prel7,prel8,prel9,prel10))


# Count of languages spoken in community
df1 <- df1 %>%
  mutate(across(c("plang1","plang2","plang3","plang4","plang5"), as.character),
         across(c("plang1","plang2","plang3","plang4","plang5"), ~replace(., . %in% c("the most spoken", "the second most commonly spoken",
                                                                                      "the third most commonly spoken"), "yes")),
         across(c("plang1","plang2","plang3","plang4","plang5"), ~replace(., . %in% c("not mentioned"), "no")),
         across(c("plang1","plang2","plang3","plang4","plang5"), numberfunction),
         across(c("plang1","plang2","plang3","plang4","plang5"), ~replace(., . %in% c(NA), "0")),
         othlang = ifelse(c(plang4 == 0 & plang5 == 0), 0, 1),
         across(c("plang1","plang2","plang3","plang4","plang5"), as.numeric)) %>%
 rowwise() %>%
 mutate(langcomp = sum(plang1,plang2,plang3,othlang)) %>%
  replace_with_na(replace = list(langcomp = c(0))) #0s are actually rows with all NAs since mutated above for the count

# Note: no ethnicity so only comparable category

df1 <- select(df1, -c(plang1,plang2,plang3,plang4,plang5,othlang))

#------------------------------------------------------------------------------
#### 6. SELECT COMMON VARIABLES ACROSS ROUNDS AND FIX COMMID     ####
#------------------------------------------------------------------------------
df1 <- select(df1, c(commid,anydis,disrel,geographic,pop,relcomp,langcomp,
                     frqpass,thfcrm,violcrm,yuthcrm,proscrm,subzone,sochel,
                     adultlit,pubtran1)) %>%
  mutate(commid = as.character(commid)) #to avoid error when joining across rounds

#------------------------------------------------------------------------------
#### 7. WRITE INTERMEDIATE FILE              ####
#------------------------------------------------------------------------------
saveRDS(df1, here("data", "intermediate", survey, paste0(round, "_", country, ".rds")))
# RDS maintains factor labeling
             