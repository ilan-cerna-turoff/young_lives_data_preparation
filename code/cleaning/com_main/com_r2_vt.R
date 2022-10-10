# File: Vietnam community survey round 2
# Date: 14/03/2022
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
country <- "vt"
linkage_vars <- "commid"

vars_i_want <- c("disaster","recvhelp","popsize","robbery","cttlthft","vlntcrme",
                 "gangs","prstitn","janacc","febacc","maracc","apracc","mayacc",
                 "junacc","julacc","augacc","sepacc","octacc","novacc","decacc",
                 paste0("trans", 1:3))

com_r2_vt <- clean(survey = survey, round = round, country = country, vars = vars_i_want,
                   linkage_vars = linkage_vars) 

#------------------------------------------------------------------------------
#### 2. PERFORMING A MANUAL CHECK ####
#------------------------------------------------------------------------------
summary(com_r2_vt$df) # a summary of the final clean data
com_r2_vt$vars_needing_careful_manual_check
com_r2_vt$vars_not_found
dd <- com_r2_vt$data_dictionary # vars included above as vars_i_want
dd_discards <- com_r2_vt$discarded_data_dictionary # vars not included

#------------------------------------------------------------------------------
#### 3. CONVERT TO TIBBLE FOR DATA CLEANING/MANIPULATION    ####
#------------------------------------------------------------------------------
df1 <- as_tibble(com_r2_vt$df)
rm(com_r2_vt) #clean environment

#------------------------------------------------------------------------------
#### 4. VARS NAMES TO AGREE WITH PAST ROUND     ####
#------------------------------------------------------------------------------
df1 <- df1 %>%
  rename(anydis = disaster,
         disrel = recvhelp,
         pop = popsize,
         violcrm = vlntcrme,
         yuthcrm = gangs,
         proscrm = prstitn)

#------------------------------------------------------------------------------
#### 5. CLEAN/MANIPULATE REST OF VARS    ####
#------------------------------------------------------------------------------
# Clean labels and remove skips
df1 <- df1 %>% 
  mutate(across(c("anydis","disrel"), as.character),
         anydis = str_trim(anydis, side = "both")) %>%
  replace_with_na(replace = list(anydis = c("crop failure due to pest/disease", # removes not natural disasters
                                            "disease epidemics affecting animals",
                                            "disease epidemics affecting humans"))) %>%
  mutate(anydis = ifelse(anydis == "no disaster occured", "none", anydis), # creates no natural disaster E+
         anydis = ifelse(anydis == "mud avalanche/slide", "avalanche/mud slide", anydis), # label comparability across rounds
         across(c("disrel"), ~replace(., . %in% c("other institutions","yes, friends and family",
                                                  "yes, ngos working in the locality","yes, the govt"), "yes")), #corresponding NA if no disaster occurred
    disrel = ifelse(anydis == "none", NA, disrel),
    disrel = ifelse(commid == "vn005", "yes", disrel), #manual re-coding of errors
    disrel = ifelse(commid == "vn010", "yes", disrel),
    disrel = ifelse(commid == "vn011", "yes", disrel),
    disrel = ifelse(commid == "vn014", "yes", disrel),
    disrel = ifelse(commid == "vn016", "yes", disrel),
    disrel = ifelse(commid == "vn019", "yes", disrel),
    disrel = ifelse(commid == "vn020", "no", disrel),
    disrel = ifelse(commid == "vn021", "yes", disrel),
    disrel = ifelse(commid == "vn032", "yes", disrel),
    disrel = ifelse(commid == "vn034", "yes", disrel),
    across(c("disrel","anydis"), as.factor))


# Create comparable category of crime types and groups against crime
df1 <- df1 %>%
  mutate(across(c("robbery","cttlthft"), as.character),
         thfcrm = ifelse((robbery == "no" & cttlthft == "no"), "no", "yes"),
         thfcrm = as.factor(thfcrm)) 

df1 <- select(df1, -c(robbery,cttlthft))


# Number of months per year inaccessible by vehicle
months_year <- c("janacc","febacc","maracc","apracc","mayacc", "junacc","julacc",
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
  mutate(across(c("trans1","trans2","trans3"), as.character),
         across(c("trans1","trans2","trans3"), ~replace(., . %in% c("bus","micro/combi","mototaxi"), "yes")),
         across(c("trans1","trans2","trans3"), ~replace(., . %in% c("bicycle","by foot",
                                                                    "more than one mean of transportation used",
                                                                    "motorcycle","animal (horse/donkey)","car","other","boat"), "no"))) %>%
  replace_na(list(trans1 = "missing", trans2 = "missing", trans3 = "missing")) %>%
  mutate(pubtran1 = ifelse(c((trans1 == "no" | trans2 == "no" | trans3 == "no") &
                               (trans1 != "yes" | trans2 != "yes" | trans3 != "yes")), "no", "yes"),
         pubtran1 = ifelse((trans1 == "missing" & trans2 == "missing" & trans3 == "missing"), "missing", pubtran1),
         pubtran1 = as.factor(pubtran1))

df1 <- select(df1, -c(trans1,trans2,trans3))

#------------------------------------------------------------------------------
#### 6. RENAME COMMID FOR CONSISTENCY             ####
#------------------------------------------------------------------------------
df1 <- df1 %>% 
  mutate(commid = str_replace(commid, "vn", "vt"))

#------------------------------------------------------------------------------
#### 7. WRITE INTERMEDIATE FILE              ####
#------------------------------------------------------------------------------
saveRDS(df1, here("data", "intermediate", survey, paste0(round, "_", country, ".rds")))
# RDS maintains factor labeling
