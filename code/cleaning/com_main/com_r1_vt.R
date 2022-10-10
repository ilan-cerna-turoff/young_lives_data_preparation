# File: Vietnam community survey round 1
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
country <- "vt"
linkage_vars <- "commid"

vars_i_want <- c("anydis","disrel",paste0("rel", 1:6),"rel8","region","pop",
                 "city","subzone",paste0("eth", 1:2),paste0("eth", 4:9),
                 paste0("lang", 1:2),paste0("lang", 4:9),"thfcrm","violcrm",
                 "yuthcrm","proscrm","frqpass",paste0("pubtran", 2:9),
                 "v4031610","v4031611")

com_r1_vt <- clean(survey = survey, round = round, country = country, vars = vars_i_want,
                   linkage_vars = linkage_vars) 

#------------------------------------------------------------------------------
#### 2. PERFORMING A MANUAL CHECK ####
#------------------------------------------------------------------------------
summary(com_r1_vt$df) # a summary of the final clean data
com_r1_vt$vars_needing_careful_manual_check
com_r1_vt$vars_not_found
dd <- com_r1_vt$data_dictionary # vars included above as vars_i_want
dd_discards <- com_r1_vt$discarded_data_dictionary # vars not included

#------------------------------------------------------------------------------
#### 3. CONVERT TO TIBBLE FOR DATA CLEANING/MANIPULATION    ####
#------------------------------------------------------------------------------
df1 <- as_tibble(com_r1_vt$df)
rm(com_r1_vt) #clean environment

#------------------------------------------------------------------------------
#### 4. CLEAN/MANIPULATE REST OF VARS    ####
#------------------------------------------------------------------------------
# Consistent naming across rounds/countries
df1 <- rename(df1, geographic = region)


# Natural disaster exposure
df1 <- df1 %>%
  mutate(across(c("anydis","disrel"), as.character), 
         anydis = ifelse(is.na(anydis), "none", anydis), # creates the no disaster category
         anydis = ifelse(anydis == "flooding (lake, river, sea)", "overflowing of river/sea/lake", anydis), # creates the no disaster category
         anydis = ifelse(anydis == "other: specify", "other", anydis),
         disrel = ifelse(anydis == "none", NA, disrel), #corresponding NA if no disaster occurred
         disrel = ifelse(commid == "vn002", "yes", disrel), #manual re-coding of errors
         disrel = ifelse(commid == "vn006", "yes", disrel),
         disrel = ifelse(commid == "vn007", "yes", disrel),
         disrel = ifelse(commid == "vn012", "yes", disrel),
         disrel = ifelse(commid == "vn016", "yes", disrel),
         across(c("anydis","disrel"), as.factor))


# Clean labels and remove skips
df1 <- df1 %>% 
  mutate(across(c("subzone","city"), as.character),
         subzone = ifelse(subzone == "coastal plain.", "coastal plain", subzone), # var = ecological zone
         city = ifelse(city == "2", "no", city),# creates yes/no city
         across(c("subzone","city"), as.factor))


# Accessible by public transit
transit_vars <- c("pubtran2","pubtran3","pubtran4","pubtran5","pubtran6",
                  "pubtran7","pubtran8","pubtran9","v4031610","v4031611")

df1 <- df1 %>% 
  mutate(across(all_of(transit_vars), as.character),
         across(all_of(transit_vars), .fns = ~replace_na(.,"no")),
         pubtran1 = ifelse((pubtran2 == "no" & 
                              pubtran3 == "no" &
                              pubtran4 == "no" & 
                              pubtran5 == "no" &
                              pubtran6 == "no" & 
                              pubtran7 == "no" &
                              pubtran8 == "no" & 
                              pubtran9 == "no" &
                              v4031610 == "no" & 
                              v4031611 == "no"), "no", "yes"),
         pubtran1 = as.factor(pubtran1))

df1 <- select(df1, -c(pubtran2,pubtran3,pubtran4,pubtran5,pubtran6,pubtran7,pubtran8,
                      pubtran9,v4031610,v4031611))


# Count of ethnic groups in community
df1 <- df1 %>% 
  mutate(across(c("eth1","eth2","eth4","eth5","eth6","eth7","eth8","eth9"), as.character),
         across(c("eth1","eth2","eth4","eth5","eth6","eth7","eth8","eth9"), .fns = ~replace_na(.,"0")),
         across(c("eth1","eth2","eth4","eth5","eth6","eth7","eth8","eth9"), ~ifelse(.x == "yes","1","0")),
         across(c("eth1","eth2","eth4","eth5","eth6","eth7","eth8","eth9"), as.numeric)) %>% 
  rowwise() %>% 
  mutate(ethcomp = sum(c(eth1,eth2,eth4,eth5,eth6,eth7,eth8,eth9))) %>%
  replace_with_na(replace = list(ethcomp = c(0))) #0s are actually rows with all NAs since mutated above for the count

df1 <- select(df1, -c(eth1,eth2,eth4,eth5,eth6,eth7,eth8,eth9))


# Count of languages spoken in community
df1 <- df1 %>% 
  mutate(across(c("lang1","lang2","lang4","lang5","lang6","lang7","lang8","lang9"), as.character),
         across(c("lang1","lang2","lang4","lang5","lang6","lang7","lang8","lang9"), .fns = ~replace_na(.,"0")),
         across(c("lang1","lang2","lang4","lang5","lang6","lang7","lang8","lang9"), ~ifelse(.x == "yes","1","0")),
         across(c("lang1","lang2","lang4","lang5","lang6","lang7","lang8","lang9"), as.numeric)) %>% 
  rowwise() %>% 
  mutate(langcomp = sum(c(lang1,lang2,lang4,lang5,lang6,lang7,lang8,lang9))) %>%
  replace_with_na(replace = list(langcomp = c(0))) #0s are actually rows with all NAs since mutated above for the count

df1 <- select(df1, -c(lang1,lang2,lang4,lang5,lang6,lang7,lang8,lang9))


# Count of religious groups in community
df1 <- df1 %>% 
  mutate(across(c("rel1_tblsec2socialenvironment","rel3_tblsec2socialenvironment","rel5","rel6","rel8"), as.character),
         across(c("rel1_tblsec2socialenvironment","rel3_tblsec2socialenvironment","rel5","rel6","rel8"), .fns = ~replace_na(.,"0")),
         across(c("rel1_tblsec2socialenvironment","rel3_tblsec2socialenvironment","rel5","rel6","rel8"), ~ifelse(.x == "yes","1","0")),
         across(c("rel1_tblsec2socialenvironment","rel3_tblsec2socialenvironment","rel5","rel6","rel8"), as.numeric)) %>% 
  rowwise() %>% 
  mutate(relcomp = sum(c(rel1_tblsec2socialenvironment,rel3_tblsec2socialenvironment,rel5,rel6,rel8))) %>%
  replace_with_na(replace = list(relcomp = c(0))) #0s are actually rows with all NAs since mutated above for the count

df1 <- select(df1, -c(rel1_tblsec2socialenvironment,rel2_tblsec2socialenvironment,
                      rel3_tblsec2socialenvironment,rel4_tblsec2socialenvironment,
                      rel5,rel6,rel8,rel1_stblsec1disasters,rel2_stblsec1disasters,
                      rel3_stblsec1disasters,rel4_stblsec1disasters)) 

# Note: rel1...4 var names used for types of disaster relief and religious groups in the same survey.
# We are only using religious groups variables so removing relief related variables
# rel2_tblsec2socialenvironment and rel4_tblsec2socialenvironment are 100% NA. 

#------------------------------------------------------------------------------
#### 5. RENAME COMMID FOR CONSISTENCY             ####
#------------------------------------------------------------------------------
df1 <- df1 %>% 
  mutate(commid = str_replace(commid, "vn", "vt"))

#------------------------------------------------------------------------------
#### 6. WRITE INTERMEDIATE FILE              ####
#------------------------------------------------------------------------------
saveRDS(df1, here("data", "intermediate", survey, paste0(round, "_", country, ".rds")))
# RDS maintains factor labeling
