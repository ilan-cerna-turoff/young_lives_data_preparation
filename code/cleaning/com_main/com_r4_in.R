# File: India community survey round 4
# Date: 24/03/2022
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
#### 1. LOAD CLEAN FUNCTIONS AND DATA  ####
#------------------------------------------------------------------------------
source(here("code", "functions", "clean_step1_label_error.R")) # necessary when a mismatch error in type or lack of labeling comes up for linkage variables
source(here("code", "functions", "low_memory_clean_v2.R")) # alternative cleaning function to deal with data superseding R's memory capacity after linkage variable issue

survey <- "com_main"
round <- "r4"
country <- "in"
linkage_vars <- "placeid"

vars_i_want <- c("ntrldist","disaster",paste0("recvhlp", 1:2),"months",
                 "popsize","socwrkr","ltrcycmp","scpridr4","prblctly","trans")

# Note: file supersedes R's memory, and linkage variable was saved as different types across dta files

#------------------------------------------------------------------------------
#### 2. CLEANING OF LINKAGE VARIABLES  ####
#------------------------------------------------------------------------------
# Note: only needed for surveys where Young Lives data analysts did not completely finish
# casting the linkage variables

# Run abridged function to identify the problem
raw <- clean_step1_label_error(survey = survey, round = round, country = country)
#raw$data_list$communitylevel$placeid
#raw$data_list$naturaldisaster$placeid

# Make linkage variables a common type
labels_as_numeric <- as.numeric(raw$data_list$communitylevel$placeid)
labels_as_factor <- as_factor(raw$data_list$communitylevel$placeid)
mapping <- setNames(labels_as_numeric, labels_as_factor)

# Applies properly coded labels to the rest of the data
raw$data_list <- map(raw$data_list, ~.x %>% 
                       mutate(placeid = labelled(placeid, mapping)))

raw_data <- raw$data_list #save progress
rm(mapping, labels_as_factor, labels_as_numeric, raw) #clean environment

#------------------------------------------------------------------------------
#### 3. USE ALTERNATIVE FUNCTION TO DEAL WITH SUPERCEDING R MEMORY CAPACITY ####
#------------------------------------------------------------------------------
com_r4_in <- low_memory_clean_v2(raw_data, vars = vars_i_want, linkage_vars = linkage_vars, compression = "none")

# Note: compression ="none" returns full lists of of variables for each dta file. 
# Review function documentation for other options in structuring data presentation

#------------------------------------------------------------------------------
#### 4. PERFORMING A MANUAL CHECK ####
#------------------------------------------------------------------------------
summary(com_r4_in$df) # a summary of the final clean data
com_r4_in$vars_needing_careful_manual_check
com_r4_in$vars_not_found
dd <- com_r4_in$data_dictionary # vars included above as vars_i_want
dd_discards <- com_r4_in$discarded_data_dictionary # vars not included

#------------------------------------------------------------------------------
#### 5. CONVERT TO TIBBLE FOR DATA CLEANING/MANIPULATION    ####
#------------------------------------------------------------------------------
df1 <- as_tibble(com_r4_in$data$communitylevel) #convert each list to a dataframe 
df2 <- as_tibble(com_r4_in$data$meansoftransport) 
df3 <- as_tibble(com_r4_in$data$naturaldisaster) 
df4 <- as_tibble(com_r4_in$data$socialproblem) 

rm(com_r4_in) #clean environment

#------------------------------------------------------------------------------
#### 6. CLEAN/MANIPULATE REST OF VARS    ####
#------------------------------------------------------------------------------
## Means of transport (df2) ##

# Public transportation (creating something similar to pubtran1)
df2 <- df2 %>% 
  mutate(trans = as.character(trans),
         across(c("trans"), ~replace(., . %in% c("bus","micro, combi, minibus","mototaxi","rail"), "yes")),
         across(c("trans"), ~replace(., . %in% c("bicycle","by foot","car","motorcycle/scooter", "other(specify)"), "no")),
         trans = ifelse(placeid == "in001", "yes", trans), #manual re-coding of errors
         trans = ifelse(placeid == "in002", "yes", trans),
         trans = ifelse(placeid == "in006", "yes", trans),
         trans = ifelse(placeid == "in012", "yes", trans),
         trans = ifelse(placeid == "in013", "yes", trans),
         trans = ifelse(placeid == "in015", "yes", trans),
         trans = ifelse(placeid == "in018", "yes", trans),
         trans = ifelse(placeid == "in019", "yes", trans),
         trans = ifelse(placeid == "in021", "yes", trans),
         trans = ifelse(placeid == "in024", "yes", trans),
         trans = ifelse(placeid == "in025", "yes", trans),
         trans = ifelse(placeid == "in026", "yes", trans),
         trans = ifelse(placeid == "in029", "yes", trans),
         trans = ifelse(placeid == "in037", "yes", trans),
         trans = ifelse(placeid == "in047", "yes", trans),
         trans = ifelse(placeid == "in048", "yes", trans),
         trans = ifelse(placeid == "in049", "yes", trans),
         trans = ifelse(placeid == "in050", "yes", trans),
         trans = ifelse(placeid == "in051", "yes", trans),
         trans = ifelse(placeid == "in052", "yes", trans),
         trans = ifelse(placeid == "in053", "yes", trans),
         trans = ifelse(placeid == "in054", "yes", trans),
         trans = ifelse(placeid == "in055", "yes", trans),
         trans = ifelse(placeid == "in056", "yes", trans),
         trans = ifelse(placeid == "in057", "yes", trans),
         trans = ifelse(placeid == "in061", "yes", trans),
         trans = ifelse(placeid == "in062", "yes", trans),
         trans = ifelse(placeid == "in063", "yes", trans),
         trans = ifelse(placeid == "in064", "yes", trans),
         trans = ifelse(placeid == "in065", "yes", trans),
         trans = ifelse(placeid == "in066", "yes", trans),
         trans = ifelse(placeid == "in067", "yes", trans),
         trans = ifelse(placeid == "in068", "yes", trans),
         trans = ifelse(placeid == "in069", "yes", trans),
         trans = ifelse(placeid == "in070", "yes", trans),
         trans = ifelse(placeid == "in074", "yes", trans),
         trans = ifelse(placeid == "in077", "yes", trans),
         trans = ifelse(placeid == "in086", "yes", trans),
         trans = ifelse(placeid == "in087", "yes", trans),
         trans = ifelse(placeid == "in088", "yes", trans),
         trans = ifelse(placeid == "in089", "yes", trans),
         trans = ifelse(placeid == "in090", "yes", trans),
         trans = ifelse(placeid == "in091", "yes", trans),
         trans = ifelse(placeid == "in097", "yes", trans),
         trans = ifelse(placeid == "in101", "yes", trans),
         trans = ifelse(placeid == "in102", "yes", trans)) %>%
  distinct() %>%
  rename(pubtran1 = trans) %>% 
  mutate(pubtran1 = as.factor(pubtran1))

##============================================================================
## Natural disasters (df3) ##

# Clean labels and create vars that correspond to past rounds
df3 <- df3 %>% 
  mutate(disaster = as.character(disaster), 
         disaster = str_trim(disaster, side = "both"),
         disaster = ifelse(disaster == "flood/ heavy rainfall", "flash flood", disaster),
         disaster = ifelse(disaster == "other (specify in the corresponding cell)", "other", disaster)) %>%
  replace_with_na(replace = list(disaster = c("outbreak of pests/diseases affecting crops  (not caused by other natural disasters already mentioned)", #not natural disasters
                                              "outbreak of pests/diseases affecting livestock (not caused by other natural disasters already mentioned)"))) 

dis_col <- df1[,c(1,2)] #get data collection date and no disaster exposure from other dataframe
df3 <- full_join(df3, dis_col, by ="placeid") #bind to current dataframe

df3 <- df3 %>% 
  mutate(disaster = ifelse(ntrldist == "no", "none", disaster), #creates no disaster
         disaster = ifelse(ntrldist == "yes" & is.na(disaster), "unknown", disaster)) 
         

# General disaster relief received
df3 <- df3 %>% 
  mutate(across(c("recvhlp1","recvhlp2"), as.character),
         across(c("recvhlp1","recvhlp2"), ~replace(., . %in% c("yes, from family and friends","yes from the  government",
                                                               "yes from ngos working here in locality"), "yes"))) %>%
  replace_na(list(recvhlp1 = "missing", recvhlp2 = "missing")) %>%
  mutate(disrel = ifelse(c((recvhlp1 == "no" | recvhlp2 == "no") &
                             (recvhlp1 != "yes" | recvhlp2 != "yes")), "no", "yes"),
         disrel = ifelse((recvhlp1 == "missing" & recvhlp2 == "missing"), "missing", disrel),
         disrel = ifelse(disaster == "none", NA, disrel), #NA to properly correspond if no disaster
         disrel = ifelse(placeid == "in001", "yes", disrel), #manual re-coding of errors
         disrel = ifelse(placeid == "in002", "yes", disrel),
         disrel = ifelse(placeid == "in003", "yes", disrel),
         disrel = ifelse(placeid == "in004", "yes", disrel),
         disrel = ifelse(placeid == "in005", "yes", disrel),
         disrel = ifelse(placeid == "in006", "yes", disrel),
         disrel = ifelse(placeid == "in014", "yes", disrel),
         disrel = ifelse(placeid == "in015", "yes", disrel),
         disrel = ifelse(placeid == "in016", "yes", disrel),
         disrel = ifelse(placeid == "in020", "yes", disrel),
         disrel = ifelse(placeid == "in022", "yes", disrel),
         disrel = ifelse(placeid == "in023", "yes", disrel),
         disrel = ifelse(placeid == "in035", "yes", disrel),
         disrel = ifelse(placeid == "in052", "yes", disrel),
         disrel = ifelse(placeid == "in062", "yes", disrel),
         disrel = ifelse(placeid == "in064", "yes", disrel),
         disrel = ifelse(placeid == "in066", "yes", disrel),
         disrel = ifelse(placeid == "in067", "yes", disrel),
         disrel = ifelse(placeid == "in068", "yes", disrel),
         disrel = ifelse(placeid == "in064", "yes", disrel),
         disrel = ifelse(placeid == "in070", "yes", disrel),
         disrel = ifelse(placeid == "in075", "yes", disrel),
         disrel = ifelse(placeid == "in076", "yes", disrel),
         disrel = ifelse(placeid == "in078", "yes", disrel),
         disrel = ifelse(placeid == "in081", "yes", disrel),
         disrel = ifelse(placeid == "in092", "yes", disrel),
         across(c("disaster","disrel"), as.factor)) %>% #comparable to past rounds
  select(placeid,disaster,disrel) %>% #select vars using
  distinct() 

##============================================================================
## Social problems (df4) ##

# Create comparable category of crime types and groups against crime
crime <- df4 %>%
  select(placeid,prblctly,scpridr4) %>%
  mutate(across(c("scpridr4","prblctly"), as.character),
         prblctly = ifelse(c(placeid == "in036" & scpridr4 == "others (specify)"), "yes", prblctly), 
         prblctly = ifelse(c(placeid == "in048" & scpridr4 == "others (specify)"), "yes", prblctly),
         prblctly = ifelse(c(placeid == "in091" & scpridr4 == "others (specify)"), "yes", prblctly)) %>% #errors from being asked the same question twice in survey
  distinct() %>%
  group_by(placeid) %>%
  pivot_wider(names_from = "scpridr4", values_from = "prblctly") %>%
  ungroup() %>%
  mutate(across(c("cattle/livestock theft","theft / robbery"), as.character)) %>% #need to be manual here because reading in as logical and not liking the column names with spaces
  rename(theft = "cattle/livestock theft", rob = "theft / robbery") %>%
  mutate(thfcrm = ifelse(theft == "no" & rob == "no", "no", "yes")) %>%
  rename(violcrm = "violent crimes", yuthcrm = "juvenile gangs", 
         proscrm = "prostitution in the local area") %>%
  mutate(across(c("thfcrm","violcrm","yuthcrm","proscrm"), as.factor)) %>%
  select(placeid,thfcrm,violcrm,yuthcrm,proscrm)

df4 <- full_join(df4, crime, by = "placeid")

df4 <- select(df4, -c(scpridr4,prblctly))

#------------------------------------------------------------------------------
#### 7. VARS NAMES TO AGREE WITH PAST ROUND     ####
#------------------------------------------------------------------------------
df1 <- df1 %>%
  rename(commid = placeid,
         frqpass = months,
         pop = popsize,
         sochel = socwrkr,
         adultlit = ltrcycmp)

df2 <- df2 %>%
  rename(commid = placeid) 

df3 <- df3 %>%
  rename(commid = placeid,
         anydis = disaster)

df4 <- df4 %>%
  rename(commid = placeid)

#------------------------------------------------------------------------------
#### 8. SELECT VARIABLES USING IN ANALYSIS, BIND AND PIVOT            ####
#------------------------------------------------------------------------------
df1 <- select(df1, c(commid,pop,frqpass,sochel,adultlit))
df2 <- select(df2, c(commid,pubtran1))
df3 <- select(df3, c(commid,anydis,disrel))
df4 <- select(df4, c(commid,thfcrm,violcrm,proscrm,yuthcrm))

combined_df <- list(df1, df2, df3, df4) %>% 
  reduce(full_join) %>% #join together
  mutate(commid = as.character(commid)) #to avoid error when joining across rounds

#------------------------------------------------------------------------------
#### 9. WRITE INTERMEDIATE FILE              ####
#------------------------------------------------------------------------------
saveRDS(combined_df, here("data", "intermediate", survey, paste0(round, "_", country, ".rds")))
# RDS maintains factor labeling