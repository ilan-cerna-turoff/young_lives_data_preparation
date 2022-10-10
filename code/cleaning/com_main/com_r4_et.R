# File: Ethiopia community survey round 4
# Date: 23/03/2022
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
country <- "et"
linkage_vars <- "placeid"

vars_i_want <- c("ntrldist","disaster","recvhelp","popsize","prblctly",
                 "scpridr4","socwrkr","ltrcycmp","commigr","trans")

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
com_r4_et <- low_memory_clean_v2(raw_data, vars = vars_i_want, linkage_vars = linkage_vars, compression = "none")

# Note: compression ="none" returns full lists of of variables for each dta file. 
# Review function documentation for other options in structuring data presentation

#------------------------------------------------------------------------------
#### 4. PERFORMING A MANUAL CHECK ####
#------------------------------------------------------------------------------
summary(com_r4_et$df) # a summary of the final clean data
com_r4_et$vars_needing_careful_manual_check
com_r4_et$vars_not_found
dd <- com_r4_et$data_dictionary # vars included above as vars_i_want
dd_discards <- com_r4_et$discarded_data_dictionary # vars not included

#------------------------------------------------------------------------------
#### 5. CONVERT TO TIBBLE FOR DATA CLEANING/MANIPULATION    ####
#------------------------------------------------------------------------------
df1 <- as_tibble(com_r4_et$data$communitylevel) #convert each list to a dataframe 
df2 <- as_tibble(com_r4_et$data$meansoftransport) 
df3 <- as_tibble(com_r4_et$data$naturaldisaster) 
df4 <- as_tibble(com_r4_et$data$socialproblem) 

rm(com_r4_et) #clean environment

#------------------------------------------------------------------------------
#### 6. CLEAN/MANIPULATE REST OF VARS    ####
#------------------------------------------------------------------------------
## Means of transport (df2) ##

# Public transportation (creating something similar to pubtran1)
df2 <- df2 %>% 
  mutate(trans = as.character(trans),
         across(c("trans"), ~replace(., . %in% c("bus","micro, combi, minibus","mototaxi","bajaj"), "yes")),
         across(c("trans"), ~replace(., . %in% c("by foot","car","animal (horse, donkey, etc)",
                                                 "bicycle","cart","motorcycle"), "no")),
         trans = ifelse(placeid == "et1011", "yes", trans),
         trans = ifelse(placeid == "et1021", "yes", trans),
         trans = ifelse(placeid == "et1022", "yes", trans),
         trans = ifelse(placeid == "et1031", "yes", trans),
         trans = ifelse(placeid == "et1032", "yes", trans),
         trans = ifelse(placeid == "et2041", "yes", trans),
         trans = ifelse(placeid == "et2061", "yes", trans),
         trans = ifelse(placeid == "et2071", "yes", trans),
         trans = ifelse(placeid == "et3081", "yes", trans),
         trans = ifelse(placeid == "et3101", "yes", trans),
         trans = ifelse(placeid == "et3111", "yes", trans),
         trans = ifelse(placeid == "et4121", "yes", trans),
         trans = ifelse(placeid == "et4131", "yes", trans),
         trans = ifelse(placeid == "et4141", "yes", trans),
         trans = ifelse(placeid == "et4143", "yes", trans),
         trans = ifelse(placeid == "et5171", "yes", trans),
         trans = ifelse(placeid == "et5181", "yes", trans),
         trans = ifelse(placeid == "et5191", "yes", trans),
         trans = ifelse(placeid == "et5201", "yes", trans)) %>%
  distinct() %>%
  rename(pubtran1 = trans) %>%
  mutate(pubtran1 = as.factor(pubtran1)) 

##============================================================================
## Natural disasters (df3) ##

# Clean labels and create vars that correspond to past rounds
df3 <- df3 %>% 
  mutate(disaster = as.character(disaster),
         disaster = str_trim(disaster, side = "both"),
         disaster = ifelse(disaster == "erosion, cracks or landslide (not caused by other natural disasters)", "erosion/cracks/landslide", disaster),
         disaster = ifelse(disaster == "flood / heavy rainfall", "flash flood", disaster),
         disaster = ifelse(disaster == "other, specify", "other", disaster),
         disaster = ifelse(disaster == "overflowing of rivers / surge of the sea", "overflowing of river/sea", disaster),
         disaster = ifelse(disaster == "mud avalanche/slide", "avalanche/mud slide", disaster)) %>%        
  replace_with_na(replace = list(disaster = c("outbreak of pests/diseases affecting crops  (not caused by other natural disasters already mentioned)", # removes non-natural disasters from question
                                            "outbreak of pests/diseases affecting humans (not caused by other natural disasters already mentioned)",
                                            "outbreak of pests/diseases affecting livestock/animals (not caused by other natural disasters already mentioned)"))) 

dis_col <- df1[,c(1,2)] #get no disaster exposure from other dataframe
df3 <- full_join(df3, dis_col, by ="placeid") #bind to current dataframe

df3 <- df3 %>% 
  mutate(across(c("ntrldist","recvhelp"), as.character),
         disaster = ifelse(ntrldist == "no", "none", disaster), #no disaster
         disaster = ifelse(ntrldist == "yes" & is.na(disaster), "unknown", disaster), 
         recvhelp = ifelse(recvhelp == "no", "no", "yes"),      # clean label disaster relief received
         recvhelp = ifelse(disaster == "none", NA, recvhelp),
         recvhelp = ifelse(placeid == "et2071", "yes", recvhelp),
         recvhelp = ifelse(placeid == "et2061", "yes", recvhelp),
         recvhelp = ifelse(placeid == "et5181", "yes", recvhelp),
         recvhelp = ifelse(placeid == "et2051", "yes", recvhelp),
         recvhelp = ifelse(placeid == "et3081", "yes", recvhelp),
         recvhelp = ifelse(placeid == "et3101", "yes", recvhelp), #NA to properly correspond if no disaster
         recvhelp = ifelse(placeid == "et4141", "missing", recvhelp),
         across(c("disaster","recvhelp"), as.factor)) %>% #comparable to past rounds
  rename(disrel = recvhelp) %>%
  select(placeid,disaster,disrel) %>% #select vars using
  distinct() 

##============================================================================
## Social problems (df4) ##

# Create comparable category of crime types and groups against crime
crime <- df4 %>%
  select(placeid,prblctly,scpridr4) %>%
  mutate(across(c("scpridr4","prblctly"), as.character)) %>%
  distinct() %>%
  group_by(placeid) %>%
  pivot_wider(names_from = "scpridr4", values_from = "prblctly") %>%
  ungroup() %>%
  mutate(across(c("cattle/livestock theft","theft / robbery"), as.character)) %>% #need to be manual here because reading in as logical and not liking the column names with spaces
  rename(theft = "cattle/livestock theft", rob = "theft / robbery") %>%
  mutate(thfcrm = ifelse(theft == "no" & rob == "no", "no", "yes")) %>%
  rename(violcrm = "violent crimes", yuthcrm = "juvenile gangs", 
         proscrm = "prostitution in the local area", forcrm = "others") %>%
  mutate(across(c("thfcrm","violcrm","yuthcrm","proscrm","forcrm"), as.factor)) %>%
  select(placeid,thfcrm,violcrm,yuthcrm,proscrm,forcrm)

df4 <- full_join(df4, crime, by = "placeid")

df4 <- select(df4, -c(scpridr4,prblctly))

#------------------------------------------------------------------------------
#### 7. VARS NAMES TO AGREE WITH PAST ROUND     ####
#------------------------------------------------------------------------------
df1 <- df1 %>%
  rename(commid = placeid,
    pop = popsize,
    movwork = commigr,
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
#### 8. SELECT VARIABLES USING IN ANALYSIS AND BIND            ####
#------------------------------------------------------------------------------
df1 <- select(df1, c(commid,pop,movwork,adultlit,sochel))
df2 <- select(df2, c(commid,pubtran1))
df3 <- select(df3, c(commid,anydis,disrel))
df4 <- select(df4, c(commid,thfcrm,violcrm,yuthcrm,proscrm,forcrm))

combined_df <- list(df1, df2, df3, df4) %>% 
  reduce(full_join) %>% #join together
  mutate(commid = as.character(commid)) #to avoid error when joining across rounds

#------------------------------------------------------------------------------
#### 9. WRITE INTERMEDIATE FILE              ####
#------------------------------------------------------------------------------
saveRDS(combined_df, here("data", "intermediate", survey, paste0(round, "_", country, ".rds")))
# RDS maintains factor labeling
