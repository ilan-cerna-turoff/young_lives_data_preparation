# File: Ethiopia community survey round 5
# Date: 26/03/2022
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
round <- "r5"
country <- "et"
linkage_vars <- "placeid"

vars_i_want <- c("ntrldist","disaster","popsize",paste0("recvhlp", 1:3),
                 "socwrkr","ltrcycmp","commigr","prblctly","scpridr5","trans")

# Note: file supersedes R's memory, and linkage variable was saved as different types across dta files

#------------------------------------------------------------------------------
#### 2. CLEANING OF LINKAGE VARIABLES  ####
#------------------------------------------------------------------------------
# Note: only needed for surveys where Young Lives data analysts did not completely 
# finish casting the linkage variables

# Run abridged function to identify the problem
raw <- clean_step1_label_error(survey = survey, round = round, country = country)
#raw$data_list$communitylevel$placeid #review data
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
com_r5_et <- low_memory_clean_v2(raw_data, vars = vars_i_want, linkage_vars = linkage_vars, compression = "none") 

# Note: compression ="none" returns full lists of of variables for each dta file. 
# Review function documentation for other options in structuring data presentation

#------------------------------------------------------------------------------
#### 4. PERFORMING A MANUAL CHECK ####
#------------------------------------------------------------------------------
summary(com_r5_et$df) # a summary of the final clean data
com_r5_et$vars_needing_careful_manual_check
com_r5_et$vars_not_found
dd <- com_r5_et$data_dictionary # vars included above as vars_i_want
dd_discards <- com_r5_et$discarded_data_dictionary # vars not included

#------------------------------------------------------------------------------
#### 5. CONVERT TO TIBBLE FOR DATA CLEANING/MANIPULATION    ####
#------------------------------------------------------------------------------
df1 <- as_tibble(com_r5_et$data$communitylevel) #convert each list to a dataframe 
df2 <- as_tibble(com_r5_et$data$meansoftransport) 
df3 <- as_tibble(com_r5_et$data$naturaldisaster) 
df4 <- as_tibble(com_r5_et$data$socialproblem) 

rm(com_r5_et) #clean environment

#------------------------------------------------------------------------------
#### 6. CLEAN/MANIPULATE REST OF VARS    ####
#------------------------------------------------------------------------------
## Means of transport (df2) ##

# Public transportation (creating something similar to pubtran1)
df2 <- df2 %>% 
  mutate(trans = as.character(trans),
         across(c("trans"), ~replace(., . %in% c("bus","micro, combi, minibus","mototaxi","bajaj"), "yes")),
         across(c("trans"), ~replace(., . %in% c("by foot","car","animal (horse, donkey, etc",
                                                 "bicycle","cart","motorcycle/scooter","other(specify)"), "no")),
         trans = ifelse(placeid == "1011", "yes", trans),
         trans = ifelse(placeid == "1021", "yes", trans),
         trans = ifelse(placeid == "1022", "yes", trans),
         trans = ifelse(placeid == "1031", "yes", trans),
         trans = ifelse(placeid == "1032", "yes", trans),
         trans = ifelse(placeid == "2041", "yes", trans),
         trans = ifelse(placeid == "2061", "yes", trans),
         trans = ifelse(placeid == "4121", "yes", trans),
         trans = ifelse(placeid == "4131", "yes", trans),
         trans = ifelse(placeid == "4141", "yes", trans),
         trans = ifelse(placeid == "4144", "yes", trans),
         trans = ifelse(placeid == "5171", "yes", trans),
         trans = ifelse(placeid == "5201", "yes", trans)) %>%
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
         disaster = ifelse(disaster == "other (specify in the corresponding cell)","other", disaster)) %>%
  replace_with_na(replace = list(disaster = c("outbreak of pests/diseases affecting humans (not caused by other natural disasters already mentioned)"))) # removes non-natural disasters from question

dis_col <- df1[,c(1,2)] #get no disaster exposure from other dataframe
df3 <- full_join(df3, dis_col, by ="placeid") #bind to current dataframe

df3 <- df3 %>% 
  mutate(across(c("ntrldist","recvhlp1","recvhlp2","recvhlp3"), as.character),
         disaster = ifelse(ntrldist == "no", "none", disaster), #creates no disaster
         disaster = ifelse(ntrldist == "yes" & is.na(disaster), "unknown", disaster), #fills in missing values
         across(c("recvhlp1","recvhlp2","recvhlp3"), ~replace(., . %in% c("yes, from other institution","yes, from family and friends",
                                                                          "yes from the government","yes, from ngos not working in locality",
                                                                          "yes from ngos working here in locality"), "yes"))) %>%
           replace_na(list(recvhlp1 = "missing", recvhlp2 = "missing", recvhlp3 = "missing")) %>%
           mutate(disrel = ifelse(c((recvhlp1 == "no" | recvhlp2 == "no" | recvhlp3 == "no") &
                                      (recvhlp1 != "yes" | recvhlp2 != "yes" | recvhlp3 != "yes")), "no", "yes"),
                  disrel = ifelse((recvhlp1 == "missing" & recvhlp2 == "missing" & recvhlp3 == "missing"), "missing", disrel),
                  disrel = ifelse(disaster == "none", NA, disrel), #NA to properly correspond if no disaster
                  disrel = ifelse(placeid == "5171", "yes", disrel),
                  across(c("disaster","disrel"), as.factor)) %>% #comparable to past rounds
           select(placeid,disaster,disrel) %>% #select vars using
           distinct() 

##============================================================================
## Social problems (df4) ##

# Create comparable category of crime types and groups against crime
crime <- df4 %>%
  select(placeid,prblctly,scpridr5) %>%
  mutate(across(c("scpridr5","prblctly"), as.character)) %>%
  distinct() %>%
  group_by(placeid) %>%
  pivot_wider(names_from = "scpridr5", values_from = "prblctly") %>%
  ungroup() %>%
  mutate(across(c("cattle/livestock theft","theft / robbery"), as.character)) %>% #need to be manual here because reading in as logical and not liking the column names with spaces
  rename(theft = "cattle/livestock theft", rob = "theft / robbery") %>%
  mutate(thfcrm = ifelse(theft == "no" & rob == "no", "no", "yes")) %>%
  rename(violcrm = "violent crimes", yuthcrm = "juvenile gangs", 
         proscrm = "prostitution in the local area", forcrm = "others (specify)") %>%
  mutate(across(c("thfcrm","violcrm","yuthcrm","proscrm","forcrm"), as.factor)) %>%
  select(placeid,thfcrm,violcrm,yuthcrm,proscrm,forcrm)

df4 <- full_join(df4, crime, by = "placeid")

df4 <- select(df4, -c(scpridr5,prblctly))

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
#### 8. SELECT VARIABLES USING IN ANALYSIS AND BIND             ####
#------------------------------------------------------------------------------
df1 <- select(df1, c(commid,pop,movwork,sochel,adultlit))
df2 <- select(df2, c(commid,pubtran1))
df3 <- select(df3, c(commid,anydis,disrel))
df4 <- select(df4, c(commid,thfcrm,violcrm,yuthcrm,proscrm,forcrm))

combined_df <- list(df1, df2, df3, df4) %>% 
  reduce(full_join) %>% #join together
  mutate(commid = as.character(commid)) #to avoid error when joining across rounds

#------------------------------------------------------------------------------
#### 9. FIX COMMID     ####
#------------------------------------------------------------------------------
combined_df <- combined_df %>% 
  mutate(commid = paste0("et", commid))

# Note: different than other surveys in how construct this linkage var

#------------------------------------------------------------------------------
#### 10. WRITE INTERMEDIATE FILE              ####
#------------------------------------------------------------------------------
saveRDS(combined_df, here("data", "intermediate", survey, paste0(round, "_", country, ".rds")))
# RDS maintains factor labeling
