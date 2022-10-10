# File: Vietnam community survey round 4
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
round <- "r4"
country <- "vt"
linkage_vars <- "placeid"

vars_i_want <- c("ntrldist","disaster",paste0("recvhlp", 1:3),"popsize","months",
                 "scpridr4","prblctly","trans")

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
com_r4_vt <- low_memory_clean_v2(raw_data, vars = vars_i_want, linkage_vars = linkage_vars, compression = "none") 

# Note: compression ="none" returns full lists of of variables for each dta file. 
# Review function documentation for other options in structuring data presentation

#------------------------------------------------------------------------------
#### 4. PERFORMING A MANUAL CHECK ####
#------------------------------------------------------------------------------
summary(com_r4_vt$df) # a summary of the final clean data
com_r4_vt$vars_needing_careful_manual_check
com_r4_vt$vars_not_found
dd <- com_r4_vt$data_dictionary # vars included above as vars_i_want
dd_discards <- com_r4_vt$discarded_data_dictionary # vars not included

#------------------------------------------------------------------------------
#### 5. CONVERT TO TIBBLE FOR DATA CLEANING/MANIPULATION    ####
#------------------------------------------------------------------------------
df1 <- as_tibble(com_r4_vt$data$communitylevel) #convert each list to a dataframe 
df2 <- as_tibble(com_r4_vt$data$meansoftransport) 
df3 <- as_tibble(com_r4_vt$data$naturaldisaster) 
df4 <- as_tibble(com_r4_vt$data$socialproblem) 

rm(com_r4_vt) #clean environment

#------------------------------------------------------------------------------
#### 6. CLEAN/MANIPULATE REST OF VARS    ####
#------------------------------------------------------------------------------
## Means of transport (df2) ##

# Public transportation (creating something similar to pubtran1)
df2 <- df2 %>% 
  mutate(trans = as.character(trans),
         across(c("trans"), ~replace(., . %in% c("bus","micro, combi, minibus","mototaxi"), "yes")),
         across(c("trans"), ~replace(., . %in% c("bicycle","by foot","car","motorcycle"), "no")),
         trans = ifelse(placeid == "vn004", "yes", trans), #manual re-coding of errors
         trans = ifelse(placeid == "vn016", "yes", trans),
         trans = ifelse(placeid == "vn001", "yes", trans),
         trans = ifelse(placeid == "vn006", "yes", trans),
         trans = ifelse(placeid == "vn008", "yes", trans),
         trans = ifelse(placeid == "vn009", "yes", trans),
         trans = ifelse(placeid == "vn010", "yes", trans),
         trans = ifelse(placeid == "vn013", "yes", trans),
         trans = ifelse(placeid == "vn015", "yes", trans),
         trans = ifelse(placeid == "vn018", "yes", trans),
         trans = ifelse(placeid == "vn019", "yes", trans)) %>%
  distinct() %>%
  rename(pubtran1 = trans) %>% 
  mutate(pubtran1 = as.factor(pubtran1))

##============================================================================
## Natural disasters (df3) ##

# Clean labels and create vars that correspond to past rounds
df3 <- df3 %>% 
  mutate(disaster = as.character(disaster), 
         disaster = str_trim(disaster, side = "both"),
         disaster = ifelse(disaster == "cyclone/tornado /hurricane", "cyclone/tornado/hurricane", disaster),
         disaster = ifelse(disaster == "erosion, cracks or landslide", "erosion/cracks/landslide", disaster),
         disaster = ifelse(disaster == "flood / heavy rainfall", "flash flood", disaster),
         disaster = ifelse(disaster == "other (specify)", "other", disaster),
         disaster = ifelse(disaster == "overflowing of rivers / surge of the sea", "overflowing of river/sea", disaster)) %>%
  replace_with_na(replace = list(disaster = c("outbreak of pests/diseases affecting", #not natural disasters and not sure if the first corresponds to epidemic
                                              "outbreak of pests/diseases affecting livestock")))

dis_col <- df1[,c(1,2)] #get data collection date and no disaster exposure from other dataframe
df3 <- full_join(df3, dis_col, by ="placeid") #bind to current dataframe

df3 <- df3 %>% 
  mutate(disaster = ifelse(ntrldist == "no", "none", disaster), #create no disaster
         disaster = ifelse(ntrldist == "yes" & is.na(disaster), "unknown", disaster))


# General disaster relief received
df3 <- df3 %>% 
  mutate(across(c("recvhlp1","recvhlp2","recvhlp3"), as.character),
         across(c("recvhlp1","recvhlp2","recvhlp3"), ~replace(., . %in% c("yes, from family and friends","yes, from the government",
                                                                          "yes from ngos working here in locality",
                                                                          "yes, from ngos not working in locality",
                                                                          "yes, from other institution"), "yes"))) %>%
  replace_na(list(recvhlp1 = "missing", recvhlp2 = "missing", recvhlp3 = "missing")) %>%
  mutate(disrel = ifelse(c((recvhlp1 == "no" | recvhlp2 == "no" | recvhlp3 == "no") &
                           (recvhlp1 != "yes" | recvhlp2 != "yes" | recvhlp3 != "yes")), "no", "yes"),
         disrel = ifelse((recvhlp1 == "missing" & recvhlp2 == "missing" & recvhlp3 == "missing"), "missing", disrel),
         disrel = ifelse(disaster == "none", NA, disrel), #NA to properly correspond if no disaster
         disrel = ifelse(placeid == "vn001", "yes", disrel), #manual re-coding of errors
         disrel = ifelse(placeid == "vn006", "yes", disrel),
         across(c("disaster","disrel"), as.factor)) %>% #comparable to past rounds
  select(placeid,disaster,disrel) %>% #select vars using
  distinct() 

##============================================================================
## Social problems (df4) ##

# Create comparable category of crime types and groups against crime
crime <- df4 %>%
  mutate(scpridr4 = as.character(scpridr4)) %>%
  select(placeid,prblctly,scpridr4) %>%
  group_by(placeid) %>%
  pivot_wider(names_from = "scpridr4", values_from = "prblctly") %>%
  ungroup() %>%
  mutate(across(c("cattle/livestock theft","asset theft / robbery"), as.character)) %>% #need to be manual here because reading in as logical and not liking the column names with spaces
  rename(theft = "cattle/livestock theft", rob = "asset theft / robbery") %>%
  mutate(thfcrm = ifelse(theft == "no" & rob == "no", "no", "yes")) %>%
  rename(violcrm = "violent crimes", yuthcrm = "juvenile gangs", proscrm = "prostitution in the local area") %>%
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
         pop = popsize)

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
df1 <- select(df1, c(commid,pop,frqpass))
df2 <- select(df2, c(commid,pubtran1))
df3 <- select(df3, c(commid,anydis,disrel))
df4 <- select(df4, c(commid,thfcrm,violcrm,yuthcrm,proscrm))

combined_df <- list(df1, df2, df3, df4) %>% 
  reduce(full_join) %>% #join together
  mutate(commid = as.character(commid)) #to avoid error when joining across rounds
                
#------------------------------------------------------------------------------
#### 9. RENAME COMMID FOR CONSISTENCY             ####
#------------------------------------------------------------------------------
combined_df <- combined_df %>% 
  mutate(commid = str_replace(commid, "vn", "vt"))

#------------------------------------------------------------------------------
#### 10. WRITE INTERMEDIATE FILE              ####
#------------------------------------------------------------------------------
saveRDS(combined_df, here("data", "intermediate", survey, paste0(round, "_", country, ".rds")))
# RDS maintains factor labeling
