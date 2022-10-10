# File: Peru community survey round 5
# Date: 27/03/2022
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
source(here("code", "functions", "low_memory_clean.R")) # cleaning function to deal with data superseding R's memory capacity

survey <- "com_main"
round <- "r5"
country <- "pe"
linkage_vars <- "placeid"

vars_i_want <- c("clustid","ntrldist","disaster",paste0("recvhlp", 1:3),
                 "popsize","janacc","febacc","maracc","apracc","mayacc",
                 "junacc","julacc","augacc","sepacc","octacc","novacc",
                 "decacc","socwrkr","ltrcycmp","scpridr5","prblctly",
                 paste0("trans", 1:3))

# Note: file supersedes R's memory

#------------------------------------------------------------------------------
#### 2. USE ALTERNATIVE FUNCTION TO DEAL WITH SUPERCEDING R MEMORY CAPACITY ####
#------------------------------------------------------------------------------
com_r5_pe <- low_memory_clean(survey = survey, round = round, country = country, vars = vars_i_want, linkage_vars = linkage_vars, compression = "none") 

# Note: compression ="none" returns full lists of of variables for each dta file. 
# Review function documentation for other options in structuring data presentation

#------------------------------------------------------------------------------
#### 3. PERFORMING A MANUAL CHECK ####
#------------------------------------------------------------------------------
summary(com_r5_pe$df) # a summary of the final clean data
com_r5_pe$vars_needing_careful_manual_check
com_r5_pe$vars_not_found
dd <- com_r5_pe$data_dictionary # vars included above as vars_i_want
dd_discards <- com_r5_pe$discarded_data_dictionary # vars not included

#------------------------------------------------------------------------------
#### 4. CONVERT TO TIBBLE FOR DATA CLEANING/MANIPULATION    ####
#------------------------------------------------------------------------------
df1 <- as_tibble(com_r5_pe$data$communitylevel) #convert each list to a dataframe 
df2 <- as_tibble(com_r5_pe$data$naturaldisaster) 
df3 <- as_tibble(com_r5_pe$data$socialproblem) 

rm(com_r5_pe) #clean environment

#------------------------------------------------------------------------------
#### 5. CLEAN/MANIPULATE REST OF VARS    ####
#------------------------------------------------------------------------------
## Community level (df1) ##

# Number of months per year inaccessible by vehicle
months_year <- c("janacc","febacc","maracc","apracc","mayacc","junacc","julacc",
                 "augacc","sepacc","octacc","novacc","decacc")

df1 <- df1 %>% 
  mutate(across(all_of(months_year), as.character),
         across(all_of(months_year),~replace(., . %in% c("99"), NA)),
         across(all_of(months_year), ~replace(., . %in% c("yes"), "1")),
         across(all_of(months_year), ~replace(., . %in% c("no"), "0")),
         across(all_of(months_year), as.numeric)) %>% 
  rowwise() %>% 
  mutate(frqpass = sum(janacc,febacc,maracc,apracc,mayacc,junacc,julacc,augacc,
                       sepacc,octacc,novacc,decacc))

df1 <- select(df1, -c(janacc,febacc,maracc,apracc,mayacc,junacc,julacc,augacc,
                      sepacc,octacc,novacc,decacc))


# Public transportation (creating something similar to pubtran1)
df1 <- df1 %>% 
  mutate(across(c("trans1","trans2","trans3"), as.character),
         across(c("trans1","trans2","trans3"), ~replace(., . %in% c("bus","micro, combi, minibus","mototaxi"), "yes")),
         across(c("trans1","trans2","trans3"), ~replace(., . %in% c("by foot","car"), "no"))) %>%
  replace_na(list(trans1 = "missing", trans2 = "missing", trans3 = "missing")) %>%
  mutate(pubtran1 = ifelse(c((trans1 == "no" | trans2 == "no" | trans3 == "no") &
                             (trans1 != "yes" | trans2 != "yes" | trans3 != "yes")), "no", "yes"),
         pubtran1 = ifelse((trans1 == "missing" & trans2 == "missing" & trans3 == "missing"), "missing", pubtran1),
         pubtran1 = as.factor(pubtran1))
         
df1 <- select(df1, -c(trans1,trans2,trans3))

##============================================================================
## Natural disasters (df2) ##

# Clean labels and create vars that correspond to past rounds
df2 <- df2 %>% 
  mutate(disaster = as.character(disaster), 
         disaster = str_trim(disaster, side = "both"),
         disaster = ifelse(disaster == "mud avalanche/ slide (huayco)", "avalanche/mud slide", disaster),
         disaster = ifelse(disaster == "cyclone/tornado /hurricane", "cyclone/tornado/hurricane", disaster),
         disaster = ifelse(disaster == "overflowing of rivers / surge of the sea (maretazo)", "overflowing of river/sea", disaster),
         disaster = ifelse(disaster == "erosion, cracks or landslide (not caused by other natural disasters)", "erosion/cracks/landslide", disaster),
         disaster = ifelse(disaster == "heavy rainfall", "flash flood", disaster),
         disaster = ifelse(disaster == "other, specify in corresponding cell", "other", disaster)) %>%
  replace_with_na(replace = list(disaster = c("outbreak of pests/diseases affecting crops (not caused by other natural disasters already mentioned)",
                                              "outbreak of pests/diseases affecting humans (not caused by other natural disasters already mentioned)",
                                              "utbreak of pests/diseases affecting livestock/animals (not caused by other natural disasters already mentioned)"))) 

dis_col <- df1[,c(1,3)] #get data collection date and no disaster exposure from other dataframe
df2 <- full_join(df2, dis_col, by ="placeid") #bind to current dataframe

df2 <- df2 %>% 
  mutate(disaster = ifelse(ntrldist == "no", "none", disaster), #create no disaster
         disaster = ifelse(ntrldist == "yes" & is.na(disaster), "unknown", disaster)) 

  
# General disaster relief received
df2 <- df2 %>% 
  mutate(across(c("recvhlp1","recvhlp2","recvhlp3"), as.character),
         across(c("recvhlp2","recvhlp3"), ~str_replace_all(., "88", NA_character_)),
         across(c("recvhlp1","recvhlp2","recvhlp3"), ~replace(., . %in% c("yes, from other institution","yes, from family and friend",
                                                                          "yes from the government","yes, from ngos not working in locality",
                                                                          "yes from ngos working here in locality"), "yes"))) %>%
  replace_na(list(recvhlp1 = "missing", recvhlp2 = "missing", recvhlp3 = "missing")) %>%
  mutate(disrel = ifelse(c((recvhlp1 == "no" | recvhlp2 == "no" | recvhlp3 == "no") &
                             (recvhlp1 != "yes" | recvhlp2 != "yes" | recvhlp3 != "yes")), "no", "yes"),
         disrel = ifelse((recvhlp1 == "missing" & recvhlp2 == "missing" & recvhlp3 == "missing"), "missing", disrel),
         disrel = ifelse(disaster == "none", NA, disrel),
         disrel = ifelse(placeid == "202", "yes", disrel), #manual re-coding of errors
         disrel = ifelse(placeid == "203", "yes", disrel),
         disrel = ifelse(placeid == "307", "yes", disrel),
         disrel = ifelse(placeid == "310", "yes", disrel),
         disrel = ifelse(placeid == "8024", "yes", disrel),
         across(c("disaster","disrel"), as.factor)) %>% #comparable to past rounds
  select(placeid,disaster,disrel) %>% #select vars using
  distinct() 

##============================================================================
## Social problems (df3) ##

# Create comparable category of crime types and groups against crime
crime <- df3 %>%
  mutate(across(c("placeid","scpridr5","prblctly"), as.character),
         prblctly = ifelse(c(placeid == "101" & scpridr5 == "other, specify"), "yes", prblctly), #manual re-coding of errors
         prblctly = ifelse(c(placeid == "102" & scpridr5 == "other, specify"), "yes", prblctly),
         prblctly = ifelse(c(placeid == "201" & scpridr5 == "other, specify"), "yes", prblctly),
         prblctly = ifelse(c(placeid == "202" & scpridr5 == "other, specify"), "yes", prblctly),
         prblctly = ifelse(c(placeid == "203" & scpridr5 == "other, specify"), "yes", prblctly),
         prblctly = ifelse(c(placeid == "204" & scpridr5 == "other, specify"), "yes", prblctly),
         prblctly = ifelse(c(placeid == "205" & scpridr5 == "other, specify"), "yes", prblctly),
         prblctly = ifelse(c(placeid == "301" & scpridr5 == "other, specify"), "yes", prblctly),
         prblctly = ifelse(c(placeid == "304" & scpridr5 == "other, specify"), "yes", prblctly),
         prblctly = ifelse(c(placeid == "307" & scpridr5 == "other, specify"), "yes", prblctly),
         prblctly = ifelse(c(placeid == "308" & scpridr5 == "other, specify"), "yes", prblctly),
         prblctly = ifelse(c(placeid == "314" & scpridr5 == "other, specify"), "yes", prblctly),
         prblctly = ifelse(c(placeid == "402" & scpridr5 == "other, specify"), "yes", prblctly),
         prblctly = ifelse(c(placeid == "403" & scpridr5 == "other, specify"), "yes", prblctly),
         prblctly = ifelse(c(placeid == "404" & scpridr5 == "other, specify"), "yes", prblctly),
         prblctly = ifelse(c(placeid == "405" & scpridr5 == "other, specify"), "yes", prblctly),
         prblctly = ifelse(c(placeid == "406" & scpridr5 == "other, specify"), "yes", prblctly),
         prblctly = ifelse(c(placeid == "503" & scpridr5 == "other, specify"), "yes", prblctly),
         prblctly = ifelse(c(placeid == "504" & scpridr5 == "other, specify"), "yes", prblctly),
         prblctly = ifelse(c(placeid == "505" & scpridr5 == "other, specify"), "yes", prblctly),
         prblctly = ifelse(c(placeid == "510" & scpridr5 == "other, specify"), "yes", prblctly),
         prblctly = ifelse(c(placeid == "512" & scpridr5 == "other, specify"), "yes", prblctly),
         prblctly = ifelse(c(placeid == "601" & scpridr5 == "other, specify"), "yes", prblctly),
         prblctly = ifelse(c(placeid == "602" & scpridr5 == "other, specify"), "yes", prblctly),
         prblctly = ifelse(c(placeid == "802" & scpridr5 == "other, specify"), "yes", prblctly),
         prblctly = ifelse(c(placeid == "901" & scpridr5 == "other, specify"), "yes", prblctly),
         prblctly = ifelse(c(placeid == "902" & scpridr5 == "other, specify"), "yes", prblctly),
         prblctly = ifelse(c(placeid == "1202" & scpridr5 == "other, specify"), "yes", prblctly),
         prblctly = ifelse(c(placeid == "1303" & scpridr5 == "other, specify"), "yes", prblctly),
         prblctly = ifelse(c(placeid == "1403" & scpridr5 == "other, specify"), "yes", prblctly),
         prblctly = ifelse(c(placeid == "8002" & scpridr5 == "other, specify"), "yes", prblctly),
         prblctly = ifelse(c(placeid == "8004" & scpridr5 == "other, specify"), "yes", prblctly),
         prblctly = ifelse(c(placeid == "8006" & scpridr5 == "other, specify"), "yes", prblctly),
         prblctly = ifelse(c(placeid == "8022" & scpridr5 == "other, specify"), "yes", prblctly),
         prblctly = ifelse(c(placeid == "8023" & scpridr5 == "other, specify"), "yes", prblctly),
         prblctly = ifelse(c(placeid == "8025" & scpridr5 == "other, specify"), "yes", prblctly),
         prblctly = ifelse(c(placeid == "8027" & scpridr5 == "other, specify"), "yes", prblctly),
         prblctly = ifelse(c(placeid == "8029" & scpridr5 == "other, specify"), "yes", prblctly),
         prblctly = ifelse(c(placeid == "8032" & scpridr5 == "other, specify"), "yes", prblctly),
         prblctly = ifelse(c(placeid == "8034" & scpridr5 == "other, specify"), "yes", prblctly)) %>%
  select(placeid,prblctly,scpridr5) %>%
  distinct() %>%
  group_by(placeid) %>%
  pivot_wider(names_from = "scpridr5", values_from = "prblctly") %>%
  ungroup() %>%
  mutate(across(c("cattle/livestock theft","theft / robbery"), as.character)) %>% #need to be manual here because reading in as logical and not liking the column names with spaces
  rename(theft = "cattle/livestock theft", rob = "theft / robbery") %>%
  mutate(thfcrm = ifelse(theft == "no" & rob == "no", "no", "yes")) %>%
  rename(violcrm = "violent crimes", yuthcrm = "juvenile gangs", proscrm = "prostitution in the local area") %>%
  mutate(across(c("thfcrm","violcrm","yuthcrm","proscrm"), as.factor),
         placeid = as.numeric(placeid)) %>%
  select(placeid,thfcrm,violcrm,yuthcrm,proscrm) 

df3 <- full_join(df3, crime, by = "placeid")

df3 <- select(df3, -c(scpridr5,prblctly))

#------------------------------------------------------------------------------
#### 6. CREATE COMMID VARIABLE     ####
#------------------------------------------------------------------------------
df1 <- df1 %>% 
  mutate(location = str_sub(placeid, -2), #LAST 2 DIGITS
         commid = paste0("pe", clustid, "c", location))

#------------------------------------------------------------------------------
#### 7. VARS NAMES TO AGREE WITH PAST ROUND     ####
#------------------------------------------------------------------------------
df1 <- df1 %>%
  rename(pop = popsize,
         sochel = socwrkr,
         adultlit = ltrcycmp)

df2 <- df2 %>%
  rename(anydis = disaster)

#------------------------------------------------------------------------------
#### 8. SELECT VARIABLES USING IN ANALYSIS, BIND AND PIVOT            ####
#------------------------------------------------------------------------------
df1 <- select(df1, c(placeid,commid,pop,frqpass,sochel,adultlit,pubtran1))
df2 <- select(df2, c(placeid,anydis,disrel))
df3 <- select(df3, c(placeid,thfcrm,violcrm,yuthcrm,proscrm))

combined_df <- list(df1, df2, df3) %>% 
  reduce(full_join) %>% #join together
  mutate(commid = as.character(commid))#to avoid error when joining across rounds

#------------------------------------------------------------------------------
#### 9. FINAL CLEAN UP OF JOINED VARIABLES             ####
#------------------------------------------------------------------------------
# Errors in variables after join
combined_df <- combined_df %>% 
  mutate(across(c("pop","pubtran1","disrel","yuthcrm"), as.character),
         pop = ifelse(commid == "pe05c05" & pop == "750", "1300", pop),
         pop = ifelse(pop == "1", NA, pop), 
         pop = as.numeric(pop),
         pubtran1 = ifelse(commid == "pe05c05", "yes", pubtran1),
         disrel = ifelse(commid == "pe05c05", "yes", disrel),
         yuthcrm = ifelse(commid == "pe05c05", "yes", yuthcrm),
         across(c("pubtran1","disrel","yuthcrm"), as.factor))

#------------------------------------------------------------------------------
#### 10. SELECT COMMON VARIABLES ACROSS ROUNDS     ####
#------------------------------------------------------------------------------
combined_df <- select(combined_df, c(commid,anydis,disrel,pop,frqpass,thfcrm,violcrm,
                                     yuthcrm,proscrm,sochel,adultlit,pubtran1)) 

#------------------------------------------------------------------------------
#### 11. WRITE INTERMEDIATE FILE              ####
#------------------------------------------------------------------------------
saveRDS(combined_df, here("data", "intermediate", survey, paste0(round, "_", country, ".rds")))
# RDS maintains factor labeling
