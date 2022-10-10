# File: Peru community survey round 4
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
source(here("code", "functions", "low_memory_clean.R")) # cleaning function to deal with data superseding R's memory capacity

survey <- "com_main"
round <- "r4"
country <- "pe"
linkage_vars <- "placeid"

vars_i_want <- c("clustid","ntrldist","disaster",paste0("recvhlp", 1:3),
                 "popsize","janacc","febacc","maracc","apracc","mayacc",
                 "junacc","julacc","augacc","sepacc","octacc","novacc",
                 "decacc","socwrkr","ltrcycmp","scpridr4","prblctly",
                 paste0("trans", 1:3))

# Note: file supersedes R's memory

#------------------------------------------------------------------------------
#### 2. USE ALTERNATIVE FUNCTION TO DEAL WITH SUPERCEDING R MEMORY CAPACITY ####
#------------------------------------------------------------------------------
com_r4_pe <- low_memory_clean(survey = survey, round = round, country = country, vars = vars_i_want, linkage_vars = linkage_vars, compression = "none") 

# Note: compression ="none" returns full lists of of variables for each dta file. 
# Review function documentation for other options in structuring data presentation

#------------------------------------------------------------------------------
#### 3. PERFORMING A MANUAL CHECK ####
#------------------------------------------------------------------------------
summary(com_r4_pe$df) # a summary of the final clean data
com_r4_pe$vars_needing_careful_manual_check
com_r4_pe$vars_not_found
dd <- com_r4_pe$data_dictionary # vars included above as vars_i_want
dd_discards <- com_r4_pe$discarded_data_dictionary # vars not included

#------------------------------------------------------------------------------
#### 4. CONVERT TO TIBBLE FOR DATA CLEANING/MANIPULATION    ####
#------------------------------------------------------------------------------
df1 <- as_tibble(com_r4_pe$data$communitylevel) #convert each list to a dataframe 
df2 <- as_tibble(com_r4_pe$data$naturaldisaster) 
df3 <- as_tibble(com_r4_pe$data$socialproblem) 

rm(com_r4_pe) #clean environment

#------------------------------------------------------------------------------
#### 5. CLEAN/MANIPULATE REST OF VARS    ####
#------------------------------------------------------------------------------
labelfunction <- function(x){ifelse(x== "s", "yes", "no")} #makes s and no into yes and no

# Clean labels not translated from Spanish
vars <- c("janacc","febacc","maracc","apracc","mayacc","junacc","julacc","augacc",
          "sepacc","octacc","novacc","decacc","socwrkr","ltrcycmp","ntrldist")

df1 <- df1 %>% 
  mutate(across(all_of(vars), labelfunction))
 
df3 <- df3 %>% 
  mutate("prblctly" = labelfunction(prblctly))

##============================================================================
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
         across(c("trans1","trans2","trans3"), ~replace(., . %in% c("micro, combi, minibus","mototaxi","omnibus"), "yes")),
         across(c("trans1","trans2","trans3"), ~replace(., . %in% c("a pie","ac","auto, camioneta rural",
                                                                    "cami","motocicleta","bicicleta"), "no"))) %>%
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
         disaster = ifelse(disaster == "avalancha, huayco", "avalanche/mud slide", disaster),
         disaster = ifelse(disaster == "cicl", "cyclone/tornado/hurricane", disaster),
         disaster = ifelse(disaster == "desborde del r", "overflowing of river/sea", disaster),
         disaster = ifelse(disaster == "erosi", "erosion/cracks/landslide", disaster),
         disaster = ifelse(disaster == "fuerte lluvia", "flash flood", disaster),
         disaster = ifelse(disaster == "incendio forestal", "forest fire", disaster),
         disaster = ifelse(disaster == "nevada / ola de fr", "frost/cold front/hailstorm", disaster),
         disaster = ifelse(disaster == "otro (especificar)", "other", disaster),
         disaster = ifelse(disaster == "sequ", "drought", disaster),
         disaster = ifelse(disaster == "terremoto", "earthquake", disaster)) %>%
replace_with_na(replace = list(disaster = c("brote de plagas / enfermedades que afectan a los seres humanos (no causada por otros desastres naturales ya mencionados)", #not natural disasters
                                              "brote de plagas / enfermedades que afectan al ganado / animales (no causado por otros desastres naturales ya mencionados",
                                              "falta de cosecha debido a par"))) 

dis_col <- df1[,c(1,3)] 
df2 <- full_join(df2, dis_col, by ="placeid") 

df2 <- df2 %>% 
  mutate(disaster = ifelse(ntrldist == "no", "none", disaster), #no disaster
         disaster = ifelse(ntrldist == "yes" & is.na(disaster), "unknown", disaster))
        
df2 <- select(df2, -c(ntrldist)) #remove var do not need


# General disaster relief received
df2 <- df2 %>% 
  mutate(across(c("recvhlp1","recvhlp2","recvhlp3"), as.character),
         across(c("recvhlp1","recvhlp2","recvhlp3"), 
                         ~replace(., . %in% c("s"), "yes"))) %>%
  replace_na(list(recvhlp1 = "missing", recvhlp2 = "missing", recvhlp3 = "missing")) %>%
  mutate(disrel = ifelse(c((recvhlp1 == "no" | recvhlp2 == "no" | recvhlp3 == "no") &
                             (recvhlp1 != "yes" | recvhlp2 != "yes" | recvhlp3 != "yes")), "no", "yes"),
         disrel = ifelse((recvhlp1 == "missing" & recvhlp2 == "missing" & recvhlp3 == "missing"), "missing", disrel),
         disrel = ifelse(disaster == "none", NA, disrel), #NA to properly correspond if no disaster
         disrel = ifelse(placeid == "101", "yes", disrel), #manual re-coding of errors
         disrel = ifelse(placeid == "204", "yes", disrel),
         disrel = ifelse(placeid == "205", "yes", disrel),
         disrel = ifelse(placeid == "314", "yes", disrel),
         disrel = ifelse(placeid == "401", "yes", disrel),
         disrel = ifelse(placeid == "406", "yes", disrel),
         disrel = ifelse(placeid == "503", "yes", disrel),
         disrel = ifelse(placeid == "504", "yes", disrel),
         disrel = ifelse(placeid == "505", "yes", disrel),
         disrel = ifelse(placeid == "1506", "yes", disrel),
         disrel = ifelse(placeid == "1509", "yes", disrel),
         disrel = ifelse(placeid == "1606", "yes", disrel),
         disrel = ifelse(placeid == "1706", "no", disrel),
         disrel = ifelse(placeid == "1709", "yes", disrel),
         disrel = ifelse(placeid == "8003", "yes", disrel),
         disrel = ifelse(placeid == "8024", "yes", disrel),
         across(c("disaster","disrel"), as.factor)) %>% #comparable to past rounds
  select(placeid,disaster,disrel) %>% #select vars using
  distinct() 

##============================================================================
## Social problems (df3) ##

# Create comparable category of crime types and groups against crime
crime <- df3 %>%
  mutate(across(c("scpridr4","prblctly"), as.character)) %>%
  select(placeid,prblctly,scpridr4) %>%
  group_by(placeid) %>%
  pivot_wider(names_from = "scpridr4", values_from = "prblctly") %>%
  ungroup() %>%
  mutate(across(c("abigeos, ladrones de ganado","robos","otros 1","otros 2"), as.character)) %>% #need to be manual here because reading in as logical and not liking the column names with spaces
  rename(abigeos = "abigeos, ladrones de ganado", otros1 = "otros 1", otros2 = "otros 2") %>%
  mutate(thfcrm = ifelse(abigeos == "no" & robos == "no", "no", "yes")) %>%
  rename(violcrm = "cr", yuthcrm = "pandillaje de adolescentes", proscrm = "prostituci") %>%
  mutate(across(c("thfcrm","violcrm","yuthcrm","proscrm"), as.factor)) %>%
  select(placeid,thfcrm,violcrm,yuthcrm,proscrm)

df3 <- full_join(df3, crime, by = "placeid")

df3 <- select(df3, -c(scpridr4,prblctly))

#------------------------------------------------------------------------------
#### 6. CREATE COMMID VARIABLE     ####
#------------------------------------------------------------------------------
df1 <- df1 %>% 
  mutate(location = str_sub(placeid, -2), #removes last two digits 
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
  mutate(commid = as.character(commid)) #to avoid error when joining across rounds

#Note: need placeid for the join, but commid is the unique identifier

#------------------------------------------------------------------------------
#### 9. FINAL CLEAN UP OF JOINED VARIABLES             ####
#------------------------------------------------------------------------------
# Errors in variables after join
combined_df <- combined_df %>% 
  mutate(across(c("pop","commid","sochel","adultlit","pubtran1","disrel",
                  "violcrm","yuthcrm","proscrm","thfcrm"), as.character),
         pop = ifelse(c(commid == "pe04c02" & pop == "27737"), "1200", pop),
         pop = ifelse(c(commid == "pe05c03" & pop == "25000"), "1100", pop),
         pop = ifelse(c(commid == "pe05c04" & pop == "40000"), "1753", pop),
         pop = ifelse(commid == "peNAc05", NA, pop),
         pop = as.numeric(pop),
         sochel = ifelse(commid == "pe04c02", "yes", sochel),
         sochel = ifelse(commid == "pe05c04", "yes", sochel),
         adultlit = ifelse(commid == "peNAc05", "yes", adultlit),
         adultlit = ifelse(commid == "pe05c04", "yes", adultlit),
         adultlit = ifelse(commid == "pe05c03", "yes", adultlit),
         pubtran1 = ifelse(commid == "pe05c04", "yes", pubtran1),
         pubtran1 = ifelse(commid == "pe05c03", "no", pubtran1),
         pubtran1 = ifelse(commid == "pe04c02", "yes", pubtran1),
         pubtran1 = ifelse(commid == "peNAc05", "no", pubtran1),
         disrel = ifelse(commid == "pe05c04", "yes", disrel),
         disrel = ifelse(commid == "pe04c02", "no", disrel),
         disrel = ifelse(commid == "pe05c01", "no", disrel),
         violcrm = ifelse(commid == "pe05c04", "yes", violcrm),
         yuthcrm = ifelse(commid == "pe05c04", "yes", yuthcrm),
         yuthcrm = ifelse(commid == "pe05c03", "yes", yuthcrm),
         proscrm = ifelse(commid == "pe05c04", "yes", proscrm),
         proscrm = ifelse(commid == "pe05c03", "yes", proscrm),
         thfcrm = ifelse(commid == "pe05c03", "yes", thfcrm),
         across(c("sochel","adultlit","pubtran1","disrel","violcrm","yuthcrm",
                  "proscrm","thfcrm"), as.factor)) %>%
  distinct()
    
# Note: unclear which community is peNAc05 at past/future rounds since missing information.
# Replacing all to NA, but community will be dropped anyway in merge across rounds.

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
