# File: com_main_final_merge.R
# Date: 27/06/2022
# Author: Lawrence Chillrud 

#--------------------------#
####      CONTENTS      ####
#--------------------------#
# N. Notes
# 0. Package imports
# 1. Merge Ethiopia, India, Peru, and Vietnam datasets

#--------------------------#
####      N. NOTES      ####
#--------------------------#
# This script is meant to merge all intermediate com_main files to produce longitudinal
# cleaned data files by country. All rounds merged into one clean data file.
#
# This script relies on the following file(s) as inputs:
#   * All .rds files inside data/intermediate/com_main
#
# This script generates the following file(s) as outputs:
#   * data/final/com_main/et.rds
#   * data/final/com_main/in.rds
#   * data/final/com_main/pe.rds
#   * data/final/com_main/vt.rds
#
# Warnings: None 

#--------------------------#
#### 0. PACKAGE IMPORTS ####
#--------------------------#
# 0a. Run the 0_requirements.R file if you have not already:
if (!exists("ran_0_requirements")) {
  here::i_am("README.md")
  source(here::here("code", "functions", "0_requirements.R"))
}
# Loads in the necessary packages for the cleaning functions. Cleaning functions rely on the haven(), here(), and tidyverse() packages

#----------------------------------#
####  1. MERGE DATASETS         ####
#----------------------------------#
source(here("code", "functions", "merge_intermediate.R"))

survey <- "com_main"

ethiopia <- merge_intermediate(survey, "et") # this automatically saves the file to the proper place
india <- merge_intermediate(survey, "in") 
peru <- merge_intermediate(survey, "pe") 
vietnam <- merge_intermediate(survey, "vt") 

ethiopia$df # for the complete dataframe
ethiopia$dict # for the data dictionary

india$df 
india$dict

peru$df 
peru$dict

vietnam$df 
vietnam$dict

# to see where merge_intermediate() saved these datasets, go to:
# data/final/com_main/