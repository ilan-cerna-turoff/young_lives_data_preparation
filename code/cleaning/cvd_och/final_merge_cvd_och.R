# File: cvd_och_final_merge.R
# Date: 06/27/2022
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
# This script is meant to merge all intermediate cvd_och files to produce final
# cleaned data files by country. All rounds merged into one clean data file.
#
# This script relies on the following file(s) as inputs:
#   * All .rds files inside data/intermediate/cvd_och
#
# This script generates the following file(s) as outputs:
#   * data/final/cvd_och/et.rds
#   * data/final/cvd_och/in.rds
#   * data/final/cvd_och/pe.rds
#   * data/final/cvd_och/vt.rds
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

#--------------------------#
####  1. MERGE          ####
#--------------------------#
source(here("code", "functions", "merge_intermediate.R"))

survey <- "cvd_och"

ethiopia <- merge_intermediate(survey, "et") # this automatically saves the file to the proper place
india <- merge_intermediate(survey, "in") # also automatically saves the file to the proper place
peru <- merge_intermediate(survey, "pe") # also automatically saves the file to the proper place
vietnam <- merge_intermediate(survey, "vt") # also automatically saves the file to the proper place

ethiopia$df # for the complete dataframe
ethiopia$dict # for the data dictionary

india$df # for the complete dataframe
india$dict # for the data dictionary

peru$df # for the complete dataframe
peru$dict # for the data dictionary

vietnam$df # for the complete dataframe
vietnam$dict # for the data dictionary

# to see where merge_intermediate() saved these datasets, go to:
# data/final/cvd_och/