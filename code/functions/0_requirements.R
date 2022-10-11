# File: 0_requirements.R
# Author: Lawrence Chillrud
# Date: 08/06/2022

#-------------------------#
#### TABLE OF CONTENTS ####
#-------------------------#
# N. Notes
# 0. Install pacman (if needed)
# 1. Install (if needed) and load all packages
# 2. Load all functions in functions folder

#--------------------------#
####      N. NOTES      ####
#--------------------------#
# This script is meant to install (if needed) and load all packages and functions required for the
# cleaning scripts. This ensures a consistent R operating environment for all those who use this code

#--------------------------#
#### 0. INSTALL PACMAN  ####
#--------------------------#
# 0a. Create variable signalling the running of this script for the benefit of other scripts:
ran_0_requirements <- T

# 0b. Install package manager software "pacman" if not already installed:
if (!("pacman" %in% installed.packages()[, "Package"])) install.packages("pacman")

#--------------------------#
####  1. LOAD PACKAGES  ####
#--------------------------#
# 1a. Install/load all necessary packages for scripts in the young_lives_data_preparation/code/cleaning repository:
pacman::p_load(
  "haven",
  "here",
  "tidyverse",  
  "tidystringdist",
  "naniar",
  "lubridate"
)

#--------------------------#
#### 2. LOAD FUNCTIONS  ####
#--------------------------#
# 2a. Load all functions in the functions folder (except this one, to avoid infinite recursion):
fns <- list.files(here("code", "functions"), full.names = T)
walk(fns[!endsWith(fns, "0_requirements.R")], source)
