############################
###   MY EXAMPLE SCRIPT  ###
############################
# Write a comment after one or more hash symbols

###############
# Purpose: To demonstrate an example
# Authors: Hoa
###############

# load packages
###############
pacman::p_load(
  rio,       # for import/export of files
  here,      # for locating files in my R project
  tidyverse # for data management and visualization
)

# load data 
###############
intel_data_raw <-import(
  here("data","Intel_CPUs.csv"), 
  na = c("", " ","   ") # and process missing
)

# export result
###############
export(intel_data_raw,here("data","clean","my_data.rds"))
