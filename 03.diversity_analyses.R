# Project: Seed Bank
# Author: Sierra Gugel
# Date: March 2026
# Co-authors: Akasha Faist, Madeline Mayorga
# version: 1
# Purpose: This code should run after 01.data_wrangling. You will load the output data from the first script which should be a single .csv with seed counts by species and by sample type. This code will run analyses for diversity. 

# Load Libraries
library(tidyverse)
library(dplyr)
library(ggplot2)

# Load the data
seedsummary <- read.csv("output/species_summary_table.csv")