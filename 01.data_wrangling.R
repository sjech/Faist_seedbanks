# Project: Seed Bank
# Author: Sierra Gugel
# Date: March 2026
# Co-authors: Akasha Faist, Madeline Mayorga
# version: 1
# Purpose: This code was created to analyze data associated with seed bank studies that follow the Faist Lab protocols. To use the code, you must use the associated data entry files so that the data format matches this code. This script will read in the metadata file, removal file, weekly pinning file, and plant codes file to wrangle the data into machine-readable formats. Adjustments may need to be made based on your specific project. The code will output files for data analysis (total seed counts, monocot counts, dicot counts, perennial v. annual, native v. non-native seeds, patterns between treatments and seed traits)

# Load Libraries
library(tidyverse)
library(dplyr)


# Load data
seeds <- read.csv("data/removals.csv")
metadata <- read.csv("data/metadata.csv")
species <- read.csv("data/plant_codes.csv")

# Create a column for the treatment by splitting the sample_ID
seeds$treatment <- substr(seeds$sample_ID, 1, 2)

# Create a column for monocot/dicot 
seeds$m_d <- substr(seeds$code, 1, 1)

# summarize the data so that time of emergence is no longer present
seed_summary <- seeds %>%
  group_by(sample_ID, code, m_d, treatment) %>%
  summarise(seedCount = sum(count))

# merge the plant information onto this
species_subset <- species %>% select(monocot_dicot_unknown_code, species_assignment, species_code, lifespan, ave_seed_mass_g_per_thousand_seeds, seed_appendage) # choose the columns that you want

seed_summary_species <- left_join(seed_summary, species_subset, by = c("code" = "monocot_dicot_unknown_code"))

# merge the metadata information to calculate seeds per area
metadata_subset <- metadata %>% select("sample_ID", "field_corer_depth_sampled_cm", "field_corer_area_sampled_cm2", "total_sample_weight_g", "greenhouse_germination_subsample_weight_g")

seed_summary_species_merge <- left_join(seed_summary_species, metadata_subset, by = "sample_ID")

# calculate the volume sampled
seed_summary_species_merge$field_corer_volume_sampled <- seed_summary_species_merge$field_corer_area_sampled_cm2 * seed_summary_species_merge$field_corer_depth_sampled_cm

# Add some calculations for the seeds
seed_summary_species_merge$seeds_m3 <- (seed_summary_species_merge$seedCount * 20000) / ((seed_summary_species_merge$greenhouse_germination_subsample_weight_g/seed_summary_species_merge$total_sample_weight_g) * seed_summary_species_merge$field_corer_volume_sampled)


# this table contains seed counts in raw numbers and also calculated per m3 of soil for each treatment and each species

# we can remove unnecessary columns as desired
seed_summary_species_merge2 <- seed_summary_species_merge %>%
  select("sample_ID", "code", "m_d", "treatment", "seedCount", "species_assignment", "species_code", "ave_seed_mass_g_per_thousand_seeds", "seed_appendage", "seeds_m3")

# you might have additional columns in your metadata or species data to address your research questions. You might also have additional treatment levels that should not be lost. Modify the code above as needed to generate your data table for analysis. 

# export
#write_csv(seed_summary_species_merge2, "output/species_summary_table.csv")


