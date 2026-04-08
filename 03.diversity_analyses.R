# Project: Seed Bank
# Author: Sierra Gugel
# Date: March 2026
# Co-authors: Akasha Faist, Madeline Mayorga
# version: 1
# Purpose: This code should run after 01.data_wrangling. You will load the output data from the first script which should be a single .csv with seed counts by species and by sample type. This code will run analyses for diversity. 

# Note: "emergent diversity under your conditions” not the full seed bank

# Load Libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(vegan) # for Shannon and Simpson diversity use diversity(), for richness use specnumber()
#library(hillR) # for Hill numbers and rarefaction 
#library(iNEXT) # for Hill numbers and rarefaction
library(betareg) # for assessing Simpson's metrics, use instead of linear model

# Load the data
seedsummary <- read.csv("output/species_summary_table.csv")

# Richness - number of different species in the samples. keep the replicates separate so that we can then get an average richness value
richness <- seedsummary %>%
  filter(!treatment %in% c("cc")) %>%
  group_by(sample_ID, treatment) %>%
  summarise(richness = n())

# use the new spreadsheet for statistical analyses
richness.lm <- lm(richness ~ treatment, data = richness)
summary(richness.lm)

# visualize the data - basic plot
richness %>%
  ggplot()+
  geom_boxplot(mapping = aes(x = treatment, y = richness)) +
  geom_jitter(mapping = aes(x = treatment, y = richness))+
  scale_y_continuous(breaks = seq(0, 5, by = 1))+
  theme_classic()

# richness does not take into account abundance. Try Simpson's or Shannon's with the vegan package
# create a community data matrix with each row representing a sample/site and each column representing a species. The cells contain the abundance values
seeds <- seedsummary %>% select(sample_ID, seedCount, species_assignment)
# wide format
seeds_wide <- seeds %>% spread(species_assignment, seedCount) # check that the result matches your original data
# make a dataframe to store the diversity data
seeds_sampleIDs <- seeds_wide$sample_ID
#drop the identifier and any unwanted species
seeds_wide <- select(seeds_wide, -sample_ID)
#seeds_wide <- ungroup(seeds_wide) %>% select(.,-"alate NA", -"ignore NA")
#replace the NAs with zeros
seeds_wide[is.na(seeds_wide)] <- 0

# Alpha Diversity Calculations
# Shannon's
H <- diversity(seeds_wide)
# Simpson's
simp <- diversity(seeds_wide, "simpson")
# Richness
S <- specnumber(seeds_wide)
# Evenness
J <- H/log(S)
# store the data
seed_diversity <- data.frame(seeds_sampleIDs, H, simp, S, J)
# in order to assess the data, you need to bring the treatments back in
seed_diversity$treatment <- substr(seed_diversity$seeds_sampleIDs, 1, 2)

# Shannon's
seed_diversity %>% 
  filter(!treatment %in% c("cc")) %>%
  ggplot() +
  geom_boxplot(mapping = aes(x = treatment, y = H))+
  theme_classic()

# linear model
shannon.lm <- lm(H ~ treatment, data = seed_diversity)
summary(shannon.lm)
plot(shannon.lm)


# Simpson's
seed_diversity %>% 
  filter(!treatment %in% c("cc")) %>%
  ggplot() +
  geom_boxplot(mapping = aes(x = treatment, y = simp))+
  theme_classic()

# beta regression for Simpson's
# deal with zeros, Rescale into (0,1)
seed_diversity$simp.adj <- (seed_diversity$simp * (nrow(seed_diversity) - 1) + 0.5) / nrow(seed_diversity)
simp.betareg <- betareg(simp.adj ~ treatment, data = seed_diversity)
summary(simp.betareg) # Positive estimate → higher diversity in that treatment
plot(residuals(simp.betareg))


# Evenness = dominance patterns 

# my data shows multiple samples without an evenness value likely because they had low abundance/no germinants, so I am going to ignore those rows in the analysis
# Simpson's
seed_diversity %>% 
  filter(!treatment %in% c("cc")) %>%
  ggplot() +
  geom_boxplot(mapping = aes(x = treatment, y = J))+
  theme_classic()

# beta regression for evenness
# I think it is also calulcated between 0-1 so use beta regression instead of linear model
evenness.betareg <- betareg(J ~ treatment, data = seed_diversity)
summary(evenness.betareg) # Positive estimate → higher diversity in that treatment
plot(residuals(evenness.betareg))
# not significantly different, so the two treatments are even


# Before finishing, save the species matrix to address community composition with ordination
write.csv(seeds_wide, "output/species_matrix.csv", row.names = FALSE)
# and keep the order of the sampleIDs just in case
write.csv(seeds_sampleIDs, "output/sampleIDs.csv", row.names = FALSE)
