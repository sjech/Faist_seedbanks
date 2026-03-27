# Project: Seed Bank
# Author: Sierra Gugel
# Date: March 2026
# Co-authors: Akasha Faist, Madeline Mayorga
# version: 1
# Purpose: This code should run after 01.data_wrangling. You will load the output data from the first script which should be a single .csv with seed counts by species and by sample type. This code will run analyses for total seeds, monocots only, dicots only, seed appendage, and single species analysis. 

# Load Libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
#library(lme4)

# Load the data
seedsummary <- read.csv("output/species_summary_table.csv")

# Total Seed Counts
# for this, we do not care about species at all. 
# summarize the data to the sample_ID level

sample_summary <- seedsummary %>%
  group_by(sample_ID, treatment) %>%
  summarise(totalSeeds_m3 = sum(seeds_m3))

# visualization
sample_summary %>%
  filter(treatment != "cc") %>%
  ggplot()+
  geom_boxplot(mapping = aes(x = treatment, y = totalSeeds_m3)) +
  geom_jitter(mapping = aes( x = treatment, y = totalSeeds_m3)) +
  theme_classic()


# statistical analysis 
# is there a difference in the total number of seeds that germinated for treatment 1 vs. treatment 2?
# try a linear model
# before running the model, actually remove the control pots from the analysis
sample_summary_noControls <- sample_summary %>% 
  filter(treatment != "cc")

# run the model
totalseed.lm <- lm(totalSeeds_m3 ~ treatment, data = sample_summary_noControls)
summary(totalseed.lm)
plot(totalseed.lm) # determine if a regular linear model is ok for your data 
# if you have random effects to include in the model, use the 'lme4' package and the function lmer()
# model interpretation: the intercept is positive and significant (p < 0.004) which just means that both treatments have positive averages (non-zero). Treatment 1 is not different from treatment 2 (F = 0.1447, df = 1 and 8, p = 0.71, adj. R2 = -0.105).


# Monocot Seed Counts
# for this, we care about only the monocots, so we will filter for that data and then run the same visualization and analysis
monocot_summary <- seedsummary %>%
  filter(treatment != "cc") %>%
  filter(m_d == "M") %>%
  group_by(sample_ID, treatment) %>%
  summarise(totalSeeds_m3 = sum(seeds_m3))

# visualization
monocot_summary %>%
  ggplot()+
  geom_boxplot(mapping = aes(x = treatment, y = totalSeeds_m3)) +
  geom_jitter(mapping = aes( x = treatment, y = totalSeeds_m3)) +
  theme_classic()


# statistical analysis 
# is there a difference in the total number of monocots that germinated for treatment 1 vs. treatment 2?
# try a linear model
monocotseed.lm <- lm(totalSeeds_m3 ~ treatment, data = monocot_summary)
summary(monocotseed.lm)
plot(monocotseed.lm) # determine if a regular linear model is ok for your data 
# if you have random effects to include in the model, use the 'lme4' package and the function lmer()
# model interpretation: the intercept is positive and significant (p < 0.007) which just means that both treatments have positive averages (non-zero). Treatment 1 is not different from treatment 2 (F = 1.084, df = 1 and 8, p = 0.33, adj. R2 = 0.009).



# Dicot Seed Counts
# for this, we care about only the dicots, so we will filter for that data and then run the same visualization and analysis
dicot_summary <- seedsummary %>%
  filter(treatment != "cc") %>%
  filter(m_d == "D") %>%
  group_by(sample_ID, treatment) %>%
  summarise(totalSeeds_m3 = sum(seeds_m3))

# visualization
dicot_summary %>%
  ggplot()+
  geom_boxplot(mapping = aes(x = treatment, y = totalSeeds_m3)) +
  geom_jitter(mapping = aes( x = treatment, y = totalSeeds_m3)) +
  theme_classic()


# statistical analysis 
# is there a difference in the total number of monocots that germinated for treatment 1 vs. treatment 2?
# try a linear model
dicotseed.lm <- lm(totalSeeds_m3 ~ treatment, data = dicot_summary)
summary(dicotseed.lm)
plot(dicotseed.lm) # determine if a regular linear model is ok for your data 
# if you have random effects to include in the model, use the 'lme4' package and the function lmer()
# model interpretation: the intercept is positive but not significant (p =0.08) which means that the average number of dicots in the soil is not significantly different from zero. Treatment 1 is not different from treatment 2 (F = 2.1, df = 1 and 3, p = 0.24, adj. R2 = 0.22).


# Seed appendage - does a seed appendage affect germination rates in treatment 1 vs. treatment 2 differently?
# for this, we will sort the species into yes seed appendage or no seed appendage categories and then run treatment level analyses. you could do something similar for plant nativeness, invasiveness, or other important characteristics

appendage_summary <- seedsummary %>%
  filter(treatment != "cc") %>%
  group_by(sample_ID, treatment, seed_appendage) %>%
  summarise(totalSeeds_m3 = sum(seeds_m3), .groups = "drop")

# you might want to consider what should be done about samples with no value. should those be zeros? If so, there are ways of adding rows for zeros because that will impact the statistical analysis. 

appendage_summary %>%
  ggplot(aes(x = treatment, y = totalSeeds_m3, fill = seed_appendage)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  geom_jitter(aes(color = seed_appendage),
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8)) +
  theme_classic()


# statistical analysis 
# is there a difference in the total number of germinated seeds that had an appendage or not for the two treatments?
# try a linear model
appendage.lm <- lm(totalSeeds_m3 ~ treatment+seed_appendage, data = appendage_summary)
summary(appendage.lm)
plot(appendage.lm) # determine if a regular linear model is ok for your data 
# if you have random effects to include in the model, use the 'lme4' package and the function lmer()
# model interpretation: the intercept is positive (non-zero). Treatment 1 is not different from treatment 2. And seed appendage was not different.


# Species - for an individual species, are there treatment differences?
# Try BRTE (cheatgrass)
brte_summary <- seedsummary %>%
  filter(treatment != "cc") %>%
  filter(species_code == "BRTE") %>%
  group_by(sample_ID, treatment) %>%
  summarise(totalSeeds_m3 = sum(seeds_m3), .groups = "drop")

# you might want to consider what should be done about samples with no value. should those be zeros? If so, there are ways of adding rows for zeros because that will impact the statistical analysis. 

brte_summary %>%
  ggplot(mapping = aes(x = treatment, y = totalSeeds_m3)) +
  geom_boxplot()+
  geom_jitter()+
  theme_classic()


# statistical analysis 
# is there a difference in the total number of germinated seeds that had an appendage or not for the two treatments?
# try a linear model
brte.lm <- lm(totalSeeds_m3 ~ treatment, data = brte_summary)
summary(brte.lm)
plot(brte.lm) # determine if a regular linear model is ok for your data 
# if you have random effects to include in the model, use the 'lme4' package and the function lmer()
# model interpretation: cheatgrass germinated and has a positive (non-zero) average. It is present. Treatment 1 is not different from treatment 2 (F=0.32, df = 1 and 5, p = 0.6, adj. R2 = -0.13)


### All species at once
# Species - for an individual species, are there treatment differences?
# Try BRTE (cheatgrass)
species_summary <- seedsummary %>%
  filter(treatment != "cc") %>%
  group_by(sample_ID, treatment, species_code) %>%
  summarise(totalSeeds_m3 = sum(seeds_m3), .groups = "drop")

# you might want to consider what should be done about samples with no value. should those be zeros? If so, there are ways of adding rows for zeros because that will impact the statistical analysis. 

species_summary %>%
  ggplot(aes(x = treatment, y = totalSeeds_m3, fill = species_code)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  geom_jitter(aes(color = species_code),
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8)) +
  theme_classic()


# statistical analysis 
# is there a difference in the total number of germinated seeds that had an appendage or not for the two treatments?
# try a linear model
species.lm <- lm(totalSeeds_m3 ~ treatment+species_code, data = species_summary)
summary(species.lm)
plot(species.lm) # determine if a regular linear model is ok for your data 
# if you have random effects to include in the model, use the 'lme4' package and the function lmer()
# model interpretation: there are seedlings in the pot (positive and significant intercept). There are no differences in treatment 1 or 2 and no differences by species

# From here, you could go into community composition analyses (PCA or PCoA) to deal with multi-dimensional data