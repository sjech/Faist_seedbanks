# Project: Seed Bank
# Author: Sierra Gugel
# Date: March 2026
# Co-authors: Akasha Faist, Madeline Mayorga
# version: 1
# Purpose: This code should run after 01.data_wrangling and 03.diversity_analyses. You will load the output data from the third script which should be two .csv files (species matrix and sample IDs). This code will use ordination methods to assess community composition.

# Load Libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(vegan)
#library(betareg) # for assessing Simpson's metrics, use instead of linear model

# Load the data
species_matrix <- read.csv("output/species_matrix.csv")
sample_IDs <- read.csv("output/sampleIDs.csv")

# add a treatment column to the sample_IDs or upload a environmental matrix
sample_IDs$treatment <- substr(sample_IDs$x, 1, 2)



# Do the communities differ in composition?

# NMDS for rank distances (non-parametric, handles zeros)
species.nmds <- metaMDS(species_matrix, distance = "bray", k = 2, trymax = 100)
# bray-curtis is used 
plot(species.nmds)
plot(species.nmds, display = "sites")
#statistics
adonis2(species_matrix ~ treatment, data = sample_IDs, method = "bray")
# R2 tells you the effect size (proportion of community variation explained), F-statistic is the ratio of between-group vs. within-group variation, p-value tells you if the group centroids differ in community composition
# check dispersion to tell if the composition is different because of composition or because of dispersion
result <- betadisper(vegdist(species_matrix, "bray"), sample_IDs$treatment)
anova(result) # if the anova is significant then the groups may differ because of dispersion and not centroids

# you could add an indicator species analysis



# PCA - not good for raw counts, transform data first
