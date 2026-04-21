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
# remove the control plots (for now)
species_matrix_noControls <- species_matrix[3:12, ]

# NMDS for rank distances (non-parametric, handles zeros)
species.nmds <- metaMDS(species_matrix_noControls, distance = "bray", k = 2, trymax = 100)
species.nmds
# bray-curtis is used 
plot(species.nmds, type = "t")
#plot(species.nmds, display = "sites")

#statistics
# remove control plots (for now)
sample_IDs_noControls <- sample_IDs %>% filter(!treatment %in% c("cc")) 

# adonis2() for statistical analysis
adonis2(species_matrix_noControls ~ treatment, data = sample_IDs_noControls, method = "bray")
# R2 tells you the effect size (proportion of community variation explained), F-statistic is the ratio of between-group vs. within-group variation, p-value tells you if the group centroids differ in community composition

# check dispersion to tell if the composition is different because of composition or because of dispersion
result <- betadisper(vegdist(species_matrix_noControls, "bray"), sample_IDs_noControls$treatment)
anova(result) # if the anova is significant then the groups may differ because of dispersion and not centroids

# extract nmds scores and plot them in ggplot instead:
# Extract species scores
species_scores <- as.data.frame(scores(species.nmds, display = "species"))
# Add a column for species names
species_scores$species <- rownames(species_scores)


data.scores <- as.data.frame(scores(species.nmds))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores$site <- sample_IDs_noControls$x  # create a column of site names, from the rownames of data.scores
data.scores$treatment <- sample_IDs_noControls$treatment  #  add the grp variable created earlier
head(data.scores)  #look at the data
#
species.scores <- as.data.frame(scores(species.nmds, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores)  #look at the data


# Visualization
ggplot() + 
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5) +  # add the species labels
  #geom_point(data=data.scores,aes(x=species.NMDS1,y=species.NMDS2,shape=treatment,colour=treatment),size=3) + # add the point markers
  #geom_text(data=data.scores,aes(x=sites.NMDS1,y=sites.NMDS2,label=site),size=6,vjust=0) +  # add the site labels
  #scale_colour_manual(values=c("A" = "red", "B" = "blue")) +
  coord_equal() +
  xlim(-2,2)+
  ylim(-2,2)+
  theme_bw()
# There is a lot to play around with here to display your specis and sites/treatments correctly for your project.




# you could add an indicator species analysis
# PCA - not good for raw counts, transform data first
