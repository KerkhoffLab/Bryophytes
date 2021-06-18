#Updating taxonomy
#Hailey Napier
#July 2, 2020

#load packages
library(BIEN)
library(dplyr)

# 1.0 BIEN package-----------------------------
#From Family
#all NAs
BIENFamilies <- BIEN_taxonomy_family(FamilyNames)

BIENFamAndOrder <- BIENFamilies %>%
  dplyr::select(c(scrubbed_family, order))
BIENFamAndOrder <- BIENFamAndOrder[!duplicated(BIENFamAndOrder$scrubbed_family),]

# 2.0 From research----------------------------
BryophytePresence <- read.csv("Data/BryophytePresence_7.2.20(2).csv")
