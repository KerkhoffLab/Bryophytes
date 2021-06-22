#03 Lineage Analysis
#Lineage Analysis Code for Moss Manuscript
#Compiled by Hailey Napier, June 2021

# 0.0 Load Packages -----
library(ape)
library(phytools)

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("ggtree")
BiocManager::install("treeio")
BiocManager::install("rphast")
library(ggtree)
library(treeio)
library(ggplot2)
library(ggimage)

library(tidyr)
library(dplyr)
library(tools)

library(grid)
library(gridExtra)

library(png)

# 1.0 Read and process Liu et al (2019) phologenetic tree ----

## 1.1 Download tree ----
  # Download tree from Dryad via https://datadryad.org/stash/dataset/doi:10.5061/dryad.tj3gd75
  # Put downloaded tree into Data/trees 

## 1.2 Read tree into script (from Data/trees) ----
FigS20 <- read.tree("Data/trees/aa-nu-RAxML-FigS20.tre")

## 1.3 Extract tree data and store as a dataframe ----
FigS20_FOG <- ggtree(FigS20)$data

FigS20_names <- data.frame(FigS20_FOG$label <- gsub("_", " ", FigS20_FOG$label))
names(FigS20_names)[1] <- "name"
FigS20_names <- separate(FigS20_names, name, into = c("genus","other"))
FigS20_names$other <- NULL
FigS20_names$family <- NA
FigS20_names$order <- NA
FigS20_names$group <- NA

## 1.4 Make list of genus names in our data ----
unique(BryophytePresence$Family[which(BryophytePresence$Genus == FigS20_FOG$genus[i])])
GenusNames <- unique(BryophytePresence$Genus)
GenusNames <- GenusNames[complete.cases(GenusNames)]

## 1.5 Clean up dataframe and assign genus and family names ----
for(i in 1:nrow(FigS20_names)){
  n <- FigS20_names$genus[i]
  FigS20_names$genus[i] <- ifelse(n == "sphagnumL" | n == "sphagnumP" | n == "sphagnumR", "Sphagnum", n)
  FigS20_names$genus[i] <- gsub('[[:digit:]]+', '', FigS20_names$genus[i])
  FigS20_names$genus[i] <- toTitleCase(FigS20_names$genus[i])
  test.genus <- FigS20_names$genus[i]
  for(j in 1:length(GenusNames)){
    ref.genus <- GenusNames[j]
    if(test.genus == ref.genus){
      FigS20_names$family[i] <- unique(BryophytePresence$Family[which(BryophytePresence$Genus == test.genus)])
      FigS20_names$order[i] <- unique(BryophytePresence$Order[which(BryophytePresence$Genus == test.genus)])
      FigS20_names$group[i] <- unique(BryophytePresence$Group[which(BryophytePresence$Genus == test.genus)])
      break
    }
  }
}

FigS20_FOG <- bind_cols(FigS20_FOG, FigS20_names)
write.csv(FigS20_FOG, "Data/FamilyTrees/FigS20_FOG.csv")

## 1.6 Group tip labels into families ----
### 1.61 Get family names for each tip label ----
#### Make a dataframe that contains the tip labels and their families for reference
clean20famlabels <- FigS20_FOG %>%
  filter(isTip == "TRUE") %>%
  select(family)
Ref20DF <- data.frame(FigS20$tip.label)
Ref20DF$Family <- clean20famlabels
names(Ref20DF)[1] <- "Species"

### 1.62 Loop to find species in each family ----
#### Creates a vector of species/tip label names for each family name
FigS20fam <- F20Fam$Family
FigS20fam
for(i in 1:length(FigS20fam)){
  fam <- FigS20fam[i]
  tempdf <- Ref20DF %>%
    filter(Family == fam)
  tempvec <- tempdf$Species
  assign(fam, tempvec)
}

## 1.7 Group tip lables into orders ----
### 1.71 Get order names for each tip label ----
#### Get order names
clean20orderlabels <- FigS20_FOG %>%
  filter(FigS20_FOG$isTip == "TRUE") %>%
  select(order)
Ref20DF <- data.frame(FigS20$tip.label)
Ref20DF$Family <- clean20famlabels
Ref20DF$Order <- clean20orderlabels
names(Ref20DF)[1] <- "Species"

### 1.72 Loop to find species in each order ----
#### Creates a vector of species names for each order name
FigS20order <- unique(FigS20_FOG$order)
FigS20order <- FigS20order[complete.cases(FigS20order)]
FigS20order
FigS20orderlist <- list()
for(i in 1:length(FigS20order)){
  order <- FigS20order[i]
  tempdf <- Ref20DF %>%
    filter(Order == order)
  tempvec <- tempdf$Species
  FigS20orderlist[[i]] <- tempvec
}

#UNFINISHED STARTING HERE -------------
# NEED (for OrderAlphaScatter.R)
  # From DataProcessing2020.R
    # MossPresence 
    # OrderNames
    # MossOrderNames
  # From OneScaleFamRichMaps.R
    # OrderRichList
  # From OrderBiomePercentSpRich.R
    # MOBPerMatSpecies <- readRDS("Data/MOBPerMatSpecies.rds")

