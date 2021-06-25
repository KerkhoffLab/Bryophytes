# 03 Lineage Analysis
# Lineage Analysis Code for Moss Manuscript
# Compiled by Hailey Napier, June 2021

# 0.0 Load Data and Packages, Source Functions -----
## 0.1 Load packages ----
#if (!requireNamespace("BiocManager", quietly = TRUE))
  #install.packages("BiocManager")
#BiocManager::install("ggtree")
#BiocManager::install("treeio")
#BiocManager::install("rphast")
install.packages("ggplot2")
install.packages("ggimage")
install.packages("tidyr")
install.packages("dplyr")
install.packages("gridExtra")
install.packages("png")
install.packages("phytools")
install.packages("ape")
install.packages("tools")

library(ggtree)
library(treeio)
library(ggplot2)
library(ggimage)
library(ape)
library(phytools)
library(tidyr)
library(dplyr)
library(tools)
library(grid)
library(gridExtra)
library(png)

## 0.2 Load data (generated in data processing script ***not yet finished***) ----
# From DataProcessing2020.R
MossPresence <- readRDS("Data/MossPresence.rds")
OrderNames <- readRDS("Data/OrderNames.rds")
MossOrderNames <- readRDS("Data/MossOrderNames.rds")
MossOrderRichList <- readRDS("Data/MossOrderRichList.rds")
MossRichnessVec <- readRDS("Data/MossRichnessVec.rds")
MossCellRichness <- readRDS("Data/MossCellRichness.rds")

## 0.3 Source function
source("Functions/ORange.R")


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

## 1.8 Manually update BryophytePresence .csv file to reflect family and order species assignments from Liu et al. (2019) ----

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

# 2.0 Group orders based on max alpha diversity ----

## 2.2 Create a dataframe including each moss order, its alpha diversity in each cell, and the biome associated with that cell ----
### If a cell has more than one biome, then the biome assigned is the one that covers the largest proportion of the cell
nrows <- 3639196
MossOrderBiomeDF <- data.frame(rep(NA,nrows))
names(MossOrderBiomeDF)[1] <- "Alpha"
MossOrderBiomeDF$Alpha <- NA
MossOrderBiomeDF$CellID <- NA
MossOrderBiomeDF$Order <- NA
MossOrderBiomeDF$Biome <- NA

start <- 1
end <- 15038
o <- MossOrderNames[1]
b <- BiomeNames[1]
MossOrderBiomeDF$Alpha[start:end] <- ORange("mosses",o,b, "clean")
MossOrderBiomeDF$CellID[start:end] <- c(1:15038)
MossOrderBiomeDF$Biome[start:end] <- b
MossOrderBiomeDF$Order[start:end] <- o

#start <- end + 1
#end <- start + 15037
#MossOrderBiomeDF$Alpha[start:end] <- ORange("mosses",o, b, "clean")
#MossOrderBiomeDF$CellID[start:end]<- c(1:15038)
#MossOrderBiomeDF$Biome[start:end] <- b
#MossOrderBiomeDF$Order[start:end] <- o

for(j in 2:length(BiomeNames)){
  b <- BiomeNames[j]
    start <- end + 1
  end <- start + 15037
  MossOrderBiomeDF$Alpha[start:end] <- ORange("mosses",o, b,"clean")
  MossOrderBiomeDF$CellID[start:end]<- c(1:15038)
  MossOrderBiomeDF$Biome[start:end] <- b
  MossOrderBiomeDF$Order[start:end] <- o
}

for(m in 2:length(MossOrderNames)){
  o <- MossOrderNames[m]
  for(n in 1:length(BiomeNames)){
    b <- BiomeNames[n]
    start <- end + 1
    end <- start + 15037
    MossOrderBiomeDF$Alpha[start:end] <- ORange("mosses",o, b, "clean")
    MossOrderBiomeDF$CellID[start:end]<- c(1:15038)
    MossOrderBiomeDF$Biome[start:end] <- b
    MossOrderBiomeDF$Order[start:end] <- o
  }
}

saveRDS(MossOrderBiomeDF, file = "Data/MossOrderBiomeDF.rds")


# 2.4 Make vectors based on max alpha diversity
#Make a dataframe to look at numbers for max alpha diversity 
MossOrderMaxAlpha <- data.frame(tapply(MossOrderBiomeDF$Alpha, MossOrderBiomeDF$Order, max, na.rm = T))
names(MossOrderMaxAlpha)[1]  <- "MaxAlpha"
Names <- sort(unique(MossOrderBiomeDF$Order))
MossOrderMaxAlpha$Names <- Names

# Put order names into vectors based on max alpha diversity 
MossOrdRichAbove100 <- vector()
MossOrdRich25to100 <- vector()
MossOrdRich10to25 <- vector()
MossOrdRichBelow10 <- vector()
for(i in 1:length(Names)){
  name <- Names[i]
  if(MossOrderMaxAlpha$MaxAlpha[i] > 100){
    MossOrdRichAbove100 <- c(MossOrdRichAbove100, name)
  }else if(MossOrderMaxAlpha$MaxAlpha[i] >= 25){
    MossOrdRich25to100 <- c(MossOrdRich25to100, name)
  }else if(MossOrderMaxAlpha$MaxAlpha[i] >= 10){
    MossOrdRich10to25 <- c(MossOrdRich10to25, name)
  }else if(MossOrderMaxAlpha$MaxAlpha[i] < 10){
    MossOrdRichBelow10 <- c(MossOrdRichBelow10, name)
  }
}

saveRDS(MossOrdRichAbove100, "Data/MossOrdRichAbove100.rds")
saveRDS(MossOrdRich25to100, "Data/MossOrdRich25to100.rds")
saveRDS(MossOrdRich10to25, "Data/MossOrdRich10to25.rds")
saveRDS(MossOrdRichBelow10, "Data/MossOrdRichBelow10.rds")





















#####################################
## 2.1 Make list of orders and corresponding richness values ----
### 2.11 Loop through order names and subset MossPresence for each order, store them in a list ----
MossOrderNames <- unique(MossPresence$Order)
MossOrderNames <- MossOrderNames[!is.na(MossOrderNames)]
saveRDS(MossOrderNames, file = "Data/MossOrderNames.rds")
NumberOrders <- length(MossOrderNames)
MossOrderList <- list()
for(i in 1:NumberOrders){
  ord <- MossOrderNames[i]
  MossOrderList[[i]] <- subset(MossPresence, Order == ord)
}

### 2.12 Loop through orders and tally richness for each order, store in a list ----
MossOrderRichList <- list()
MossOrderPresList <- list()
for(i in 1:NumberOrders){
  MossOrderPresList[[i]] <- tally(group_by(MossOrderList[[i]], CellID))
  names(MossOrderPresList[[i]])[2] <- "Richness"
  MossOrderRichList[[i]] <- numeric(15038)
  MossOrderRichList[[i]][MossOrderPresList[[i]]$CellID] <- MossOrderPresList[[i]]$Richness
  MossOrderRichList[[i]][which(MossOrderRichList[[i]]==0)] = NA
}

#### 2.13 Make dataframe for plotting
# 1.3 Make dataframe for plotting ----
Lat <- as.vector(LongLatDF$Latitude)
MossOrdLogAlphaDF <- data.frame("Order" = rep(NA, 330836), 
                                "Alpha" = rep(NA, 330836),
                                "LogAlpha" = rep(NA, 330836),
                                "LogTen" = rep(NA, 330836),
                                "Percent"  = rep(NA, 330836),
                                "CellID" = rep((1:15038), 22), 
                                "Latitude" = rep(Lat, 22))

for(i in 1:length(MossOrderNames)){
  order <- MossOrderNames[i]
  totalrich <- MOBMat[order, "Total"]
  if(order %in% MossOrdRichBelow10){
    group <- "Least diverse (10 or fewer species)"
  }else if(order %in% MossOrdRich10to25){
    group <- "Less diverse (11 - 25 species)"
  }else if(order %in% MossOrdRich25to100){
    group <- "More diverse (26 - 100 species)"
  }else if(order %in% MossOrdRichAbove100){
    group <- "Most diverse (greater than 100 species)"
  }
  list <- MossOrderRichList[[i]]
  start <- (i-1) * 15038 + 1
  end <- start + 15037
  MossOrdLogAlphaDF$Order[start:end] <- order
  MossOrdLogAlphaDF$Group[start:end] <- group
  for(j in 1:15038){
    index <- (i-1)*15038+j
    alpha <- as.numeric(list[j])
    log <- log(alpha)
    logten <- log10(alpha)
    percent <- (alpha/totalrich) * 100
    MossOrdLogAlphaDF$LogAlpha[index] <- log
    MossOrdLogAlphaDF$Alpha[index] <- alpha
    MossOrdLogAlphaDF$LogTen[index] <- logten
    MossOrdLogAlphaDF$Percent[index] <- percent
  }
}



# 3.0 Biome analysis ----
# 4.0 Latitude analysis ----

