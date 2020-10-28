# GAM
# Testing out the moss generalized additive model
# October 2020

# 0.0 FIRST -------------------------------------
# 0.1 Load Packages
library(dplyr)
library(raster)

# 0.2 Load Data
MossOrderRichList <- readRDS("Data/MossOrderRichList.rds")
MossOrderNames <- readRDS("Data/MossOrderNames.rds")
RichnessVec <- readRDS("Data/RichnessVec.rds")
BiomeCells <- readRDS("Data/BiomeCellsClean.rds")


# 1.0 Get WorldClim aata ------------------------


# 2.0 Make biome vector
AllBiomeCells <- data.frame(CellID = rep(1:15038))
AllBiomeCells$Biome <- NA
for(i in BiomeCells$CellID){
  biome <- BiomeCells$Type[which(BiomeCells$CellID == i)]
  AllBiomeCells$Biome[which(AllBiomeCells$CellID == i)] <- biome
}

BiomeVec <- AllBiomeCells$Biome


# 2.0 Create the dataframe ----------------------
# 2.1 Make base dataframe
nrows <- 15038*22
GAMDF <- data.frame(CellID = rep(1:15038, 22))
GAMDF$TotalRichness <- rep(RichnessVec, 22)
GAMDF$OrderName <- NA
GAMDF$OrderRichness <- NA
GAMDF$Biome <- rep(BiomeVec, 22)

# 2.2 Add order names and order richness
start <- 1
end <- 15038
for(i in 1:22){
  ordername <- MossOrderNames[i]
  richlist <- MossOrderRichList[[i]]
  GAMDF$OrderName[start:end] <- ordername 
  GAMDF$OrderRichness[start:end] <- richlist
  start = end + 1
  end = start + 15037
}




