# Biome Beta Diversity
# Hailey Napier,  July 2020
# Adapted from FamilyDiversity.R

#DON'T NEED THIS SCRIPT!!! USE BIOMEBETACELLS.R!

# Load packages
library(reshape2)

# Load data
LongLatBetaDF <- readRDS("Data/LongLatBetaDF.rds")
LongLatDF <- readRDS("Data/LongLatDF.rds")
BiomeCellsWeighted <- readRDS("Data/BiomeCellsWeighted.rds")
BiomeCellsCentCov <- readRDS("Data/BiomeCellsCentCov.rds")
BiomeCellsClean <- readRDS("Data/BiomeCellsClean.rds")
LongLatBetaCellDF <- full_join(LongLatDF, LongLatBetaDF, by = "Longitude")


# 1.0 DATA PROCESSING ------------------------------------------------------------------
# Convert biome column from factor to character
BiomeCellsWeighted$Biome <- as.character(BiomeCellsWeighted$Type)
BiomeCellsCentCov$Biome <- as.character(BiomeCellsCentCov$Type)
BiomeCellsClean$Biome <- as.character(BiomeCellsClean$Type)


# 2.0 WRITE DATAFRAME -------------------------------------------------------------------
# 2.1; weight = F dataframe (only count cells if a biome covers the center)
#Loop through cells and assign biome and beta diversity
BiomeBetaCentCells <- data.frame("CellID" = 1:15038)
BiomeBetaCentCells$Beta <- NA
BiomeBetaCentCells$Biome <- NA

for(i in 1:15038){
  cell <- i
  biome <- BiomeCellsCentCov$Type[which(BiomeCellsCentCov$CellID == cell)]
  if(length(biome) == 0){
    biome = NA
  }
  beta <- LongLatBetaCellDF$Beta[which(LongLatBetaCellDF$CellID == cell)]
  BiomeBetaCentCells$Biome[i] <- biome
  BiomeBetaCentCells$Beta[i] <- beta
}


# Subset NAs out for plotting
NoNACentCells <- BiomeBetaCentCells[which(!is.na(BiomeBetaCentCells$Biome)),]

# Save dataframe
saveRDS(NoNACentCells, "Data/NoNaCentCells.R")


# 2.2; weight = T (count all cells that are at all covered by a biome -- repeat cells if covered by more than one biome)
BiomeBetaAllWeight<- data.frame("CellID" = 1:15038)
BiomeBetaAllWeight$Beta <- NA
BiomeBetaAllWeight$Biome <- NA

for(i in 1:15038){
  cell <- i
  biome <- BiomeCellsWeighted$Type[which(BiomeCellsWeighted$CellID == cell)]
  if(length(biome) == 0){
    biome = NA
  }
  beta <- LongLatBetaCellDF$Beta[which(LongLatBetaCellDF$CellID == cell)]
  BiomeBetaAllWeight$Biome[i] <- biome
  BiomeBetaAllWeight$Beta[i] <- beta
}

# Subset NAs out for plotting
NoNAWeightedCells <- BiomeBetaAllWeight[which(!is.na(BiomeBetaAllWeight$Biome)),]

# Save dataframe
saveRDS(NoNAWeightedCells, "Data/NoNaWeightedCells.R")


# 2.3; weight = T (omit repeats by choosing biome that covers the higher proportion of the cell)
BiomeBetaCleanWeight<- data.frame("CellID" = 1:15038)
BiomeBetaCleanWeight$Beta <- NA
BiomeBetaCleanWeight$Biome <- NA

for(i in 1:15038){
  cell <- i
  biome <- BiomeCellsClean$Type[which(BiomeCellsClean$CellID == cell)]
  if(length(biome) == 0){
    biome = NA
  }
  beta <- LongLatBetaCellDF$Beta[which(LongLatBetaCellDF$CellID == cell)]
  BiomeBetaCleanWeight$Biome[i] <- biome
  BiomeBetaCleanWeight$Beta[i] <- beta
}

# Subset NAs out for plotting
NoNAClean <- BiomeBetaCleanWeight[which(!is.na(BiomeBetaCleanWeight$Biome)),]

# Save dataframe
saveRDS(NoNAClean, "Data/NoNaClean.R")
