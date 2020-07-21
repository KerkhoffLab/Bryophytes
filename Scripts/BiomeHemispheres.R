#Grouping biomes by hemisphere
#Hailey Napier
#July 20, 2020

#Load packages -----------------------------------------------------------------------------
library(dplyr)

#Load data ---------------------------------------------------------------------------------
BiomeRichness <- readRDS("Data/BiomeRichness.rds")


# 1.0 Loop to bin biomes by hemisphere -----------------------------------------------------
 
# 1.1 Make a vector of biome names
BiomeNames <- unique(BiomeRichness$Type)

# 1.2 Create the binned dataframe
biome <- BiomeNames[1]
s <- paste("Southern", biome, sep = "_")
n <- paste("Northern", biome, sep = "_")

BinnedBiomeRichness <- BiomeRichness %>%
  filter(BiomeRichness$Type == biome)

BinnedBiomeRichness$BinHem = cut(BinnedBiomeRichness$Latitude, c(-70,0,70), labels = c(s, n))
table(BinnedBiomeRichness$BinHem)

# 1.3 Loop through names and cut by hemisphere
for(i in 2:length(BiomeNames)){
  biome <- BiomeNames[i]
  #make labels
  s <- paste("Southern", biome, sep = "_")
  n <- paste("Northern", biome, sep = "_")
  
  #subset BiomeRichness and add bins
  tempdf <- BiomeRichness %>%
    filter(BiomeRichness$Type == biome)
  tempdf$BinHem = cut(tempdf$Latitude, c(-70,0,70), labels = c(s, n))
  
  #add biome to larger binned biome dataframe
  BinnedBiomeRichness <- bind_rows(BinnedBiomeRichness, tempdf)
}

table(BinnedBiomeRichness$BinHem)


# 2.0 Bin all observations by hemisphere, regardless of biome type ------------------------
BinnedBiomeRichness$AllObsHem = cut(BinnedBiomeRichness$Latitude, c(-70,0,70), labels = c("Southern", "Northern"))
table(BinnedBiomeRichness$AllObsHem)

saveRDS(BinnedBiomeRichness, "Data/BinnedBiomeRichness.rds")


# 3.0 Create lists of richness vectors for the individual biomes in each hemisphere -------

# 3.1 Northern hemisphere
NorthBiomeRichList <- list()
for(i in 1:length(BiomeNames)){
  biome  <- BiomeNames[i]
  cellvec <- BinnedBiomeRichness %>%
    filter(BinnedBiomeRichness$AllObsHem == "Northern") %>%
    filter(Type == biome) %>%
    select(CellID)
  cellvec <- as.vector(cellvec)
  NorthBiomeRichList[i] <- cellvec
}

saveRDS(NorthBiomeRichList, "Data/NorthBiomeRichList.rds")

# 3.2 Southern hemisphere
SouthBiomeRichList <- list()
for(i in 1:length(BiomeNames)){
  biome  <- BiomeNames[i]
  cellvec <- BinnedBiomeRichness %>%
    filter(BinnedBiomeRichness$AllObsHem == "Southern") %>%
    filter(Type == biome) %>%
    select(CellID)
  cellvec <- as.vector(cellvec)
  SouthBiomeRichList[i] <- cellvec
}

saveRDS(SouthBiomeRichList, "Data/SouthBiomeRichList.rds")

