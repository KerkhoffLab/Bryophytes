#Grouping biomes by hemisphere
#Hailey Napier
#July 20, 2020

#Load packages -------------------------------------
library(dplyr)

#Load data ------------------------------------------
BiomeRichness <- readRDS("Data/BiomeRichness.rds")



# 1.0 Loop to bin biomes by hemisphere --------------
 
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
saveRDS(BinnedBiomeRichness, "Data/BinnedBiomeRichness.rds")


