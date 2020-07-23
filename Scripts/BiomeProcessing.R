#Biome Processing
#Process biome data from BiomeDiversity.R so it can be used with the OrdBiomeBP function
#Hailey Napier
#July 17, 2020

# Load packages
library(dplyr)

# Load data
BiomeRichness <- readRDS("Data/BiomeRichness.rds")


# 1.0 Filter BiomeRichness for each type and store the CellIDs in a vector -----------------------

# 1.1 Create a list of biome names
BiomeNames <- unique(BiomeRichness$Type)
NumberBiomes <- length(BiomeNames)

saveRDS(BiomeNames, "Data/BiomeNames.rds")

# 1.2 Loop through biomes and store CellIDs in a vector
for(i in 1:NumberBiomes){
  biome <- BiomeNames[i]
  name <-paste(biome, "Vec", sep = "")
  filename <- paste("Data/", name, ".rds", sep = "")
  
  x <- BiomeRichness %>%
    filter(BiomeRichness$Type == biome) %>%
    dplyr::select(CellID)
  
  x <- as.vector(x$CellID)
  
  assign(name, x)
  
  saveRDS(x, filename)
}


