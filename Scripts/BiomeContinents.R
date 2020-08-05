# BiomeContinents
# Separating biome data by continent
# Hailey Napier, July 2020

# Load Data ----------------------------------------------------------------------------------
# Find in BiomeDiversity.R
ContBiomeRichness <- readRDS("Data/BiomeRichness.rds")
BiomeNames <- unique(BiomeRichness$Type)


# 1.0 Add Continent column to dataframe ------------------------------------------------------
ContBiomeRichness$Cont <- NA

for(i in 1:nrow(ContBiomeRichness)){
  cell = ContBiomeRichness$CellID[i]
  if(cell %in% NorthAmericaVec){
    ContBiomeRichness$Cont[i] <- "NorthAmerica"
  }else if(cell %in% SouthAmericaVec){
    ContBiomeRichness$Cont[i] <- "SouthAmerica"
  }
}


# 2.0 Filter dataframe by continent and extract cell IDs for each biome, put in a list -------
# 2.1 North America
NorthAmBiomeList <- list()
for(i in 1:length(BiomeNames)){
  b <- BiomeNames[i]
  vec <- ContBiomeRichness %>%
    filter(ContBiomeRichness$Cont == "NorthAmerica") %>%
    filter(Type == b)
  vec <- as.vector(vec$CellID)
  NorthAmBiomeList[[i]] <- vec
}

# 2.2 South America
SouthAmBiomeList <- list()
for(i in 1:length(BiomeNames)){
  b <- BiomeNames[i]
  vec <- ContBiomeRichness %>%
    filter(ContBiomeRichness$Cont == "SouthAmerica") %>%
    filter(Type == b)
  vec <- as.vector(vec$CellID)
  SouthAmBiomeList[[i]] <- vec
}

# 2.3 Save lists
saveRDS(NorthAmBiomeList, "Data/NorthAmBiomeList.rds")
saveRDS(SouthAmBiomeList, "Data/SouthAmBiomeList.rds")

