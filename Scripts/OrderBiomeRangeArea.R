# Order/Biome Quantitative Analysis for Presentation
# Hailey Napier
# September 2020

# Make a dataframe with each order and the percent range area for each biome
## Also include a measure of variablilty if possible (not sure what yet)


# 0.0 Load Data ------------------------------------------------------------
#From DataProcessing2020.R
MossPresence <- readRDS("Data/MossPresence.rds")
MossOrderNames <- readRDS("Data/MossOrderNames.rds")
BiomeNames <- readRDS("Data/BiomeNames.rds")
# From BiomeBetaCellsClean.R
BiomeBetaCellsClean <- readRDS("Data/BiomeBetaCellsClean.rds")


# 1.0 Make CellID/biome data frames for each order -------------------------
NumberOrders <- length(MossOrderNames)
MossOrderBiomeList <- list()

for(i in 1:NumberOrders){
  order <- MossOrderNames[i]
  CellID <- MossPresence %>%
    dplyr::filter(Order == order) %>%
    dplyr::select(CellID)
  CellID <- unique(as.vector(CellID$CellID))
  DF <- data.frame(CellID)
  DF$Biome <- NA
  for(j in CellID){
    cell <- j
    biome <- BiomeCellsClean$Type[which(BiomeBetaCellsClean$CellID == cell)]
    if(length(biome) > 0){
      DF$Biome[which(DF$CellID == cell)] <- biome
    }
  }
  DF$Order <- order
  MossOrderBiomeList[[i]] <- DF
}


# 2.0 Find percentage of cells with biome for biomes in each order --------------
# 2.1 Make a matrix with total cell counts for each order within each biome
NumberBiomes <- 11
BiomeNamesAndTotal <- c(BiomeNames, "Total")

#MossOrderBiome = MOB
MOBMat <- matrix(NA, 22, 12)
rownames(MOBMat) <- MossOrderNames
colnames(MOBMat) <- BiomeNamesAndTotal

for(i in 1:NumberOrders){
  order <- MossOrderNames[i]
  
  DF <- MossOrderBiomeList[[i]]
  DF <- DF %>%
    dplyr::filter(!is.na(DF$Biome))
  
  total <- nrow(DF)
  biome = "Total"
  MOBMat[order, biome] <- total
  
  biomes <- DF %>%
    dplyr::select(Biome)
  biomes <- unique(as.vector(biomes$Biome))
  print(order)
  print(biomes)
  
  for(j in 1:length(biomes)){
    biome = biomes[j]
    nbiome <- DF %>%
      dplyr::filter(DF$Biome == biome)
    nbiome <- nrow(nbiome)
    
    MOBMat[order, biome] <- nbiome
  }
}

# Find percentages based on circle plot data and put into a table
MOBPercentMat <- MOBMat
for(i in 1:NumberOrders){
  order <- MossOrderNames[i]
  total <- MOBPercentMat[order, "Total"]
  for(j in 1:NumberBiomes){
    biome <- BiomeNames[j]
    biome_total <- MOBPercentMat[order, biome]
    percent <- biome_abundance/total*100
    MOBPercentMat[order, biome] <- percent
  }
}

