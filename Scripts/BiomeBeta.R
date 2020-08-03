# Biome Beta Diversity
# Hailey Napier,  July 2020
# Adapted from FamilyDiversity.R

# Load packages
library(reshape2)

# Load data
LongLatBetaDF <- readRDS("Data/LongLatBetaDF.rds")
LongLatDF <- readRDS("Data/LongLatDF.rds")
BiomeCellsDF <- readRDS("Data/BiomeCells.rds")

LongLatBetaCellDF <- full_join(LongLatDF, LongLatBetaDF, by = "Longitude")

BiomeBetaMat <- matrix(NA, length(BiomeNames), c(15038))
rownames(BiomeBetaMat) <- BiomeNames



for(i in 1:length(BiomeNames)){
  biome <- BiomeNames[i]
  cells <- BiomeCellsDF %>%
    filter(Biome == biome)
  cells <- as.vector(cells$CellID)
  for(j in cells){
    cell <- j
    beta <- LongLatBetaCellDF$Beta[which(LongLatBetaCellDF$CellID == cell)]
    if(!is.na(beta)){
      BiomeBetaMat[biome,cell] <- beta
    }
  }
}

BiomeBeta <- data.frame(t(BiomeBetaMat))
BiomeBetaDF <- data.frame("CellID" = rep(c(1:15038), 11))

end <- 0
for(i in 1:length(BiomeNames)){
  biome <- BiomeNames[i]
  biomebetavec <- BiomeBeta[,i]
  start <- end + 1
  end <- start + 15037
  for(j in start:end){
    cell <- j
    if(j %in% c(1:15038)){
      betaindex <- j
    }else{
      betaindex <- j %% 15038 + 1
    }
    BiomeBetaDF$Biome[j] <- biome
    BiomeBetaDF$Beta[j] <- biomebetavec[betaindex]
  }
}


saveRDS(BiomeBetaDF, "Data/BiomeBetaDF.rds")
