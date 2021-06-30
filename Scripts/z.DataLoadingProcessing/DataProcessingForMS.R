# Data Processing for the Moss MS
# Compiled by Hailey Napier
# June 2021

# 0.0 Load Packages

#### ROUGH ORDER ####

# ?.0 Find order names -------
OrderNames <- unique(BryophytePresence$Order)
OrderNames <- OrderNames[!is.na(OrderNames)]
NumberOrders <- length(OrderNames)

      # Tally richness by cell and create richness vector
      #CellRichness <- tally(group_by(BryophytePresence, CellID))
      #colnames(CellRichness)[2] <- "Richness"
      #RichnessVec <- numeric(15038)
      #RichnessVec[CellRichness$CellID] <- CellRichness$Richness

# ?.0 Make MossPresence and find moss order names ----
MossPresence <- BryophytePresence %>%
  filter(Group == "Moss" | Group == "Mosses")
MossOrderNames <- unique(MossPresence$Order)

# Tally moss richness by cell and create richness vector
MossCellRichness <- tally(group_by(MossPresence, CellID))
colnames(MossCellRichness)[2] <- "Richness"
MossRichnessVec <- numeric(15038)
MossRichnessVec[MossCellRichness$CellID] <- MossCellRichness$Richness
MossRichnessVec[which(MossRichnessVec==0)]=NA

# Make moss richness raster for plotting
MossRichnessRaster <- setValues(BlankRas, MossRichnessVec)

# Save richness and presence data
saveRDS(OrderNames, file = "Data/OrderNames.rds")
saveRDS(MossCellRichness, file = "Data/MossCellRichness.rds")
saveRDS(MossRichnessVec, file = "Data/MossRichnessVec.rds")
saveRDS(BryophytePresence, file = "Data/BryophytePresence.rds")
saveRDS(MossPresence, file = "Data/MossPresence.rds")
saveRDS(MossOrderNames, file = "Data/MossOrderNames.rds")
saveRDS(MossRichnessRaster, "Data/MossRichnessRaster.rds")

# ?.0 Make MossOrderRichList for ORange function ----
OrderList <- list()
for(i in 1:length(OrderNames)){
  ord <- OrderNames[i]
  OrderList[[i]] <- subset(BryophytePresence, Order == ord)
}

OrderRichList <- list()
OrderPresList <- list()
for(i in 1:length(OrderNames)){
  OrderPresList[[i]] <- tally(group_by(OrderList[[i]], CellID))
  names(OrderPresList[[i]])[2] <- "Richness"
  OrderRichList[[i]] <- numeric(15038)
  OrderRichList[[i]][OrderPresList[[i]]$CellID] <- OrderPresList[[i]]$Richness
  OrderRichList[[i]][which(OrderRichList[[i]]==0)] = NA
}

saveRDS(OrderRichList, file = "Data/OrderRichList.rds")

# ?.0 Biome MOSS BETA Diversity Data -------------------------------------------------------------

#WEIGHTED DATAFRAME --------------------------------------------------------
#Coniferous Forests --------------------------------------------------------
WeightedConFor <- raster::extract(LongLatMossBetaRaster, Coniferous_Forests, df = TRUE, cellnumbers = TRUE, weight = TRUE)
colnames(WeightedConFor) <- c("Type", "CellID", "Beta", "Weight")
WeightedConFor$Type <- "Coniferous_Forests"
WeightedConForVec <- WeightedConFor$CellID

MossConForClean <- WeightedConFor
ConForCellID <- unique(WeightedConForVec)

for(i in ConForCellID){
  vec <- MossConForClean$Weight[which(MossConForClean$CellID == i)]
  if(length(vec) > 1){
    max <- max(vec)
    drop <- which(MossConForClean$CellID == i & MossConForClean$Weight != max)
    MossConForClean <- MossConForClean[-drop,]
  }
}

MossConForVec <- MossConForClean$CellID

saveRDS(MossConForClean, "Data/Moss_Coniferous_Forests_Beta_Biome_DF.rds")
saveRDS(MossConForVec, "Data/Coniferous_ForestsCleanVec.rds")

#Dry Forest ----------------------------------------------------------------
WeightedDryFor <- raster::extract(LongLatMossBetaRaster, Dry_Forest, df = TRUE, cellnumbers = TRUE, weight = TRUE)
colnames(WeightedDryFor) <- c("Type", "CellID", "Beta", "Weight")
WeightedDryFor$Type <- "Dry_Forest"
WeightedDryForVec <- WeightedDryFor$CellID

MossDryForClean <- WeightedDryFor
DryForCellID <- unique(WeightedDryForVec)

for(i in DryForCellID){
  vec <- MossDryForClean$Weight[which(MossDryForClean$CellID == i)]
  if(length(vec) > 1){
    max <- max(vec)
    drop <- which(MossDryForClean$CellID == i & MossDryForClean$Weight != max)
    MossDryForClean <- MossDryForClean[-drop,]
  }
}

MossDryForVec <- MossDryForClean$CellID

saveRDS(MossDryForClean, "Data/Moss_Dry_Forest_Beta_Biome_DF.rds")
saveRDS(MossDryForVec, "Data/Dry_ForestCleanVec.rds")

#Mediterranean Woodlands ---------------------------------------------------
WeightedMedWood <- raster::extract(LongLatMossBetaRaster, Mediterranean_Woodlands, df = TRUE, cellnumbers = TRUE, weight = TRUE)
colnames(WeightedMedWood) <- c("Type", "CellID", "Beta", "Weight")
WeightedMedWood$Type <- "Mediterranean_Woodlands"
WeightedMedWoodVec <- WeightedMedWood$CellID

MossMedWoodClean <- WeightedMedWood
MedWoodCellID <- unique(WeightedMedWoodVec)

for(i in MedWoodCellID){
  vec <- MossMedWoodClean$Weight[which(MossMedWoodClean$CellID == i)]
  if(length(vec) > 1){
    max <- max(vec)
    drop <- which(MossMedWoodClean$CellID == i & MossMedWoodClean$Weight != max)
    MossMedWoodClean <- MossMedWoodClean[-drop,]
  }
}

MossMedWoodVec <- MossMedWoodClean$CellID

saveRDS(MossMedWoodClean, "Data/Moss_Mediterranean_Woodlands_Beta_Biome_DF.rds")
saveRDS(MossMedWoodVec, "Data/Mediterranean_WoodlandsCleanVec.rds")

#Moist Forest --------------------------------------------------------------
WeightedMoistFor <- raster::extract(LongLatMossBetaRaster, Moist_Forest, df = TRUE, cellnumbers = TRUE, weight = TRUE)
colnames(WeightedMoistFor) <- c("Type", "CellID", "Beta", "Weight")
WeightedMoistFor$Type <- "Moist_Forest"
WeightedMoistForVec <- WeightedMoistFor$CellID

MossMoistForClean <- WeightedMoistFor
MoistForCellID <- unique(WeightedMoistForVec)

for(i in MoistForCellID){
  vec <- MossMoistForClean$Weight[which(MossMoistForClean$CellID == i)]
  if(length(vec) > 1){
    max <- max(vec)
    drop <- which(MossMoistForClean$CellID == i & MossMoistForClean$Weight != max)
    MossMoistForClean <- MossMoistForClean[-drop,]
  }
}

MossMoistForVec <- MossMoistForClean$CellID

saveRDS(MossMoistForClean, "Data/Moss_Moist_Forest_Beta_Biome_DF.rds")
saveRDS(MossMoistForVec, "Data/Moist_ForestCleanVec.rds")

#Savannas ------------------------------------------------------------------
WeightedSavanna <- raster::extract(LongLatMossBetaRaster, Savannas, df = TRUE, cellnumbers = TRUE, weight = TRUE)
colnames(WeightedSavanna) <- c("Type", "CellID", "Beta", "Weight")
WeightedSavanna$Type <- "Savannas"
WeightedSavannaVec <- WeightedSavanna$CellID

MossSavannaClean <- WeightedSavanna
SavannaCellID <- unique(WeightedSavannaVec)

for(i in SavannaCellID){
  vec <- MossSavannaClean$Weight[which(MossSavannaClean$CellID == i)]
  if(length(vec) > 1){
    max <- max(vec)
    drop <- which(MossSavannaClean$CellID == i & MossSavannaClean$Weight != max)
    MossSavannaClean <- MossSavannaClean[-drop,]
  }
}

MossSavannaVec <- MossSavannaClean$CellID

saveRDS(MossSavannaClean, "Data/Moss_Savannas_Beta_Biome_DF.rds")
saveRDS(MossSavannaVec, "Data/Savannas_CleanVec.rds")

#Taiga ---------------------------------------------------------------------
WeightedTaiga <- raster::extract(LongLatMossBetaRaster, Taiga, df = TRUE, cellnumbers = TRUE, weight = TRUE)
colnames(WeightedTaiga) <- c("Type", "CellID", "Beta", "Weight")
WeightedTaiga$Type <- "Taiga"
WeightedTaigaVec <- WeightedTaiga$CellID

MossTaigaClean <- WeightedTaiga
TaigaCellID <- unique(WeightedTaigaVec)

for(i in TaigaCellID){
  vec <- MossTaigaClean$Weight[which(MossTaigaClean$CellID == i)]
  if(length(vec) > 1){
    max <- max(vec)
    drop <- which(MossTaigaClean$CellID == i & MossTaigaClean$Weight != max)
    MossTaigaClean <- MossTaigaClean[-drop,]
  }
}

MossTaigaVec <- MossTaigaClean$CellID

saveRDS(MossTaigaClean, "Data/Moss_Taiga_Beta_Biome_DF.rds")
saveRDS(MossTaigaVec, "Data/Taiga_CleanVec.rds")

#Temperate Grasslands ------------------------------------------------------
WeightedTempGrass <- raster::extract(LongLatMossBetaRaster, Temperate_Grasslands, df = TRUE, cellnumbers = TRUE, weight = TRUE)
colnames(WeightedTempGrass) <- c("Type", "CellID", "Beta", "Weight")
WeightedTempGrass$Type <- "Temperate_Grasslands"
WeightedTempGrassVec <- WeightedTempGrass$CellID

MossTempGrassClean <- WeightedTempGrass
TempGrassCellID <- unique(WeightedTempGrassVec)

for(i in TempGrassCellID){
  vec <- MossTempGrassClean$Weight[which(MossTempGrassClean$CellID == i)]
  if(length(vec) > 1){
    max <- max(vec)
    drop <- which(MossTempGrassClean$CellID == i & MossTempGrassClean$Weight != max)
    MossTempGrassClean <- MossTempGrassClean[-drop,]
  }
}

MossTempGrassVec <- MossTempGrassClean$CellID

saveRDS(MossTempGrassClean, "Data/Moss_Temperate_Grasslands_Beta_Biome_DF.rds")
saveRDS(MossTempGrassVec, "Data/Temperate_GrasslandsCleanVec.rds")

#Temperate Mixed -----------------------------------------------------------
WeightedTempMix <- raster::extract(LongLatMossBetaRaster, Temperate_Mixed, df = TRUE, cellnumbers = TRUE, weight = TRUE)
colnames(WeightedTempMix) <- c("Type", "CellID", "Beta", "Weight")
WeightedTempMix$Type <- "Temperate_Mixed"
WeightedTempMixVec <- WeightedTempMix$CellID

MossTempMixClean <- WeightedTempMix
TempMixCellID <- unique(WeightedTempMixVec)

for(i in TempMixCellID){
  vec <- MossTempMixClean$Weight[which(MossTempMixClean$CellID == i)]
  if(length(vec) > 1){
    max <- max(vec)
    drop <- which(MossTempMixClean$CellID == i & MossTempMixClean$Weight != max)
    MossTempMixClean <- MossTempMixClean[-drop,]
  }
}

MossTempMixVec <- MossTempMixClean$CellID

saveRDS(MossTempMixClean, "Data/Moss_Temperate_Mixed_Beta_Biome_DF.rds")
saveRDS(MossTempMixVec, "Data/Temperate_MixedCleanVec.rds")

#Tropical Grasslands -------------------------------------------------------
WeightedTropGrass <- raster::extract(LongLatMossBetaRaster, Tropical_Grasslands, df = TRUE, cellnumbers = TRUE, weight = TRUE)
colnames(WeightedTropGrass) <- c("Type", "CellID", "Beta", "Weight")
WeightedTropGrass$Type <- "Tropical_Grasslands"
WeightedTropGrassVec <- WeightedTropGrass$CellID

MossTropGrassClean <- WeightedTropGrass
TropGrassCellID <- unique(WeightedTropGrassVec)

for(i in TropGrassCellID){
  vec <- MossTropGrassClean$Weight[which(MossTropGrassClean$CellID == i)]
  if(length(vec) > 1){
    max <- max(vec)
    drop <- which(MossTropGrassClean$CellID == i & MossTropGrassClean$Weight != max)
    MossTropGrassClean <- MossTropGrassClean[-drop,]
  }
}

MossTropGrassVec <- MossTropGrassClean$CellID

saveRDS(MossTropGrassClean, "Data/Moss_Tropical_Grasslands_Beta_Biome_DF.rds")
saveRDS(MossTropGrassVec, "Data/Tropical_GrasslandsCleanVec.rds")

#Tundra --------------------------------------------------------------------
WeightedTundra <- raster::extract(LongLatMossBetaRaster, Tundra, df = TRUE, cellnumbers = TRUE, weight = TRUE)
colnames(WeightedTundra) <- c("Type", "CellID", "Beta", "Weight")
WeightedTundra$Type <- "Tundra"
WeightedTundraVec <- WeightedTundra$CellID

MossTundraClean <- WeightedTundra
TundraCellID <- unique(WeightedTundraVec)

for(i in TundraCellID){
  vec <- MossTundraClean$Weight[which(MossTundraClean$CellID == i)]
  if(length(vec) > 1){
    max <- max(vec)
    drop <- which(MossTundraClean$CellID == i & MossTundraClean$Weight != max)
    MossTundraClean <- MossTundraClean[-drop,]
  }
}

MossTundraVec <- MossTundraClean$CellID

saveRDS(MossTundraClean, "Data/Moss_Tundra_Beta_Biome_DF.rds")
saveRDS(MossTundraVec, "Data/TundraCleanVec.rds")

#Xeric Woodlands -----------------------------------------------------------
WeightedXericWood <- raster::extract(LongLatMossBetaRaster, Xeric_Woodlands, df = TRUE, cellnumbers = TRUE, weight = TRUE)
colnames(WeightedXericWood) <- c("Type", "CellID", "Beta", "Weight")
WeightedXericWood$Type <- "Xeric_Woodlands"
WeightedXericWoodVec <- WeightedXericWood$CellID

MossXericWoodClean <- WeightedXericWood
XericWoodCellID <- unique(WeightedXericWoodVec)

for(i in XericWoodCellID){
  vec <- MossXericWoodClean$Weight[which(MossXericWoodClean$CellID == i)]
  if(length(vec) > 1){
    max <- max(vec)
    drop <- which(MossXericWoodClean$CellID == i & MossXericWoodClean$Weight != max)
    MossXericWoodClean <- MossXericWoodClean[-drop,]
  }
}

MossXericWoodVec <- MossXericWoodClean$CellID

saveRDS(MossXericWoodClean, "Data/Moss_Xeric_Woodlands_Beta_Biome_DF.rds")
saveRDS(MossXericWoodVec, "Data/Xeric_WoodlandsCleanVec.rds")

#~Use for biome beta box violin plots~
#Bind biome dataframes -----------------------------------------------------
MossBiomeBetaCellsClean <- bind_rows(MossConForClean, MossDryForClean, MossMedWoodClean,
                                    MossMoistForClean,MossSavannaClean, MossTaigaClean, 
                                    MossTempGrassClean,MossTempMixClean,MossTropGrassClean,
                                    MossTundraClean, MossXericWoodClean)
saveRDS(MossBiomeBetaCellsClean, "Data/MossBiomeBetaCellsClean")

MossBiomeBetaCellsClean
