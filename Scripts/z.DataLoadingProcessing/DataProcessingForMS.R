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

#Save richness and presence data
saveRDS(OrderNames, file = "Data/OrderNames.rds")
saveRDS(MossCellRichness, file = "Data/MossCellRichness.rds")
saveRDS(MossRichnessVec, file = "Data/MossRichnessVec.rds")
saveRDS(BryophytePresence, file = "Data/BryophytePresence.rds")
saveRDS(MossPresence, file = "Data/MossPresence.rds")
saveRDS(MossOrderNames, file = "Data/MossOrderNames.rds")
saveRDS(MossRichnessRaster, "Data/MossRichnessRaster.rds")

# ?.0 Make MossOrderRichList for ORange function ----
MossOrderList <- list()
for(i in 1:length(MossOrderNames)){
  ord <- MossOrderNames[i]
  MossOrderList[[i]] <- subset(BryophytePresence, Order == ord)
}

MossOrderRichList <- list()
MossOrderPresList <- list()
for(i in 1:length(MossOrderNames)){
  MossOrderPresList[[i]] <- tally(group_by(MossOrderList[[i]], CellID))
  names(MossOrderPresList[[i]])[2] <- "Richness"
  MossOrderRichList[[i]] <- numeric(15038)
  MossOrderRichList[[i]][MossOrderPresList[[i]]$CellID] <- MossOrderPresList[[i]]$Richness
  MossOrderRichList[[i]][which(MossOrderRichList[[i]]==0)] = NA
}

saveRDS(MossOrderRichList, file = "Data/MossOrderRichList.rds")

# ?.0 Biome MOSS Alpha Diversity Data -------------------------------------------------------------

#WEIGHTED DATAFRAME --------------------------------------------------------
#Coniferous Forests --------------------------------------------------------
WeightedConFor <- raster::extract(LongLatBetaRaster, Coniferous_Forests, df = TRUE, cellnumbers = TRUE, weight = TRUE)
colnames(WeightedConFor) <- c("Type", "CellID", "Beta", "Weight")
WeightedConFor$Type <- "Coniferous_Forests"
WeightedConForVec <- WeightedConFor$CellID

ConForCellID <- unique(WeightedConForVec)

for(i in ConForCellID){
  vec <- WeightedConFor$Weight[which(BiomeBetaCellsClean$CellID == i)]
  if(length(vec) > 1){
    max <- max(vec)
    drop <- which(WeightedConFor$CellID == i & WeightedConFor$Weight != max)
    BiomeBetaCellsClean <- BiomeBetaCellsClean[-drop,]
  }
}


#Dry Forest ----------------------------------------------------------------
WeightedDryFor <- raster::extract(LongLatBetaRaster, Dry_Forest, df = TRUE, cellnumbers = TRUE, weight = TRUE)
colnames(WeightedDryFor) <- c("Type", "CellID", "Beta", "Weight")
WeightedDryFor$Type <- "Dry_Forest"
WeightedDryForVec <- WeightedDryFor$CellID

#Mediterranean Woodlands ---------------------------------------------------
WeightedMedWood <- raster::extract(LongLatBetaRaster, Mediterranean_Woodlands, df = TRUE, cellnumbers = TRUE, weight = TRUE)
colnames(WeightedMedWood) <- c("Type", "CellID", "Beta", "Weight")
WeightedMedWood$Type <- "Mediterranean_Woodlands"
WeightedMedWoodVec <- WeightedMedWood$CellID

#Moist Forest --------------------------------------------------------------
WeightedMoistFor <- raster::extract(LongLatBetaRaster, Moist_Forest, df = TRUE, cellnumbers = TRUE, weight = TRUE)
colnames(WeightedMoistFor) <- c("Type", "CellID", "Beta", "Weight")
WeightedMoistFor$Type <- "Moist_Forest"
WeightedMoistForVec <- WeightedMoistFor$CellID

#Savannas ------------------------------------------------------------------
WeightedSavanna <- raster::extract(LongLatBetaRaster, Savannas, df = TRUE, cellnumbers = TRUE, weight = TRUE)
colnames(WeightedSavanna) <- c("Type", "CellID", "Beta", "Weight")
WeightedSavanna$Type <- "Savannas"
WeightedSavannaVec <- WeightedSavanna$CellID

#Taiga ---------------------------------------------------------------------
WeightedTaiga <- raster::extract(LongLatBetaRaster, Taiga, df = TRUE, cellnumbers = TRUE, weight = TRUE)
colnames(WeightedTaiga) <- c("Type", "CellID", "Beta", "Weight")
WeightedTaiga$Type <- "Taiga"
WeightedTaigaVec <- WeightedTaiga$CellID

#Temperate Grasslands ------------------------------------------------------
WeightedTempGrass <- raster::extract(LongLatBetaRaster, Temperate_Grasslands, df = TRUE, cellnumbers = TRUE, weight = TRUE)
colnames(WeightedTempGrass) <- c("Type", "CellID", "Beta", "Weight")
WeightedTempGrass$Type <- "Temperate_Grasslands"
WeightedTempGrassVec <- WeightedTempGrass$CellID

#Temperate Mixed -----------------------------------------------------------
WeightedTempMix <- raster::extract(LongLatBetaRaster, Temperate_Mixed, df = TRUE, cellnumbers = TRUE, weight = TRUE)
colnames(WeightedTempMix) <- c("Type", "CellID", "Beta", "Weight")
WeightedTempMix$Type <- "Temperate_Mixed"
WeightedTempMixVec <- WeightedTempMix$CellID

#Tropical Grasslands -------------------------------------------------------
WeightedTropGrass <- raster::extract(LongLatBetaRaster, Tropical_Grasslands, df = TRUE, cellnumbers = TRUE, weight = TRUE)
colnames(WeightedTropGrass) <- c("Type", "CellID", "Beta", "Weight")
WeightedTropGrass$Type <- "Tropical_Grasslands"
WeightedTropGrassVec <- WeightedTropGrass$CellID

#Tundra --------------------------------------------------------------------
WeightedTundra <- raster::extract(LongLatBetaRaster, Tundra, df = TRUE, cellnumbers = TRUE, weight = TRUE)
colnames(WeightedTundra) <- c("Type", "CellID", "Beta", "Weight")
WeightedTundra$Type <- "Tundra"
WeightedTundraVec <- WeightedTundra$CellID

#Xeric Woodlands -----------------------------------------------------------
WeightedXericWood <- raster::extract(LongLatBetaRaster, Xeric_Woodlands, df = TRUE, cellnumbers = TRUE, weight = TRUE)
colnames(WeightedXericWood) <- c("Type", "CellID", "Beta", "Weight")
WeightedXericWood$Type <- "Xeric_Woodlands"
WeightedXericWoodVec <- WeightedXericWood$CellID


#Bind biome dataframes -----------------------------------------------------
BiomeBetaCellsWeighted <- bind_rows(WeightedConFor, WeightedDryFor, WeightedMedWood,
                                    WeightedMoistFor,WeightedSavanna, WeightedTaiga, 
                                    WeightedTempGrass, WeightedTempMix,WeightedTropGrass,
                                    WeightedTundra, WeightedXericWood)

#Choose one biome per cell (cells with multiple biomes go to biome with higher proportion coverage)
BiomeBetaCellsClean <- BiomeBetaCellsWeighted



