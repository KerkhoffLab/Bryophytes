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
# ?.1 Create dataframes for each biome with cell coordinates and richness
#Coniferous Forests
AlphaConFor <- raster::extract(MossRichnessRaster, Coniferous_Forests, df = TRUE, cellnumbers = TRUE)
colnames(AlphaConFor) <- c("Type", "CellID", "Alpha")
AlphaConFor$Type <- "Coniferous_Forests"
AlphaConForVec <- AlphaConFor$CellID
AlphaConFor <- merge(AlphaConFor, LongLatDF)
saveRDS(AlphaConForVec, "Data/Moss_Coniferous_Forests_Vec.rds")

#Dry Forest
AlphaDryFor <- raster::extract(MossRichnessRaster, Dry_Forest, df = TRUE, cellnumbers = TRUE)
colnames(AlphaDryFor) <- c("Type", "CellID", "Alpha")
AlphaDryFor$Type <- "Dry_Forest"
AlphaDryForVec <- AlphaDryFor$CellID
AlphaDryFor <- merge(AlphaDryFor, LongLatDF)
saveRDS(AlphaDryForVec, "Data/Moss_Dry_Forest_Vec.rds")

#Mediterranean Woodlands
AlphaMedWood <- raster::extract(MossRichnessRaster, Mediterranean_Woodlands, df = TRUE, cellnumbers = TRUE)
colnames(AlphaMedWood) <- c("Type", "CellID", "Alpha")
AlphaMedWood$Type <- "Mediterranean_Woodlands"
AlphaMedWoodVec <- AlphaMedWood$CellID
AlphaMedWood <- merge(AlphaMedWood, LongLatDF)
saveRDS(AlphaMedWoodVec, "Data/Moss_Mediterranean_Woodlands_Vec.rds")

#Moist Forest
AlphaMoistFor <- raster::extract(MossRichnessRaster, Moist_Forest, df = TRUE, cellnumbers = TRUE)
colnames(AlphaMoistFor) <- c("Type", "CellID", "Alpha")
AlphaMoistFor$Type <- "Moist_Forest"
AlphaMoistForVec <- AlphaMoistFor$CellID
AlphaMoistFor <- merge(AlphaMoistFor, LongLatDF)
saveRDS(AlphaMoistForVec, "Data/Moss_Moist_Forest_Vec.rds")

#Savannas
AlphaSavanna <- raster::extract(MossRichnessRaster, Savannas, df = TRUE, cellnumbers = TRUE)
colnames(AlphaSavanna) <- c("Type", "CellID", "Alpha")
AlphaSavanna$Type <- "Savannas"
AlphaSavannaVec <- AlphaSavanna$CellID
AlphaSavanna <- merge(AlphaSavanna, LongLatDF)
saveRDS(AlphaSavannaVec, "Data/Moss_Savannas_Vec.rds")

#Taiga
AlphaTaiga <- raster::extract(MossRichnessRaster, Taiga, df = TRUE, cellnumbers = TRUE)
colnames(AlphaTaiga) <- c("Type", "CellID", "Alpha")
AlphaTaiga$Type <- "Taiga"
AlphaTaigaVec <- AlphaTaiga$CellID
AlphaTaiga <- merge(AlphaTaiga, LongLatDF)
saveRDS(AlphaTaigaVec, "Data/Moss_Taiga_Vec.rds")

#Temperate Grasslands
AlphaTempGrass <- raster::extract(MossRichnessRaster, Temperate_Grasslands, df = TRUE, cellnumbers = TRUE)
colnames(AlphaTempGrass) <- c("Type", "CellID", "Alpha")
AlphaTempGrass$Type <- "Temperate_Grasslands"
AlphaTempGrassVec <- AlphaTempGrass$CellID
AlphaTempGrass <- merge(AlphaTempGrass, LongLatDF)
saveRDS(AlphaTempGrassVec, "Data/Moss_Temperate_Grasslands_Vec.rds")

#Temperate Mixed
AlphaTempMix <- raster::extract(MossRichnessRaster, Temperate_Mixed, df = TRUE, cellnumbers = TRUE)
colnames(AlphaTempMix) <- c("Type", "CellID", "Alpha")
AlphaTempMix$Type <- "Temperate_Mixed"
AlphaTempMixVec <- AlphaTempMix$CellID
AlphaTempMix <- merge(AlphaTempMix, LongLatDF)
saveRDS(AlphaTempMixVec, "Data/Moss_Temperate_Mixed_Vec.rds")

#Tropical Grasslands
AlphaTropGrass <- raster::extract(MossRichnessRaster, Tropical_Grasslands, df = TRUE, cellnumbers = TRUE)
colnames(AlphaTropGrass) <- c("Type", "CellID", "Alpha")
AlphaTropGrass$Type <- "Tropical_Grasslands"
AlphaTropGrassVec <- AlphaTropGrass$CellID
AlphaTropGrass <- merge(AlphaTropGrass, LongLatDF)
saveRDS(AlphaTropGrassVec, "Data/Moss_Tropical_Grasslands_Vec.rds")

#Tundra
AlphaTundra <- raster::extract(MossRichnessRaster, Tundra, df = TRUE, cellnumbers = TRUE)
colnames(AlphaTundra) <- c("Type", "CellID", "Alpha")
AlphaTundra$Type <- "Tundra"
AlphaTundraVec <- AlphaTundra$CellID
AlphaTundra <- merge(AlphaTundra, LongLatDF)
saveRDS(AlphaTundraVec, "Data/Moss_Tundra_Vec.rds")

#Xeric Woodlands
AlphaXericWood <- raster::extract(MossRichnessRaster, Xeric_Woodlands, df = TRUE, cellnumbers = TRUE)
colnames(AlphaXericWood) <- c("Type", "CellID", "Alpha")
AlphaXericWood$Type <- "Xeric_Woodlands"
AlphaXericWoodVec <- AlphaXericWood$CellID
AlphaXericWood <- merge(AlphaXericWood, LongLatDF)
saveRDS(AlphaXericWoodVec, "Data/Moss_Xeric_Woodlands_Vec.rds")

