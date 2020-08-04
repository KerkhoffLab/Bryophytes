# Find all cells that biomes overlap, weighted by proportion covered
# Hailey Napier, August 2020

Coniferous_Forests <- subset(biomes_shp, biomes == "Coniferous_Forests")
Dry_Forest <- subset(biomes_shp, biomes == "Dry_Forest")
Mediterranean_Woodlands <- subset(biomes_shp, biomes == "Mediterranean_Woodlands")
Moist_Forest <- subset(biomes_shp, biomes == "Moist_Forest")
Savannas <- subset(biomes_shp, biomes == "Savannas")
Taiga <- subset(biomes_shp, biomes == "Taiga")
Temperate_Grasslands <- subset(biomes_shp, biomes == "Temperate_Grasslands")
Temperate_Mixed <- subset(biomes_shp, biomes == "Temperate_Mixed")
Tropical_Grasslands <- subset(biomes_shp, biomes == "Tropical_Grasslands")
Tundra <- subset(biomes_shp, biomes == "Tundra")
Xeric_Woodlands <- subset(biomes_shp, biomes == "Xeric_Woodlands")

#Coniferous Forests --------------------------------------------------------
AlphaConFor <- raster::extract(LongLatRaster, Coniferous_Forests, df = TRUE, cellnumbers = TRUE, weight = TRUE)
colnames(AlphaConFor) <- c("Type", "CellID", "CellID", "Weight")
AlphaConFor$Type <- "Coniferous_Forests"
AlphaConForVec <- AlphaConFor$CellID

#Dry Forest ----------------------------------------------------------------
AlphaDryFor <- raster::extract(LongLatRaster, Dry_Forest, df = TRUE, cellnumbers = TRUE, weight = TRUE)
colnames(AlphaDryFor) <- c("Type", "CellID", "CellID", "Weight")
AlphaDryFor$Type <- "Dry_Forest"
AlphaDryForVec <- AlphaDryFor$CellID

#Mediterranean Woodlands ---------------------------------------------------
AlphaMedWood <- raster::extract(LongLatRaster, Mediterranean_Woodlands, df = TRUE, cellnumbers = TRUE, weight = TRUE)
colnames(AlphaMedWood) <- c("Type", "CellID", "CellID", "Weight")
AlphaMedWood$Type <- "Mediterranean_Woodlands"
AlphaMedWoodVec <- AlphaMedWood$CellID

#Moist Forest --------------------------------------------------------------
AlphaMoistFor <- raster::extract(LongLatRaster, Moist_Forest, df = TRUE, cellnumbers = TRUE, weight = TRUE)
colnames(AlphaMoistFor) <- c("Type", "CellID", "CellID", "Weight")
AlphaMoistFor$Type <- "Moist_Forest"
AlphaMoistForVec <- AlphaMoistFor$CellID

#Savannas ------------------------------------------------------------------
AlphaSavanna <- raster::extract(LongLatRaster, Savannas, df = TRUE, cellnumbers = TRUE, weight = TRUE)
colnames(AlphaSavanna) <- c("Type", "CellID", "CellID", "Weight")
AlphaSavanna$Type <- "Savannas"
AlphaSavannaVec <- AlphaSavanna$CellID

#Taiga ---------------------------------------------------------------------
AlphaTaiga <- raster::extract(LongLatRaster, Taiga, df = TRUE, cellnumbers = TRUE, weight = TRUE)
colnames(AlphaTaiga) <- c("Type", "CellID", "CellID", "Weight")
AlphaTaiga$Type <- "Taiga"
AlphaTaigaVec <- AlphaTaiga$CellID

#Temperate Grasslands ------------------------------------------------------
AlphaTempGrass <- raster::extract(LongLatRaster, Temperate_Grasslands, df = TRUE, cellnumbers = TRUE, weight = TRUE)
colnames(AlphaTempGrass) <- c("Type", "CellID", "CellID", "Weight")
AlphaTempGrass$Type <- "Temperate_Grasslands"
AlphaTempGrassVec <- AlphaTempGrass$CellID

#Temperate Mixed -----------------------------------------------------------
AlphaTempMix <- raster::extract(LongLatRaster, Temperate_Mixed, df = TRUE, cellnumbers = TRUE, weight = TRUE)
colnames(AlphaTempMix) <- c("Type", "CellID", "CellID", "Weight")
AlphaTempMix$Type <- "Temperate_Mixed"
AlphaTempMixVec <- AlphaTempMix$CellID

#Tropical Grasslands -------------------------------------------------------
AlphaTropGrass <- raster::extract(LongLatRaster, Tropical_Grasslands, df = TRUE, cellnumbers = TRUE, weight = TRUE)
colnames(AlphaTropGrass) <- c("Type", "CellID", "CellID", "Weight")
AlphaTropGrass$Type <- "Tropical_Grasslands"
AlphaTropGrassVec <- AlphaTropGrass$CellID

#Tundra --------------------------------------------------------------------
AlphaTundra <- raster::extract(LongLatRaster, Tundra, df = TRUE, cellnumbers = TRUE, weight = TRUE)
colnames(AlphaTundra) <- c("Type", "CellID", "CellID", "Weight")
AlphaTundra$Type <- "Tundra"
AlphaTundraVec <- AlphaTundra$CellID

#Xeric Woodlands -----------------------------------------------------------
AlphaXericWood <- raster::extract(LongLatRaster, Xeric_Woodlands, df = TRUE, cellnumbers = TRUE, weight = TRUE)
colnames(AlphaXericWood) <- c("Type", "CellID", "CellID", "Weight")
AlphaXericWood$Type <- "Xeric_Woodlands"
AlphaXericWoodVec <- AlphaXericWood$CellID


#Bind biome dataframes -----------------------------------------------------
BiomeCells <- bind_rows(AlphaConFor, AlphaDryFor, AlphaMedWood,
                           AlphaMoistFor,AlphaSavanna, AlphaTaiga, 
                           AlphaTempGrass, AlphaTempMix,AlphaTropGrass,
                           AlphaTundra, AlphaXericWood)
BiomeCells$CellID...2 <- NULL
names(BiomeCells)[2] <- "CellID"
saveRDS(BiomeCells, file = "Data/BiomeCells")

#Number of Overlapped cells
length(as.vector(BiomeCells$CellID)) - length(unique(BiomeCells$CellID))

#Choose one biome per cell (cells with multiple biomes go to biome with higher proportion coverage)
BiomeCellsClean <- BiomeCells
BiomeCellID <- unique(BiomeCells$CellID)
for(i in BiomeCellID){
  vec <- BiomeCellsClean$Weight[which(BiomeCellsClean$CellID == i)]
  if(length(vec) > 1){
    min <- min(vec)
    drop <- which(BiomeCellsClean$CellID == i & BiomeCellsClean$Weight == min)
    BiomeCellsClean <- BiomeCellsClean[-drop,]
  }
}


