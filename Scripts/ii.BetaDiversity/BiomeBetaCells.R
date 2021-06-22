# BiomeBetaCells
## Hailey Napier, August 2020
# Find all cells in each biome
  # 3 different measures:
    # 1. Weighted; each cell is counted if it is overlapped by a biome. If it's overlapped by multiple biomes,
        # then it's counted multiple times
    # 2. Weighted clean; each cell is counted if it is overlapped by a biome. If it's overlapped by multiple 
        # biomes, then it's counted in the biome that covers a larger proportion of the cell
    # 3. Unweighted; each cell is counted in a biome ONLY if that biome overlaps the center of the cell

#FIRST -----------------------------------------------------------------
  # Run DataProcessing.R Bryophytes.rmd section under "ONLY NEEDED FOR MAPPING AND SHAPEFILE EXRACTION" (~ln. 160)
  # Run BiomeDiversity.R to load in biome shapefile data

#Load packages -----------------------------------------------------------
library(raster)
library(dplyr)

#Load data ---------------------------------------------------------------
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

LongLatBetaRaster <- readRDS("Data/LongLatBetaRaster.rds")


#WEIGHTED DATAFRAME --------------------------------------------------------
#Coniferous Forests --------------------------------------------------------
WeightedConFor <- raster::extract(LongLatBetaRaster, Coniferous_Forests, df = TRUE, cellnumbers = TRUE, weight = TRUE)
colnames(WeightedConFor) <- c("Type", "CellID", "Beta", "Weight")
WeightedConFor$Type <- "Coniferous_Forests"
WeightedConForVec <- WeightedConFor$CellID

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

#Number of Overlapped cells
length(as.vector(BiomeBetaCellsWeighted$CellID)) - length(unique(BiomeBetaCellsWeighted$CellID))

#Choose one biome per cell (cells with multiple biomes go to biome with higher proportion coverage)
BiomeBetaCellsClean <- BiomeBetaCellsWeighted
BiomeBetaCellID <- unique(BiomeBetaCellsWeighted$CellID)
for(i in BiomeBetaCellID){
  vec <- BiomeBetaCellsClean$Weight[which(BiomeBetaCellsClean$CellID == i)]
  if(length(vec) > 1){
    max <- max(vec)
    drop <- which(BiomeBetaCellsClean$CellID == i & BiomeBetaCellsClean$Weight != max)
    BiomeBetaCellsClean <- BiomeBetaCellsClean[-drop,]
  }
}


#Save dataframes ----------------------------------------------------------
#All cells, including repeats where a cell is covered by multiple biomes
saveRDS(BiomeBetaCellsWeighted, file = "Data/BiomeBetaCellsWeighted.rds")
#Weighted cells, excluding repeats
saveRDS(BiomeBetaCellsClean, file = "Data/BiomeBetaCellsClean.rds")



#UNWEIGHTED DATAFRAME (CELL COUNTED IN BIOME IF BIOME POLYGON COVERS CELL CENTER)
#Coniferous Forests --------------------------------------------------------
CenterCovConFor <- raster::extract(LongLatBetaRaster, Coniferous_Forests, df = TRUE, cellnumbers = TRUE, weight = FALSE)
colnames(CenterCovConFor) <- c("Type", "CellID", "Beta")
CenterCovConFor$Type <- "Coniferous_Forests"
CenterCovConForVec <- CenterCovConFor$CellID

#Dry Forest ----------------------------------------------------------------
CenterCovDryFor <- raster::extract(LongLatBetaRaster, Dry_Forest, df = TRUE, cellnumbers = TRUE, weight = FALSE)
colnames(CenterCovDryFor) <- c("Type", "CellID", "Beta")
CenterCovDryFor$Type <- "Dry_Forest"
CenterCovDryForVec <- CenterCovDryFor$CellID

#Mediterranean Woodlands ---------------------------------------------------
CenterCovMedWood <- raster::extract(LongLatBetaRaster, Mediterranean_Woodlands, df = TRUE, cellnumbers = TRUE, weight = FALSE)
colnames(CenterCovMedWood) <- c("Type", "CellID", "Beta")
CenterCovMedWood$Type <- "Mediterranean_Woodlands"
CenterCovMedWoodVec <- CenterCovMedWood$CellID

#Moist Forest --------------------------------------------------------------
CenterCovMoistFor <- raster::extract(LongLatBetaRaster, Moist_Forest, df = TRUE, cellnumbers = TRUE, weight = FALSE)
colnames(CenterCovMoistFor) <- c("Type", "CellID", "Beta")
CenterCovMoistFor$Type <- "Moist_Forest"
CenterCovMoistForVec <- CenterCovMoistFor$CellID

#Savannas ------------------------------------------------------------------
CenterCovSavanna <- raster::extract(LongLatBetaRaster, Savannas, df = TRUE, cellnumbers = TRUE, weight = FALSE)
colnames(CenterCovSavanna) <- c("Type", "CellID", "Beta")
CenterCovSavanna$Type <- "Savannas"
CenterCovSavannaVec <- CenterCovSavanna$CellID

#Taiga ---------------------------------------------------------------------
CenterCovTaiga <- raster::extract(LongLatBetaRaster, Taiga, df = TRUE, cellnumbers = TRUE, weight = FALSE)
colnames(CenterCovTaiga) <- c("Type", "CellID", "Beta")
CenterCovTaiga$Type <- "Taiga"
CenterCovTaigaVec <- CenterCovTaiga$CellID

#Temperate Grasslands ------------------------------------------------------
CenterCovTempGrass <- raster::extract(LongLatBetaRaster, Temperate_Grasslands, df = TRUE, cellnumbers = TRUE, weight = FALSE)
colnames(CenterCovTempGrass) <- c("Type", "CellID", "Beta")
CenterCovTempGrass$Type <- "Temperate_Grasslands"
CenterCovTempGrassVec <- CenterCovTempGrass$CellID

#Temperate Mixed -----------------------------------------------------------
CenterCovTempMix <- raster::extract(LongLatBetaRaster, Temperate_Mixed, df = TRUE, cellnumbers = TRUE, weight = FALSE)
colnames(CenterCovTempMix) <- c("Type", "CellID", "Beta")
CenterCovTempMix$Type <- "Temperate_Mixed"
CenterCovTempMixVec <- CenterCovTempMix$CellID

#Tropical Grasslands -------------------------------------------------------
CenterCovTropGrass <- raster::extract(LongLatBetaRaster, Tropical_Grasslands, df = TRUE, cellnumbers = TRUE, weight = FALSE)
colnames(CenterCovTropGrass) <- c("Type", "CellID", "Beta")
CenterCovTropGrass$Type <- "Tropical_Grasslands"
CenterCovTropGrassVec <- CenterCovTropGrass$CellID

#Tundra --------------------------------------------------------------------
CenterCovTundra <- raster::extract(LongLatBetaRaster, Tundra, df = TRUE, cellnumbers = TRUE, weight = FALSE)
colnames(CenterCovTundra) <- c("Type", "CellID", "Beta")
CenterCovTundra$Type <- "Tundra"
CenterCovTundraVec <- CenterCovTundra$CellID

#Xeric Woodlands -----------------------------------------------------------
CenterCovXericWood <- raster::extract(LongLatBetaRaster, Xeric_Woodlands, df = TRUE, cellnumbers = TRUE, weight = FALSE)
colnames(CenterCovXericWood) <- c("Type", "CellID", "Beta")
CenterCovXericWood$Type <- "Xeric_Woodlands"
CenterCovXericWoodVec <- CenterCovXericWood$CellID

#Bind biome dataframes -----------------------------------------------------
BiomeBetaCellsCenterCov <- bind_rows(CenterCovConFor, CenterCovDryFor, CenterCovMedWood,
                                CenterCovMoistFor,CenterCovSavanna, CenterCovTaiga, 
                                CenterCovTempGrass, CenterCovTempMix,CenterCovTropGrass,
                                CenterCovTundra, CenterCovXericWood)

#Save dataframe ------------------------------------------------------------
#weight = false; cells counted in biome only if biome polygon overlaps center of cell
saveRDS(BiomeBetaCellsCenterCov, file = "Data/BiomeBetaCellsCentCov.rds")


