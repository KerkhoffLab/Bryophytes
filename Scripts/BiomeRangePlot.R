# Plotting range size of BRYOPHYTES by biome
# Adapted from BiomeDiversity.R
# Kathryn Dawdy, July 2020


# 0.0 FIRST -----------------------------------------------------------------
#Run DataProcessing.R - section under ##SpeciesRanges.R##
#Run BiomeDiversity.R for biome shapefiles

# 0.1 Load packages ---------------------------------------------------------
require(BIEN)
require(maps) 
require(dplyr)
require(maptools)
require(raster)
require(sp)
require(rgdal)
require(mapdata)
require(mapproj)
require(wesanderson)
require(ggplot2)
require(rasterVis)
require(knitr)
require(latexpdf)
require(vegan)
require(gridExtra)
require(sf)
require(rgeos)
require(rworldmap)
require(filesstrings)
require(forcats)
require(tidyverse)
require(tmap)


# 0.2 Load data -------------------------------------------------------------
BlankRas <-raster("Data/blank_100km_raster.tif")
RichnessVec <- readRDS("Data/RichnessVec.rds")
BryophytePresence <- readRDS("Data/BryophytePresence.rds")

LongLatDF <- readRDS("Data/LongLatDF.rds")
RichnessRaster <- readRDS("Data/RichnessRaster.rds")
CellVec <- c(1:15038)

biomes_shp <- shapefile("Data/Biomes/Biomes_olson_projected.shp")

CellRange <- readRDS("Data/CellRange.rds")

RangeRaster <- readRDS("Data/RangeRaster.rds")

# 0.3 Load colors ----------------------------------------------------------
#From wes_palette() hex numbers on GitHub: karthik/wesanderson
#Color scheme for biomes (in order of BiomeNames (BiomeProcessing.R))
cols7 <- c("#D8B70A", "#972D15", "#A2A475", "#81A88D", "#02401B",
           "#446455", "#FDD262", "#D3DDDC", "#C7B19C",
           "#798E87", "#C27D38")

#Colors used for plots (# corresponds to # of boxplots/length of data)
biome_cols_11 <- c("#D8B70A", "#972D15", "#A2A475", "#81A88D", "#02401B",
                   "#446455", "#FDD262", "#D3DDDC", "#C7B19C", "#798E87", 
                   "#C27D38")

biome_cols_22 <- c(biome_cols_11, biome_cols_11)


# 1.0 SUBSET BIOMES --------------------------------------------------------
sort(biomes_shp@data[["biomes"]])

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


# 2.0 MAKE BIOME RANGE DATAFRAME -------------------------------------------

# 2.1 INDIVIDUAL BIOMES DATAFRAMES -----------------------------------------
#Coniferous Forests --------------------------------------------------------
RangeConFor <- raster::extract(RangeRaster, Coniferous_Forests, df = TRUE, cellnumbers = TRUE)
colnames(RangeConFor) <- c("Type", "CellID", "Range")
RangeConFor$Type <- "Coniferous_Forests"
RangeConForVec <- RangeConFor$CellID
RangeConFor <- merge(RangeConFor, LongLatDF)

#scatterplot
RangeConForScatterplot <- ggplot() + 
  geom_point(data = RangeConFor, 
             aes(Latitude, Range), 
             shape = 16, 
             size = 5, 
             show.legend = FALSE, 
             alpha=0.5, 
             color = "goldenrod2") + 
  ylab("α div in Coniferous Forests") + 
  ylim(0, 500) + 
  xlab("Latitude") + 
  theme_minimal() +  
  theme(axis.title.y = element_text(size=32),
        axis.title.x = element_text(size=32),  
        axis.text = element_text(size=20))
RangeConForScatterplot

#boxplot
theme_set(theme_grey())
ggplot(RangeConFor, aes(x=Type, y=Range)) + geom_boxplot()

#Dry Forest ----------------------------------------------------------------
RangeDryFor <- raster::extract(RangeRaster, Dry_Forest, df = TRUE, cellnumbers = TRUE)
colnames(RangeDryFor) <- c("Type", "CellID", "Range")
RangeDryFor$Type <- "Dry_Forest"
RangeDryForVec <- RangeDryFor$CellID
RangeDryFor <- merge(RangeDryFor, LongLatDF)

#Mediterranean Woodlands ---------------------------------------------------
RangeMedWood <- raster::extract(RangeRaster, Mediterranean_Woodlands, df = TRUE, cellnumbers = TRUE)
colnames(RangeMedWood) <- c("Type", "CellID", "Range")
RangeMedWood$Type <- "Mediterranean_Woodlands"
RangeMedWoodVec <- RangeMedWood$CellID
RangeMedWood <- merge(RangeMedWood, LongLatDF)

#Moist Forest --------------------------------------------------------------
RangeMoistFor <- raster::extract(RangeRaster, Moist_Forest, df = TRUE, cellnumbers = TRUE)
colnames(RangeMoistFor) <- c("Type", "CellID", "Range")
RangeMoistFor$Type <- "Moist_Forest"
RangeMoistForVec <- RangeMoistFor$CellID
RangeMoistFor <- merge(RangeMoistFor, LongLatDF)

#Savannas ------------------------------------------------------------------
RangeSavanna <- raster::extract(RangeRaster, Savannas, df = TRUE, cellnumbers = TRUE)
colnames(RangeSavanna) <- c("Type", "CellID", "Range")
RangeSavanna$Type <- "Savannas"
RangeSavannaVec <- RangeSavanna$CellID
RangeSavanna <- merge(RangeSavanna, LongLatDF)

#Taiga ---------------------------------------------------------------------
RangeTaiga <- raster::extract(RangeRaster, Taiga, df = TRUE, cellnumbers = TRUE)
colnames(RangeTaiga) <- c("Type", "CellID", "Range")
RangeTaiga$Type <- "Taiga"
RangeTaigaVec <- RangeTaiga$CellID
RangeTaiga <- merge(RangeTaiga, LongLatDF)

#Temperate Grasslands ------------------------------------------------------
RangeTempGrass <- raster::extract(RangeRaster, Temperate_Grasslands, df = TRUE, cellnumbers = TRUE)
colnames(RangeTempGrass) <- c("Type", "CellID", "Range")
RangeTempGrass$Type <- "Temperate_Grasslands"
RangeTempGrassVec <- RangeTempGrass$CellID
RangeTempGrass <- merge(RangeTempGrass, LongLatDF)

#Temperate Mixed -----------------------------------------------------------
RangeTempMix <- raster::extract(RangeRaster, Temperate_Mixed, df = TRUE, cellnumbers = TRUE)
colnames(RangeTempMix) <- c("Type", "CellID", "Range")
RangeTempMix$Type <- "Temperate_Mixed"
RangeTempMixVec <- RangeTempMix$CellID
RangeTempMix <- merge(RangeTempMix, LongLatDF)

#Tropical Grasslands -------------------------------------------------------
RangeTropGrass <- raster::extract(RangeRaster, Tropical_Grasslands, df = TRUE, cellnumbers = TRUE)
colnames(RangeTropGrass) <- c("Type", "CellID", "Range")
RangeTropGrass$Type <- "Tropical_Grasslands"
RangeTropGrassVec <- RangeTropGrass$CellID
RangeTropGrass <- merge(RangeTropGrass, LongLatDF)

#Tundra --------------------------------------------------------------------
RangeTundra <- raster::extract(RangeRaster, Tundra, df = TRUE, cellnumbers = TRUE)
colnames(RangeTundra) <- c("Type", "CellID", "Range")
RangeTundra$Type <- "Tundra"
RangeTundraVec <- RangeTundra$CellID
RangeTundra <- merge(RangeTundra, LongLatDF)

#Xeric Woodlands -----------------------------------------------------------
RangeXericWood <- raster::extract(RangeRaster, Xeric_Woodlands, df = TRUE, cellnumbers = TRUE)
colnames(RangeXericWood) <- c("Type", "CellID", "Range")
RangeXericWood$Type <- "Xeric_Woodlands"
RangeXericWoodVec <- RangeXericWood$CellID
RangeXericWood <- merge(RangeXericWood, LongLatDF)


# 2.2 BIND BIOME DATAFRAMES ------------------------------------------------
BiomeRange <- bind_rows(RangeConFor, RangeDryFor, RangeMedWood,
                           RangeMoistFor,RangeSavanna, RangeTaiga, 
                           RangeTempGrass, RangeTempMix,RangeTropGrass,
                           RangeTundra, RangeXericWood)
saveRDS(BiomeRange, file = "Data/BiomeRange.rds")


# 3.0 MAKE PLOTS -----------------------------------------------------------
# 3.1 Biome richness boxplot -----------------------------------------------
BiomeRangeBox <- ggplot(BiomeRange, aes(x=Type, y=Range, fill=Type)) + 
  geom_boxplot(show.legend = FALSE, fill=biome_cols_11) +
  guides(x = guide_axis(angle=30)) +
  theme_minimal() +
  xlab("Biome") +
  ylab("Range") +  
  theme(axis.title.y = element_text(size=32), 
        axis.title.x = element_text(size=32),
        axis.text.y = element_text(size=20), 
        axis.text.x = element_text(angle = 30, hjust = 1, size = 12))
BiomeRangeBox

png("Figures/RangeBiomeBox.png", width = 1500, height = 1000, pointsize = 20)
BiomeRangeBox
dev.off()

# 3.2 Biome richness boxplot with violins ----------------------------------
BiomeRangeBV <- ggplot(BiomeRange, aes(x=Type, y=Range, fill=Type, color=Type)) + 
  geom_boxplot(show.legend = FALSE, fill=biome_cols_11, color="black") +
  guides(x = guide_axis(angle=30)) +
  theme_minimal() +        #un-comment whichever theme you want
  #theme_gray() +
  #theme_light() +
  #theme_bw() +
  geom_violin(scale="count", show.legend=FALSE, fill="gray", alpha=0.35,
              color="gray25") +
  xlab("Biome") +
  ylab("Range") +  
  theme(axis.title.y = element_text(size=32), 
        axis.title.x = element_text(size=32),
        axis.text.y = element_text(size=20), 
        axis.text.x = element_text(angle = 30, hjust = 1, size = 12))
BiomeRangeBV

png("Figures/RangeBiomeBoxViolin.png", width = 1500, height = 1000, pointsize = 20)
BiomeRangeBV
dev.off()

