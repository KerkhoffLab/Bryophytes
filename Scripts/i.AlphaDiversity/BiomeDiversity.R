#Load biome shapefiles
#Map bryophyte alpha diversity with biomes
#Plot bryophyte richness by biome
#Code adapted from MappingRichness.R, Bryophytes.Rmd, TreeMaps.R, MountainRanges.R, and MountainLowland.Rmd
#Kathryn Dawdy, July 2020


# Load packages ------------------------------------------------------------
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


# 0.0 Load blank raster and richness/presence data -------------------------
BlankRas <-raster("Data/blank_100km_raster.tif")
RichnessVec <- readRDS("Data/RichnessVec.rds")
BryophytePresence <- readRDS("Data/BryophytePresence.rds")

LongLatBetaRaster <- readRDS("Data/LongLatBetaRaster.rds")
RichnessRaster <- readRDS("Data/RichnessRaster.rds")
CellVec <- c(1:15038)

# Make and save the below dataframes in section 4.0
LongLatDF <- readRDS("Data/LongLatDF.rds")
BiomeRichness <- readRDS("Data/BiomeRichness.rds")

# Make and save in section 3.1
RichnessDF <- readRDS("Data/RichnessDF.rds")

# 1.0 LOAD BIOME SHAPEFILES ------------------------------------------------
# 1.1 Load entire BIEN_FEE_paper repository (branch: Trait_phylo)
download.file(url = "https://github.com/susyelo/BIEN_FEE_paper/archive/Trait_phylo.zip", 
              destfile = "./Data/Biomes/BIEN_FEE_paper-Trait_phylo.zip")
dir.create("./Data/Biomes/")
setwd("./Data/Biomes/")
unzip("BIEN_FEE_paper-Trait_phylo.zip")
setwd("../../")

# 1.2 Move the shapefiles
file.move("./Data/Biomes/BIEN_FEE_paper-Trait_phylo/data/processed/Olson_processed/Biomes_olson_projected.dbf", "./Data/Biomes/")
file.move("./Data/Biomes/BIEN_FEE_paper-Trait_phylo/data/processed/Olson_processed/Biomes_olson_projected.prj", "./Data/Biomes/")
file.move("./Data/Biomes/BIEN_FEE_paper-Trait_phylo/data/processed/Olson_processed/Biomes_olson_projected.shp", "./Data/Biomes/")
file.move("./Data/Biomes/BIEN_FEE_paper-Trait_phylo/data/processed/Olson_processed/Biomes_olson_projected.shx", "./Data/Biomes/")

# 1.3 Delete the repository folder and .zip file
unlink("./Data/Biomes/BIEN_FEE_paper-Trait_phylo", recursive=TRUE)
unlink("./Data/Biomes/BIEN_FEE_paper-Trait_phylo.zip")



# 2.0 SET THEME AND COLORS -------------------------------------------------
# 2.1 Richness map colors
cols <- (wes_palette("Zissou1", 500, type = "continuous"))
theme_set(theme_void())

# 2.2 Violin plot colors
cols1 <- (wes_palette("Zissou1", 5632, type = "continuous"))

# 2.3 Color scheme for biomes (in order of BiomeNames (BiomeProcessing.R))
#wes_palette() hex numbers on GitHub: karthik/wesanderson

BiomeRichness <- readRDS("Data/BiomeRichness.rds")
BiomeNames <- unique(BiomeRichness$Type)
BiomeNames

cols7 <- c("#D8B70A", "#972D15", "#A2A475", "#81A88D", "#02401B",
           "#446455", "#FDD262", "#D3DDDC", "#C7B19C",
           "#798E87", "#C27D38")

# 2.4 Colors used for plots 
# Number in name corresponds to # of boxes per plot
# Removed hex number where box is absent (no values)
# Therefore, total # of boxes = length of data = # of hex numbers
biome_cols_11 <- c("#D8B70A", "#972D15", "#A2A475", "#81A88D", "#02401B",
                   "#446455", "#FDD262", "#D3DDDC", "#C7B19C", "#798E87", 
                   "#C27D38")

biome_cols_22 <- c(biome_cols_11, biome_cols_11)

biome_cols_87 <- c(biome_cols_22, biome_cols_11,
                   "#D8B70A", "#972D15", "#A2A475", "#81A88D", "#02401B",
                   "#446455", "#FDD262", "#D3DDDC", "#C7B19C", 
                   "#C27D38",
                   biome_cols_22, biome_cols_22)

biome_cols_66 <- c(biome_cols_22, biome_cols_22, biome_cols_22)

biome_cols_166 <- c(biome_cols_22,
                    c("#D8B70A", "#972D15", "#A2A475", "#81A88D", "#02401B",
                      "#FDD262", "#D3DDDC", "#C7B19C", 
                      "#C27D38"),
                    biome_cols_11,
                    c("#D8B70A", "#972D15", "#A2A475", "#81A88D", "#02401B",
                    "#446455", "#FDD262", "#D3DDDC",
                    "#C27D38"),
                    c("#D8B70A", "#972D15", "#81A88D", "#02401B",
                      "#446455", "#FDD262", "#D3DDDC", "#C7B19C", "#798E87", 
                      "#C27D38"),
                    c("#81A88D",
                      "#FDD262", "#D3DDDC"),
                    c("#D8B70A", "#972D15", "#81A88D",
                      "#446455", "#D3DDDC", "#C7B19C", "#798E87"),
                    c("#D8B70A", "#972D15", "#A2A475", "#81A88D", "#02401B",
                      "#FDD262", "#D3DDDC", "#C7B19C", 
                      "#C27D38"),
                    "#81A88D",
                    c("#972D15", "#A2A475", "#81A88D", "#02401B",
                      "#FDD262", "#D3DDDC", "#C7B19C", 
                      "#C27D38"),
                    c("#D8B70A",
                      "#446455", "#FDD262", "#D3DDDC"),
                    biome_cols_22,
                    c("#D8B70A", "#972D15", "#81A88D",
                      "#D3DDDC", "#C7B19C", 
                      "#C27D38"),
                    c("#D8B70A",
                      "#446455", "#FDD262", "#D3DDDC", "#798E87", 
                      "#C27D38"),
                    c("#A2A475", "#81A88D", "#02401B",
                      "#FDD262", "#D3DDDC", "#C7B19C"),
                    c("#D8B70A", "#972D15", "#A2A475", "#81A88D", "#02401B",
                      "#FDD262", "#D3DDDC", "#C7B19C", 
                      "#C27D38"),
                    biome_cols_11,
                    c("#D8B70A",
                      "#C27D38"),
                    biome_cols_11)

biome_cols_29 <- c(biome_cols_11,
                   c("#D8B70A", "#972D15", "#A2A475", "#81A88D", "#02401B",
                     "#446455", "#FDD262", "#D3DDDC", "#798E87", 
                     "#C27D38"),
                   c("#972D15", "#A2A475", "#81A88D", "#02401B",
                     "#FDD262", "#D3DDDC", "#C7B19C", 
                     "#C27D38"))

cols7 <- c("#D8B70A", "#972D15", "#A2A475", "#81A88D", "#02401B",
           "#446455", "#FDD262", "#D3DDDC", "#C7B19C",
           "#798E87", "#C27D38")

ConForVec <- vector(mode="character", length=260)
ConForVec[1:260] <- "#D8B70A"

DryForVec <- vector(mode="character", length=244)
DryForVec[1:244] <- "#972D15"

MedWoodVec <- vector(mode="character", length=30)
MedWoodVec[1:30] <- "#A2A475"

MoistVec <- vector(mode="character", length=929)
MoistVec[1:929] <- "#81A88D"

SavVec <- vector(mode="character", length=336)
SavVec[1:336] <- "#02401B"

TaigaVec <- vector(mode="character", length=612)
TaigaVec[1:612] <- "#446455"

TempGrassVec <- vector(mode="character", length=425)
TempGrassVec[1:425] <- "#FDD262"

TempMixVec <- vector(mode="character", length=334)
TempMixVec[1:334] <- "#D3DDDC"

TropGrassVec <- vector(mode="character", length=89)
TropGrassVec[1:89] <- "#C7B19C"

TundraVec <- vector(mode="character", length=385)
TundraVec[1:385] <- "#798E87"

XericVec <- vector(mode="character", length=392)
XericVec[1:392] <- "#C27D38"

biome_cols_4036 <- c(ConForVec, DryForVec, MedWoodVec, MoistVec, SavVec, 
                     TaigaVec, TempGrassVec, TempMixVec, TropGrassVec, 
                     TundraVec, XericVec)



# 3.0 BRYOPHYTE RICHNESS MAPS ----------------------------------------------
# 3.1 Create bryophyte richness dataframe ----------------------------------
RichnessVec[which(RichnessVec==0)]=NA
RichnessRaster <- setValues(BlankRas, RichnessVec)
RichnessDF <- rasterToPoints(RichnessRaster)
RichnessDF <- data.frame(RichnessDF)
colnames(RichnessDF) <- c("Longitude", "Latitude", "Alpha")

saveRDS(RichnessDF, file="Data/RichnessDF.rds")


# 3.2 BRYOPHYTE RICHNESS - biomes ------------------------------------------
# 3.2.1 Add biomes outlines (and continental and mountainous outlines)
biomes_shp <- shapefile("Data/Biomes/Biomes_olson_projected.shp")
biomes_sf <- st_as_sf(biomes_shp)

nw_mount <- shapefile("Data/MapOutlines/Mountains/Koeppen-Geiger_biomes.shp")
nw_bound <- shapefile("Data/MapOutlines/Global_bound/Koeppen-Geiger_biomes.shp")

nw_mount_sf <- st_as_sf(nw_mount)
nw_bound_sf <- st_as_sf(nw_bound)

# 3.2.2 Create map
BiomeRichnessMap <- ggplot(fill=biomes_shp$biomes) +            #delete "fill=biomes_shp$biomes if not coloring the biomes
  geom_tile(data=RichnessDF, aes(x=Longitude, y=Latitude, fill=Alpha)) + 
  scale_fill_gradientn(name="α diversity", colours=cols, na.value="transparent") + 
  coord_equal() +
  #geom_sf(data = nw_bound_sf, size = 0.5, fill=NA) +           #un-comment for continental outlines
  #geom_sf(data = nw_mount_sf, size = 0.5, fill=NA) +           #un-comment for mountain outlines
  geom_sf(data = biomes_sf, size = 0.5, fill=NA) +
  theme_void() +
  theme(legend.text=element_text(size=20), 
        legend.title=element_text(size=32), 
        axis.title = element_blank())
BiomeRichnessMap

# 3.2.3 Save map
png("Figures/AlphaBiomeMap.png", width = 1000, height = 1000, pointsize = 30)
BiomeRichnessMap
dev.off()



# 4.0 CREATE RICHNESS BY BIOME DATAFRAME -----------------------------------
# 4.1 Subset biomes --------------------------------------------------------
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


# 4.2 Create dataframe including coordinates for each cell -----------------
LongLatRaster <- setValues(BlankRas, CellVec)
LongLatPoints<-rasterToPoints(LongLatRaster)
LongLatDF <- data.frame(LongLatPoints)
colnames(LongLatDF) <- c("Longitude", "Latitude", "CellID")

coordinates(LongLatDF) <- ~Longitude+Latitude 
proj4string(LongLatDF) <- CRS("+proj=utm +zone=10") 
LongLat <- spTransform(LongLatDF, CRS("+proj=longlat")) 
LongLatDF <- data.frame(LongLat)
LongLatDF[c("Longitude", "Latitude", "CellID")]
LongLatDF <- subset(LongLatDF, select = -c(optional))
saveRDS(LongLatDF, "Data/LongLatDF.rds")


# 4.3 Create dataframes for each biome with cell coordinates and richness --
#Coniferous Forests --------------------------------------------------------
AlphaConFor <- raster::extract(RichnessRaster, Coniferous_Forests, df = TRUE, cellnumbers = TRUE)
colnames(AlphaConFor) <- c("Type", "CellID", "Alpha")
AlphaConFor$Type <- "Coniferous_Forests"
AlphaConForVec <- AlphaConFor$CellID
AlphaConFor <- merge(AlphaConFor, LongLatDF)

#scatterplot
AlphaConForScatterplot <- ggplot() + geom_point(data = AlphaConFor, aes(Latitude, Alpha), shape = 16, size = 5, show.legend = FALSE, alpha=0.5, color = "goldenrod2") + ylab("α div in Coniferous Forests") + ylim(0, 500) + xlab("Latitude") + theme_minimal() +  theme(axis.title.y = element_text(size=32), axis.title.x = element_text(size=32),  axis.text = element_text(size=20))
AlphaConForScatterplot

#boxplot
theme_set(theme_grey())
ggplot(AlphaConFor, aes(x=Type, y=Alpha)) + geom_boxplot()

#Dry Forest ----------------------------------------------------------------
AlphaDryFor <- raster::extract(RichnessRaster, Dry_Forest, df = TRUE, cellnumbers = TRUE)
colnames(AlphaDryFor) <- c("Type", "CellID", "Alpha")
AlphaDryFor$Type <- "Dry_Forest"
AlphaDryForVec <- AlphaDryFor$CellID
AlphaDryFor <- merge(AlphaDryFor, LongLatDF)

#Mediterranean Woodlands ---------------------------------------------------
AlphaMedWood <- raster::extract(RichnessRaster, Mediterranean_Woodlands, df = TRUE, cellnumbers = TRUE)
colnames(AlphaMedWood) <- c("Type", "CellID", "Alpha")
AlphaMedWood$Type <- "Mediterranean_Woodlands"
AlphaMedWoodVec <- AlphaMedWood$CellID
AlphaMedWood <- merge(AlphaMedWood, LongLatDF)

#Moist Forest --------------------------------------------------------------
AlphaMoistFor <- raster::extract(RichnessRaster, Moist_Forest, df = TRUE, cellnumbers = TRUE)
colnames(AlphaMoistFor) <- c("Type", "CellID", "Alpha")
AlphaMoistFor$Type <- "Moist_Forest"
AlphaMoistForVec <- AlphaMoistFor$CellID
AlphaMoistFor <- merge(AlphaMoistFor, LongLatDF)

#Savannas ------------------------------------------------------------------
AlphaSavanna <- raster::extract(RichnessRaster, Savannas, df = TRUE, cellnumbers = TRUE)
colnames(AlphaSavanna) <- c("Type", "CellID", "Alpha")
AlphaSavanna$Type <- "Savannas"
AlphaSavannaVec <- AlphaSavanna$CellID
AlphaSavanna <- merge(AlphaSavanna, LongLatDF)

#Taiga ---------------------------------------------------------------------
AlphaTaiga <- raster::extract(RichnessRaster, Taiga, df = TRUE, cellnumbers = TRUE)
colnames(AlphaTaiga) <- c("Type", "CellID", "Alpha")
AlphaTaiga$Type <- "Taiga"
AlphaTaigaVec <- AlphaTaiga$CellID
AlphaTaiga <- merge(AlphaTaiga, LongLatDF)

#Temperate Grasslands ------------------------------------------------------
AlphaTempGrass <- raster::extract(RichnessRaster, Temperate_Grasslands, df = TRUE, cellnumbers = TRUE)
colnames(AlphaTempGrass) <- c("Type", "CellID", "Alpha")
AlphaTempGrass$Type <- "Temperate_Grasslands"
AlphaTempGrassVec <- AlphaTempGrass$CellID
AlphaTempGrass <- merge(AlphaTempGrass, LongLatDF)

#Temperate Mixed -----------------------------------------------------------
AlphaTempMix <- raster::extract(RichnessRaster, Temperate_Mixed, df = TRUE, cellnumbers = TRUE)
colnames(AlphaTempMix) <- c("Type", "CellID", "Alpha")
AlphaTempMix$Type <- "Temperate_Mixed"
AlphaTempMixVec <- AlphaTempMix$CellID
AlphaTempMix <- merge(AlphaTempMix, LongLatDF)

#Tropical Grasslands -------------------------------------------------------
AlphaTropGrass <- raster::extract(RichnessRaster, Tropical_Grasslands, df = TRUE, cellnumbers = TRUE)
colnames(AlphaTropGrass) <- c("Type", "CellID", "Alpha")
AlphaTropGrass$Type <- "Tropical_Grasslands"
AlphaTropGrassVec <- AlphaTropGrass$CellID
AlphaTropGrass <- merge(AlphaTropGrass, LongLatDF)

#Tundra --------------------------------------------------------------------
AlphaTundra <- raster::extract(RichnessRaster, Tundra, df = TRUE, cellnumbers = TRUE)
colnames(AlphaTundra) <- c("Type", "CellID", "Alpha")
AlphaTundra$Type <- "Tundra"
AlphaTundraVec <- AlphaTundra$CellID
AlphaTundra <- merge(AlphaTundra, LongLatDF)

#Xeric Woodlands -----------------------------------------------------------
AlphaXericWood <- raster::extract(RichnessRaster, Xeric_Woodlands, df = TRUE, cellnumbers = TRUE)
colnames(AlphaXericWood) <- c("Type", "CellID", "Alpha")
AlphaXericWood$Type <- "Xeric_Woodlands"
AlphaXericWoodVec <- AlphaXericWood$CellID
AlphaXericWood <- merge(AlphaXericWood, LongLatDF)


# 4.4 Bind biome dataframes ------------------------------------------------
BiomeRichness <- bind_rows(AlphaConFor, AlphaDryFor, AlphaMedWood,
                           AlphaMoistFor,AlphaSavanna, AlphaTaiga, 
                           AlphaTempGrass, AlphaTempMix,AlphaTropGrass,
                           AlphaTundra, AlphaXericWood)
saveRDS(BiomeRichness, file = "Data/BiomeRichness.rds")



# 5.0 MAKE PLOTS -----------------------------------------------------------
# Using richness values of cells whose centers are within each biome
# Load data
BiomeRichness <- readRDS("Data/BiomeRichness.rds")

#Biome richness scatterplot ------------------------------------------------
BiomeRichScatter <- ggplot(BiomeRichness, aes(Latitude, Alpha, color=Type), show.legend=TRUE) +
  geom_point(shape=16, size=2.5, alpha=0.5) +
  scale_color_manual(values=c("Coniferous_Forests"="#D8B70A", 
                              "Dry_Forest"="#972D15",
                              "Mediterranean_Woodlands"="#A2A475",
                              "Moist_Forest"="#81A88D",
                              "Savannas"="#02401B",
                              "Taiga"="#446455",
                              "Temperate_Grasslands"="#FDD262",
                              "Temperate_Mixed"="#D3DDDC",
                              "Tropical_Grasslands"="#C7B19C",
                              "Tundra"="#798E87",
                              "Xeric_Woodlands"="#C27D38")) +
  #geom_smooth() +
  xlab("Latitude") +
  ylab("Biome Alpha Diversity") +
  labs(color="Biome") +
  theme_minimal() +
  theme(axis.title.y = element_text(size=32),
        axis.title.x = element_text(size=32),
        axis.text = element_text(size=20))
BiomeRichScatter

png("Figures/AlphaBiomeScatter.png", width = 1500, height = 1000, pointsize = 20)
BiomeRichScatter
dev.off()

#Biome richness scatterplot 2 (for the archives) ---------------------------
BiomeRichScatter2 <- ggplot(BiomeRichness, aes(Latitude, Alpha, color=Type), show.legend=TRUE) +
  geom_point(shape=16, size=2.5, alpha=0.5,
             color=biome_cols_4036, show.legend=TRUE) +
  #geom_smooth() +
  xlab("Latitude") +
  ylab("Biome Alpha Diversity") +
  labs(color="Biome") +
  theme_minimal() +
  theme(axis.title.y = element_text(size=32),
        axis.title.x = element_text(size=32),
        axis.text = element_text(size=20))
BiomeRichScatter2

#Biome richness boxplot ----------------------------------------------------
BiomeRichBox <- ggplot(BiomeRichness, aes(x=Type, y=Alpha, fill=Type)) + 
  geom_boxplot(show.legend = FALSE, fill=cols7) +
  guides(x = guide_axis(angle=30)) +
  theme_minimal() +
  xlab("Biome") +
  ylab("Richness") +  
  theme(axis.title.y = element_text(size=32), 
        axis.title.x = element_text(size=32),
        axis.text.y = element_text(size=20), 
        axis.text.x = element_text(angle = 30, hjust = 1, size = 17))
BiomeRichBox

png("Figures/AlphaBiomeBox.png", width = 1500, height = 1000, pointsize = 20)
BiomeRichBox
dev.off()

#Biome richness violin plot ------------------------------------------------
BiomeRichViolin <- ggplot(BiomeRichness, 
                          aes(x=Type, y=Alpha, fill=Type)) +
  geom_violin(scale="count", show.legend = FALSE) +
  scale_fill_manual(values=c("Coniferous_Forests"="#D8B70A", 
                             "Dry_Forest"="#972D15",
                             "Mediterranean_Woodlands"="#A2A475",
                             "Moist_Forest"="#81A88D",
                             "Savannas"="#02401B",
                             "Taiga"="#446455",
                             "Temperate_Grasslands"="#FDD262",
                             "Temperate_Mixed"="#D3DDDC",
                             "Tropical_Grasslands"="#C7B19C",
                             "Tundra"="#798E87",
                             "Xeric_Woodlands"="#C27D38")) +
  geom_boxplot(width=0.05, fill="white", show.legend=FALSE) +
  xlab("Biome") +
  ylab("Richness") +
  theme_minimal() +  
  theme(axis.title.y = element_text(size=32),
        axis.title.x = element_text(size=32),
        axis.text.y = element_text(size=20),
        axis.text.x = element_text(angle = 30, hjust = 1, size = 17))
BiomeRichViolin

png("Figures/AlphaBiomeViolin.png", width = 1500, height = 1000, pointsize = 20)
BiomeRichViolin
dev.off()

#Biome richness boxplot with violins ---------------------------------------
BiomeRichBV <- ggplot(BiomeRichness, aes(x=Type, y=Alpha, fill=Type, color=Type)) + 
  geom_boxplot(show.legend = FALSE, fill=cols7, color="black") +
  guides(x = guide_axis(angle=30)) +
  theme_minimal() +        #un-comment whichever theme you want
  #theme_gray() +
  #theme_light() +
  #theme_bw() +
  geom_violin(scale="count", show.legend=FALSE, fill="gray", alpha=0.35,
              color="gray25") +
  xlab("Biome") +
  ylab("Richness") +  
  theme(axis.title.y = element_text(size=32), 
        axis.title.x = element_text(size=32),
        axis.text.y = element_text(size=20), 
        axis.text.x = element_text(angle = 30, hjust = 1, size = 17))
BiomeRichBV

png("Figures/AlphaBiomeBoxViolin.png", width = 1500, height = 1000, pointsize = 20)
BiomeRichBV
dev.off()

# 5.5 Dataframe and BV plot with weighted cells counted in the biome which 
 ## covers the greatest proportion of the cell
#Coniferous Forests
AlphaConFor <- raster::extract(RichnessRaster, Coniferous_Forests, df = TRUE, cellnumbers = TRUE, weight = TRUE)
colnames(AlphaConFor) <- c("Type", "CellID", "Alpha", "Weight")
AlphaConFor$Type <- "Coniferous_Forests"

#Dry Forest
AlphaDryFor <- raster::extract(RichnessRaster, Dry_Forest, df = TRUE, cellnumbers = TRUE,  weight = TRUE)
colnames(AlphaDryFor) <- c("Type", "CellID", "Alpha", "Weight")
AlphaDryFor$Type <- "Dry_Forest"

#Mediterranean Woodlands
AlphaMedWood <- raster::extract(RichnessRaster, Mediterranean_Woodlands, df = TRUE, cellnumbers = TRUE, weight = TRUE)
colnames(AlphaMedWood) <- c("Type", "CellID", "Alpha", "Weight")
AlphaMedWood$Type <- "Mediterranean_Woodlands"

#Moist Forest
AlphaMoistFor <- raster::extract(RichnessRaster, Moist_Forest, df = TRUE, cellnumbers = TRUE,  weight = TRUE)
colnames(AlphaMoistFor) <- c("Type", "CellID", "Alpha", "Weight")
AlphaMoistFor$Type <- "Moist_Forest"

#Savannas
AlphaSavanna <- raster::extract(RichnessRaster, Savannas, df = TRUE, cellnumbers = TRUE, weight = TRUE)
colnames(AlphaSavanna) <- c("Type", "CellID", "Alpha", "Weight")
AlphaSavanna$Type <- "Savannas"

#Taiga
AlphaTaiga <- raster::extract(RichnessRaster, Taiga, df = TRUE, cellnumbers = TRUE, weight = TRUE)
colnames(AlphaTaiga) <- c("Type", "CellID", "Alpha", "Weight")
AlphaTaiga$Type <- "Taiga"

#Temperate Grasslands
AlphaTempGrass <- raster::extract(RichnessRaster, Temperate_Grasslands, df = TRUE, cellnumbers = TRUE, weight = TRUE)
colnames(AlphaTempGrass) <- c("Type", "CellID", "Alpha", "Weight")
AlphaTempGrass$Type <- "Temperate_Grasslands"

#Temperate Mixed
AlphaTempMix <- raster::extract(RichnessRaster, Temperate_Mixed, df = TRUE, cellnumbers = TRUE,  weight = TRUE)
colnames(AlphaTempMix) <- c("Type", "CellID", "Alpha", "Weight")
AlphaTempMix$Type <- "Temperate_Mixed"

#Tropical Grasslands
AlphaTropGrass <- raster::extract(RichnessRaster, Tropical_Grasslands, df = TRUE, cellnumbers = TRUE, weight = TRUE)
colnames(AlphaTropGrass) <- c("Type", "CellID", "Alpha", "Weight")
AlphaTropGrass$Type <- "Tropical_Grasslands"

#Tundra
AlphaTundra <- raster::extract(RichnessRaster, Tundra, df = TRUE, cellnumbers = TRUE, weight = TRUE)
colnames(AlphaTundra) <- c("Type", "CellID", "Alpha", "Weight")
AlphaTundra$Type <- "Tundra"

#Xeric Woodlands
AlphaXericWood <- raster::extract(RichnessRaster, Xeric_Woodlands, df = TRUE, cellnumbers = TRUE, weight = TRUE)
colnames(AlphaXericWood) <- c("Type", "CellID", "Alpha", "Weight")
AlphaXericWood$Type <- "Xeric_Woodlands"


# Bind biome dataframes
BiomeRichnessWeight <- bind_rows(AlphaConFor, AlphaDryFor, AlphaMedWood,
                           AlphaMoistFor,AlphaSavanna, AlphaTaiga, 
                           AlphaTempGrass, AlphaTempMix,AlphaTropGrass,
                           AlphaTundra, AlphaXericWood)

#Choose one biome per cell (cells with multiple biomes go to biome with higher proportion coverage)
BiomeRichClean <- BiomeRichnessWeight
BiomeRichCellID <- unique(BiomeRichnessWeight$CellID)
for(i in BiomeRichCellID){
  vec <- BiomeRichClean$Weight[which(BiomeRichClean$CellID == i)]
  if(length(vec) > 1){
    min <- min(vec)
    drop <- which(BiomeRichClean$CellID == i & BiomeRichClean$Weight == min)
    BiomeRichClean <- BiomeRichClean[-drop,]
  }
}

saveRDS(BiomeRichClean, "Data/BiomeRichClean.rds")

#Biome richness boxplot with violins 
BiomeRichBVCleanWeight <- ggplot(BiomeRichClean, aes(x=Type, y=Alpha, fill=Type, color=Type)) + 
  geom_boxplot(show.legend = FALSE, fill=cols7, color="black") +
  guides(x = guide_axis(angle=30)) +
  theme_minimal() +        #un-comment whichever theme you want
  #theme_gray() +
  #theme_light() +
  #theme_bw() +
  geom_violin(scale="count", show.legend=FALSE, fill="gray", alpha=0.35,
              color="gray25") +
  xlab("Biome") +
  ylab("Richness") +  
  theme(axis.title.y = element_text(size=32), 
        axis.title.x = element_text(size=32),
        axis.text.y = element_text(size=20), 
        axis.text.x = element_text(angle = 30, hjust = 1, size = 17))
BiomeRichBVCleanWeight

png("Figures/CleanAlphaBiomeBoxViolin.png", width = 1500, height = 1000, pointsize = 20)
BiomeRichBVCleanWeight
dev.off()


# 6.0 BIOMES MAP -----------------------------------------------------------
# 6.1 Biome map wih legend inside frame
BiomeMap <- qtm(biomes_shp,
                        fill="biomes", 
                        fill.style="fixed",
                        fill.labels=biomes_shp$biomes,
                        fill.palette=cols7,
                        fill.title="Biomes",
                        layout.legend.position=c("left","bottom"),
                        layout.legend.width=1.5,
                        layout.frame=FALSE)
BiomeMap

# 6.2 Save map
png("Figures/BiomeMap.png", width = 1000, height = 1000, pointsize = 30)
BiomeMap
dev.off()

