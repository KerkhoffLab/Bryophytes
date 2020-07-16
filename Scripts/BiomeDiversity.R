#Mapping bryophyte alpha diversity with biomes
#Code adapted from MappingRichness.R, Bryophytes.Rmd, TreeMaps.R, MountainRanges.R, and MountainLowland.Rmd
#Kathryn Dawdy, July 2020

#Load packages -------------------------------------------------------------
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

##Load blank raster and richness/presence data -----------------------------
BlankRas <-raster("Data/blank_100km_raster.tif")
RichnessVec <- readRDS("Data/RichnessVec.rds")
BryophytePresence <- readRDS("Data/BryophytePresence.rds")


#Load biome shapefiles -----------------------------------------------------
##Load entire BIEN_FEE_paper repository (branch: Trait_phylo)
download.file(url = "https://github.com/susyelo/BIEN_FEE_paper/archive/Trait_phylo.zip", 
              destfile = "./Data/Biomes/BIEN_FEE_paper-Trait_phylo.zip")
setwd("./Data/Biomes/")
unzip("BIEN_FEE_paper-Trait_phylo.zip")
setwd("../../")

##Move the shapefiles
file.move("./Data/Biomes/BIEN_FEE_paper-Trait_phylo/data/processed/Olson_processed/Biomes_olson_projected.dbf", "./Data/Biomes/")
file.move("./Data/Biomes/BIEN_FEE_paper-Trait_phylo/data/processed/Olson_processed/Biomes_olson_projected.prj", "./Data/Biomes/")
file.move("./Data/Biomes/BIEN_FEE_paper-Trait_phylo/data/processed/Olson_processed/Biomes_olson_projected.shp", "./Data/Biomes/")
file.move("./Data/Biomes/BIEN_FEE_paper-Trait_phylo/data/processed/Olson_processed/Biomes_olson_projected.shx", "./Data/Biomes/")

##Delete the repository folder and zip file
unlink("./Data/Biomes/BIEN_FEE_paper-Trait_phylo", recursive=TRUE)
unlink("./Data/Biomes/BIEN_FEE_paper-Trait_phylo.zip")


#Set theme and colors for gplots -------------------------------------------
cols <- (wes_palette("Zissou1", 500, type = "continuous"))
theme_set(theme_void())

cols2 <- (wes_palette("Zissou1", 11, type = "continuous"))

#Create bryophyte richness dataframe ---------------------------------------
RichnessVec[which(RichnessVec==0)]=NA
RichnessRaster <- setValues(BlankRas, RichnessVec)
RichnessDF <- rasterToPoints(RichnessRaster)
RichnessDF <- data.frame(RichnessDF)
colnames(RichnessDF) <- c("Longitude", "Latitude", "Alpha")



#BRYOPHYTE RICHNESS - continental and mountainous outlines -----------------
#Add continental and mountainous outlines
nw_mount <- shapefile("Data/MapOutlines/Mountains/Koeppen-Geiger_biomes.shp")
nw_bound <- shapefile("Data/MapOutlines/Global_bound/Koeppen-Geiger_biomes.shp")

nw_mount_sf <- st_as_sf(nw_mount)
nw_bound_sf <- st_as_sf(nw_bound)

#Create map
#RichnessMap <- ggplot() +          #un-comment this section to make bryophyte richness map with continental/mountain outlines
  #geom_tile(data=RichnessDF, aes(x=Longitude, y=Latitude, fill=Alpha)) + 
  #scale_fill_gradientn(name="α diversity", colours=cols, na.value="transparent") + 
  #scale_fill_gradientn(colours=cols, na.value="transparent") +
  #coord_equal() +
  #geom_sf(data = nw_bound_sf, size = 0.5, fill=NA) + 
  #geom_sf(data = nw_mount_sf, size = 0.5, fill=NA) + 
  #theme_void() +
  #theme(legend.text=element_text(size=20), 
        #legend.title=element_text(size=32), 
        #axis.title = element_blank())
#RichnessMap



#BRYOPHYTE RICHNESS - biomes -----------------------------------------------
#Add biomes outlines
biomes_shp <- shapefile("Data/Biomes/Biomes_olson_projected.shp")
biomes_sf <- st_as_sf(biomes_shp)

#Create map
BiomeRichnessMap <- ggplot() +
  geom_tile(data=RichnessDF, aes(x=Longitude, y=Latitude, fill=Alpha)) + 
  scale_fill_gradientn(name="α diversity", colours=cols, na.value="transparent") + 
  scale_fill_gradientn(colours=cols, na.value="transparent") +
  coord_equal() +
  #geom_sf(data = nw_bound_sf, size = 0.5, fill=NA) +           #remove continental outlines for visual clarity
  #geom_sf(data = nw_mount_sf, size = 0.5, fill=NA) +           #remove mountain outlines for visual clarity
  geom_sf(data = biomes_sf, size = 0.5, aes(fill=biomes)) +
  theme_void() +
  theme(legend.text=element_text(size=20), 
        legend.title=element_text(size=32), 
        axis.title = element_blank())
BiomeRichnessMap


#SUBSETTING BIOMES ---------------------------------------------------------
#biome_names=biomes_shp$biomes
#View(biome_names)
#View(biomes_shp)

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


#load data etc.
LongLatBetaRaster <- readRDS("Data/LongLatBetaRaster.rds")
RichnessRaster <- readRDS("Data/RichnessRaster.rds")
BlankRas <-raster("Data/blank_100km_raster.tif")
CellVec <- c(1:15038)

#Creates dataframe including coordinates for each cell
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


#---------------------------------------------------------------------------
#Scatterplots of α-diversity values of cells whose centers are within biomes
LongLatDF <- readRDS("Data/LongLatDF.rds")

#All Biomes ----------------------------------------------------------------
#Doesn't really tell us much...
LongLatDF <- readRDS("Data/LongLatDF.rds")
AlphaBiomes <- extract(RichnessRaster, biomes_shp, df = TRUE, cellnumbers = TRUE)
colnames(AlphaBiomes) <- c("Type", "CellID", "Alpha")
AlphaBiomes$Type <- "Biome"
AlphaBiomesVec <- AlphaBiomes$CellID
AlphaBiomes <- merge(AlphaBiomes, LongLatDF)
#saveRDS(AlphaBiomes, file = "Data/AlphaBiomes.rds")
AlphaBiomesScatterplot <- ggplot() + geom_point(data = AlphaBiomes, aes(Latitude, Alpha), shape = 16, size = 5, show.legend = FALSE, alpha=0.5, color = "goldenrod2") + ylab("Alpha diversity in biomes") + ylim(0, 1000) + xlab("Latitude") + theme_minimal() +  theme(axis.title.y = element_text(size=32), axis.title.x = element_text(size=32),  axis.text = element_text(size=20))
AlphaBiomesScatterplot


#INDIVIDUAL BIOMES ---------------------------------------------------------
#Coniferous Forests --------------------------------------------------------
AlphaConFor <- extract(RichnessRaster, Coniferous_Forests, df = TRUE, cellnumbers = TRUE)
colnames(AlphaConFor) <- c("Type", "CellID", "Alpha")
AlphaConFor$Type <- "Coniferous_Forests"
AlphaConForVec <- AlphaConFor$CellID
AlphaConFor <- merge(AlphaConFor, LongLatDF)
#saveRDS(AlphaConFor, file = "Data/AlphaConFor.rds")

#scatterplot
AlphaConForScatterplot <- ggplot() + geom_point(data = AlphaConFor, aes(Latitude, Alpha), shape = 16, size = 5, show.legend = FALSE, alpha=0.5, color = "goldenrod2") + ylab("α div in Coniferous Forests") + ylim(0, 500) + xlab("Latitude") + theme_minimal() +  theme(axis.title.y = element_text(size=32), axis.title.x = element_text(size=32),  axis.text = element_text(size=20))
AlphaConForScatterplot

#boxplot
theme_set(theme_grey())
ggplot(AlphaConFor, aes(x=Type, y=Alpha)) + geom_boxplot()

#Dry Forest ----------------------------------------------------------------
AlphaDryFor <- extract(RichnessRaster, Dry_Forest, df = TRUE, cellnumbers = TRUE)
colnames(AlphaDryFor) <- c("Type", "CellID", "Alpha")
AlphaDryFor$Type <- "Dry_Forest"
AlphaDryForVec <- AlphaDryFor$CellID
AlphaDryFor <- merge(AlphaDryFor, LongLatDF)
#saveRDS(AlphaDryFor, file = "Data/AlphaDryFor.rds")

#Mediterranean Woodlands ---------------------------------------------------
AlphaMedWood <- extract(RichnessRaster, Mediterranean_Woodlands, df = TRUE, cellnumbers = TRUE)
colnames(AlphaMedWood) <- c("Type", "CellID", "Alpha")
AlphaMedWood$Type <- "Mediterranean_Woodlands"
AlphaMedWoodVec <- AlphaMedWood$CellID
AlphaMedWood <- merge(AlphaMedWood, LongLatDF)
#saveRDS(AlphaMedWood, file = "Data/AlphaMedWood.rds")

#Moist Forest --------------------------------------------------------------
AlphaMoistFor <- extract(RichnessRaster, Moist_Forest, df = TRUE, cellnumbers = TRUE)
colnames(AlphaMoistFor) <- c("Type", "CellID", "Alpha")
AlphaMoistFor$Type <- "Moist_Forest"
AlphaMoistForVec <- AlphaMoistFor$CellID
AlphaMoistFor <- merge(AlphaMoistFor, LongLatDF)
#saveRDS(AlphaMoistFor, file = "Data/AlphaMoistFor.rds")

#Savannas ------------------------------------------------------------------
AlphaSavanna <- extract(RichnessRaster, Savannas, df = TRUE, cellnumbers = TRUE)
colnames(AlphaSavanna) <- c("Type", "CellID", "Alpha")
AlphaSavanna$Type <- "Savannas"
AlphaSavannaVec <- AlphaSavanna$CellID
AlphaSavanna <- merge(AlphaSavanna, LongLatDF)
#saveRDS(AlphaSavanna, file = "Data/AlphaSavanna.rds")

#Taiga ---------------------------------------------------------------------
AlphaTaiga <- extract(RichnessRaster, Taiga, df = TRUE, cellnumbers = TRUE)
colnames(AlphaTaiga) <- c("Type", "CellID", "Alpha")
AlphaTaiga$Type <- "Taiga"
AlphaTaigaVec <- AlphaTaiga$CellID
AlphaTaiga <- merge(AlphaTaiga, LongLatDF)
#saveRDS(AlphaTaiga, file = "Data/AlphaTaiga.rds")

#Temperate Grasslands ------------------------------------------------------
AlphaTempGrass <- extract(RichnessRaster, Temperate_Grasslands, df = TRUE, cellnumbers = TRUE)
colnames(AlphaTempGrass) <- c("Type", "CellID", "Alpha")
AlphaTempGrass$Type <- "Temperate_Grasslands"
AlphaTempGrassVec <- AlphaTempGrass$CellID
AlphaTempGrass <- merge(AlphaTempGrass, LongLatDF)
#saveRDS(AlphaTempGrass, file = "Data/AlphaTempGrass.rds")

#Temperate Mixed -----------------------------------------------------------
AlphaTempMix <- extract(RichnessRaster, Temperate_Mixed, df = TRUE, cellnumbers = TRUE)
colnames(AlphaTempMix) <- c("Type", "CellID", "Alpha")
AlphaTempMix$Type <- "Temperate_Mixed"
AlphaTempMixVec <- AlphaTempMix$CellID
AlphaTempMix <- merge(AlphaTempMix, LongLatDF)
#saveRDS(AlphaTempMix, file = "Data/AlphaTempMix.rds")

#Tropical Grasslands -------------------------------------------------------
AlphaTropGrass <- extract(RichnessRaster, Tropical_Grasslands, df = TRUE, cellnumbers = TRUE)
colnames(AlphaTropGrass) <- c("Type", "CellID", "Alpha")
AlphaTropGrass$Type <- "Tropical_Grasslands"
AlphaTropGrassVec <- AlphaTropGrass$CellID
AlphaTropGrass <- merge(AlphaTropGrass, LongLatDF)
#saveRDS(AlphaTropGrass, file = "Data/AlphaTropGrass.rds")

#Tundra --------------------------------------------------------------------
AlphaTundra <- extract(RichnessRaster, Tundra, df = TRUE, cellnumbers = TRUE)
colnames(AlphaTundra) <- c("Type", "CellID", "Alpha")
AlphaTundra$Type <- "Tundra"
AlphaTundraVec <- AlphaTundra$CellID
AlphaTundra <- merge(AlphaTundra, LongLatDF)
#saveRDS(AlphaTundra, file = "Data/AlphaTundra.rds")

#Xeric Woodlands -----------------------------------------------------------
AlphaXericWood <- extract(RichnessRaster, Xeric_Woodlands, df = TRUE, cellnumbers = TRUE)
colnames(AlphaXericWood) <- c("Type", "CellID", "Alpha")
AlphaXericWood$Type <- "Xeric_Woodlands"
AlphaXericWoodVec <- AlphaXericWood$CellID
AlphaXericWood <- merge(AlphaXericWood, LongLatDF)
#saveRDS(AlphaXericWood, file = "Data/AlphaXericWood.rds")


#Bind biome dataframes
BiomesRichness <- bind_rows(AlphaConFor, AlphaDryFor, AlphaMedWood, AlphaMoistFor, 
                      AlphaSavanna, AlphaTaiga, AlphaTempGrass, AlphaTempMix,
                      AlphaTropGrass, AlphaTundra, AlphaXericWood)
View(BiomesRichness)
saveRDS(BiomesRichness, file = "Data/BiomesRichness.rds")
BiomesRichness <- readRDS("Data/BiomesRichness.rds")
