# Plot MOSS biome diversity (alpha and beta)
  # boxyviolins
  # maps
# Code adapted from MappingRichness.R, BiomeDiversity.R, MappingMossDiversity.R
# Kathryn Dawdy, September 2020


# 0.0 Load Packages --------------------------------------------------------
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

# 0.1 Load data ------------------------------------------------------------
MossRichnessRaster <- readRDS("Data/MossRichnessRaster.rds")  #run 0.2 for data
LongLatDF <- readRDS("Data/LongLatDF.rds")  #run BiomeDiversity.R for data
biomes_shp <- shapefile("Data/Biomes/Biomes_olson_projected.shp")
biomes_sf <- st_as_sf(biomes_shp)

# 0.2 Run for MossRichnessRaster -------------------------------------------

#Load blank raster and richness/presence data
BlankRas <-raster("Data/blank_100km_raster.tif")
RichnessVec <- readRDS("Data/RichnessVec.rds")
BryophytePresence <- readRDS("Data/BryophytePresence.rds")

#Set theme and colors for gplots
cols <- (wes_palette("Zissou1", 500, type = "continuous"))
theme_set(theme_void())

cols1 <- (wes_palette("Zissou1", 5632, type = "continuous"))
cols7 <- c("#D8B70A", "#972D15", "#A2A475", "#81A88D", "#02401B",
           "#446455", "#FDD262", "#D3DDDC", "#C7B19C",
           "#798E87", "#C27D38")

#BRYOPHYTE RICHNESS
RichnessVec[which(RichnessVec==0)]=NA
RichnessRaster <- setValues(BlankRas, RichnessVec)
gplot(RichnessRaster, maxpixels=15038) + geom_raster(aes(fill = value))+ scale_fill_gradientn(colours=cols, na.value="transparent") +
  coord_equal() 


#MOSS RICHNESS
MossPresence <- subset(BryophytePresence, BryophytePresence$Group=="Mosses")
MossPresence <- tally(group_by(MossPresence, CellID))
colnames(MossPresence)[2] <- "Richness"

MossRichness <- numeric(15038)
MossRichness[MossPresence$CellID] <- MossPresence$Richness
MossRichness[which(MossRichness==0)]=NA
MossRichnessRaster <- setValues(BlankRas, MossRichness)

#Plot moss richness
gplot(MossRichnessRaster, maxpixels=15038) + geom_raster(aes(fill = value))+ scale_fill_gradientn(colours=cols, na.value="transparent") +
  coord_equal() 

saveRDS(MossRichnessRaster, file = "Data/MossRichnessRaster.rds")


# 1.0 CREATE MOSS RICHNESS BY BIOME DATAFRAME ------------------------------
# 1.1 Subset biomes --------------------------------------------------------
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


# 1.2 Create dataframes for each biome w/ cell coordinates and richness ----
#Coniferous Forests --------------------------------------------------------
AlphaConFor <- raster::extract(MossRichnessRaster, Coniferous_Forests, df = TRUE, cellnumbers = TRUE)
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
AlphaDryFor <- raster::extract(MossRichnessRaster, Dry_Forest, df = TRUE, cellnumbers = TRUE)
colnames(AlphaDryFor) <- c("Type", "CellID", "Alpha")
AlphaDryFor$Type <- "Dry_Forest"
AlphaDryForVec <- AlphaDryFor$CellID
AlphaDryFor <- merge(AlphaDryFor, LongLatDF)

#Mediterranean Woodlands ---------------------------------------------------
AlphaMedWood <- raster::extract(MossRichnessRaster, Mediterranean_Woodlands, df = TRUE, cellnumbers = TRUE)
colnames(AlphaMedWood) <- c("Type", "CellID", "Alpha")
AlphaMedWood$Type <- "Mediterranean_Woodlands"
AlphaMedWoodVec <- AlphaMedWood$CellID
AlphaMedWood <- merge(AlphaMedWood, LongLatDF)

#Moist Forest --------------------------------------------------------------
AlphaMoistFor <- raster::extract(MossRichnessRaster, Moist_Forest, df = TRUE, cellnumbers = TRUE)
colnames(AlphaMoistFor) <- c("Type", "CellID", "Alpha")
AlphaMoistFor$Type <- "Moist_Forest"
AlphaMoistForVec <- AlphaMoistFor$CellID
AlphaMoistFor <- merge(AlphaMoistFor, LongLatDF)

#Savannas ------------------------------------------------------------------
AlphaSavanna <- raster::extract(MossRichnessRaster, Savannas, df = TRUE, cellnumbers = TRUE)
colnames(AlphaSavanna) <- c("Type", "CellID", "Alpha")
AlphaSavanna$Type <- "Savannas"
AlphaSavannaVec <- AlphaSavanna$CellID
AlphaSavanna <- merge(AlphaSavanna, LongLatDF)

#Taiga ---------------------------------------------------------------------
AlphaTaiga <- raster::extract(MossRichnessRaster, Taiga, df = TRUE, cellnumbers = TRUE)
colnames(AlphaTaiga) <- c("Type", "CellID", "Alpha")
AlphaTaiga$Type <- "Taiga"
AlphaTaigaVec <- AlphaTaiga$CellID
AlphaTaiga <- merge(AlphaTaiga, LongLatDF)

#Temperate Grasslands ------------------------------------------------------
AlphaTempGrass <- raster::extract(MossRichnessRaster, Temperate_Grasslands, df = TRUE, cellnumbers = TRUE)
colnames(AlphaTempGrass) <- c("Type", "CellID", "Alpha")
AlphaTempGrass$Type <- "Temperate_Grasslands"
AlphaTempGrassVec <- AlphaTempGrass$CellID
AlphaTempGrass <- merge(AlphaTempGrass, LongLatDF)

#Temperate Mixed -----------------------------------------------------------
AlphaTempMix <- raster::extract(MossRichnessRaster, Temperate_Mixed, df = TRUE, cellnumbers = TRUE)
colnames(AlphaTempMix) <- c("Type", "CellID", "Alpha")
AlphaTempMix$Type <- "Temperate_Mixed"
AlphaTempMixVec <- AlphaTempMix$CellID
AlphaTempMix <- merge(AlphaTempMix, LongLatDF)

#Tropical Grasslands -------------------------------------------------------
AlphaTropGrass <- raster::extract(MossRichnessRaster, Tropical_Grasslands, df = TRUE, cellnumbers = TRUE)
colnames(AlphaTropGrass) <- c("Type", "CellID", "Alpha")
AlphaTropGrass$Type <- "Tropical_Grasslands"
AlphaTropGrassVec <- AlphaTropGrass$CellID
AlphaTropGrass <- merge(AlphaTropGrass, LongLatDF)

#Tundra --------------------------------------------------------------------
AlphaTundra <- raster::extract(MossRichnessRaster, Tundra, df = TRUE, cellnumbers = TRUE)
colnames(AlphaTundra) <- c("Type", "CellID", "Alpha")
AlphaTundra$Type <- "Tundra"
AlphaTundraVec <- AlphaTundra$CellID
AlphaTundra <- merge(AlphaTundra, LongLatDF)

#Xeric Woodlands -----------------------------------------------------------
AlphaXericWood <- raster::extract(MossRichnessRaster, Xeric_Woodlands, df = TRUE, cellnumbers = TRUE)
colnames(AlphaXericWood) <- c("Type", "CellID", "Alpha")
AlphaXericWood$Type <- "Xeric_Woodlands"
AlphaXericWoodVec <- AlphaXericWood$CellID
AlphaXericWood <- merge(AlphaXericWood, LongLatDF)


# 1.3 Bind biome dataframes ------------------------------------------------
MossBiomeRichness <- bind_rows(AlphaConFor, AlphaDryFor, AlphaMedWood,
                           AlphaMoistFor,AlphaSavanna, AlphaTaiga, 
                           AlphaTempGrass, AlphaTempMix,AlphaTropGrass,
                           AlphaTundra, AlphaXericWood)
saveRDS(MossBiomeRichness, file = "Data/MossBiomeRichness.rds")



# 2.0 MOSS BIOME RICHNESS MAP ----------------------------------------------
# 2.1 Create moss richness dataframe ---------------------------------------
MossRichnessDF <- rasterToPoints(MossRichnessRaster)
MossRichnessDF <- data.frame(MossRichnessDF)
colnames(MossRichnessDF) <- c("Longitude", "Latitude", "Alpha")

# 2.2 Add biomes outlines (and continental and mountainous outlines) -------
biomes_shp <- shapefile("Data/Biomes/Biomes_olson_projected.shp")
biomes_sf <- st_as_sf(biomes_shp)

nw_mount <- shapefile("Data/MapOutlines/Mountains/Koeppen-Geiger_biomes.shp")
nw_bound <- shapefile("Data/MapOutlines/Global_bound/Koeppen-Geiger_biomes.shp")

nw_mount_sf <- st_as_sf(nw_mount)
nw_bound_sf <- st_as_sf(nw_bound)

# 2.3 Create map -----------------------------------------------------------
MossBiomeRichnessMap <- ggplot(fill=biomes_shp$biomes) +            #delete "fill=biomes_shp$biomes if not coloring the biomes
  geom_tile(data=MossRichnessDF, aes(x=Longitude, y=Latitude, fill=Alpha)) + 
  scale_fill_gradientn(name="α diversity", colours=cols, na.value="transparent") + 
  coord_equal() +
  #geom_sf(data = nw_bound_sf, size = 0.5, fill=NA) +           #un-comment for continental outlines
  #geom_sf(data = nw_mount_sf, size = 0.5, fill=NA) +           #un-comment for mountain outlines
  geom_sf(data = biomes_sf, size = 0.5, fill=NA) +
  theme_void() +
  theme(legend.text=element_text(size=20), 
        legend.title=element_text(size=32), 
        axis.title = element_blank())
MossBiomeRichnessMap

# 2.4 Save map
png("Figures/MossAlphaBiomeMap.png", width = 1000, height = 1000, pointsize = 30)
MossBiomeRichnessMap
dev.off()



# 3.0 MOSS BIOME BETA MAP --------------------------------------------------
### FIRST RUN MAPPINGMOSSDIVERSITY.R ###
MossBiomeBetaMap <- ggplot() +
  geom_tile(data = dplyr::filter(gplotOutlier, !is.na(value)), 
            aes(x = x, y = y), fill = "gray25") +
  geom_tile(data = gplotB, 
            aes(x = x, y = y, fill = value)) +
  scale_fill_gradientn(name = "β diversity", colours=cols, na.value="transparent", limits = c(0,0.5)) +
  coord_quickmap() + 
  #geom_sf(data = nw_bound_sf, size = 0.5, fill=NA) +                   #un-comment for continental boundaries
  #geom_sf(data = nw_mount_sf, size = 0.5, alpha=0.1) + theme_void() +  #un-comment for mountains
  geom_sf(data = biomes_sf, size = 0.5, fill=NA) +
  theme(legend.text=element_text(size=20), legend.title=element_text(size=32), axis.title = element_blank())
MossBiomeBetaMap

png("Figures/MossBetaBiomeMap.png", width = 1000, height = 1000, pointsize = 30)
MossBiomeBetaMap
dev.off()


# 4.0 MAKE PLOTS -----------------------------------------------------------
# Using richness values of cells whose centers are within each biome
# Load data
BiomeRichness <- readRDS("Data/BiomeRichness.rds")

# 4.1 Biome richness scatterplot -------------------------------------------
MossBiomeRichScatter <- ggplot(MossBiomeRichness, aes(Latitude, Alpha, color=Type), show.legend=TRUE) +
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
MossBiomeRichScatter

png("Figures/MossAlphaBiomeScatter.png", width = 1500, height = 1000, pointsize = 20)
MossBiomeRichScatter
dev.off()


# 4.2 Biome richness boxplot -----------------------------------------------
MossBiomeRichBox <- ggplot(MossBiomeRichness, aes(x=Type, y=Alpha, fill=Type)) + 
  geom_boxplot(show.legend = FALSE, fill=cols7) +
  guides(x = guide_axis(angle=30)) +
  theme_minimal() +
  xlab("Biome") +
  ylab("Richness") +  
  theme(axis.title.y = element_text(size=32), 
        axis.title.x = element_text(size=32),
        axis.text.y = element_text(size=20), 
        axis.text.x = element_text(angle = 30, hjust = 1, size = 17))
MossBiomeRichBox

png("Figures/MossAlphaBiomeBox.png", width = 1500, height = 1000, pointsize = 20)
MossBiomeRichBox
dev.off()

# 4.3 Biome richness violin plot -------------------------------------------
MossBiomeRichViolin <- ggplot(MossBiomeRichness, 
                          aes(x=Type, y=Alpha, fill=Type)) +
  geom_violin(scale="area", show.legend = FALSE, 
              fill=cols1
              ) +  #change fill colors
  xlab("Biome") +
  ylab("Richness") +
  theme_minimal() +  
  theme(axis.title.y = element_text(size=32),
        axis.title.x = element_text(size=32),
        axis.text.y = element_text(size=20),
        axis.text.x = element_text(angle = 30, hjust = 1, size = 17))
MossBiomeRichViolin

png("Figures/MossAlphaBiomeViolin.png", width = 1500, height = 1000, pointsize = 20)
MossBiomeRichViolin
dev.off()

# 4.4 Biome richness boxplot with violins ----------------------------------
MossBiomeRichBV <- ggplot(MossBiomeRichness, aes(x=Type, y=Alpha, fill=Type, color=Type)) + 
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
MossBiomeRichBV

png("Figures/MossAlphaBiomeBoxViolin.png", width = 1500, height = 1000, pointsize = 20)
MossBiomeRichBV
dev.off()
