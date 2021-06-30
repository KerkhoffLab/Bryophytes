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
MossRichnessDF <- readRDS("Data/MossRichnessDF.rds")  #run through 3.1 for data
nw_mount <- shapefile("Data/MapOutlines/Mountains/Koeppen-Geiger_biomes.shp")
MossBiomeRichness <- readRDS("Data/MossBiomeRichness.rds")  #run through 1.3 for data
MossBiomeMountRichness <- readRDS("Data/MossBiomeMountRichness.rds")  #run through 2.1
MossRichBM <- readRDS("Data/MossRichBM.rds")
MossRichBM2 <- readRDS("Data/MossRichBM2.rds")
MossAllBiomeMount <- readRDS("Data/MossAllBiomeMount")
MossBiomePanelRichness <- readRDS("Data/MossBiomePanelRichness.rds")



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
                           AlphaTempGrass, AlphaTempMix, AlphaTropGrass,
                           AlphaTundra, AlphaXericWood)
saveRDS(MossBiomeRichness, file = "Data/MossBiomeRichness.rds")



# 2.0 CREATE MOSS RICHNESS BY BIOME AND MOUNTAINS DATAFRAME ----------------
# 2.1 DF with mountainous regions considered Type --------------------------
AlphaMount <- raster::extract(MossRichnessRaster, nw_mount, df = TRUE, cellnumbers = TRUE)
colnames(AlphaMount) <- c("Type", "CellID", "Alpha")
AlphaMount$Type <-"nw_mount"
AlphaMountVec <- AlphaMount$CellID
AlphaMount <- merge(AlphaMount, LongLatDF)

MossBiomeMountRichness <- bind_rows(MossBiomeRichness, AlphaMount)

saveRDS(MossBiomeMountRichness, file = "Data/MossBiomeMountRichness.rds")

# 2.2 DF with mountainous regions a new column -----------------------------
AlphaMount1 <- raster::extract(MossRichnessRaster, nw_mount, df = TRUE, cellnumbers = TRUE)
colnames(AlphaMount1) <- c("Region", "CellID", "Alpha")
AlphaMount1$Region <- "Mountainous"
AlphaMount1Vec <- AlphaMount1$CellID
AlphaMount1 <- merge(AlphaMount1, LongLatDF)

MossRichBM <- full_join(MossBiomeRichness, AlphaMount1,
                                     by="CellID")
View(MossRichBM)
colnames(MossRichBM)[3] <- "Alpha"
colnames(MossRichBM)[4] <- "Longitude"
colnames(MossRichBM)[5] <- "Latitude"
MossRichBM$Alpha.y <- NULL
MossRichBM$Longitude.y <- NULL
MossRichBM$Latitude.y <- NULL
MossRichBM$Region[is.na(MossRichBM$Region)] <- "Lowland"
#ignore rows with Biome NA values
MossRichBM <- MossRichBM[complete.cases(MossRichBM[ , 2]),]

saveRDS(MossRichBM, file = "Data/MossRichBM.rds")

# 2.3 DF with 2 regions columns? -------------------------------------------
AlphaMount2 <- raster::extract(MossRichnessRaster, nw_mount, df = TRUE, cellnumbers = TRUE)
colnames(AlphaMount2) <- c("Region", "CellID", "Alpha")
AlphaMount2$Region <- "Mountainous"
AlphaMount2Vec <- AlphaMount2$CellID
AlphaMount2 <- merge(AlphaMount2, LongLatDF)

MossRichBM1 <- full_join(MossBiomeRichness, AlphaMount2,
                        by="CellID")
View(MossRichBM1)
colnames(MossRichBM1)[3] <- "Alpha1"
colnames(MossRichBM1)[4] <- "Longitude1"
colnames(MossRichBM1)[5] <- "Latitude1"
colnames(MossRichBM1)[6] <- "Region1"
MossRichBM1$Alpha.y <- NULL
MossRichBM1$Longitude.y <- NULL
MossRichBM1$Latitude.y <- NULL
MossRichBM1$Region1[is.na(MossRichBM1$Region1)] <- "Lowland"
#ignore rows with Biome NA values
MossRichBM1 <- MossRichBM1[complete.cases(MossRichBM1[ , 2]),]

# 2nd half of DF
AlphaMount3 <- raster::extract(MossRichnessRaster, nw_mount, df = TRUE, cellnumbers = TRUE)
colnames(AlphaMount3) <- c("Region", "CellID", "Alpha")
AlphaMount3$Region <- "Mountainous"
AlphaMount3Vec <- AlphaMount3$CellID
AlphaMount3 <- merge(AlphaMount3, LongLatDF)

MossRichBM2 <- bind_rows(MossRichBM1, AlphaMount3)

View(MossRichBM2)
colnames(MossRichBM2)[8] <- "Alpha2"
colnames(MossRichBM2)[9] <- "Longitude2"
colnames(MossRichBM2)[10] <- "Latitude2"
colnames(MossRichBM2)[7] <- "Region2"


saveRDS(MossRichBM2, file = "Data/MossRichBM2.rds")


# 2.4 DF with additional "All" variable in Type ----------------------------
MossAllBiomeMount <- MossRichBM
MossAllBiomeMount$AllBiomes <- "All Biomes"

saveRDS(MossAllBiomeMount, file="Data/MossAllBiomeMount")

# 3.0 MOSS BIOME RICHNESS MAP ----------------------------------------------
# 3.1 Create moss richness dataframe ---------------------------------------
MossRichnessDF <- rasterToPoints(MossRichnessRaster)
MossRichnessDF <- data.frame(MossRichnessDF)
colnames(MossRichnessDF) <- c("Longitude", "Latitude", "Alpha")

saveRDS(MossRichnessDF, file="Data/MossRichnessDF.rds")

# 3.2 Add biomes outlines (and continental and mountainous outlines) -------
biomes_shp <- shapefile("Data/Biomes/Biomes_olson_projected.shp")
biomes_sf <- st_as_sf(biomes_shp)

nw_mount <- shapefile("Data/MapOutlines/Mountains/Koeppen-Geiger_biomes.shp")
nw_bound <- shapefile("Data/MapOutlines/Global_bound/Koeppen-Geiger_biomes.shp")

nw_mount_sf <- st_as_sf(nw_mount)
nw_bound_sf <- st_as_sf(nw_bound)

# 3.3 Create map -----------------------------------------------------------
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

# 3.4 Save map
png("Figures/MossAlphaBiomeMap.png", width = 1000, height = 1000, pointsize = 30)
MossBiomeRichnessMap
dev.off()

# 3.5 Add lat/long to map --------------------------------------------------
CoordMossBiomeRichnessMap <- ggplot(fill=biomes_shp$biomes) +            #delete "fill=biomes_shp$biomes if not coloring the biomes
  geom_tile(data=MossRichnessDF, aes(x=Longitude, y=Latitude, fill=Alpha)) + 
  scale_fill_gradientn(name="α diversity", colours=cols, na.value="transparent") + 
  coord_equal() +
  #geom_sf(data = nw_bound_sf, size = 0.5, fill=NA) +           #un-comment for continental outlines
  #geom_sf(data = nw_mount_sf, size = 0.5, fill=NA) +           #un-comment for mountain outlines
  geom_sf(data = biomes_sf, size = 0.5, fill=NA) +
  theme_minimal() + #alternate: theme_gray()
  theme(legend.text=element_text(size=20), 
        legend.title=element_text(size=32), 
        axis.title = element_blank())
CoordMossBiomeRichnessMap

png("Figures/CoordMossAlphaBiomeMap.png", width = 1000, height = 1000, pointsize = 30)
CoordMossBiomeRichnessMap
dev.off()


# 4.0 MOSS BIOME BETA MAP --------------------------------------------------
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
MossBiomeRichness <- readRDS("Data/MossBiomeRichness.rds")

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
MossBiomeRichViolin

png("Figures/MossAlphaBiomeViolin.png", width = 1500, height = 1000, pointsize = 20)
MossBiomeRichViolin
dev.off()

# 4.4 Biome richness boxplot with violins ----------------------------------
MossBiomeRichBV <- ggplot(MossBiomeRichness, aes(x=Type, y=Alpha, 
                                                 fill=Type, color=Type)) + 
  geom_boxplot(show.legend = FALSE, fill=cols7, color="black") +
  guides(x = guide_axis(angle=30)) +
  theme_minimal() +        #un-comment whichever theme you want
  #theme_gray() +
  #theme_light() +
  #theme_bw() +
  geom_violin(scale="count", show.legend=FALSE, fill="gray", alpha=0.35,
              color="gray25") +
  xlab("Biome") +
  ylab("Alpha Diversity") +  
  theme(axis.title.y = element_text(size=32), 
        axis.title.x = element_text(size=32),
        axis.text.y = element_text(size=20), 
        axis.text.x = element_text(angle = 30, hjust = 1, size = 17))
MossBiomeRichBV

png("Figures/MossAlphaBiomeBoxViolin.png", width = 1500, height = 1000, pointsize = 20)
MossBiomeRichBV
dev.off()


# 4.5 Biome and mountainous richness scatterplots --------------------------
# 4.5.1 Black circles around mountainous regions ---------------------------
MossBiomeMountRichScatter <- ggplot(MossBiomeMountRichness, aes(Latitude, Alpha, color=Type, shape=Type), show.legend=TRUE) +
  geom_point(size=2.5, alpha=0.5) +
  scale_shape_manual(values=c("Coniferous_Forests"=16, 
                              "Dry_Forest"=16,
                              "Mediterranean_Woodlands"=16,
                              "Moist_Forest"=16,
                              "Savannas"=16,
                              "Taiga"=16,
                              "Temperate_Grasslands"=16,
                              "Temperate_Mixed"=16,
                              "Tropical_Grasslands"=16,
                              "Tundra"=16,
                              "Xeric_Woodlands"=16,
                              "nw_mount"=1)) +
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
                              "Xeric_Woodlands"="#C27D38",
                              "nw_mount"="#000000"),
                     labels=c("Coniferous Forests",
                              "Dry Forest",
                              "Mediterranean Woodlands",
                              "Moist Forest", 
                              "Mountainous",
                              "Savannas",
                              "Taiga",
                              "Temperate Grasslands",
                              "Temperate Mixed",
                              "Tropical Grasslands",
                              "Tundra",
                              "Xeric Woodlands")) +
  #geom_smooth() +
  xlab("Latitude") +
  ylab("Biome Alpha Diversity") +
  labs(color="Biome") +
  theme_minimal() +
  theme(axis.title.y = element_text(size=32),
        axis.title.x = element_text(size=32),
        axis.text = element_text(size=20)) +
  guides(shape="none")
MossBiomeMountRichScatter

png("Figures/MossAlphaBiomeMountScatter.png", width = 1500, height = 1000, pointsize = 20)
MossBiomeMountRichScatter
dev.off()


# 4.5.2 Change shape of mountainous/lowland regions ------------------------
MossRichBMScatter <- ggplot(MossRichBM, 
                                    aes(Latitude, Alpha, color=Type, 
                                        shape=Region), show.legend=TRUE) +
  geom_point(size=2.5, alpha=0.5) +
  #guides(shape="none") +
  scale_shape_manual(values=c("Lowland"=1,
                              "Mountainous"=16)) +
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
                              "Xeric_Woodlands"="#C27D38"),
                     labels=c("Coniferous Forests",
                              "Dry Forest",
                              "Mediterranean Woodlands", 
                              "Mountainous",
                              "Savannas",
                              "Taiga",
                              "Temperate Grasslands",
                              "Temperate Mixed",
                              "Tropical Grasslands",
                              "Tundra",
                              "Xeric Woodlands")) +
  xlab("Latitude") +
  ylab("Biome Alpha Diversity") +
  labs(color="Biome") +
  theme_minimal() +
  theme(axis.title.y = element_text(size=32),
        axis.title.x = element_text(size=32),
        axis.text = element_text(size=20))
MossRichBMScatter

png("Figures/MossAlphaBiomeMountLow.png", width = 1500, height = 1000, pointsize = 20)
MossRichBMScatter
dev.off()


# 4.5.3 3-D Biome and mountainous richness scatterplot ---------------------
#Code adapted from MossLMToo.R
#Load packages
require(plotly)
#Load data
LMDF3 <- readRDS("Data/LMDF3.rds")

# Biome categories, colored by topography
f <- plot_ly(x=LMDF3$Biome, y=LMDF3$Lat, z=LMDF3$TotalRichness, 
                type="scatter3d", mode="markers", color=LMDF3$Topo, 
                marker=list(size=5))
axx <- list(
  title = "Biome",
  nticks=11
)

axy <- list(
  title = "Latitude"
)

axz <- list(
  title = "Total Richness"
)
f <- f %>% layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz))
f

# Topography categories, colored by biome
f1 <- plot_ly(x=LMDF3$Topo, y=LMDF3$Lat, z=LMDF3$TotalRichness, 
             type="scatter3d", mode="markers", color=LMDF3$Biome, 
             marker=list(size=5))
axx <- list(
  title = "Topography",
  nticks=11
)

axy <- list(
  title = "Latitude"
)

axz <- list(
  title = "Total Richness"
)
f1 <- f1 %>% layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz))
f1

# 4.5.4 Biome and mountainous richness scatterplot w/ more control ---------
#Not really working - probably scrap DF
MossRichBMScatter2 <- ggplot(show.legend=TRUE) +
  geom_point(data=MossRichBM2, aes(Latitude1, Alpha1, color=Type), 
             size=2.5, alpha=0.5, shape=16) +
  #guides(shape="none") +
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
                              "Xeric_Woodlands"="#C27D38"),
                     labels=c("Coniferous Forests",
                              "Dry Forest",
                              "Mediterranean Woodlands", 
                              "Mountainous",
                              "Savannas",
                              "Taiga",
                              "Temperate Grasslands",
                              "Temperate Mixed",
                              "Tropical Grasslands",
                              "Tundra",
                              "Xeric Woodlands")) +
  geom_point(data=MossRichBM2, aes(Latitude2, Alpha2, color=Region2, 
                                   shape=Region2),
             size=2.5, alpha=0.5) +
  xlab("Latitude") +
  ylab("Biome Alpha Diversity") +
  labs(color="Biome") +
  theme_minimal() +
  theme(axis.title.y = element_text(size=32),
        axis.title.x = element_text(size=32),
        axis.text = element_text(size=20))
MossRichBMScatter2


# 4.5.5 Trying again to get more control over shapes + colors --------------
#Grr this is also not working
MountVec <- vector(mode="character", length=1133)
MountVec[1:260] <- "#000000"

LowVec <- vector(mode="character", length=3102)
LowVec[1:244] <- NA

MountLow_cols <- c(MountVec, LowVec)

MossRichBMScatter3 <- ggplot(MossRichBM, 
                            aes(Latitude, Alpha), show.legend=TRUE, color=Type) +
  geom_point(data=MossRichBM, shape=16, size=2.5, alpha=0.5) +
  #guides(shape="none") +
  geom_point(data=MossRichBM, shape=1, size=2.5, alpha=0.5) +
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
                              "Xeric_Woodlands"="#C27D38"),
                     labels=c("Coniferous Forests",
                              "Dry Forest",
                              "Mediterranean Woodlands", 
                              "Mountainous",
                              "Savannas",
                              "Taiga",
                              "Temperate Grasslands",
                              "Temperate Mixed",
                              "Tropical Grasslands",
                              "Tundra",
                              "Xeric Woodlands")) +
  xlab("Latitude") +
  ylab("Biome Alpha Diversity") +
  labs(color="Biome") +
  theme_minimal() +
  theme(axis.title.y = element_text(size=32),
        axis.title.x = element_text(size=32),
        axis.text = element_text(size=20))
MossRichBMScatter3







# 5.0 Panels of different biomes with all biomes in gray -------------------
###IN PROGRESS###
#I'll have to make separate DFs for each biome with AllBiomes, make a column to mark each biome+all as that biome, then bind_rows, divide by that biome marker

# 5.1 Make dataframes ------------------------------------------------------
# 5.1.1 Make DFs with each biome and all occurrences together; 
  #one new column with name of biome in order to panel it out by that biome but with all occurrences also shown in each panel
MossBiomeRichnessAllBiomes <- MossBiomeRichness
MossBiomeRichnessAllBiomes$Type <- "All Biomes"

ConForDF <- bind_rows(MossBiomeRichnessAllBiomes, AlphaConFor)
ConForDF$Biome <- "Coniferous Forests"

DryForDF <- bind_rows(MossBiomeRichnessAllBiomes, AlphaDryFor)
DryForDF$Biome <- "Dry Forest"

MedWoodDF <- bind_rows(MossBiomeRichnessAllBiomes, AlphaMedWood)
MedWoodDF$Biome <- "Mediterranean Woodlands"

MoistForDF <- bind_rows(MossBiomeRichnessAllBiomes, AlphaMoistFor)
MoistForDF$Biome <- "Moist Forest"

SavannaDF <- bind_rows(MossBiomeRichnessAllBiomes, AlphaSavanna)
SavannaDF$Biome <- "Savannas"

TaigaDF <- bind_rows(MossBiomeRichnessAllBiomes, AlphaTaiga)
TaigaDF$Biome <- "Taiga"

TempGrassDF <- bind_rows(MossBiomeRichnessAllBiomes, AlphaTempGrass)
TempGrassDF$Biome <- "Temperate Grasslands"

TempMixDF <- bind_rows(MossBiomeRichnessAllBiomes, AlphaTempMix)
TempMixDF$Biome <- "Temperate Mixed"

TropGrassDF <- bind_rows(MossBiomeRichnessAllBiomes, AlphaTropGrass)
TropGrassDF$Biome <- "Tropical Grasslands"

TundraDF <- bind_rows(MossBiomeRichnessAllBiomes, AlphaTundra)
TundraDF$Biome <- "Tundra"

XericWoodDF <- bind_rows(MossBiomeRichnessAllBiomes, AlphaXericWood)
XericWoodDF$Biome <- "Xeric Woodlands"

# 5.1.2 Bind all biomes together -------------------------------------------
MossBiomePanelRichness <- bind_rows(ConForDF, DryForDF, MedWoodDF, MoistForDF,
                                    SavannaDF, TaigaDF, TempGrassDF, TempMixDF,
                                    TropGrassDF, TundraDF, XericWoodDF)

saveRDS(MossBiomePanelRichness, file="Data/MossBiomePanelRichness.rds")

# 5.1.3 Make DF (same as above but also with montane cells) ----------------
AlphaMount <- raster::extract(MossRichnessRaster, nw_mount, df = TRUE, cellnumbers = TRUE)
colnames(AlphaMount) <- c("Type", "CellID", "Alpha")
AlphaMount$Type <-"Montane"
AlphaMountVec <- AlphaMount$CellID
AlphaMount <- merge(AlphaMount, LongLatDF)

MossBiomeRichnessAllBiomes <- MossBiomeRichness
MossBiomeRichnessAllBiomes$Type <- "All Biomes"

MountConForDF <- bind_rows(MossBiomeRichnessAllBiomes, AlphaConFor, AlphaMount)
MountConForDF$Biome <- "Coniferous Forests"

MountDryForDF <- bind_rows(MossBiomeRichnessAllBiomes, AlphaDryFor, AlphaMount)
MountDryForDF$Biome <- "Dry Forest"

MountMedWoodDF <- bind_rows(MossBiomeRichnessAllBiomes, AlphaMedWood, AlphaMount)
MountMedWoodDF$Biome <- "Mediterranean Woodlands"

MountMoistForDF <- bind_rows(MossBiomeRichnessAllBiomes, AlphaMoistFor, AlphaMount)
MountMoistForDF$Biome <- "Moist Forest"

MountSavannaDF <- bind_rows(MossBiomeRichnessAllBiomes, AlphaSavanna, AlphaMount)
MountSavannaDF$Biome <- "Savannas"

MountTaigaDF <- bind_rows(MossBiomeRichnessAllBiomes, AlphaTaiga, AlphaMount)
MountTaigaDF$Biome <- "Taiga"

MountTempGrassDF <- bind_rows(MossBiomeRichnessAllBiomes, AlphaTempGrass, AlphaMount)
MountTempGrassDF$Biome <- "Temperate Grasslands"

MountTempMixDF <- bind_rows(MossBiomeRichnessAllBiomes, AlphaTempMix, AlphaMount)
MountTempMixDF$Biome <- "Temperate Mixed"

MountTropGrassDF <- bind_rows(MossBiomeRichnessAllBiomes, AlphaTropGrass, AlphaMount)
MountTropGrassDF$Biome <- "Tropical Grasslands"

MountTundraDF <- bind_rows(MossBiomeRichnessAllBiomes, AlphaTundra, AlphaMount)
MountTundraDF$Biome <- "Tundra"

MountXericWoodDF <- bind_rows(MossBiomeRichnessAllBiomes, AlphaXericWood, AlphaMount)
MountXericWoodDF$Biome <- "Xeric Woodlands"

MossBiomeMountPanelRichness <- bind_rows(MountConForDF, MountDryForDF, 
                                          MountMedWoodDF, MountMoistForDF,
                                          MountSavannaDF, MountTaigaDF, 
                                          MountTempGrassDF, MountTempMixDF,
                                          MountTropGrassDF, MountTundraDF, 
                                          MountXericWoodDF)

#saveRDS(MossBiomeMountPanelRichness, file="Data/MossBiomeMountPanelRichness.rds")


# 5.2 Plot richness by latitude paneled by biome (w/ all points gray) ------
MossBiomePanelScatter <- ggplot(MossBiomePanelRichness,
                                 aes(Latitude, Alpha, color=Type, alpha=Type,
                                     fill=Type)) +
  geom_point(size=1.5, shape=16, show.legend=FALSE) +
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
                             "Xeric_Woodlands"="#C27D38",
                             "All Biomes"="gray85")) +
  scale_alpha_manual(values=c("Coniferous_Forests"=0.5, 
                              "Dry_Forest"=0.5,
                              "Mediterranean_Woodlands"=0.5,
                              "Moist_Forest"=0.5,
                              "Savannas"=0.5,
                              "Taiga"=0.5,
                              "Temperate_Grasslands"=0.5,
                              "Temperate_Mixed"=0.5,
                              "Tropical_Grasslands"=0.5,
                              "Tundra"=0.5,
                              "Xeric_Woodlands"=0.5,
                              "All Biomes"=0.1)) +
  xlab("Latitude") +
  ylab("Biome Alpha Diversity") +
  labs(color="Biome") +
  theme_minimal() +
  theme(axis.title.y = element_text(size=32),
        axis.title.x = element_text(size=32),
        axis.text = element_text(size=10)) +
  #guides(alpha="none", color="none") +
  facet_wrap(~Biome)
MossBiomePanelScatter

png("Figures/MossAlphaBiomePanel.png", width = 1500, height = 1000, pointsize = 20)
MossBiomePanelScatter
dev.off()


# 5.3 Make same plot as 5.2 but w/ black circles around montane cells ------
MossBiomeMountPanelScatter <- ggplot(MossBiomeMountPanelRichness,
                                aes(Latitude, Alpha, color=Type, alpha=Type,
                                    fill=Type, shape=Type, size=Type)) +
  geom_point(show.legend=FALSE) +
  scale_shape_manual(values=c("Montane"=20,
                              "Coniferous_Forests"=16, 
                              "Dry_Forest"=16,
                              "Mediterranean_Woodlands"=16,
                              "Moist_Forest"=16,
                              "Savannas"=16,
                              "Taiga"=16,
                              "Temperate_Grasslands"=16,
                              "Temperate_Mixed"=16,
                              "Tropical_Grasslands"=16,
                              "Tundra"=16,
                              "Xeric_Woodlands"=16,
                              "All Biomes"=16)) +
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
                              "Xeric_Woodlands"="#C27D38",
                              "All Biomes"="gray85",
                              "Montane"="#000000")) +
  scale_alpha_manual(values=c("Coniferous_Forests"=0.5, 
                              "Dry_Forest"=0.5,
                              "Mediterranean_Woodlands"=0.5,
                              "Moist_Forest"=0.5,
                              "Savannas"=0.5,
                              "Taiga"=0.5,
                              "Temperate_Grasslands"=0.5,
                              "Temperate_Mixed"=0.5,
                              "Tropical_Grasslands"=0.5,
                              "Tundra"=0.5,
                              "Xeric_Woodlands"=0.5,
                              "All Biomes"=0.2,
                              "Montane"=0.2)) +
  scale_size_manual(values=c("Coniferous_Forests"=1.5, 
                             "Dry_Forest"=1.5,
                             "Mediterranean_Woodlands"=1.5,
                             "Moist_Forest"=1.5,
                             "Savannas"=1.5,
                             "Taiga"=1.5,
                             "Temperate_Grasslands"=1.5,
                             "Temperate_Mixed"=1.5,
                             "Tropical_Grasslands"=1.5,
                             "Tundra"=1.5,
                             "Xeric_Woodlands"=1.5,
                             "All Biomes"=1.5,
                             "Montane"=0.7)) +
  xlab("Latitude") +
  ylab("Biome Alpha Diversity") +
  #labs(color="Biome") +
  #guides(alpha="none", color="none") +
  theme_minimal() +
  theme(axis.title.y = element_text(size=20),
        axis.title.x = element_text(size=20),
        axis.text = element_text(size=10)) +
  facet_wrap(~Biome)
MossBiomeMountPanelScatter


# 6.0 New color scheme -----------------------------------------------------
MossBiomeMountPanelScatter2 <- ggplot(MossBiomeMountPanelRichness,
                                     aes(Latitude, Alpha, color=Type, alpha=Type,
                                         fill=Type, shape=Type, size=Type)) +
  geom_point(show.legend=FALSE) +
  scale_shape_manual(values=c("Montane"=1,
                              "Coniferous_Forests"=16, 
                              "Dry_Forest"=16,
                              "Mediterranean_Woodlands"=16,
                              "Moist_Forest"=16,
                              "Savannas"=16,
                              "Taiga"=16,
                              "Temperate_Grasslands"=16,
                              "Temperate_Mixed"=16,
                              "Tropical_Grasslands"=16,
                              "Tundra"=16,
                              "Xeric_Woodlands"=16,
                              "All Biomes"=16)) +
scale_color_manual(values=c("Coniferous_Forests"="#271a5b", 
                            "Dry_Forest"="#82eb80",
                            "Mediterranean_Woodlands"="#ff6baf",
                            "Moist_Forest"="#01db96",
                            "Savannas"="#8e0039",
                            "Taiga"="#fbcf59",
                            "Temperate_Grasslands"="#2d95ff",
                            "Temperate_Mixed"="#ab4a00",
                            "Tropical_Grasslands"="#015eb1",
                            "Tundra"="#3b6100",
                            "Xeric_Woodlands"="#ff7591",
                            "All Biomes"="gray85",
                            "Montane"="#000000")) +
  #scale_color_manual(values=c("Coniferous_Forests"="#8c2520", 
   #                           "Dry_Forest"="#50a0e7",
    #                          "Mediterranean_Woodlands"="#5a388b",
     #                         "Moist_Forest"="#56b873",
      #                        "Savannas"="#7d9b3a",
       #                       "Taiga"="#c563af",
        #                      "Temperate_Grasslands"="#6d80d8",
         #                     "Temperate_Mixed"="#c8993c",
          #                    "Tropical_Grasslands"="#43c9b0",
           #                   "Tundra"="#ba4758",
            #                  "Xeric_Woodlands"="#b85937",
             #                 "All Biomes"="gray85",
              #                "Montane"="#000000")) +
  scale_alpha_manual(values=c("Coniferous_Forests"=0.5, 
                              "Dry_Forest"=0.5,
                              "Mediterranean_Woodlands"=0.5,
                              "Moist_Forest"=0.5,
                              "Savannas"=0.5,
                              "Taiga"=0.5,
                              "Temperate_Grasslands"=0.5,
                              "Temperate_Mixed"=0.5,
                              "Tropical_Grasslands"=0.5,
                              "Tundra"=0.5,
                              "Xeric_Woodlands"=0.5,
                              "All Biomes"=0.2,
                              "Montane"=0.2)) +
  scale_size_manual(values=c("Coniferous_Forests"=1.5, 
                             "Dry_Forest"=1.5,
                             "Mediterranean_Woodlands"=1.5,
                             "Moist_Forest"=1.5,
                             "Savannas"=1.5,
                             "Taiga"=1.5,
                             "Temperate_Grasslands"=1.5,
                             "Temperate_Mixed"=1.5,
                             "Tropical_Grasslands"=1.5,
                             "Tundra"=1.5,
                             "Xeric_Woodlands"=1.5,
                             "All Biomes"=1.5,
                             "Montane"=1.5)) +
  xlab("Latitude") +
  ylab("Biome Alpha Diversity") +
  #labs(color="Biome") +
  #guides(alpha="none", color="none") +
  theme_minimal() +
  theme(axis.title.y = element_text(size=20),
        axis.title.x = element_text(size=20),
        axis.text = element_text(size=10)) +
  facet_wrap(~Biome)
MossBiomeMountPanelScatter2


# 5.3 Divide by montane/lowland areas --------------------------------------