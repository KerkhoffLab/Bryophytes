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

require(tidyverse)
require(tmap)


##Load blank raster and richness/presence data -----------------------------
BlankRas <-raster("Data/blank_100km_raster.tif")
RichnessVec <- readRDS("Data/RichnessVec.rds")
BryophytePresence <- readRDS("Data/BryophytePresence.rds")


#Load biome shapefiles -----------------------------------------------------
##Load entire BIEN_FEE_paper repository (branch: Trait_phylo)
download.file(url = "https://github.com/susyelo/BIEN_FEE_paper/archive/Trait_phylo.zip", 
              destfile = "./Data/Biomes/BIEN_FEE_paper-Trait_phylo.zip")
dir.create("./Data/Biomes/")
setwd("./Data/Biomes/")
unzip("BIEN_FEE_paper-Trait_phylo.zip")
setwd("../../")

##Move the shapefiles
file.move("./Data/Biomes/BIEN_FEE_paper-Trait_phylo/data/processed/Olson_processed/Biomes_olson_projected.dbf", "./Data/Biomes/")
file.move("./Data/Biomes/BIEN_FEE_paper-Trait_phylo/data/processed/Olson_processed/Biomes_olson_projected.prj", "./Data/Biomes/")
file.move("./Data/Biomes/BIEN_FEE_paper-Trait_phylo/data/processed/Olson_processed/Biomes_olson_projected.shp", "./Data/Biomes/")
file.move("./Data/Biomes/BIEN_FEE_paper-Trait_phylo/data/processed/Olson_processed/Biomes_olson_projected.shx", "./Data/Biomes/")

##Delete the repository folder and .zip file
unlink("./Data/Biomes/BIEN_FEE_paper-Trait_phylo", recursive=TRUE)
unlink("./Data/Biomes/BIEN_FEE_paper-Trait_phylo.zip")


#Set theme and colors for gplots -------------------------------------------
cols <- (wes_palette("Zissou1", 500, type = "continuous"))
theme_set(theme_void())

#testing color schemes
cols1 <- (wes_palette("Zissou1", 5632, type = "continuous"))
cols2 <- (wes_palette("Zissou1", 11, type="continuous"))
cols3 <- (wes_palette("Cavalcanti1", 11, type = "continuous"))
cols4 <- c(wes_palette("Cavalcanti1",4,type="continuous"),
           wes_palette("Chevalier1",5,type="continuous"),
           "slateblue1", "orangered3")
cols5 <- c("#D8B70A", "#81A88D", "#A2A475", "#02401B", "#972D15",
           "#446455", "#FDD262", "#D3DDDC", "#C7B19C",
           "#798E87", "#C27D38")
cols6 <- c("#0B775E", "#C93312", "#FAEFD1", "#00A08A", "#7294D4",
           "#446455", "#FDD262", "#D3DDDC", "#D8B70A", "#02401B", "#A2A475")

#Color scheme for biomes (in order of BiomeNames (BiomeProcessing.R))
cols7 <- c("#D8B70A", "#972D15", "#A2A475", "#81A88D", "#02401B",
           "#446455", "#FDD262", "#D3DDDC", "#C7B19C",
           "#798E87", "#C27D38")

#Colors used for plots (# corresponds to # of boxplots/length of data)
biome_cols_11 <- c("#D8B70A", "#972D15", "#A2A475", "#81A88D", "#02401B",
                   "#446455", "#FDD262", "#D3DDDC", "#C7B19C", "#798E87", 
                   "#C27D38")

biome_cols_22 <- c(biome_cols_11, biome_cols_11)

biome_cols_87 <- c(biome_cols_22, biome_cols_11,
                   "#D8B70A", "#972D15", "#A2A475", "#81A88D", "#02401B",
                   "#446455", "#FDD262", "#D3DDDC", "#C7B19C", 
                   "#C27D38",
                   biome_cols_22, biome_cols_22)

biome_cols_232 <- c(biome_cols_22,
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
                    biome_cols_11,
                    c("#D8B70A", "#972D15", "#A2A475", "#81A88D", "#02401B",
                      "#FDD262", "#D3DDDC", "#C7B19C", 
                      "#C27D38"),
                    "#81A88D",
                    biome_cols_11,
                    c("#972D15", "#A2A475", "#81A88D", "#02401B",
                      "#FDD262", "#D3DDDC", "#C7B19C", 
                      "#C27D38"),
                    biome_cols_22,
                    c("#D8B70A",
                      "#446455", "#FDD262", "#D3DDDC"),
                    biome_cols_22,
                    c("#D8B70A", "#972D15", "#81A88D",
                      "#D3DDDC", "#C7B19C", 
                      "#C27D38"),
                    biome_cols_11,
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
                    biome_cols_22)


#wes_palette() hex numbers for reference
#from GitHub: karthik/wesanderson
#wes_palettes <- list(
#  BottleRocket1 = c("#A42820", "#5F5647", "#9B110E", "#3F5151", "#4E2A1E", "#550307", "#0C1707"),
#  BottleRocket2 = c("#FAD510", "#CB2314", "#273046", "#354823", "#1E1E1E"),
#  Rushmore1 = c("#E1BD6D", "#EABE94", "#0B775E", "#35274A" ,"#F2300F"),
#  Rushmore = c("#E1BD6D", "#EABE94", "#0B775E", "#35274A" ,"#F2300F"),
#  Royal1 = c("#899DA4", "#C93312", "#FAEFD1", "#DC863B"),
#  Royal2 = c("#9A8822", "#F5CDB4", "#F8AFA8", "#FDDDA0", "#74A089"),
#  Zissou1 = c("#3B9AB2", "#78B7C5", "#EBCC2A", "#E1AF00", "#F21A00"),
#  Darjeeling1 = c("#FF0000", "#00A08A", "#F2AD00", "#F98400", "#5BBCD6"),
#  Darjeeling2 = c("#ECCBAE", "#046C9A", "#D69C4E", "#ABDDDE", "#000000"),
#  Chevalier1 = c("#446455", "#FDD262", "#D3DDDC", "#C7B19C"),
#  FantasticFox1 = c("#DD8D29", "#E2D200", "#46ACC8", "#E58601", "#B40F20"),
#  Moonrise1 = c("#F3DF6C", "#CEAB07", "#D5D5D3", "#24281A"),
#  Moonrise2 = c("#798E87", "#C27D38", "#CCC591", "#29211F"),
#  Moonrise3 = c("#85D4E3", "#F4B5BD", "#9C964A", "#CDC08C", "#FAD77B"),
#  Cavalcanti1 = c("#D8B70A", "#02401B", "#A2A475", "#81A88D", "#972D15"),
#  GrandBudapest1 = c("#F1BB7B", "#FD6467", "#5B1A18", "#D67236"),
#  GrandBudapest2 = c("#E6A0C4", "#C6CDF7", "#D8A499", "#7294D4"),
#  IsleofDogs1 = c("#9986A5", "#79402E", "#CCBA72", "#0F0D0E", "#D9D0D3", "#8D8680"),
#  IsleofDogs2 = c("#EAD3BF", "#AA9486", "#B6854D", "#39312F", "#1C1718")
#)


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
BiomeRichnessMap <- ggplot(fill=biomes_shp$biomes) +            #delete "fill=biomes_shp$biomes if not coloring the biomes
  geom_tile(data=RichnessDF, aes(x=Longitude, y=Latitude, fill=Alpha)) + 
  scale_fill_gradientn(name="α diversity", colours=cols, na.value="transparent") + 
  scale_fill_gradientn(colours=cols, na.value="transparent") +
  coord_equal() +
  #geom_sf(data = nw_bound_sf, size = 0.5, fill=NA) +           #remove continental outlines for visual clarity
  #geom_sf(data = nw_mount_sf, size = 0.5, fill=NA) +           #remove mountain outlines for visual clarity
  geom_sf(data = biomes_sf, size = 0.5, fill=NA) +
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


#Bind biome dataframes -----------------------------------------------------
BiomeRichness <- bind_rows(AlphaConFor, AlphaDryFor, AlphaMedWood,
                           AlphaMoistFor,AlphaSavanna, AlphaTaiga, 
                           AlphaTempGrass, AlphaTempMix,AlphaTropGrass,
                           AlphaTundra, AlphaXericWood)
saveRDS(BiomeRichness, file = "Data/BiomeRichness.rds")


#MAKE PLOTS ----------------------------------------------------------------

#Biome richness scatterplot ------------------------------------------------
BiomeRichness <- readRDS("Data/BiomeRichness.rds")
#BiomeRichnessScatter <- ggplot() + 
  #geom_point(data = BiomeRichness, 
             #aes(Latitude, Alpha), 
             #shape = 16, size = 5, 
             #show.legend = FALSE, 
             #alpha=0.5, 
             #color = Type
             #) + 
  #ylab("α div in biomes") + 
  #ylim(0, 500) + 
  #xlab("Latitude") +
  #labs(color="Biome")
  #theme_minimal() +  
  #theme(axis.title.y = element_text(size=32), 
        #axis.title.x = element_text(size=32),  
        #axis.text = element_text(size=20))
#BiomeRichnessScatter

BiomeRichScatter <- ggplot(BiomeRichness, aes(Latitude, Alpha, color=Type)) +
  geom_smooth() +
  geom_point(shape=16, size=1, alpha=0.5) +
  xlab("Latitude") +
  ylab("Biome Alpha Diversity") +
  labs(color="Biome") +
  theme_minimal() +
  theme(axis.title.y = element_text(size=32),
        axis.title.x = element_text(size=32),
        axis.text = element_text(size=20))
BiomeRichScatter


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
        axis.text.x = element_text(angle = 30, hjust = 1, size = 12))
BiomeRichBox

#Biome richness violin plot ------------------------------------------------
BiomeRichViolin <- ggplot(BiomeRichness, 
                          aes(x=Type, y=Alpha, fill=Type)) +
  geom_violin(scale="area", show.legend = FALSE, fill=cols1) +  #change fill colors
  guides(x = guide_axis(angle=30)) +
  xlab("Biome") +
  ylab("Richness") +
  theme_minimal() +  
  theme(axis.title.y = element_text(size=32),
        axis.title.x = element_text(size=32),
        axis.text.y = element_text(size=20))
BiomeRichViolin

#Biome richness boxplot with violins ---------------------------------------
BiomeRichBV <- ggplot(BiomeRichness, aes(x=Type, y=Alpha, fill=Type, color=Type)) + 
  geom_boxplot(show.legend = FALSE, fill=cols7, color="black") +
  guides(x = guide_axis(angle=30)) +
  theme_minimal() +
  geom_violin(scale="count", show.legend=FALSE, fill="gray", alpha=0.35,
              color="gray25") +
  xlab("Biome") +
  ylab("Richness") +  
  theme(axis.title.y = element_text(size=32), 
        axis.title.x = element_text(size=32),
        axis.text.y = element_text(size=20), 
        axis.text.x = element_text(angle = 30, hjust = 1, size = 12))
BiomeRichBV


#Biomes Map ----------------------------------------------------------------
#BiomeMap <- ggplot(fill=biomes_shp$biomes, color=biomes_shp$biomes) +
#  coord_equal() +
  #geom_sf(data = nw_bound_sf, size = 0.5, fill=NA) +           #remove continental outlines for visual clarity
  #geom_sf(data = nw_mount_sf, size = 0.5, fill=NA) +           #remove mountain outlines for visual clarity
#  geom_sf(data = biomes_sf, size = 0.5, fill=cols4, color="black") +
#  geom_sf_label(data = biomes_sf, 
#                aes(alpha=0.5, label = biomes_shp$biomes, size=.01), 
#                show.legend = FALSE) +
#  theme_void()
#BiomeMap


#Biome map with legend outside frame
#BiomeMapLegendOUT <-qtm(biomes_shp, fill="biomes", fill.style="fixed",
#                   fill.labels=biomes_shp$biomes,
#                   fill.palette=cols4, 
#                   fill.title="Biomes",
#                   layout.legend.outside=TRUE,
#                   layout.legend.width)
#BiomeMapLegendOUT

#Biome map wih legend inside frame
BiomeMapLegendIN <- qtm(biomes_shp,
                        fill="biomes", 
                        fill.style="fixed",
                        fill.labels=biomes_shp$biomes,
                        fill.palette=cols7,
                        fill.title="Biomes",
                        layout.legend.position=c("left","bottom"),
                        layout.legend.width=1.5,
                        layout.frame=FALSE)
BiomeMapLegendIN


#Biome Richness by Order ---------------------------------------------------
#Hailey's function:
source("Functions/OrdBiomeBP.R")
###First run BiomeProcessing.R, ORange.R, and Data.Processing

#Enter any order for box, violin, or boxyviolin plots, "Southern" or "Northern" or "both" hem
OrdBiomeBP("Hypnales", "box")
OrdBiomeBP("Hypnales", "violin")
OrdBiomeBP("Hypnales", "boxyviolin")
OrdBiomeBP("Hypnales", "boxyviolin", hem="Southern")


###Run OrderBiomeDF.R
OrderBiomeDF <- readRDS("Data/OrderBiomeDF.rds")
View(OrderBiomeDF)

#Facet wrap of richness in biomes by order ---------------------------------
FacetOrdBiomeRich <- ggplot(OrderBiomeDF, 
                      aes(x=Biome, y=Alpha,)) + 
  geom_boxplot(show.legend = FALSE) +
  guides(x = guide_axis(angle=30)) +
  theme_minimal() +
  xlab("Biome") +
  ylab("Richness") +  
  theme(axis.title.y = element_text(size=32), 
        axis.title.x = element_text(size=32),
        axis.text.y = element_text(size=20), 
        axis.text.x = element_text(angle = 30, hjust = 1, size = 12))+
  facet_wrap(~Order)
FacetOrdBiomeRich

#way too many plots in this^ facet, so subset order by max alpha value
###Run OrderRichness.R
#>100
OrdRichAbove100
OBRAbove100DF <- subset(OrderBiomeDF, 
                        OrderBiomeDF$Order=="Hypnales"|
                          OrderBiomeDF$Order=="Dicranales")

#25-100
OrdRich25to100
OBR25to100DF <- subset(OrderBiomeDF,
                       OrderBiomeDF$Order=="Bartramiales"|
                         OrderBiomeDF$Order=="Bryales"|
                         OrderBiomeDF$Order=="Grimmiales"|
                         OrderBiomeDF$Order=="Hookeriales"|
                         OrderBiomeDF$Order=="Jungermanniales"|
                         OrderBiomeDF$Order=="Orthotrichales"|
                         OrderBiomeDF$Order=="Porellales"|
                         OrderBiomeDF$Order=="Pottiales")

#<25
OrdRichBelow25
OBRBelow25DF <- subset(OrderBiomeDF,
                     OrderBiomeDF$Order!="Hypnales"&
                       OrderBiomeDF$Order!="Dicranales"&
                       OrderBiomeDF$Order!="Bartramiales"&
                       OrderBiomeDF$Order!="Bryales"&
                       OrderBiomeDF$Order!="Grimmiales"&
                       OrderBiomeDF$Order!="Hookeriales"&
                       OrderBiomeDF$Order!="Jungermanniales"&
                       OrderBiomeDF$Order!="Orthotrichales"&
                       OrderBiomeDF$Order!="Porellales"&
                       OrderBiomeDF$Order!="Pottiales")

#Facet wrap of richness in biomes by order with >100 max richness ----------
FacetOBRAbove100 <- ggplot(OBRAbove100DF, 
                            aes(x=Biome, y=Alpha, fill=Biome, color=Biome)) + 
  geom_boxplot(show.legend = FALSE, fill=biome_cols_22, color="black",
               outlier.size=1) +
  theme_gray() +
  geom_violin(scale="count", show.legend=FALSE, fill="gray", alpha=0.35,
              color="gray25") +
  xlab("Biome") +
  ylab("Richness") +  
  theme(axis.title.y = element_text(size=32), 
        axis.title.x = element_text(size=32),
        axis.text.y = element_text(size=15), 
        axis.text.x = element_text(angle = 30, hjust = 1, size = 8))+
  facet_wrap(~Order, ncol=1)
FacetOBRAbove100

#Facet wrap of richness in biomes by order with 25-100 max richness --------
FacetOBR25to100 <- ggplot(OBR25to100DF, 
                           aes(x=Biome, y=Alpha, fill=Biome, color=Biome)) + 
  geom_boxplot(show.legend = FALSE, fill=biome_cols_87, color="black",
               outlier.size=0.7) +
  theme_gray() +
  geom_violin(scale="count", show.legend=FALSE, fill="gray", alpha=0.35,
              color="gray25") +
  xlab("Biome") +
  ylab("Richness") +  
  theme(axis.title.y = element_text(size=32), 
        axis.title.x = element_text(size=32),
        axis.text.y = element_text(size=15), 
        axis.text.x = element_text(angle = 30, hjust = 1, size = 8))+
  facet_wrap(~Order, ncol=2)
FacetOBR25to100

#Facet wrap of richness in biomes by order with <25 max richness -----------
FacetOBRBelow25 <- ggplot(OBRBelow25DF, 
                          aes(x=Biome, y=Alpha, fill=Biome, color=Biome)) + 
  geom_boxplot(show.legend = FALSE, fill=biome_cols_232, color="black",
               outlier.size=0.5) +
  theme_gray() +
  #geom_violin(scale="count", show.legend=FALSE, fill="gray", alpha=0.35, color="gray25") +       #un-comment for transparent violins
  xlab("Biome") +
  ylab("Richness") +  
  theme(axis.title.y = element_text(size=32), 
        axis.title.x = element_text(size=32),
        axis.text.y = element_text(size=15), 
        axis.text.x = element_text(angle = 90, hjust = 1, size = 8))+
  facet_wrap(~Order, ncol=7)
FacetOBRBelow25
