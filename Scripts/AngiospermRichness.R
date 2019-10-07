#Mapping angiosperm alpha diversity for the poster

#Load packages
require(BIEN)
require(maps) 
require(maptools)
require(raster)
require(sp)
require(rgdal)
require(mapdata)
require(mapproj)
require(wesanderson)
require(ggplot2)
require(rasterVis)
require(dplyr)

#Load data and source functions
SpeciesPresence <- readRDS("Data/SpeciesPresence.rds")
BlankRas <-raster("Data/blank_100km_raster.tif")

nw_mount <- shapefile("Data/MapOutlines/Mountains/Koeppen-Geiger_biomes.shp")
nw_bound <- shapefile("Data/MapOutlines/Global_bound/Koeppen-Geiger_biomes.shp")
nw_mount_sf <- st_as_sf(nw_mount)
nw_bound_sf <- st_as_sf(nw_bound)

source("Functions/BIEN_taxonomy_higher_plant_group.R")

#Table of all angiosperm occurrences
AngSpecies <- BIEN_taxonomy_higher_plant_group("flowering plants", print.query = FALSE)
SpeciesPresence$Species <- gsub("_", " ", SpeciesPresence$Species)
AngiospermPresence <- SpeciesPresence[SpeciesPresence$Species %in% unique(AngSpecies$scrubbed_species_binomial), ]
AngiospermPresence <- distinct(AngiospermPresence)

#Tally richness by cell and create richness vector
AngCellRichness <- tally(group_by(AngiospermPresence, CellID))
colnames(AngCellRichness)[2] <- "Richness"
AngRichnessVec <- numeric(15038)
AngRichnessVec[AngCellRichness$CellID] <- AngCellRichness$Richness

#Make color scheme
cols <- (wes_palette("Zissou1", 500, type = "continuous"))

#Map of angiosperm richness 
AngRichnessVec[which(AngRichnessVec==0)]=NA
AngRichnessRaster <- setValues(BlankRas, AngRichnessVec)

AngRichnessDF <- rasterToPoints(AngRichnessRaster)
AngRichnessDF <- data.frame(AngRichnessDF)
colnames(AngRichnessDF) <- c("Longitude", "Latitude", "Alpha")

AngRichnessMap <- ggplot() + geom_tile(data=AngRichnessDF, aes(x=Longitude, y=Latitude, fill=Alpha)) +   
  scale_fill_gradientn(name="α diversity", colours=cols, na.value="transparent") +
  coord_equal() + geom_sf(data = nw_bound_sf, size = 0.5, fill=NA) + 
  geom_sf(data = nw_mount_sf, size = 0.5, alpha=0.1) + theme_void() + 
  theme(legend.text=element_text(size=20), legend.title=element_text(size=32))
AngRichnessMap

png("Figures/AngRichnessMap.png", width = 1000, height = 1000, pointsize = 30)
AngRichnessMap
dev.off()
