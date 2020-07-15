#Mapping bryophyte alpha diversity with biomes
#Code adapted from MappingRichness.R, Bryophytes.Rmd, TreeMaps.R, and MountainRanges.R
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

library(filesstrings)


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


#Create richness dataframe -------------------------------------------------
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
#Add biomes
biomes <- shapefile("Data/Biomes/Biomes_olson_projected.shp")
biomes_sf <- st_as_sf(biomes)

#Create map
BiomeRichnessMap <- ggplot() +
  geom_tile(data=RichnessDF, aes(x=Longitude, y=Latitude, fill=Alpha)) + 
  scale_fill_gradientn(name="α diversity", colours=cols, na.value="transparent") + 
  scale_fill_gradientn(colours=cols, na.value="transparent") +
  coord_equal() +
  #geom_sf(data = nw_bound_sf, size = 0.5, fill=NA) +           #remove continental outlines for visual clarity
  #geom_sf(data = nw_mount_sf, size = 0.5, fill=NA) +           #remove mountain outlines for visual clarity
  geom_sf(data = Susys_biomes_sf, size = 0.5, fill=NA) +
  theme_void() +
  theme(legend.text=element_text(size=20), 
        legend.title=element_text(size=32), 
        axis.title = element_blank())
BiomeRichnessMap
