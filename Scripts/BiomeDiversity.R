#Testing the shpfile created in 01_Prepare_biome_shpfiles.R
  #and shpfile derived from susyelo's GitHub BIEN_FEE_paper/data/processed/Olson_processed
#Code adapted from MappingRichness.R, Bryophytes.Rmd, TreeMaps.R, and MountainRanges.R
#Kathryn Dawdy, July 2020

#Load packages
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


##Load blank raster and richness/presence data
BlankRas <-raster("Data/blank_100km_raster.tif")
RichnessVec <- readRDS("Data/RichnessVec.rds")
BryophytePresence <- readRDS("Data/BryophytePresence.rds")

#Load biome shapefiles
dir.create("./Data/Biomes/")

download.file("https://raw.github.com/susyelo/BIEN_FEE_paper/tree/Trait_phlyo/data/processed/Olson_processed/Biomes_olson_projected.shp",
              destfile="./Data/Biomes/")

#Set theme and colors for gplots
cols <- (wes_palette("Zissou1", 500, type = "continuous"))
theme_set(theme_void())

#Add continental and mountainous outlines
nw_mount <- shapefile("Data/MapOutlines/Mountains/Koeppen-Geiger_biomes.shp")
nw_bound <- shapefile("Data/MapOutlines/Global_bound/Koeppen-Geiger_biomes.shp")

nw_mount_sf <- st_as_sf(nw_mount)
nw_bound_sf <- st_as_sf(nw_bound)

#Create richness dataframe
RichnessVec[which(RichnessVec==0)]=NA
RichnessRaster <- setValues(BlankRas, RichnessVec)
RichnessDF <- rasterToPoints(RichnessRaster)
RichnessDF <- data.frame(RichnessDF)
colnames(RichnessDF) <- c("Longitude", "Latitude", "Alpha")

#BRYOPHYTE RICHNESS --------------------------------------------------------
#map
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

#BRYOPHYTE RICHNESS - with my biome shapefile ------------------------------
#Add biomes
#my_biomes <- shapefile("Data/processed/test/Biomes_olson_projected.shp")     #doesn't actually exist right now...
#my_biomes_sf <- st_as_sf(my_biomes)

#Make map
#MyRichnessMap <- ggplot() +
  #geom_tile(data=RichnessDF, aes(x=Longitude, y=Latitude, fill=Alpha)) + 
  #scale_fill_gradientn(name="α diversity", colours=cols, na.value="transparent") + 
  #scale_fill_gradientn(colours=cols, na.value="transparent") +
  #coord_equal() +
  #geom_sf(data = nw_bound_sf, size = 0.5, fill=NA) +         #remove continental outlines for clarity
  #geom_sf(data = nw_mount_sf, size = 0.5, fill=NA) +         #remove mountain outlines for clarity
  #geom_sf(data = my_biomes_sf, size = 0.5, fill=NA) +
  #theme_void() +
  #theme(legend.text=element_text(size=20), 
        #legend.title=element_text(size=32), 
        #axis.title = element_blank())
#MyRichnessMap

#BRYOPHYTE RICHNESS - with Susy's biome shapefile --------------------------
#Add biomes
dir.create("./Data/processed/")
#First download Susy's repository and move Olson_processed folder to "./Data/processed/"
Susys_biomes <- shapefile("Data/processed/Olson_processed/Biomes_olson_projected.shp")
Susys_biomes_sf <- st_as_sf(Susys_biomes)

#Make map
SusysRichnessMap <- ggplot() +
  geom_tile(data=RichnessDF, aes(x=Longitude, y=Latitude, fill=Alpha)) + 
  scale_fill_gradientn(name="α diversity", colours=cols, na.value="transparent") + 
  scale_fill_gradientn(colours=cols, na.value="transparent") +
  coord_equal() +
  #geom_sf(data = nw_bound_sf, size = 0.5, fill=NA) +           #remove continental outlines for clarity
  #geom_sf(data = nw_mount_sf, size = 0.5, fill=NA) +           #remove mountain outlines for clarity
  geom_sf(data = Susys_biomes_sf, size = 0.5, fill=NA) +
  theme_void() +
  theme(legend.text=element_text(size=20), 
        legend.title=element_text(size=32), 
        axis.title = element_blank())
SusysRichnessMap
