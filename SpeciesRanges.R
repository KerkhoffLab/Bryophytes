#Calculating ranges 

#Load packages 
require(knitr)
require(latexpdf)
require(sp)
require(vegan)
require(raster)
require(rasterVis)
require(wesanderson)
require(ggplot2)
require(gridExtra)
require(sf)
require(rgdal)
require(dplyr)

#Load in data (including outlines) and create color gradient 
BlankRas <-raster("Data/blank_100km_raster.tif")
BryophytePresence <- readRDS("Data/BryophytePresence.rds")
cols <- (wes_palette("Zissou1", 500, type = "continuous"))

nw_mount <- shapefile("Data/MapOutlines/Mountains/Koeppen-Geiger_biomes.shp")
nw_bound <- shapefile("Data/MapOutlines/Global_bound/Koeppen-Geiger_biomes.shp")
nw_mount_sf <- st_as_sf(nw_mount)
nw_bound_sf <- st_as_sf(nw_bound)

#Count the number of cells in which a species is found, includes latitude and longitude values 
Range <- tally(group_by(BryophytePresence, Species))
Range <- merge(Range, BryophytePresence, by = "Species")

Range <- subset(Range, select = c("n", "Latitude")) %>%
  group_by(Latitude) %>%
  summarize(Avg = median(n))

#Map of the median of the average range size of the bryophytes found in a single cell/ at a specific latitude, mapped using latitude values 
RangeScatterplot <- ggplot(Range, aes(Latitude, Avg)) + geom_point(shape = 16, size = 5, show.legend = FALSE, alpha=0.5, color = "orangered2") + 
  ylab("Average Range Size") + theme_minimal() + 
  theme(axis.title.y = element_text(size=32), axis.title.x = element_text(size=32),  axis.text = element_text(size=20))
RangeScatterplot

png("Figures/RangeScatterplot.png", width = 1000, height = 1000, pointsize = 30)
RangeScatterplot
dev.off()

#Range map of bryophytes using cellID, but still showing median of the average range size of bryophytes in a single cell 
#Count the number of cells in which a species is found 
Range <- tally(group_by(BryophytePresence, Species))
Range <- merge(Range, BryophytePresence, by = "Species")

Range <- subset(Range, select = c("n", "CellID")) %>%
  group_by(CellID) %>%
  summarize(Avg = median(n))

BryophyteRange <- numeric(15038)
BryophyteRange[Range$CellID] <- Range$Avg 
BryophyteRange[which(BryophyteRange==0)]=NA

RangeRaster <- setValues(BlankRas, BryophyteRange)
RangeDF <- rasterToPoints(RangeRaster)
RangeDF <- data.frame(RangeDF)
colnames(RangeDF) <- c("Longitude", "Latitude", "Avg")

RangeMap <- ggplot() + geom_tile(data=RangeDF, aes(x=Longitude, y=Latitude, fill=Avg)) +   
  scale_fill_gradientn(name="Median Range Size", colours=cols, na.value="transparent") +
  coord_equal() + geom_sf(data = nw_bound_sf, size = 0.5, fill=NA) + geom_sf(data = nw_mount_sf, size = 0.5, alpha=0.1) + theme_void() + 
  theme(legend.text=element_text(size=20), legend.title=element_text(size=23))
RangeMap

png("Figures/RangeMap.png", width = 1000, height = 1000, pointsize = 30)
RangeMap
dev.off()