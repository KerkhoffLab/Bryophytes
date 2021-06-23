# Moss Species Ranges
# Adapted from SpeciesRanges.R and MappingRichness.R
# Kathryn Dawdy June 2021

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
BryophytePresence <- read.csv("Data/BryophytePresence_7.2.20(2).csv")
cols <- (wes_palette("Zissou1", 500, type = "continuous"))

nw_mount <- shapefile("Data/MapOutlines/Mountains/Koeppen-Geiger_biomes.shp")
nw_bound <- shapefile("Data/MapOutlines/Global_bound/Koeppen-Geiger_biomes.shp")
nw_mount_sf <- st_as_sf(nw_mount)
nw_bound_sf <- st_as_sf(nw_bound)


#Subset moss species from BryophytePresence
MossPresence <- subset(BryophytePresence, BryophytePresence$Group=="Mosses")
saveRDS(MossPresence, file = "Data/MossPresence.rds")


#Make MossRichnessVec (which I think is the same as MossRichness.rds but I wanted to make it clearer)
MossCellRichness <- tally(group_by(MossPresence, CellID))
colnames(MossCellRichness)[2] <- "Richness"

MossRichnessVec <- numeric(15038)
MossRichnessVec[MossCellRichness$CellID] <- MossCellRichness$Richness
MossRichnessVec[which(MossRichnessVec==0)]=NA

saveRDS(MossRichnessVec, file = "Data/MossRichnessVec.rds")


#Count how many cells each moss species is found in (species range)
MossCellRange <- tally(group_by(MossPresence, Species))
MossCellRange <- merge(MossCellRange, MossPresence, by = "Species")

#Calculate the median species range of all species found in each cell
MossCellRange <- subset(MossCellRange, select = c("n", "CellID")) %>%
  group_by(CellID) %>%
  summarize(Median = median(n))
saveRDS(MossCellRange, file = "Data/MossCellRange.rds")

MossCellRangeVec <- MossCellRange$CellID

MossRange <- numeric(15038)
MossRange[MossCellRange$CellID] <- MossCellRange$Median
MossRange[which(MossRange==0)]=NA

MossRangeRaster <- setValues(BlankRas, MossRange)
saveRDS(MossRangeRaster, "Data/MossRangeRaster.rds")

MossRangeDF <- rasterToPoints(MossRangeRaster)
MossRangeDF <- data.frame(MossRangeDF)
colnames(MossRangeDF) <- c("Longitude", "Latitude", "Median")

MossRangeMap <- ggplot() + geom_tile(data=MossRangeDF, aes(x=Longitude, y=Latitude, fill=Median)) +   
  scale_fill_gradientn(name="Median Range Size", colours=cols, na.value="transparent") +
  coord_equal() + geom_sf(data = nw_bound_sf, size = 0.5, fill=NA) + geom_sf(data = nw_mount_sf, size = 0.5, alpha=0.0) + theme_void() + 
  theme(legend.text=element_text(size=20), legend.title=element_text(size=23))
MossRangeMap

png("Figures/MossRangeMap.png", width = 1000, height = 1000, pointsize = 30)
MossRangeMap
dev.off()
