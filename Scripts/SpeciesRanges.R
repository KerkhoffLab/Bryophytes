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
RangeBeta <- readRDS("Data/RangeBeta.rds")
RangeAlpha <- readRDS("Data/RangeAlpha.rds")

nw_mount <- shapefile("Data/MapOutlines/Mountains/Koeppen-Geiger_biomes.shp")
nw_bound <- shapefile("Data/MapOutlines/Global_bound/Koeppen-Geiger_biomes.shp")
nw_mount_sf <- st_as_sf(nw_mount)
nw_bound_sf <- st_as_sf(nw_bound)

#Count the number of cells in which a species is found, includes latitude and longitude values and calculates the median value
Range <- tally(group_by(BryophytePresence, Species))
Range <- merge(Range, BryophytePresence, by = "Species")

Range <- subset(Range, select = c("n", "Latitude")) %>%
  group_by(Latitude) %>%
  summarize(Avg = median(n))

#Does the same as above, but looks at range by species 
Range <- tally(group_by(BryophytePresence, Species))
Range <- merge(Range, BryophytePresence, by = "Species")

SpeciesRange <- subset(Range, select = c("n", "Species")) %>%
  group_by(Species) %>%
  summarize(Avg = median(n))
colnames(SpeciesRange) <- c("Species", "RangeAvg")
saveRDS(SpeciesRange, file = "Data/SpeciesRange.rds")

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
CellRange <- tally(group_by(BryophytePresence, Species))
CellRange <- merge(CellRange, BryophytePresence, by = "Species")

CellRange <- subset(CellRange, select = c("n", "CellID")) %>%
  group_by(CellID) %>%
  summarize(Avg = median(n))
saveRDS(CellRange, file = "Data/CellRange.rds")

CellRangeVec <- CellRange$CellID

BryophyteRange <- numeric(15038)
BryophyteRange[CellRange$CellID] <- CellRange$Avg 
BryophyteRange[which(BryophyteRange==0)]=NA

RangeRaster <- setValues(BlankRas, BryophyteRange)
saveRDS(RangeRaster, "Data/RangeRaster.rds")

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

#Separate mountainous range values and make a scatterplot 
RangeMount <- extract(RangeRaster, nw_mount, df = TRUE, cellnumbers = TRUE)

colnames(RangeMount) <- c("Type", "CellID", "Avg")
RangeMount$Type <- "Mountain"

RangeMountVec <- RangeMount$CellID
RangeMount <- merge(RangeMount, LongLatDF)
saveRDS(RangeMount, file = "Data/BetaMount.rds")

RangeMountScatterplot <- ggplot() + geom_point(data = RangeMount, aes(Latitude, Avg), shape = 16, size = 5, show.legend = FALSE, alpha=0.5, color = "goldenrod2") + 
  ylab("Montane Range Size") + xlab("Latitude") + theme_minimal() +  
  theme(axis.title.y = element_text(size=32), axis.title.x = element_text(size=32),  axis.text = element_text(size=20))
RangeMountScatterplot

png("Figures/RangeMountScatterplot.png", width = 1500, height = 1000, pointsize = 20)
RangeMountScatterplot
dev.off()

#Separate lowland range values and make a scatterplot 
Bound <- extract(RangeRaster, nw_bound, df = TRUE, cellnumbers = TRUE)
colnames(Bound) <- c("Type", "CellID", "Avg")

BoundMatch <- Bound[Bound$CellID %in% RangeMountVec, ]
BoundMatchVec <- BoundMatch[, "CellID"]
RangeLowland <- Bound[!Bound$CellID %in% BoundMatchVec, ]

RangeLowland$Type <- "Lowland"

RangeLowlandVec <- RangeLowland$CellID
RangeLowland <- merge(RangeLowland, LongLatDF)

RangeLowlandScatterplot <- ggplot(RangeLowland, aes(Latitude, Avg)) + geom_point(shape = 16, size = 5, show.legend = FALSE, alpha=0.5, color = "cyan4") + 
  ylab("Lowland Range Size") + xlab("Latitude") + theme_minimal() + 
  theme(axis.title.y = element_text(size=32), axis.title.x = element_text(size=32),  axis.text = element_text(size=20))
RangeLowlandScatterplot

png("Figures/RangeMountainLowlandScatter.png", width = 1000, height = 1000, pointsize = 20)
grid.arrange(RangeMountScatterplot, RangeLowlandScatterplot, ncol=1)
dev.off()

#Plotting lowland and montane range values across latitudes together (like we did with alpha and beta diverstiy in FullScatter.png)
FullRange <- rbind(RangeLowland, RangeMount)

FullRangeScatter <- ggplot(data = FullRange, aes(Latitude, Avg, color=Type)) + 
  geom_point(shape = 16, size = 3, alpha=0.8) + 
  ylab("Median Range Size") + xlab("Latitude") + 
  theme_minimal() + theme(axis.title.y = element_text(size=40), axis.title.x = element_text(size=40),  
                          axis.text = element_text(size=20), legend.text = element_text(size=32), 
                          legend.position = "bottom", legend.title = element_blank()) + 
  scale_color_manual(values = c("cyan4", "goldenrod2")) + geom_smooth(size = 2, show.legend = FALSE)
FullRangeScatter

png("Figures/FullRangeScatter.png", width = 1000, height = 1000, pointsize = 20)
FullRangeScatter
dev.off()

#Plotting range sizes versus beta diversity values 
RangeBeta <- merge(FullBeta, CellRange, by = "CellID")
saveRDS(RangeBeta, file = "Data/RangeBeta.rds")

RangeBetaScatterplot <- ggplot(RangeBeta, aes(Avg, Beta)) + 
  geom_point(shape = 16, size = 5, show.legend = FALSE, alpha=0.5, color = "cyan4") + 
  ylab("β diversity") + ylim(0, 0.5) + xlab("Median Range Size") + theme_minimal() + 
  theme(axis.title.y = element_text(size=32), axis.title.x = element_text(size=32),  axis.text = element_text(size=20))
RangeBetaScatterplot

#Plotting range sizes versus alpha diversity values 
RangeAlpha <- merge(FullAlpha, CellRange, by = "CellID")
saveRDS(RangeAlpha, file = "Data/RangeAlpha.rds")

RangeAlphaScatterplot <- ggplot(RangeAlpha, aes(Avg, Alpha)) + 
  geom_point(shape = 16, size = 5, show.legend = FALSE, alpha=0.5, color = "cyan4") + 
  ylab("α diversity") + xlab("Median Range Size") + theme_minimal() + 
  theme(axis.title.y = element_text(size=32), axis.title.x = element_text(size=32),  axis.text = element_text(size=20))
RangeAlphaScatterplot

png("Figures/RangeAlphaBetaScatter.png", width = 2000, height = 1000, pointsize = 20)
grid.arrange(RangeBetaScatterplot, RangeAlphaScatterplot, ncol=2)
dev.off()

