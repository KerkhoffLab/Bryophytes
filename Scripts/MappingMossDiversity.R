#MAPPING MOSS BETA DIVERSITY
#Adapted from MappingLiverwortDiversity.R
#Kathryn Dawdy, Summer 2020

#Load Packages
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

# 0.0 Run DataProcessing.R and MossDiversity.R to generate necessary data -----------------------------------------------------------------

## Load blank raster and cell richness data 
##Change file depending on if you want to map bryophytes, mosses, liverworts, etc. 
BlankRas <-raster("Data/blank_100km_raster.tif")
BetaMat <- readRDS("Data/MossBetaMat.rds")
CellRichness <- readRDS("Data/MossRichness.rds")


# 1.0 Extract cell IDs and create vector for all cells -----------------------------------------------------------------------------------------
CellID <- CellRichness$CellID
CellVec <- c(1:15038)


# 1.1 Identify occupied cells that are adjacent to each occuppied cell + convert to vector
neighbor <- function(CellVec) {(adjacent(BlankRas, CellVec, directions=8, pairs=FALSE, target=CellID, sorted=TRUE, include=FALSE, id=FALSE))}
Neighbors <- lapply(CellVec, neighbor)
names(Neighbors) <- CellVec

bryneighbors <- Neighbors[CellID]
bryneighborvect <- unlist(lapply(bryneighbors, length))


# 1.2 Separate out occuppied cells with 8 and 7 occuppied neighbors
Cell8 <- CellID[which(bryneighborvect==8)]
Neighbors8 <-Neighbors[Cell8]
Neighbors8 <- data.frame(Neighbors8)
names(Neighbors8) <- Cell8

Cell7 <- CellID[which(bryneighborvect==7)]
Neighbors7 <- Neighbors[Cell7]
Neighbors7 <- data.frame(Neighbors7)
names(Neighbors7) <- Cell7


# 1.3 Make beta diversity matrix for all cells
BetaMat<-as.matrix(BetaMat)
row.names(BetaMat) <- CellID
names(BetaMat) <- CellID


# 1.4 Make beta diversity matrix for cells with 8 neighbors and cells with 7 neighbors
BetaMat8<- BetaMat[!Cell8, !Cell8, drop=TRUE]
inx8 <- match(as.character(Cell8), rownames(BetaMat8))
BetaMat8 <- BetaMat8[inx8,inx8]

BetaMat7 <- BetaMat[!Cell7, !Cell7, drop=TRUE]
inx7 <- match(as.character(Cell7), rownames(BetaMat7))
BetaMat7 <- BetaMat7[inx7,inx7]


# 1.5 For each cell, pairwise beta diversity is calculated for that focal cell and each of its 8 (or 7) neighbors, and the mean of those values is found
Cell8CH <- as.character(Cell8)
Beta8 <- lapply(Cell8CH, function(x)mean(BetaMat[x, as.character(Neighbors8[,x])]))
names(Beta8) <- Cell8CH

Cell7CH <- as.character(Cell7)
Beta7 <- lapply(Cell7CH, function(x)mean(BetaMat[x, as.character(Neighbors7[,x])]))
names(Beta7) <- Cell7CH


# 1.6 Plot mean pairwise beta diversity by Cell ID and convert to dissimilarity using 1-x where x is Jaccard similarity
Beta7Vec<-unlist(Beta7)
Beta8Vec<-unlist(Beta8)

BetaVec <- rep(0, 15038)

BetaVec[Cell8]<-Beta8Vec
BetaVec[Cell7]<-Beta7Vec

BetaVec[BetaVec==0]<-NA
BetaVec <- 1-BetaVec

plot(BetaVec, ylab = "Mean Pairwise β-Diversity", xlab = "Cell ID")


# 2.0 Map moss beta diversity -------------------------------------------------------------------------------------------------------------

# 2.1 Create colorscheme
cols <- (wes_palette("Zissou1", 500, type = "continuous"))


# 2.2 Add continental and mountainous outlines
nw_mount <- shapefile("Data/MapOutlines/Mountains/Koeppen-Geiger_biomes.shp")
nw_bound <- shapefile("Data/MapOutlines/Global_bound/Koeppen-Geiger_biomes.shp")

nw_mount_sf <- st_as_sf(nw_mount)
nw_bound_sf <- st_as_sf(nw_bound)


# 2.3 Mapping method from Bryophytes.Rmd
BetaRaster <- setValues(BlankRas, BetaVec)
BetaPoints<-rasterToPoints(BetaRaster)
BetaDF <- data.frame(BetaPoints)
colnames(BetaDF) <- c("Longitude", "Latitude", "Beta")

theme_set(theme_void())
RawBetaMap <- ggplot() + geom_tile(data=BetaDF, aes(x=Longitude, y=Latitude, fill=Beta)) + 
  scale_fill_gradientn(name="β-diversity", colours=cols, na.value="transparent", limits = c(0,1)) + 
  coord_equal()
RawBetaMap


# 2.4 Map outlier beta values (>0.5)
OutlierBetaVec <- rep(0, 15038)
OutlierBetaVec[Cell8]<-Beta8Vec
OutlierBetaVec[Cell7]<-Beta7Vec
OutlierBetaVec[OutlierBetaVec==0]<-NA
OutlierBetaVec[OutlierBetaVec>0.5]<-NA

OutlierBetaRaster <- setValues(BlankRas, OutlierBetaVec)
OutlierBetaPoints<-rasterToPoints(OutlierBetaRaster)
OutlierBetaDF <- data.frame(OutlierBetaPoints)
colnames(OutlierBetaDF) <- c("Longitude", "Latitude", "Beta")

theme_set(theme_void())
OutlierBetaMap <- gplot(OutlierBetaRaster, maxpixels=15038) +  geom_tile(aes(fill = value)) + 
  scale_fill_gradientn(colours="black", na.value="transparent", limits = c(0,1)) + theme(legend.position = "none") + 
  coord_equal() 
OutlierBetaMap


#Map beta diversity with values over 0.5 shown in dark grey (how many values over 0.5?)
source("Functions/gplot_data.R")
gplotB<- gplot_data(BetaRaster)
gplotOutlier<- gplot_data(OutlierBetaRaster)

MossBetaMap <- ggplot() +
  geom_tile(data = dplyr::filter(gplotOutlier, !is.na(value)), 
            aes(x = x, y = y), fill = "gray25") +
  geom_tile(data = gplotB, 
            aes(x = x, y = y, fill = value)) +
  scale_fill_gradientn(name = "β diversity", colours=cols, na.value="transparent", limits = c(0,0.5)) +
  coord_quickmap() + geom_sf(data = nw_bound_sf, size = 0.5, fill=NA) + 
  geom_sf(data = nw_mount_sf, size = 0.5, alpha=0.1) + theme_void() +
  theme(legend.text=element_text(size=20), legend.title=element_text(size=32), axis.title = element_blank())
MossBetaMap

png("Figures/MossBetaMap.png", width = 1000, height = 1000, pointsize = 30)
MossBetaMap
dev.off()