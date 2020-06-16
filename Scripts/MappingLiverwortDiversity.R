#MAPPING LIVERWORT BETA DIVERSITY
#Adapted from MappingDiversity.R
#Kathryn Dawdy, Summer 2020

#Load blank raster and cell richness data + extract cell IDs and create vector for all cells
#Change file depending on if you want to map bryophytes, mosses, liverworts, etc. 

BlankRas <-raster("Data/blank_100km_raster.tif")
LiverwortBetaMat <- readRDS("Data/LiverwortBetaMat.rds")
LiverwortRichness <- readRDS("Data/LiverwortRichness.rds")
CellID <- LiverwortRichness$CellID
CellVec <- c(1:15038)

#Identify occupied cells that are adjacent to each occuppied cell + convert to vector

neighbor <- function(CellVec) {(adjacent(BlankRas, CellVec, directions=8, pairs=FALSE, target=CellID, sorted=TRUE, include=FALSE, id=FALSE))}
Neighbors <- lapply(CellVec, neighbor)
names(Neighbors) <- CellVec

bryneighbors <- Neighbors[CellID]
bryneighborvect <- unlist(lapply(bryneighbors, length))



#Separate out occuppied cells with 8 and 7 occuppied neighbors

Cell8 <- CellID[which(bryneighborvect==8)]
Neighbors8 <-Neighbors[Cell8]
Neighbors8 <- data.frame(Neighbors8)
names(Neighbors8) <- Cell8

Cell7 <- CellID[which(bryneighborvect==7)]
Neighbors7 <- Neighbors[Cell7]
Neighbors7 <- data.frame(Neighbors7)
names(Neighbors7) <- Cell7



#Make beta diversity matrix for all cells

LiverwortBetaMat<-as.matrix(LiverwortBetaMat)
row.names(LiverwortBetaMat) <- CellID
names(LiverwortBetaMat) <- CellID



#Make beta diversity matrix for cells with 8 neighbors and cells with 7 neighbors

LiverwortBetaMat8<- LiverwortBetaMat[!Cell8, !Cell8, drop=TRUE]
inx8 <- match(as.character(Cell8), rownames(LiverwortBetaMat8))
LiverwortBetaMat8 <- LiverwortBetaMat8[inx8,inx8]

LiverwortBetaMat7 <- LiverwortBetaMat[!Cell7, !Cell7, drop=TRUE]
inx7 <- match(as.character(Cell7), rownames(LiverwortBetaMat7))
LiverwortBetaMat7 <- LiverwortBetaMat7[inx7,inx7]



#For each cell, pairwise beta diversity is calculated for that focal cell and each of its 8 (or 7) neighbors, and the mean of those values is found

Cell8CH <- as.character(Cell8)
Beta8 <- lapply(Cell8CH, function(x)mean(LiverwortBetaMat[x, as.character(Neighbors8[,x])]))
names(Beta8) <- Cell8CH

Cell7CH <- as.character(Cell7)
Beta7 <- lapply(Cell7CH, function(x)mean(LiverwortBetaMat[x, as.character(Neighbors7[,x])]))
names(Beta7) <- Cell7CH



#Plot mean pairwise beta diversity by Cell ID

Beta7Vec<-unlist(Beta7)
Beta8Vec<-unlist(Beta8)

BetaVec <- rep(0, 15038)

BetaVec[Cell8]<-Beta8Vec
BetaVec[Cell7]<-Beta7Vec

BetaVec[BetaVec==0]<-NA


plot(BetaVec, ylab = "Mean Pairwise β-Diversity", xlab = "Cell ID")


#Map beta diversity
require(wesanderson)
require(ggplot2)

cols <- rev(wes_palette("Zissou1", 500, type = "continuous"))

#BetaVec[BetaVec<0.5]<-NA
BetaRaster <- setValues(BlankRas, BetaVec)

theme_set(theme_void())

#Mapping method from MappingDiversity.R
RawLivBetaMapRas <- gplot(BetaRaster, maxpixels=15038) + geom_raster(aes(fill = value))+ scale_fill_gradientn(name="β-diversity", colours=cols, na.value="transparent", limits=c(0,1)) +
  coord_equal() 
RawLivBetaMapRas

#Mapping method from Bryophytes.Rmd
BetaRaster <- setValues(BlankRas, BetaVec)
BetaPoints<-rasterToPoints(BetaRaster)
BetaDF <- data.frame(BetaPoints)
colnames(BetaDF) <- c("Longitude", "Latitude", "Beta")

theme_set(theme_void())
RawLivBetaMapTile <- ggplot() + geom_tile(data=BetaDF, aes(x=Longitude, y=Latitude, fill=Beta)) + 
  scale_fill_gradientn(name="β-diversity", colours=cols, na.value="transparent", limits = c(0,1)) + 
  coord_equal()
RawLivBetaMapTile

#Add continental and mountainous outlines
nw_mount <- shapefile("Data/MapOutlines/Mountains/Koeppen-Geiger_biomes.shp")
nw_bound <- shapefile("Data/MapOutlines/Global_bound/Koeppen-Geiger_biomes.shp")

nw_mount_sf <- st_as_sf(nw_mount)
nw_bound_sf <- st_as_sf(nw_bound)

