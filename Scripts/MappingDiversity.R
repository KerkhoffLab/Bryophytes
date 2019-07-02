#MAPPING BETA DIVERSITY
#Load blank raster and cell richness data + extract cell IDs and create vector for all cells
#Change file depending on if you want to map bryophytes, mosses, liverworts, etc. 

BlankRas <-raster("Data/blank_100km_raster.tif")
BetaMat <- readRDS("Data/HLBetaMat.rds")
CellRichness <- readRDS("Data/HLRichness.rds")
CellID <- CellRichness$CellID
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

BetaMat<-as.matrix(BetaMat)
row.names(BetaMat) <- CellID
names(BetaMat) <- CellID



#Make beta diversity matrix for cells with 8 neighbors and cells with 7 neighbors

BetaMat8<- BetaMat[!Cell8, !Cell8, drop=TRUE]
inx8 <- match(as.character(Cell8), rownames(BetaMat8))
BetaMat8 <- BetaMat8[inx8,inx8]

BetaMat7 <- BetaMat[!Cell7, !Cell7, drop=TRUE]
inx7 <- match(as.character(Cell7), rownames(BetaMat7))
BetaMat7 <- BetaMat7[inx7,inx7]



#For each cell, pairwise beta diversity is calculated for that focal cell and each of its 8 (or 7) neighbors, and the mean of those values is found

Cell8CH <- as.character(Cell8)
Beta8 <- lapply(Cell8CH, function(x)mean(BetaMat[x, as.character(Neighbors8[,x])]))
names(Beta8) <- Cell8CH

Cell7CH <- as.character(Cell7)
Beta7 <- lapply(Cell7CH, function(x)mean(BetaMat[x, as.character(Neighbors7[,x])]))
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

BetaVec[BetaVec<0.5]<-NA
BetaRaster <- setValues(BlankRas, BetaVec)

theme_set(theme_void())
gplot(BetaRaster, maxpixels=15038) + geom_raster(aes(fill = value))+ scale_fill_gradientn(colours=cols, na.value="transparent") +
  coord_equal() 
