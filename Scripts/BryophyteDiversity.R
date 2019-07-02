#Load data
BryophytePresence <- readRDS("Data/BryophytePresence.rds")

#Create occurrence by cell matrix by reshaping dataframe, then convert to presence-absence matrix
require(reshape2)
SpeciesCellID <- BryophytePresence[,c(1,4)]
melted <- melt(SpeciesCellID, id=c("Species", "CellID"), na.rm = TRUE)

SpeciesCellMatrix <- acast(melted, CellID~Species, margins=FALSE)
SpeciesCellMatrix[SpeciesCellMatrix > 0] <- 1

#Using betadiver to compute B-diversity using Sorensen dissimilarity
#betadiver(help = TRUE) gives you indices
betamat <- betadiver(SpeciesCellMatrix, method = "sor", order = FALSE, help = FALSE)
saveRDS(betamat, file="Data/BetaMat.rds")


#Separate out occuppied cells with 8 occuppied neighbors

bryneighborvect <- unlist(lapply(bryneighbors, length))
cell8 <- CellID[which(bryneighborvect==8)]
cell7 <- CellID[which(bryneighborvect==7)]
BetaMat8<-as.matrix(BetaMat)
BetaMat8[!cell8, !cell8, drop=TRUE]
BetaMat7<-as.matrix(BetaMat)
BetaMat7[!cell7, !cell7, drop=TRUE]
inx8 <- match(as.character(cell8), rownames(BetaMat8))
inx7 <- match(as.character(cell7), rownames(BetaMat7))
BetaMat8 <- BetaMat8[inx8,inx8]
BetaMat7 <- BetaMat7[inx7,inx7]


#Trying to extract the vector of neighbors for each cell with 8 neighbors

readRDS(BetaMat, file = "Data/BetaMat.rds")

bry8neighbors <-neighbors[cell8]
bry7neighbors <- neighbors[cell7]
bry8neighbors <- data.frame(bry8neighbors)
names(bry8neighbors)[1:3788] <- cell8
bry7neighbors <- data.frame(bry7neighbors)
names(bry7neighbors)[1:318] <- cell7
row.names(BetaMat8) <- cell8
names(BetaMat8)[1:3788] <- cell8
row.names(BetaMat7) <- cell7
names(BetaMat7)[1:318] <- cell7
full.BetaMat <- as.matrix(BetaMat)
row.names(full.BetaMat) <- CellID
names(full.BetaMat)[1:4761] <- CellID
#neighborlist <- as.list(cell8)



#Beta diversity for each cell and its 8neighbors

cell8_ch <- as.character(cell8)
cell7_ch <- as.character(cell7)
ultimatebeta8 <- lapply(cell8_ch, function(x)mean(full.BetaMat[x, as.character(bry8neighbors[,x])]))
names(ultimatebeta8) <- cell8_ch
ultimatebeta7 <- lapply(cell7_ch, function(x)mean(full.BetaMat[x, as.character(bry7neighbors[,x])]))
names(ultimatebeta7) <- cell7_ch
plot(cell8_ch, ultimatebeta8)
plot(cell7_ch, ultimatebeta7)
points(cell7_ch, ultimatebeta7)
#check on cell 767. it has crazy LOW value



#Plot richness

betavector7<-unlist(ultimatebeta7)
betavector8<-unlist(ultimatebeta8)
cellmatrix <- as.matrix(cellvector)
x <- rep(0,15038)
x
x[cell8]<-betavector8
x[cell7]<-betavector7
x[x==0]<-NA
plot(cellvector, x)
#MAP
require(wesanderson)
cols <- rev(wes_palette("Zissou1", 500, type = "continuous"))
require(rasterVis)
#different way to plot, easier to mess with the breaks
x[x<0.5]<-NA
cellmatrix<-cbind(cellmatrix, x)
cellmatrix<-cellmatrix[,-1]
betavector<-as.vector(cellmatrix)
BetaRaster <- setValues(BlankRas, betavector)
spplot(BetaRaster,  maxpixels=15038, col.regions=cols)
gplot(BetaRaster,  maxpixels=15038)
library(ggplot2)
theme_set(theme_void())
gplot(BetaRaster) + geom_raster(aes(fill = value))+ scale_fill_gradientn(colours=cols, na.value="white") +
  coord_equal() 
#plot(BetaRaster, col=rev(cols), axes = F)

