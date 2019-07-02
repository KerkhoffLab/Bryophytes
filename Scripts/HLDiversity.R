#Load data
BryophytePresence <- readRDS("Data/BryophytePresence.rds")


#Subset hornwort and liverwort data
HornwortPresence <- subset(BryophytePresence, BryophytePresence$Group=="Hornworts")
LiverwortPresence <- subset(BryophytePresence, BryophytePresence$Group=="Liverworts")
HLPresence <- subset(BryophytePresence, BryophytePresence$Group=="Hornworts"|BryophytePresence$Group=="Liverworts")

#Create occurrence by cell matrix by reshaping dataframe, then convert to presence-absence matrix
require(reshape2)
HLRichness <- tally(group_by(HLPresence, CellID))
SpeciesCellID <- HLPresence[,c(1,4)]
melted <- melt(SpeciesCellID, id=c("Species", "CellID"), na.rm = TRUE)

cellID <- HLRichness$CellID
cellvector <- c(1:15038)
neighbor <- function(cellvector) {(adjacent(BlankRas, cellvector, directions=8, pairs=FALSE, target=cellID, sorted=TRUE, include=FALSE, id=FALSE))}
neighbors <- lapply(cellvector, neighbor)
names(neighbors) <- cellvector
bryneighbors <- neighbors[cellID]

HLCellMatrix <- acast(melted, CellID~Species, margins=FALSE)
HLCellMatrix[HLCellMatrix > 0] <- 1

#Using betadiver to compute B-diversity using Sorensen dissimilarity
#betadiver(help = TRUE) gives you indices
HLBetaMat <- betadiver(HLCellMatrix, method = "sor", order = FALSE, help = FALSE)

#Separate out occuppied cells with 8 occuppied neighbors

bryneighborvect <- unlist(lapply(bryneighbors, length))
cell8 <- cellID[which(bryneighborvect==8)]
cell7 <- cellID[which(bryneighborvect==7)]
HLBetaMat8<-as.matrix(HLBetaMat)
HLBetaMat8[!cell8, !cell8, drop=TRUE]
HLBetaMat7<-as.matrix(HLBetaMat)
HLBetaMat7[!cell7, !cell7, drop=TRUE]
inx8 <- match(as.character(cell8), rownames(HLBetaMat8))
inx7 <- match(as.character(cell7), rownames(HLBetaMat7))
HLBetaMat8 <-HLBetaMat8[inx8,inx8]
HLBetaMat7 <-HLBetaMat7[inx7,inx7]


#Trying to extract the vector of neighbors for each cell with 8 neighbors


bry8neighbors <-neighbors[cell8]
bry7neighbors <- neighbors[cell7]
bry8neighbors <- data.frame(bry8neighbors)
names(bry8neighbors)[1:3776] <- cell8
bry7neighbors <- data.frame(bry7neighbors)
names(bry7neighbors)[1:310] <- cell7
row.names(HLBetaMat8) <- cell8
names(HLBetaMat8)[1:3776] <- cell8
row.names(HLBetaMat7) <- cell7
names(HLBetaMat7)[1:310] <- cell7
full.HLBetaMat <- as.matrix(HLBetaMat)
row.names(full.HLBetaMat) <- cellID
names(full.HLBetaMat)[1:4737] <- cellID



#Beta diversity for each cell and its 8neighbors

cell8_ch <- as.character(cell8)
cell7_ch <- as.character(cell7)
ultimatebeta8 <- lapply(cell8_ch, function(x)mean(full.HLBetaMat[x, as.character(bry8neighbors[,x])]))
names(ultimatebeta8) <- cell8_ch
ultimatebeta7 <- lapply(cell7_ch, function(x)mean(full.BetaMat[x, as.character(bry7neighbors[,x])]))
names(ultimatebeta7) <- cell7_ch
plot(cell8_ch, ultimatebeta8)
plot(cell7_ch, ultimatebeta7)
points(cell7_ch, ultimatebeta7)



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
#spplot(BetaRaster,  maxpixels=15038, col.regions=cols)
gplot(BetaRaster,  maxpixels=15038)
library(ggplot2)
theme_set(theme_void())
gplot(BetaRaster) + geom_raster(aes(fill = value))+ scale_fill_gradientn(colours=cols, na.value="white") +
  coord_equal() 
