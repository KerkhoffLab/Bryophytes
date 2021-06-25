# 02 Moss Beta Diversity Analysis for MS
# Compiled by Hailey Napier
# June 2021

# 0.0 Load Packages and Data ----
## 0.1 Load packages ----
install.packages("reshape2")
install.packages("vegan")
library(reshape2)
library(vegan)

## 0.2 Load data ----


# 1.0 Create beta diversity matrix for mosses ----
## 1.1 Make presence/absence matrix ----
SpeciesCellID <- MossPresence[,c(1,4)]
melted <- melt(SpeciesCellID, id=c("Species", "CellID"), na.rm = TRUE)

MossCellMatrix <- acast(melted, CellID~Species, margins=FALSE)
MossCellMatrix[MossCellMatrix > 0] <- 1

## 1.2 Use betadiver to compute B-diversity using Sorensen dissimilarity ----
### betadiver(help = TRUE) gives you indices
MossBetaMat <- betadiver(MossCellMatrix, method = "sor", order = FALSE, help = FALSE)

CellID <- MossRichness$CellID
CellVec <- c(1:15038)

MossBetaMat<-as.matrix(MossBetaMat)
row.names(MossBetaMat) <- CellID
names(MossBetaMat) <- CellID

saveRDS(MossBetaMat, file="Data/MossBetaMat.rds")


# 2.0 Calculate pairwise beta diversity for cells with 7 or 8 neighbors ----
## 2.1 Identify occupied cells that are adjacent to each occupied cell and convert to vector ----
neighbor <- function(CellVec) {(adjacent(BlankRas, CellVec, directions=8, pairs=FALSE, target=CellID, sorted=TRUE, include=FALSE, id=FALSE))}
Neighbors <- lapply(CellVec, neighbor)
names(Neighbors) <- CellVec

mossneighbors <- Neighbors[CellID]
mossneighborvect <- unlist(lapply(mossneighbors, length))

## 2.2 Extract cells that have 7 or 8 neighbors ----
Cell8 <- CellID[which(mossneighborvect==8)]
Neighbors8 <-Neighbors[Cell8]
Neighbors8 <- data.frame(Neighbors8)
names(Neighbors8) <- Cell8

Cell7 <- CellID[which(mossneighborvect==7)]
Neighbors7 <- Neighbors[Cell7]
Neighbors7 <- data.frame(Neighbors7)
names(Neighbors7) <- Cell7

## 2.3 Create beta diversity matrices for cells with 7 and 8 neighbors, respectively ----
MossBetaMat8<- MossBetaMat[!Cell8, !Cell8, drop=TRUE]
inx8 <- match(as.character(Cell8), rownames(MossBetaMat8))
MossBetaMat8 <- MossBetaMat8[inx8,inx8]

MossBetaMat7 <- MossBetaMat[!Cell7, !Cell7, drop=TRUE]
inx7 <- match(as.character(Cell7), rownames(MossBetaMat7))
MossBetaMat7 <- MossBetaMat7[inx7,inx7]

## 2.4 Extract pairwise beta diversity for each cell and each of its 7 or 8 neighbors. Calculate the mean of these values. 
### Method taken from McFadden et al. 2019
Cell8CH <- as.character(Cell8)
Beta8 <- lapply(Cell8CH, function(x)mean(MossBetaMat[x, as.character(Neighbors8[,x])]))
names(Beta8) <- Cell8CH

Cell7CH <- as.character(Cell7)
Beta7 <- lapply(Cell7CH, function(x)mean(MossBetaMat[x, as.character(Neighbors7[,x])]))
names(Beta7) <- Cell7CH

## 2.5 Plot mean beta pairwise beta diversity by cell ID for cells with 7 or 8 neighbors ----
Beta7Vec<-unlist(Beta7)
Beta8Vec<-unlist(Beta8)
BetaVec <- rep(0, 15038)
BetaVec[Cell8]<-Beta8Vec
BetaVec[Cell7]<-Beta7Vec
BetaVec[BetaVec==0]<-NA
BetaVec <- 1-BetaVec

plot(BetaVec, ylab = "Mean Pairwise β-Diversity", xlab = "Cell ID")

#####STOPPED HERE FOR LUNCH#####
LongLatMossBetaVec <- rep(0, 15038)
LongLatMossBetaVec[Cell8]<-Beta8Vec
LongLatMossBetaVec[Cell7]<-Beta7Vec
LongLatMossBetaVec[LongLatMossBetaVec==0]<-NA
LongLatMossBetaVec <- 1-LongLatMossBetaVec

LongLatMossBetaRaster <- setValues(BlankRas, LongLatMossBetaVec)
LongLatMossBetaPoints<-rasterToPoints(LongLatMossBetaRaster)
LongLatMossBetaDF <- data.frame(LongLatMossBetaPoints)
colnames(LongLatMossBetaDF) <- c("Longitude", "Latitude", "Beta")

coordinates(LongLatMossBetaDF) <- ~Longitude+Latitude 
proj4string(LongLatMossBetaDF) <- CRS("+proj=utm +zone=10") 
MossBetaLongLat <- spTransform(LongLatMossBetaDF, CRS("+proj=longlat")) 
LongLatMossBetaDF <- data.frame(MossBetaLongLat)
LongLatMossBetaDF[c("Longitude", "Latitude", "Beta")]
saveRDS(LongLatMossBetaDF, file = "Data/LongLatMossBetaDF.rds")

MossBetaLongLat <- data.frame(MossBetaLongLat)
colnames(MossBetaLongLat) <- c("Beta", "Longitude", "Latitude")


saveRDS(MossBetaMat, file="Data/MossBetaMat.rds")
saveRDS(bryneighbors, file = "Data/bryneighbors.rds")
saveRDS(bryneighborvect, file="Data/bryneighborvect.rds")
saveRDS(CellID, file="Data/CellID.rds")

saveRDS(LongLatMossBetaRaster, file="Data/LongLatMossBetaRaster.rds")

