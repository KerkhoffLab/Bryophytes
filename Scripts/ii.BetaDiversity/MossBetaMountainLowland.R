# MOSS mountain lowland beta scatterplots
# Essentially MountainLowland.Rmd but with MOSSES
# Adapted from MountainLowland.Rmd, DataProcessing.R
# Kathryn Dawdy, June 2021


# load packages ----
require(sp)
require(raster)
require(ggplot2)
require(gridExtra)
require(rgdal)
require(forcats)


# 0.0 Load data ----------------------------------------------------------------
#Load blank raster data + extract cell IDs and create vector for all cells
#Change file for BetaMat depending on if you want to map bryophytes, mosses, liverworts, etc. 
MossLongLatBetaRaster <- readRDS("Data/MossLongLatBetaRaster.rds")
BlankRas <-raster("Data/blank_100km_raster.tif")
CellVec <- c(1:15038)
CellID <- CellRichness$CellID

# Run beginning of MossBiomeDiversity.R for data
MossPresence <- readRDS("Data/MossPresence.rds")
BetaMat <- readRDS("Data/MossBetaMat.rds")
LongLatDF <- readRDS("Data/LongLatDF.rds")  #run BiomeDiversity.R or MountainLowland.Rmd for data

# Add continental and mountainous region outlines
nw_mount <- shapefile("Data/MapOutlines/Mountains/Koeppen-Geiger_biomes.shp")
nw_bound <- shapefile("Data/MapOutlines/Global_bound/Koeppen-Geiger_biomes.shp")

# Generate this data below
BetaMount <- readRDS("Data/MossBetaMount.rds")
BetaLowland <- readRDS("Data/MossBetaLowland.rds")


# 1.0 Make MossLongLatBetaRaster (from DataProcessing.R) -----------------------
# Identify occupied cells that are adjacent to each occuppied cell + convert to vector
neighbor <- function(CellVec) {(adjacent(BlankRas, CellVec, directions=8, 
                                         pairs=FALSE, target=CellID, sorted=TRUE, 
                                         include=FALSE, id=FALSE))}
Neighbors <- lapply(CellVec, neighbor)
names(Neighbors) <- CellVec

bryneighbors <- Neighbors[CellID]
bryneighborvect <- unlist(lapply(bryneighbors, length))

# from Bryophytes.Rmd
Cell8 <- CellID[which(bryneighborvect==8)]
Neighbors8 <-Neighbors[Cell8]
Neighbors8 <- data.frame(Neighbors8)
names(Neighbors8) <- Cell8

Cell7 <- CellID[which(bryneighborvect==7)]
Neighbors7 <- Neighbors[Cell7]
Neighbors7 <- data.frame(Neighbors7)
names(Neighbors7) <- Cell7

BetaMat<-as.matrix(BetaMat)
row.names(BetaMat) <- CellID
names(BetaMat) <- CellID

BetaMat8<- BetaMat[!Cell8, !Cell8, drop=TRUE]
inx8 <- match(as.character(Cell8), rownames(BetaMat8))
BetaMat8 <- BetaMat8[inx8,inx8]

BetaMat7 <- BetaMat[!Cell7, !Cell7, drop=TRUE]
inx7 <- match(as.character(Cell7), rownames(BetaMat7))
BetaMat7 <- BetaMat7[inx7,inx7]

Cell8CH <- as.character(Cell8)
Beta8 <- lapply(Cell8CH, function(x)mean(BetaMat[x, as.character(Neighbors8[,x])]))
names(Beta8) <- Cell8CH

Cell7CH <- as.character(Cell7)
Beta7 <- lapply(Cell7CH, function(x)mean(BetaMat[x, as.character(Neighbors7[,x])]))
names(Beta7) <- Cell7CH

Beta7Vec<-unlist(Beta7)
Beta8Vec<-unlist(Beta8)
BetaVec <- rep(0, 15038)
BetaVec[Cell8]<-Beta8Vec
BetaVec[Cell7]<-Beta7Vec
BetaVec[BetaVec==0]<-NA
BetaVec <- 1-BetaVec

MossLongLatBetaVec <- rep(0, 15038)
MossLongLatBetaVec[Cell8]<-Beta8Vec
MossLongLatBetaVec[Cell7]<-Beta7Vec
MossLongLatBetaVec[MossLongLatBetaVec==0]<-NA
MossLongLatBetaVec <- 1-MossLongLatBetaVec

MossLongLatBetaRaster <- setValues(BlankRas, MossLongLatBetaVec)
MossLongLatBetaPoints<-rasterToPoints(MossLongLatBetaRaster)
MossLongLatBetaDF <- data.frame(MossLongLatBetaPoints)
colnames(MossLongLatBetaDF) <- c("Longitude", "Latitude", "Beta")

coordinates(MossLongLatBetaDF) <- ~Longitude+Latitude 
proj4string(MossLongLatBetaDF) <- CRS("+proj=utm +zone=10") 
MossBetaLongLat <- spTransform(MossLongLatBetaDF, CRS("+proj=longlat")) 
MossLongLatBetaDF <- data.frame(MossBetaLongLat)
MossLongLatBetaDF[c("Longitude", "Latitude", "Beta")]
saveRDS(MossLongLatBetaDF, file = "Data/MossLongLatBetaDF.rds")

MossBetaLongLat <- data.frame(MossBetaLongLat)
colnames(MossBetaLongLat) <- c("Beta", "Longitude", "Latitude")

saveRDS(MossLongLatBetaRaster, file="Data/MossLongLatBetaRaster.rds")



# 2.0 Make mountainous / lowland BETA scatterplots -----------------------------
# 2.1 Beta diversity montane by latitude scatterplot
BetaMount <- raster::extract(MossLongLatBetaRaster, nw_mount, df = TRUE, cellnumbers = TRUE)

colnames(BetaMount) <- c("Type", "CellID", "Beta")
BetaMount$Type <- "Mountain"

BetaMountVec <- BetaMount$CellID
BetaMount <- merge(BetaMount, LongLatDF)
saveRDS(BetaMount, file = "Data/MossBetaMount.rds")

MossBetaMountScatterplot <- ggplot() + geom_point(data = BetaMount, aes(Latitude, Beta), shape = 16, size = 5, show.legend = FALSE, alpha=0.5, color = "goldenrod2") + ylab("Mountainous β diversity") + ylim(0, 0.6) + xlab("Latitude") + theme_minimal() +  theme(axis.title.y = element_text(size=32), axis.title.x = element_text(size=32),  axis.text = element_text(size=20))
MossBetaMountScatterplot

# 2.2 Beta diversity lowland by latitude scatterplot
BetaBound <- raster::extract(LongLatBetaRaster, nw_bound, df = TRUE, cellnumbers = TRUE)
colnames(BetaBound) <- c("Type", "CellID", "Beta")

BetaMatch <- BetaBound[BetaBound$CellID %in% BetaMountVec, ]
BetaMatchVec <- BetaMatch[, "CellID"]
BetaLowland <- BetaBound[!BetaBound$CellID %in% BetaMatchVec, ]

BetaLowland$Type <- "Lowland"

BetaLowlandVec <- BetaLowland$CellID
BetaLowland <- merge(BetaLowland, LongLatDF)
saveRDS(BetaLowland, file = "Data/MossBetaLowland.rds")

BetaLowlandScatterplot <- ggplot() + geom_point(data = BetaLowland, aes(Latitude, Beta), shape = 16, size = 5, show.legend = FALSE, alpha=0.5, color = "cyan4") + ylab("Lowland β diversity") + ylim(0, 0.6) + xlab("Latitude") + theme_minimal() + theme(axis.title.y = element_text(size=32), axis.title.x = element_text(size=32),  axis.text = element_text(size=20))
BetaLowlandScatterplot


# 2.3 Combine beta mountainous and lowland scatterplots
FullBeta <- rbind(BetaMount, BetaLowland)

BetaScatter <- ggplot(data = FullBeta, aes(Latitude, Beta, color=Type)) + 
  geom_point(shape = 16, size = 3, alpha=0.8) + 
  ylab("β diversity") + ylim(0, 0.5) + xlab("Latitude") + 
  theme_minimal() + theme(axis.title.y = element_text(size=40), axis.title.x = element_text(size=40),  
                          axis.text = element_text(size=20), legend.text = element_text(size=32), 
                          legend.position = "bottom", legend.title = element_blank()) + 
  scale_color_manual(values = c("cyan4", "goldenrod2")) + geom_smooth(size = 2, show.legend = FALSE)
BetaScatter

# save figure
png("Figures/MossBetaMountLowScatter.png", width = 1500, height = 1000, pointsize = 20)
BetaScatter
dev.off()
