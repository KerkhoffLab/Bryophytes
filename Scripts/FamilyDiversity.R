#Bryophyte Family Diversity
#Hailey Napier
#June 2020

# Load Packages
require(maps) 
require(dplyr)
require(maptools)
require(raster)
require(sp)
require(rgdal)
require(mapdata)
require(mapproj)

require(wesanderson)
require(ggplot2)
require(rasterVis)

require(vegan)
require(gridExtra)
require(sf)
require(rgdal)

require(reshape2)


# 0.0 Run DataProcessing.R to generate necessary data---------------------------------------------------------
BryophytePresence <- readRDS("Data/BryophytePresence.rds")

nw_mount <- shapefile("Data/MapOutlines/Mountains/Koeppen-Geiger_biomes.shp")
nw_bound <- shapefile("Data/MapOutlines/Global_bound/Koeppen-Geiger_biomes.shp")

nw_mount_sf <- st_as_sf(nw_mount)
nw_bound_sf <- st_as_sf(nw_bound)


# 1.0 Plot all family richness (how many families are in each cell)-------------------------------------------

# 1.1 Make family name dataframe and vector for number of families
Families <- data.frame(table(BryophytePresence$Family))
names(Families)[1] <- "Family"
names(Families)[2] <- "Richness"
Families <- left_join(Families, ByGroup, by = "Family")

FamilyNames <- Families$Family
NumberFamilies <- length(FamilyNames)


# 1.2 Find family richness
#this is probably not the fastest way to do this, but it works
FamPresence <- data.frame(CellID)
FamPresence$Richness <- NA

#loop takes about 2 minutes to run
for(i in 1:length(CellID)){
  cell <- CellID[i]
  famcell <- subset(BryophytePresence, CellID == cell)
  r <- length(unique(famcell$Family))
  FamPresence$Richness[CellID == cell] <- r
}

FamRichness <- numeric(15038)
FamRichness[FamPresence$CellID] <- FamPresence$Richness
FamRichness[which(FamRichness==0)]=NA


# 1.3 Map family richness (one plot for all families)
FamRichnessRaster <- setValues(BlankRas, FamRichness)
FamDF <- rasterToPoints(FamRichnessRaster)
FamDF <- data.frame(FamDF)
colnames(FamDF) <- c("Longitude", "Latitude", "Alpha")

FamRichnessMap <- ggplot() + geom_tile(data=FamDF, aes(x=Longitude, y=Latitude, fill=Alpha)) +   
  scale_fill_gradientn(name="α diversity", colours=cols, na.value="transparent") +
  coord_equal() +
  geom_sf(data = nw_bound_sf, size = 0.5, fill=NA) + 
  geom_sf(data = nw_mount_sf, size = 0.5, alpha=0.1) + theme_void() + 
  theme(legend.text=element_text(size=20), legend.title=element_text(size=32))
FamRichnessMap

png("Figures/FamRichnessMap.png", width= 1000, height = 1000, pointsize = 30)
FamRichnessMap
dev.off()



# 2.0 Plot species richness for each family (separate plot for each family)-----------------------------------

# 2.1 Loop through family names and subset BryophtePresence for each family, store them in a list
FamList <- list()
for(i in 1:NumberFamilies){
  fam <- FamilyNames[i]
  FamList[[i]] <- subset(BryophytePresence, Family == fam)
}


#2.2 Loop through families and tally richness for each family, store in a list
FamRichList <- list()
FamPresList <- list()
for(i in 1:NumberFamilies){
  FamPresList[[i]] <- tally(group_by(FamList[[i]], CellID))
  names(FamPresList[[i]])[2] <- "Richness"
  FamRichList[[i]] <- numeric(15038)
  FamRichList[[i]][FamPresList[[i]]$CellID] <- FamPresList[[i]]$Richness
  FamRichList[[i]][which(FamRichList[[i]]==0)] = NA
}

# 2.3 Make a new folder for richness maps w/in each family
setwd("./Figures")
dir.create("./RichnessByFamilyMaps")


# 2.4 Loop through families and map all species in each family (one map for each family)

#make sure to set working directory to default

for(i in 1:NumberFamilies){
  TempFamRichnessRaster <- setValues(BlankRas, FamRichList[[i]])
  TempFamDF <- rasterToPoints(TempFamRichnessRaster)
  TempFamDF <- data.frame(TempFamDF)
  colnames(TempFamDF) <- c("Longitude", "Latitude", "Alpha")
  
  Map <- ggplot() + geom_tile(data=TempFamDF, aes(x=Longitude, y=Latitude, fill=Alpha)) +   
    scale_fill_gradientn(name="α diversity", colours=cols, na.value="transparent") +
    coord_equal() +
    geom_sf(data = nw_bound_sf, size = 0.5, fill=NA) + 
    geom_sf(data = nw_mount_sf, size = 0.5, alpha=0.1) + theme_void() + 
    theme(legend.text=element_text(size=20), legend.title=element_text(size=32))
  
  filename <- paste("./Figures/RichnessByFamilyMaps/RichMap_", FamilyNames[i], ".png", sep = "")
  png(filename, width= 1000, height = 1000, pointsize = 30)
  print({Map})
  dev.off()
}


# 3.0 Looking at the numbers--------------------------------------------------------------------------------
FamNSpecies <- data.frame(tally(group_by(BrySpecies, Family)))
names(FamNSpecies)[2] <- "NumberSpecies"
meanSpecies <- mean(FamNSpecies$NumberSpecies)
medianSpecies <- median(FamNSpecies$NumberSpecies)

b <- ggplot(data = FamNSpecies, aes(reorder(Family, -NumberSpecies), NumberSpecies)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette="Blues") +
  theme(axis.text.x=element_text(angle=90, hjust=1))
png("./Figures/FamSpeciesBarChart.png", width= 1000, height = 1000, pointsize = 30)
print({b})
dev.off()

h <- ggplot(data = FamNSpecies, aes(NumberSpecies)) +
  geom_histogram(binwidth = 10) +
  geom_vline(aes(xintercept = mean(NumberSpecies)), color = "blue", linetype = "dashed") + 
  geom_vline(aes(xintercept = median(NumberSpecies)), color = "red", linetype = "dashed")
png("./Figures/FamSpeciesHist.png", width= 1000, height = 1000, pointsize = 30)
print({h})
dev.off()


# 3.1 Map families with more species than mean
#All families on one map (richness represents number of families in each cell)
FamMostSpecies <- subset(FamNSpecies, NumberSpecies > meanSpecies)
FamMostSpeciesList <- FamMostSpecies$Family
length(FamMostSpeciesList)
FamMostSpeciesList

MostSpecioseFamPres <- subset(BryophytePresence, Family == FamMostSpeciesList[1])
FamByGroup <- subset(ByGroup, Family == FamMostSpeciesList[1])
for(i in 2:length(FamMostSpeciesList)){
  temp1 <- subset(BryophytePresence, Family == FamMostSpeciesList[i])
  MostSpecioseFamPres <- bind(MostSpecioseFamPres, temp1)
  temp2 <- subset(ByGroup, Family == FamMostSpeciesList[i])
  FamByGroup <- bind(FamByGroup, temp2)
}

SpecioseFam <- data.frame(table(MostSpecioseFamPres$Family))
names(SpecioseFam)[1] <- "Family"
names(SpecioseFam)[2] <- "Richness"
SpecioseFam <- left_join(SpecioseFam, FamByGroup, by = "Family")

MostSpeciesPresence <- data.frame(CellID)
MostSpeciesPresence$Richness <- NA

for(i in 1:length(CellID)){
  cell <- CellID[i]
  famcell <- subset(MostSpecioseFamPres, CellID == cell)
  r <- length(unique(famcell$Family))
  MostSpeciesPresence$Richness[CellID == cell] <- r
}

MostSpeciesRichness <- numeric(15038)
MostSpeciesRichness[MostSpeciesPresence$CellID] <- MostSpeciesPresence$Richness
MostSpeciesRichness[which(MostSpeciesRichness==0)]=NA

MostSpeciesRichnessRaster <- setValues(BlankRas, MostSpeciesRichness)
MostSpeciesDF <- rasterToPoints(MostSpeciesRichnessRaster)
MostSpeciesDF <- data.frame(MostSpeciesDF)
colnames(MostSpeciesDF) <- c("Longitude", "Latitude", "Alpha")

MostSpeciesFamRichnessMap <- ggplot() + geom_tile(data=MostSpeciesDF, aes(x=Longitude, y=Latitude, fill=Alpha)) +   
  scale_fill_gradientn(name="α diversity", colours=cols, na.value="transparent") +
  coord_equal() +
  geom_sf(data = nw_bound_sf, size = 0.5, fill=NA) + 
  geom_sf(data = nw_mount_sf, size = 0.5, alpha=0.1) + theme_void() + 
  theme(legend.text=element_text(size=20), legend.title=element_text(size=32))
MostSpeciesFamRichnessMap

png("Figures/MostSpeciesFamRichnessMap.png", width= 1000, height = 1000, pointsize = 30)
MostSpeciesFamRichnessMap
dev.off()


# 4.0 Beta diversity--------------------------------------------------------------------------------------------

# 4.1 Make beta diversity matrix
FamBryPres <- BryophytePresence
FamBryPres$Species <- NULL
nrow(FamBryPres)

#Create occurrence by cell matrix by reshaping dataframe, then convert to presence-absence matrix
FamilyCellID <- FamBryPres[,c(4,3)]
melted <- melt(FamilyCellID, id=c("Family", "CellID"), na.rm = TRUE)

FamilyCellMatrix <- acast(melted, CellID~Family, margins=FALSE)
FamilyCellMatrix[FamilyCellMatrix > 0] <- 1

#Using betadiver to compute B-diversity using Jaccard similarity
#betadiver(help = TRUE) gives you indices
fam_betamat <- betadiver(FamilyCellMatrix, method = "j", order = FALSE, help = FALSE)

#Save species-cell matrix and beta diversity matrix
saveRDS(FamilyCellMatrix, file="Data/FamilyCellMatrix.rds")
saveRDS(fam_betamat, file="Data/FamBetaMat.rds")


# 4.2 Make/load other necessary data
#adapted from Bryophytes.rmd
CellVec <- c(1:15038)
FamBetaMat <- readRDS("Data/FamBetaMat.rds")


# 4.3 Identify occupied cells that are adjacent to each occuppied cell + convert to vector
neighbor <- function(CellVec) {(adjacent(BlankRas, CellVec, directions=8, pairs=FALSE, target=CellID, sorted=TRUE, include=FALSE, id=FALSE))}
Neighbors <- lapply(CellVec, neighbor)
names(Neighbors) <- CellVec

bryneighbors <- Neighbors[CellID]
bryneighborvect <- unlist(lapply(bryneighbors, length))


# 4.4 Separate out occuppied cells with 8 and 7 occuppied neighbors
Cell8 <- CellID[which(bryneighborvect==8)]
Neighbors8 <-Neighbors[Cell8]
Neighbors8 <- data.frame(Neighbors8)
names(Neighbors8) <- Cell8

Cell7 <- CellID[which(bryneighborvect==7)]
Neighbors7 <- Neighbors[Cell7]
Neighbors7 <- data.frame(Neighbors7)
names(Neighbors7) <- Cell7


# 4.5 Make beta diversity matrix for all cells
FamBetaMat<-as.matrix(FamBetaMat)
row.names(FamBetaMat) <- CellID
names(FamBetaMat) <- CellID


# 4.6 Make beta diversity matrix for cells with 8 neighbors and cells with 7 neighbors
FamBetaMat8<- FamBetaMat[!Cell8, !Cell8, drop=TRUE]
inx8 <- match(as.character(Cell8), rownames(FamBetaMat8))
FamBetaMat8 <- FamBetaMat8[inx8,inx8]

FamBetaMat7 <- FamBetaMat[!Cell7, !Cell7, drop=TRUE]
inx7 <- match(as.character(Cell7), rownames(FamBetaMat7))
FamBetaMat7 <- FamBetaMat7[inx7,inx7]


# 4.7 For each cell, pairwise beta diversity is calculated for that focal cell and each of its 8 (or 7) neighbors, and the mean of those values is found
Cell8CH <- as.character(Cell8)
famBeta8 <- lapply(Cell8CH, function(x)mean(FamBetaMat[x, as.character(Neighbors8[,x])]))
names(famBeta8) <- Cell8CH

Cell7CH <- as.character(Cell7)
famBeta7 <- lapply(Cell7CH, function(x)mean(FamBetaMat[x, as.character(Neighbors7[,x])]))
names(famBeta7) <- Cell7CH


# 4.8 Plot mean pairwise beta diversity by Cell ID and convert to dissimilarity using 1-x where x is Jaccard similarity
FamBeta7Vec<-unlist(famBeta7)
FamBeta8Vec<-unlist(famBeta8)

FamBetaVec <- rep(0, 15038)

FamBetaVec[Cell8]<-FamBeta8Vec
FamBetaVec[Cell7]<-FamBeta7Vec

FamBetaVec[FamBetaVec==0]<-NA
FamBetaVec <- 1-FamBetaVec

# 4.9 Convert UTM to longitude and latitude
FamLongLatBetaVec <- rep(0, 15038)
FamLongLatBetaVec[Cell8]<-FamBeta8Vec
FamLongLatBetaVec[Cell7]<-FamBeta7Vec
FamLongLatBetaVec[FamLongLatBetaVec==0]<-NA
FamLongLatBetaVec <- 1-FamLongLatBetaVec

FamLongLatBetaRaster <- setValues(BlankRas, FamLongLatBetaVec)
FamLongLatBetaPoints<-rasterToPoints(FamLongLatBetaRaster)
FamLongLatBetaDF <- data.frame(FamLongLatBetaPoints)
colnames(FamLongLatBetaDF) <- c("Longitude", "Latitude", "Beta")

coordinates(FamLongLatBetaDF) <- ~Longitude+Latitude 
proj4string(FamLongLatBetaDF) <- CRS("+proj=utm +zone=10") 
FamBetaLongLat <- spTransform(FamLongLatBetaDF, CRS("+proj=longlat")) 
FamLongLatBetaDF <- data.frame(FamBetaLongLat)
FamLongLatBetaDF[c("Longitude", "Latitude", "Beta")]
saveRDS(FamLongLatBetaDF, file = "Data/FamLongLatBetaDF.rds")

FamBetaLongLat <- data.frame(FamBetaLongLat)
colnames(FamBetaLongLat) <- c("Beta", "Longitude", "Latitude")

saveRDS(FamLongLatBetaRaster, file="Data/FamLongLatBetaRaster.rds")


# 4.10 Map beta diversity with values over 0.5 shown in dark grey (19 values are over 0.5)
#theme_void for white background, theme_gray for latitude curves
FamBetaRaster <- setValues(BlankRas, FamBetaVec)
FamBetaPoints<-rasterToPoints(FamBetaRaster)
FamBetaDF <- data.frame(FamBetaPoints)
colnames(FamBetaDF) <- c("Longitude", "Latitude", "Beta")

# 4.11 Outlier beta values (>0.5)
FamOutlierBetaVec <- rep(0, 15038)
FamOutlierBetaVec[Cell8]<-FamBeta8Vec
FamOutlierBetaVec[Cell7]<-FamBeta7Vec
FamOutlierBetaVec[FamOutlierBetaVec==0]<-NA
FamOutlierBetaVec[FamOutlierBetaVec>0.5]<-NA

FamOutlierBetaRaster <- setValues(BlankRas, FamOutlierBetaVec)
FamOutlierBetaPoints<-rasterToPoints(FamOutlierBetaRaster)
FamOutlierBetaDF <- data.frame(FamOutlierBetaPoints)
colnames(FamOutlierBetaDF) <- c("Longitude", "Latitude", "Beta")

# 4.12 Mapping
source("Functions/gplot_data.R")
gplotB<- gplot_data(FamBetaRaster)
gplotOutlier<- gplot_data(FamOutlierBetaRaster)

FamBetaMap <- ggplot() +
  geom_tile(data = dplyr::filter(gplotOutlier, !is.na(value)), 
            aes(x = x, y = y), fill = "gray25") +
  geom_tile(data = gplotB, 
            aes(x = x, y = y, fill = value)) +
  scale_fill_gradientn(name = "β diversity", colours=cols, na.value="transparent", limits = c(0,0.5)) +
  coord_quickmap() + geom_sf(data = nw_bound_sf, size = 0.5, fill=NA) + 
  geom_sf(data = nw_mount_sf, size = 0.5, alpha=0.1) + theme_void() +
  theme(legend.text=element_text(size=20), legend.title=element_text(size=32), axis.title = element_blank())
FamBetaMap

png("Figures/fam_beta.png", width = 1000, height = 1000, pointsize = 30)
FamBetaMap
dev.off()

# 4.13 Scatterplot of beta diversity by family x latitude
FamBetaLongLat$pc <- predict(prcomp(~Latitude+Beta, FamBetaLongLat))[,1]
cols2 <- rev(wes_palette("Zissou1", 1, type = "continuous"))

FamBetaLongLat
FamBetaScatterplot <- ggplot(FamBetaLongLat, aes(Latitude, Beta)) + 
  geom_point(shape = 16, size = 5, show.legend = FALSE, alpha=0.5, color = "orangered2") + 
  ylab("β diversity") + 
  ylim(0,0.5)+ 
  theme_minimal() + 
  theme(axis.title.y = element_text(size=32), axis.title.x = element_text(size=32),  axis.text = element_text(size=20))
FamBetaScatterplot

png("Figures/fam_beta_scatter.png", width = 1000, height = 1000, pointsize = 30)
FamBetaScatterplot
dev.off()


# 5.0 Make beta diversity maps for each family------------------------------------------------------------------
#####have not finished this, code below is for a-div

for(i in 1:NumberFamilies){
  TempFamRichnessRaster <- setValues(BlankRas, FamRichList[[i]])
  TempFamDF <- rasterToPoints(TempFamRichnessRaster)
  TempFamDF <- data.frame(TempFamDF)
  colnames(TempFamDF) <- c("Longitude", "Latitude", "Alpha")
  
  Map <- ggplot() + geom_tile(data=TempFamDF, aes(x=Longitude, y=Latitude, fill=Alpha)) +   
    scale_fill_gradientn(name="α diversity", colours=cols, na.value="transparent") +
    coord_equal() +
    geom_sf(data = nw_bound_sf, size = 0.5, fill=NA) + 
    geom_sf(data = nw_mount_sf, size = 0.5, alpha=0.1) + theme_void() + 
    theme(legend.text=element_text(size=20), legend.title=element_text(size=32))
  
  filename <- paste("./Figures/RichnessByFamilyMaps/RichMap_", FamilyNames[i], ".png", sep = "")
  png(filename, width= 1000, height = 1000, pointsize = 30)
  print({Map})
  dev.off()
}


# 6.0 Map families with more species than the mean--------------------------------------------------------------
MSFamCellID <- MostSpecioseFamPres %>%
  filter(Family %in% FamMostSpeciesList) %>%
  select(CellID, Family)

melted <- melt(MSFamCellID, id=c("Family", "CellID"), na.rm = TRUE)

MSFamCellMatrix <- acast(melted, CellID~Family, margins=FALSE)
MSFamCellMatrix[MSFamCellMatrix > 0] <- 1

#Using betadiver to compute B-diversity using Jaccard similarity
#betadiver(help = TRUE) gives you indices
MSFamBetaMat <- betadiver(MSFamCellMatrix, method = "j", order = FALSE, help = FALSE)

#Make beta diversity matrix for all cells
MSFamBetaMat<-as.matrix(MSFamBetaMat)
row.names(MSFamBetaMat) <- CellID
names(MSFamBetaMat) <- CellID



# 4.6 Make beta diversity matrix for cells with 8 neighbors and cells with 7 neighbors
FamBetaMat8<- FamBetaMat[!Cell8, !Cell8, drop=TRUE]
inx8 <- match(as.character(Cell8), rownames(FamBetaMat8))
FamBetaMat8 <- FamBetaMat8[inx8,inx8]

FamBetaMat7 <- FamBetaMat[!Cell7, !Cell7, drop=TRUE]
inx7 <- match(as.character(Cell7), rownames(FamBetaMat7))
FamBetaMat7 <- FamBetaMat7[inx7,inx7]


# 4.7 For each cell, pairwise beta diversity is calculated for that focal cell and each of its 8 (or 7) neighbors, and the mean of those values is found
Cell8CH <- as.character(Cell8)
famBeta8 <- lapply(Cell8CH, function(x)mean(FamBetaMat[x, as.character(Neighbors8[,x])]))
names(famBeta8) <- Cell8CH

Cell7CH <- as.character(Cell7)
famBeta7 <- lapply(Cell7CH, function(x)mean(FamBetaMat[x, as.character(Neighbors7[,x])]))
names(famBeta7) <- Cell7CH


# 4.8 Plot mean pairwise beta diversity by Cell ID and convert to dissimilarity using 1-x where x is Jaccard similarity
FamBeta7Vec<-unlist(famBeta7)
FamBeta8Vec<-unlist(famBeta8)

FamBetaVec <- rep(0, 15038)

FamBetaVec[Cell8]<-FamBeta8Vec
FamBetaVec[Cell7]<-FamBeta7Vec

FamBetaVec[FamBetaVec==0]<-NA
FamBetaVec <- 1-FamBetaVec

# 4.9 Convert UTM to longitude and latitude
FamLongLatBetaVec <- rep(0, 15038)
FamLongLatBetaVec[Cell8]<-FamBeta8Vec
FamLongLatBetaVec[Cell7]<-FamBeta7Vec
FamLongLatBetaVec[FamLongLatBetaVec==0]<-NA
FamLongLatBetaVec <- 1-FamLongLatBetaVec

FamLongLatBetaRaster <- setValues(BlankRas, FamLongLatBetaVec)
FamLongLatBetaPoints<-rasterToPoints(FamLongLatBetaRaster)
FamLongLatBetaDF <- data.frame(FamLongLatBetaPoints)
colnames(FamLongLatBetaDF) <- c("Longitude", "Latitude", "Beta")

coordinates(FamLongLatBetaDF) <- ~Longitude+Latitude 
proj4string(FamLongLatBetaDF) <- CRS("+proj=utm +zone=10") 
FamBetaLongLat <- spTransform(FamLongLatBetaDF, CRS("+proj=longlat")) 
FamLongLatBetaDF <- data.frame(FamBetaLongLat)
FamLongLatBetaDF[c("Longitude", "Latitude", "Beta")]
saveRDS(FamLongLatBetaDF, file = "Data/FamLongLatBetaDF.rds")

FamBetaLongLat <- data.frame(FamBetaLongLat)
colnames(FamBetaLongLat) <- c("Beta", "Longitude", "Latitude")

saveRDS(FamLongLatBetaRaster, file="Data/FamLongLatBetaRaster.rds")


# 4.10 Map beta diversity with values over 0.5 shown in dark grey (19 values are over 0.5)
#theme_void for white background, theme_gray for latitude curves
FamBetaRaster <- setValues(BlankRas, FamBetaVec)
FamBetaPoints<-rasterToPoints(FamBetaRaster)
FamBetaDF <- data.frame(FamBetaPoints)
colnames(FamBetaDF) <- c("Longitude", "Latitude", "Beta")

# 4.11 Outlier beta values (>0.5)
FamOutlierBetaVec <- rep(0, 15038)
FamOutlierBetaVec[Cell8]<-FamBeta8Vec
FamOutlierBetaVec[Cell7]<-FamBeta7Vec
FamOutlierBetaVec[FamOutlierBetaVec==0]<-NA
FamOutlierBetaVec[FamOutlierBetaVec>0.5]<-NA

FamOutlierBetaRaster <- setValues(BlankRas, FamOutlierBetaVec)
FamOutlierBetaPoints<-rasterToPoints(FamOutlierBetaRaster)
FamOutlierBetaDF <- data.frame(FamOutlierBetaPoints)
colnames(FamOutlierBetaDF) <- c("Longitude", "Latitude", "Beta")

# 4.12 Mapping
source("Functions/gplot_data.R")
gplotB<- gplot_data(FamBetaRaster)
gplotOutlier<- gplot_data(FamOutlierBetaRaster)

FamBetaMap <- ggplot() +
  geom_tile(data = dplyr::filter(gplotOutlier, !is.na(value)), 
            aes(x = x, y = y), fill = "gray25") +
  geom_tile(data = gplotB, 
            aes(x = x, y = y, fill = value)) +
  scale_fill_gradientn(name = "β diversity", colours=cols, na.value="transparent", limits = c(0,0.5)) +
  coord_quickmap() + geom_sf(data = nw_bound_sf, size = 0.5, fill=NA) + 
  geom_sf(data = nw_mount_sf, size = 0.5, alpha=0.1) + theme_void() +
  theme(legend.text=element_text(size=20), legend.title=element_text(size=32), axis.title = element_blank())
FamBetaMap

png("Figures/fam_beta.png", width = 1000, height = 1000, pointsize = 30)
FamBetaMap
dev.off()

# 4.13 Scatterplot of beta diversity by family x latitude
FamBetaLongLat$pc <- predict(prcomp(~Latitude+Beta, FamBetaLongLat))[,1]
cols2 <- rev(wes_palette("Zissou1", 1, type = "continuous"))

FamBetaLongLat
FamBetaScatterplot <- ggplot(FamBetaLongLat, aes(Latitude, Beta)) + 
  geom_point(shape = 16, size = 5, show.legend = FALSE, alpha=0.5, color = "orangered2") + 
  ylab("β diversity") + 
  ylim(0,0.5)+ 
  theme_minimal() + 
  theme(axis.title.y = element_text(size=32), axis.title.x = element_text(size=32),  axis.text = element_text(size=20))
FamBetaScatterplot

png("Figures/fam_beta_scatter.png", width = 1000, height = 1000, pointsize = 30)
FamBetaScatterplot
dev.off()


