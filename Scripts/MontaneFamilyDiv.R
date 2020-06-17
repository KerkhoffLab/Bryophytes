#Family diversity in mountain ranges
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

# 1.0 Family alpha diversity in montane regions---------------------------------------------------------------

# 1.1 Run MountainRanges.R to get necessary data

# 1.2 Subset richness data to include only montane regions of interest
AndesCellID <- AndesVec
AppCellID <- AppVec

AndesBryPres <- BryophytePresence %>%
  filter(CellID %in% AndesCellID)

AppBryPres <- BryophytePresence %>%
  filter(CellID %in% AppCellID)


# 1.3 Look at the families in each region
AndesFamList <- unique(AndesBryPres$Family)
AppFamList <- unique(AppBryPres$Family)

length(AndesFamList)
length(AppFamList)

AndesSpecies <- unique(AndesBryPres$Species)
length(AndesSpecies)
AppSpecies <- unique(AppBryPres$Species)
length(AppSpecies)

AndesSF <- BrySpecies %>%
  filter(Species %in% AndesSpecies) %>%
  select(Family, Species)
AndesSF <- AndesSF[!duplicated(AndesSF$Species),]

AppSF <- BrySpecies %>%
  filter(Species %in% AppSpecies) %>%
  select(Family, Species)
AppSF <- AppSF[!duplicated(AppSF$Species),]

AndesFamNSpecies <- data.frame(tally(group_by(AndesSF, Family)))
AppFamNSpecies <- data.frame(tally(group_by(AppSF, Family)))


# 1.4 Make a combined dataframe that shows species richness for each family in each mountain range and which families are shared
shared_families_andes <- AndesFamNSpecies %>%
  filter(Family %in% AppFamNSpecies$Family)
names(shared_families_andes)[2] <- "Andes.Rich"
shared_families_andes$Shared <- 1

shared_families_app <- AppFamNSpecies %>%
  filter(Family %in% AndesFamNSpecies$Family)
names(shared_families_app)[2] <- "App.Rich"
shared_families_app$Shared <- 1

shared_families <- left_join(shared_families_andes, shared_families_app, by = "Family")
sf <- shared_families$Family

shared_families_all <- full_join(shared_families_andes, shared_families_app)

notshared_families_andes <- AndesFamNSpecies %>%
  filter(!Family %in% AppFamNSpecies$Family)
notshared_families_andes$Shared <- 0
names(notshared_families_andes)[2] <- "Andes.Rich"

notshared_families_app <- AppFamNSpecies %>%
  filter(!Family %in% AndesFamNSpecies$Family)
names(notshared_families_app)[2] <- "App.Rich"
notshared_families_app$Shared <- 0

AndesAppFamilies <- full_join(shared_families_all, notshared_families_andes)
AndesAppFamilies <- full_join(AndesAppFamilies, notshared_families_app)


# 1.5 Map the families with species richness above the median

# 1.51 Andes
AndesFamMedian <- median(AndesAppFamilies$Andes.Rich, na.rm = TRUE)
AndesFamMean <- mean(AndesAppFamilies$Andes.Rich, na.rm = T)
TopFamAndes <- AndesAppFamilies %>%
  filter(Andes.Rich > AndesFamMean)
TopFamAndesList <- TopFamAndes$Family


TopFamAndesPresence <- BryophytePresence %>%
  filter(Family %in% TopFamAndesList)

TopFamAndesTotalRich <- data.frame(table(TopFamAndesPresence$Family))
names(TopFamAndesTotalRich)[1] <- "Family"
names(TopFamAndesTotalRich)[2] <- "Richness"

AndesTFPresence <- data.frame(CellID)
AndesTFPresence$Richness <- NA

for(i in 1:length(CellID)){
  cell <- CellID[i]
  famcell <- subset(TopFamAndesPresence, CellID == cell)
  r <- length(unique(famcell$Family))
  AndesTFPresence$Richness[CellID == cell] <- r
}

AndesTFRichness <- numeric(15038)
AndesTFRichness[AndesTFPresence$CellID] <- AndesTFPresence$Richness
AndesTFRichness[which(AndesTFRichness==0)]=NA

AndesTFRichnessRaster <- setValues(BlankRas, AndesTFRichness)
AndesTFDF <- rasterToPoints(AndesTFRichnessRaster)
AndesTFDF <- data.frame(AndesTFDF)
colnames(AndesTFDF) <- c("Longitude", "Latitude", "Alpha")

AndesTopFamRichnessMap <- ggplot() + geom_tile(data=AndesTFDF, aes(x=Longitude, y=Latitude, fill=Alpha)) +   
  scale_fill_gradientn(name="α diversity", colours=cols, na.value="transparent") +
  coord_equal() +
  geom_sf(data = nw_bound_sf, size = 0.5, fill=NA) + 
  geom_sf(data = nw_mount_sf, size = 0.5, alpha=0.1) + theme_void() + 
  theme(legend.text=element_text(size=20), legend.title=element_text(size=32))
AndesTopFamRichnessMap

png("Figures/AndesTopFamRichMapMean.png", width= 1000, height = 1000, pointsize = 30)
AndesTopFamRichnessMap
options(device = "RStudioGD")
dev.off()


# 1.52 Appalachians
AppFamMedian <- median(AndesAppFamilies$App.Rich, na.rm = TRUE)
AppFamMean <- mean(AndesAppFamilies$App.Rich, na.rm = T)
TopFamApp <- AndesAppFamilies %>%
  filter(App.Rich > AppFamMean)
TopFamAppList <- TopFamApp$Family

TopFamAppPresence <- BryophytePresence %>%
  filter(Family %in% TopFamAppList)

TopFamAppTotalRich <- data.frame(table(TopFamAppPresence$Family))
names(TopFamAppTotalRich)[1] <- "Family"
names(TopFamAppTotalRich)[2] <- "Richness"

AppTFPresence <- data.frame(CellID)
AppTFPresence$Richness <- NA

for(i in 1:length(CellID)){
  cell <- CellID[i]
  famcell <- subset(TopFamAppPresence, CellID == cell)
  r <- length(unique(famcell$Family))
  AppTFPresence$Richness[CellID == cell] <- r
}

AppTFRichness <- numeric(15038)
AppTFRichness[AppTFPresence$CellID] <- AppTFPresence$Richness
AppTFRichness[which(AppTFRichness==0)]=NA

AppTFRichnessRaster <- setValues(BlankRas, AppTFRichness)
AppTFDF <- rasterToPoints(AppTFRichnessRaster)
AppTFDF <- data.frame(AppTFDF)
colnames(AppTFDF) <- c("Longitude", "Latitude", "Alpha")

AppTopFamRichnessMap <- ggplot() + geom_tile(data=AppTFDF, aes(x=Longitude, y=Latitude, fill=Alpha)) +   
  scale_fill_gradientn(name="α diversity", colours=cols, na.value="transparent") +
  coord_equal() +
  geom_sf(data = nw_bound_sf, size = 0.5, fill=NA) + 
  geom_sf(data = nw_mount_sf, size = 0.5, alpha=0.1) + theme_void() + 
  theme(legend.text=element_text(size=20), legend.title=element_text(size=32))
AppTopFamRichnessMap

png("Figures/AppTopFamRichMapMean.png", width= 1000, height = 1000, pointsize = 30)
AppTopFamRichnessMap
options(device = "RStudioGD")
dev.off()
dev.off()


# 2.0 Family beta diversity in montane regions-------------------------------------------------------------
#Didn't really work

# 2.1 Create occurrence by cell matrix by reshaping dataframe, then convert to presence-absence matrix
#adapted from BryophyteDiversity.R

# 2.11 Andes
AndesFamilyCellID <- AndesBryPres[,c(5,4)]

diff <- unique(AndesFamilyCellID$CellID)
n <- setdiff(CellID, diff)
fix <- data.frame(n)
fix$Family <- NA
fix$CellID <- fix$n
fix$n <- NULL
names(fix)[1] <- "Family"
names(fix)[2] <- "CellID"
AndesFamilyCellID <- bind_rows(AndesFamilyCellID, fix)

melted <- melt(AndesFamilyCellID, id=c("Family", "CellID"), na.rm = TRUE)

AndesFamilyCellMatrix <- acast(melted, CellID~Family, margins=FALSE)
AndesFamilyCellMatrix[AndesFamilyCellMatrix > 0] <- 1

#Using betadiver to compute B-diversity using Jaccard similarity
#betadiver(help = TRUE) gives you indices
AndesBetaMat <- betadiver(AndesFamilyCellMatrix, method = "j", order = FALSE, help = FALSE)

#Make beta diversity matrix for all cells
AndesBetaMat<-as.matrix(AndesBetaMat)
row.names(AndesBetaMat) <- CellID
names(AndesBetaMat) <- CellID

#Make beta diversity matrix for cells with 8 neighbors and cells with 7 neighbors
AndesBetaMat8<- AndesBetaMat[!Cell8, !Cell8, drop=TRUE]
inx8 <- match(as.character(Cell8), rownames(AndesBetaMat8))
AndesBetaMat8 <- FamBetaMat8[inx8,inx8]

AndesBetaMat7 <- AndesBetaMat[!Cell7, !Cell7, drop=TRUE]
inx7 <- match(as.character(Cell7), rownames(AndesBetaMat7))
AndesBetaMat7 <- AndesBetaMat7[inx7,inx7]

#For each cell, pairwise beta diversity is calculated for that focal cell and each of its 8 (or 7) neighbors, and the mean of those values is found
AndesBeta8 <- lapply(Cell8CH, function(x)mean(AndesBetaMat[x, as.character(Neighbors8[,x])]))
names(AndesBeta8) <- Cell8CH

AndesBeta7 <- lapply(Cell7CH, function(x)mean(AndesBetaMat[x, as.character(Neighbors7[,x])]))
names(AndesBeta7) <- Cell7CH

#Plot mean pairwise beta diversity by Cell ID and convert to dissimilarity using 1-x where x is Jaccard similarity
AndesBeta7Vec<-unlist(AndesBeta7)
AndesBeta8Vec<-unlist(AndesBeta8)

AndesBetaVec <- rep(0, 15038)

AndesBetaVec[Cell8]<-AndesBeta8Vec
AndesBetaVec[Cell7]<-AndesBeta7Vec

AndesBetaVec[AndesBetaVec==0]<-NA
AndesBetaVec <- 1-AndesBetaVec

#Convert UTM to longitude and latitude
AndesLongLatBetaVec <- rep(0, 15038)
AndesLongLatBetaVec[Cell8]<-AndesBeta8Vec
AndesLongLatBetaVec[Cell7]<-AndesBeta7Vec
AndesLongLatBetaVec[AndesLongLatBetaVec==0]<-NA
AndesLongLatBetaVec <- 1-AndesLongLatBetaVec

AndesLongLatBetaRaster <- setValues(BlankRas, AndesLongLatBetaVec)
AndesLongLatBetaPoints<-rasterToPoints(AndesLongLatBetaRaster)
AndesLongLatBetaDF <- data.frame(AndesLongLatBetaPoints)
colnames(AndesLongLatBetaDF) <- c("Longitude", "Latitude", "Beta")

coordinates(AndesLongLatBetaDF) <- ~Longitude+Latitude 
proj4string(AndesLongLatBetaDF) <- CRS("+proj=utm +zone=10") 
AndesBetaLongLat <- spTransform(AndesLongLatBetaDF, CRS("+proj=longlat")) 
AndesLongLatBetaDF <- data.frame(AndesBetaLongLat)
AndesLongLatBetaDF[c("Longitude", "Latitude", "Beta")]
saveRDS(AndesLongLatBetaDF, file = "Data/AndesLongLatBetaDF.rds")

AndesBetaLongLat <- data.frame(AndesBetaLongLat)
colnames(AndesBetaLongLat) <- c("Beta", "Longitude", "Latitude")

saveRDS(AndesLongLatBetaRaster, file="Data/AndesLongLatBetaRaster.rds")


#Map beta diversity with values over 0.5 shown in dark grey
#theme_void for white background, theme_gray for latitude curves
AndesBetaRaster <- setValues(BlankRas, AndesBetaVec)
AndesBetaPoints<-rasterToPoints(AndesBetaRaster)
AndesBetaDF <- data.frame(AndesBetaPoints)
colnames(AndesBetaDF) <- c("Longitude", "Latitude", "Beta")

# 4.11 Outlier beta values (>0.5)
AndesOutlierBetaVec <- rep(0, 15038)
AndesOutlierBetaVec[Cell8]<-AndesBeta8Vec
AndesOutlierBetaVec[Cell7]<-AndesBeta7Vec
AndesOutlierBetaVec[AndesOutlierBetaVec==0]<-NA
AndesOutlierBetaVec[AndesOutlierBetaVec>0.5]<-NA

AndesOutlierBetaRaster <- setValues(BlankRas, AndesOutlierBetaVec)
AndesOutlierBetaPoints<-rasterToPoints(AndesOutlierBetaRaster)
AndesOutlierBetaDF <- data.frame(AndesOutlierBetaPoints)
colnames(AndesOutlierBetaDF) <- c("Longitude", "Latitude", "Beta")

# 4.12 Mapping
source("Functions/gplot_data.R")
gplotB<- gplot_data(AndesBetaRaster)
gplotOutlier<- gplot_data(AndesOutlierBetaRaster)

AndesBetaMap <- ggplot() +
  geom_tile(data = dplyr::filter(gplotOutlier, !is.na(value)), 
            aes(x = x, y = y), fill = "gray25") +
  geom_tile(data = gplotB, 
            aes(x = x, y = y, fill = value)) +
  scale_fill_gradientn(name = "β diversity", colours=cols, na.value="transparent", limits = c(0,0.5)) +
  coord_quickmap() + geom_sf(data = nw_bound_sf, size = 0.5, fill=NA) + 
  geom_sf(data = nw_mount_sf, size = 0.5, alpha=0.1) + theme_void() +
  theme(legend.text=element_text(size=20), legend.title=element_text(size=32), axis.title = element_blank())
AndesBetaMap

png("Figures/andes_beta.png", width = 1000, height = 1000, pointsize = 30)
AndesBetaMap
dev.off()

#Scatterplot of beta diversity by family x latitude
AndesBetaLongLat$pc <- predict(prcomp(~Latitude+Beta, AndesBetaLongLat))[,1]
cols2 <- rev(wes_palette("Zissou1", 1, type = "continuous"))

AndesBetaLongLat
AndesBetaScatterplot <- ggplot(AndesBetaLongLat, aes(Latitude, Beta)) + 
  geom_point(shape = 16, size = 5, show.legend = FALSE, alpha=0.5, color = "orangered2") + 
  ylab("β diversity") + 
  ylim(0,0.5)+ 
  theme_minimal() + 
  theme(axis.title.y = element_text(size=32), axis.title.x = element_text(size=32),  axis.text = element_text(size=20))
AndesBetaScatterplot

png("Figures/andesbeta_scatter.png", width = 1000, height = 1000, pointsize = 30)
AndesBetaScatterplot
dev.off()


#2.12 Appalachians
AppFamilyCellID <- AppBryPres[,c(5,4)]

diff <- unique(AppFamilyCellID$CellID)
n <- setdiff(CellID, diff)
fix <- data.frame(n)
fix$Family <- NA
fix$CellID <- fix$n
fix$n <- NULL
names(fix)[1] <- "Family"
names(fix)[2] <- "CellID"
AppFamilyCellID <- bind_rows(AppFamilyCellID, fix)

melted <- melt(AppFamilyCellID, id=c("Family", "CellID"), na.rm = TRUE)

AppFamilyCellMatrix <- acast(melted, CellID~Family, margins=FALSE)
AppFamilyCellMatrix[AppFamilyCellMatrix > 0] <- 1

#Using betadiver to compute B-diversity using Jaccard similarity
#betadiver(help = TRUE) gives you indices
AppBetaMat <- betadiver(AppFamilyCellMatrix, method = "j", order = FALSE, help = FALSE)

#Make beta diversity matrix for all cells
AppBetaMat<-as.matrix(AppBetaMat)
row.names(AppBetaMat) <- CellID
names(AppBetaMat) <- CellID

#Make beta diversity matrix for cells with 8 neighbors and cells with 7 neighbors
AppBetaMat8<- AppBetaMat[!Cell8, !Cell8, drop=TRUE]
inx8 <- match(as.character(Cell8), rownames(AppBetaMat8))
AppBetaMat8 <- AppBetaMat8[inx8,inx8]

AppBetaMat7 <- AppBetaMat[!Cell7, !Cell7, drop=TRUE]
inx7 <- match(as.character(Cell7), rownames(AppBetaMat7))
AppBetaMat7 <- AppBetaMat7[inx7,inx7]

#For each cell, pairwise beta diversity is calculated for that focal cell and each of its 8 (or 7) neighbors, and the mean of those values is found
AppBeta8 <- lapply(Cell8CH, function(x)mean(AppBetaMat[x, as.character(Neighbors8[,x])]))
names(AppBeta8) <- Cell8CH

AppBeta7 <- lapply(Cell7CH, function(x)mean(AppBetaMat[x, as.character(Neighbors7[,x])]))
names(AppBeta7) <- Cell7CH

#Plot mean pairwise beta diversity by Cell ID and convert to dissimilarity using 1-x where x is Jaccard similarity
AppBeta7Vec<-unlist(AppBeta7)
AppBeta8Vec<-unlist(AppBeta8)

AppBetaVec <- rep(0, 15038)

AppBetaVec[Cell8]<-AppBeta8Vec
AppBetaVec[Cell7]<-AppBeta7Vec

AppBetaVec[AppBetaVec==0]<-NA
AppBetaVec <- 1-AppBetaVec

#Convert UTM to longitude and latitude
AppLongLatBetaVec <- rep(0, 15038)
AppLongLatBetaVec[Cell8]<-AppBeta8Vec
AppLongLatBetaVec[Cell7]<-AppBeta7Vec
AppLongLatBetaVec[AppLongLatBetaVec==0]<-NA
AppLongLatBetaVec <- 1-AppLongLatBetaVec

AppLongLatBetaRaster <- setValues(BlankRas, AppLongLatBetaVec)
AppLongLatBetaPoints<-rasterToPoints(AppLongLatBetaRaster)
AppLongLatBetaDF <- data.frame(AppLongLatBetaPoints)
colnames(AppLongLatBetaDF) <- c("Longitude", "Latitude", "Beta")

coordinates(AppLongLatBetaDF) <- ~Longitude+Latitude 
proj4string(AppLongLatBetaDF) <- CRS("+proj=utm +zone=10") 
AppBetaLongLat <- spTransform(AppLongLatBetaDF, CRS("+proj=longlat")) 
AppLongLatBetaDF <- data.frame(AppBetaLongLat)
AppLongLatBetaDF[c("Longitude", "Latitude", "Beta")]
saveRDS(AppLongLatBetaDF, file = "Data/AppLongLatBetaDF.rds")

AppBetaLongLat <- data.frame(AppBetaLongLat)
colnames(AppBetaLongLat) <- c("Beta", "Longitude", "Latitude")

saveRDS(AppLongLatBetaRaster, file="Data/AppLongLatBetaRaster.rds")


#Map beta diversity with values over 0.5 shown in dark grey
#theme_void for white background, theme_gray for latitude curves
AppBetaRaster <- setValues(BlankRas, AppBetaVec)
AppBetaPoints<-rasterToPoints(AppBetaRaster)
AppBetaDF <- data.frame(AppBetaPoints)
colnames(AppBetaDF) <- c("Longitude", "Latitude", "Beta")

# 4.11 Outlier beta values (>0.5)
AppOutlierBetaVec <- rep(0, 15038)
AppOutlierBetaVec[Cell8]<-AppBeta8Vec
AppOutlierBetaVec[Cell7]<-AppBeta7Vec
AppOutlierBetaVec[AppOutlierBetaVec==0]<-NA
AppOutlierBetaVec[AppOutlierBetaVec>0.5]<-NA

AppOutlierBetaRaster <- setValues(BlankRas, AppOutlierBetaVec)
AppOutlierBetaPoints<-rasterToPoints(AppOutlierBetaRaster)
AppOutlierBetaDF <- data.frame(AppOutlierBetaPoints)
colnames(AppOutlierBetaDF) <- c("Longitude", "Latitude", "Beta")

# 4.12 Mapping
source("Functions/gplot_data.R")
gplotB<- gplot_data(AppBetaRaster)
gplotOutlier<- gplot_data(AppOutlierBetaRaster)

AppBetaMap <- ggplot() +
  geom_tile(data = dplyr::filter(gplotOutlier, !is.na(value)), 
            aes(x = x, y = y), fill = "gray25") +
  geom_tile(data = gplotB, 
            aes(x = x, y = y, fill = value)) +
  scale_fill_gradientn(name = "β diversity", colours=cols, na.value="transparent", limits = c(0,0.5)) +
  coord_quickmap() + geom_sf(data = nw_bound_sf, size = 0.5, fill=NA) + 
  geom_sf(data = nw_mount_sf, size = 0.5, alpha=0.1) + theme_void() +
  theme(legend.text=element_text(size=20), legend.title=element_text(size=32), axis.title = element_blank())
AppBetaMap

png("Figures/app_beta.png", width = 1000, height = 1000, pointsize = 30)
AppBetaMap
dev.off()

#Scatterplot of beta diversity by family x latitude
AppBetaLongLat$pc <- predict(prcomp(~Latitude+Beta, AppBetaLongLat))[,1]
cols2 <- rev(wes_palette("Zissou1", 1, type = "continuous"))

AppBetaLongLat
AppBetaScatterplot <- ggplot(AppBetaLongLat, aes(Latitude, Beta)) + 
  geom_point(shape = 16, size = 5, show.legend = FALSE, alpha=0.5, color = "orangered2") + 
  ylab("β diversity") + 
  ylim(0,0.5)+ 
  theme_minimal() + 
  theme(axis.title.y = element_text(size=32), axis.title.x = element_text(size=32),  axis.text = element_text(size=20))
AppBetaScatterplot

png("Figures/app_beta_scatter.png", width = 1000, height = 1000, pointsize = 30)
AppBetaScatterplot
dev.off()

