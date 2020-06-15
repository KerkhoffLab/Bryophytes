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
AndesCellID <- AndesAlphaBeta$CellID
AppCellID <- AppalachianBeta$CellID

AndesBryPres <- subset(BryophytePresence, CellID == AndesCellID[1])
AppBryPres <- subset(BryophytePresence, CellID == AppCellID[1])
for(i in 1:length(AndesCellID)){
  temp <- subset(BryophytePresence, CellID == AndesCellID[i])
  AndesBryPres <- bind(AndesBryPres, temp)
  temp2 <- subset(BryophytePresence, CellID == AppCellID[i])
  AppBryPres <- bind(AppBryPres, temp2)
}


# 1.3 Look at the families in each region
AndesFamList <- unique(AndesBryPres$Family)
AppFamList <- unique(AppBryPres$Family)

length(AndesFamList)
length(AppFamList)

AndesSpecies <- unique(AndesBryPres$Species)
length(AndesSpecies)
AppSpecies <- unique(AppBryPres$Species)

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
dev.off()


# 2.0 Family beta diversity in montane regions-------------------------------------------------------------

# 2.1 Create occurrence by cell matrix by reshaping dataframe, then convert to presence-absence matrix
#taken from BryophyteDiversity.R

# 2.11 Andes
AndesFamilyCellID <- AndesBryPres[,c(5,4)]
melted <- melt(AndesFamilyCellID, id=c("Family", "CellID"), na.rm = TRUE)

AndesFamilyCellMatrix <- acast(melted, CellID~Family, margins=FALSE)
AndesFamilyCellMatrix[AndesFamilyCellMatrix > 0] <- 1

#Using betadiver to compute B-diversity using Jaccard similarity
#betadiver(help = TRUE) gives you indices
require(vegan)
andes_betamat <- betadiver(AndesFamilyCellMatrix, method = "j", order = FALSE, help = FALSE)

#Save species-cell matrix and beta diversity matrix
saveRDS(AndesFamilyCellMatrix, file="Data/AndesFamilyCellMatrix.rds")
saveRDS(andes_betamat, file="Data/AndesBetaMat.rds")


#2.12 Appalachians
AppFamilyCellID <- AppBryPres[,c(5,4)]
melted <- melt(AppFamilyCellID, id=c("Family", "CellID"), na.rm = TRUE)

AppFamilyCellMatrix <- acast(melted, CellID~Family, margins=FALSE)
AppFamilyCellMatrix[AppFamilyCellMatrix > 0] <- 1

#Using betadiver to compute B-diversity using Jaccard similarity
#betadiver(help = TRUE) gives you indices
require(vegan)
app_betamat <- betadiver(AppFamilyCellMatrix, method = "j", order = FALSE, help = FALSE)

#Save species-cell matrix and beta diversity matrix
saveRDS(AppFamilyCellMatrix, file="Data/AppFamilyCellMatrix.rds")
saveRDS(app_betamat, file="Data/AppBetaMat.rds")

