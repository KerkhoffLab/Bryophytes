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


# 0.0 Run DataProcessing.R to generate necessary data---------------------------------------------------------
BryophytePresence <- readRDS("Data/BryophytePresence.rds")

nw_mount <- shapefile("Data/MapOutlines/Mountains/Koeppen-Geiger_biomes.shp")
nw_bound <- shapefile("Data/MapOutlines/Global_bound/Koeppen-Geiger_biomes.shp")

nw_mount_sf <- st_as_sf(nw_mount)
nw_bound_sf <- st_as_sf(nw_bound)


# 1.0 Plot all family richness (how many families are in each cell)-------------------------------------------
-
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

MostSpecioseFamPres<- subset(BryophytePresence, Family == FamMostSpeciesList[1])
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

