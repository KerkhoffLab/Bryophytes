#Family Diversity
#Hailey Napier
#June 2020

# 0. Load Packages
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


# 0.5 Run DataProcessing.R to generate necessary data
BryophytePresence <- readRDS("Data/BryophytePresence.rds")

nw_mount <- shapefile("Data/MapOutlines/Mountains/Koeppen-Geiger_biomes.shp")
nw_bound <- shapefile("Data/MapOutlines/Global_bound/Koeppen-Geiger_biomes.shp")

nw_mount_sf <- st_as_sf(nw_mount)
nw_bound_sf <- st_as_sf(nw_bound)


# 1.0 Plot all family richness (how many families are in each cell)



# 2.0 Plot species richness for each family (separate plot for each family)

# 2.1 Make family name dataframe and vector for number of families
Families <- data.frame(table(BryophytePresence$Family))
names(Families)[1] <- "Family"
names(Families)[2] <- "Richness"
Families <- left_join(Families, ByGroup, by = "Family")

FamilyNames <- Families$Family
NumberFamilies <- length(FamilyNames)


# 2.2 Loop through family names and subset BryophtePresence for each family, store them in a list
FamList <- list()
for(i in 1:NumberFamilies){
  fam <- FamilyNames[i]
  FamList[[i]] <- subset(BryophytePresence, Family == fam)
  i <- i + 1
}


#2.3 Loop through families and tally richness for each family, store in a list
FamRichList <- list()
FamPresList <- list()
for(i in 1:NumberFamilies){
  FamPresList[[i]] <- tally(group_by(FamList[[i]], CellID))
  names(FamPresList[[i]])[2] <- "Richness"
  FamRichList[[i]] <- numeric(15038)
  FamRichList[[i]][FamPresList[[i]]$CellID] <- FamPresList[[i]]$Richness
  FamRichList[[i]][which(FamRichList[[i]]==0)] = NA
}


# 2.4 Make a new folder for richness w/in each family
setwd("./Figures")
dir.create("./RichnessByFamilyMaps")


# 2.5 Loop through families and map all species in each family (one map for each family)
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
  print("map done")
  
  filename <- paste("./Figures/RichnessByFamilyMaps/RichMap_", FamilyNames[i], ".png", sep = "")
  print("A")
  png(filename, width= 1000, height = 1000, pointsize = 30)
  print("B")
  Map
  print("C")
  dev.off()
  print("D")
}

dev.list()

i <- 1
