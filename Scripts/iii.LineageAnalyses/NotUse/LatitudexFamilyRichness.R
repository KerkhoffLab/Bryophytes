#Bryophyte Family Richness By Latitude
#Hailey Napier and Kathryn Dawdy
#June 10, 2020

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



# 1.0 Make a dataframe that contains every cell and the species in each cell----------------------------------
cell <- CellID[1]
famcell <- subset(BryophytePresence, CellID == cell)
famlist <- unique(famcell$Family)
r <- length(famlist)
LatFamRich <- data.frame(rep(cell, r))
names(LatFamRich)[1] <- "CellID"
LatFamRich$Family <- NA
LatFamRich$Longitude <- LongLatDF$Longitude[CellID == cell][1]
LatFamRich$Latitude <- LongLatDF$Latitude[CellID == cell][1]
for(i in 1:length(famlist)){
  famname <- famlist[i]
  LatFamRich$Family[i] <- famname
}


for(i in 2:length(CellID)){
  cell <- CellID[i]
  famcell <- subset(BryophytePresence, CellID == cell)
  famlist <- unique(famcell$Family)
  r <- length(famlist)
  tempDF <- data.frame(rep(cell, r))
  names(tempDF)[1] <- "CellID"
  tempDF$Family <- NA
  tempDF$Longitude <- LongLatDF$Longitude[CellID == cell][1]
  tempDF$Latitude <- LongLatDF$Latitude[CellID == cell][1]
  for(i in 1:length(famlist)){
    famname <- famlist[i]
    tempDF$Family[i] <- famname
  }
  LatFamRich <- bind(LatFamRich, tempDF)
}

i <- 1


# 1.2 Add species richness data
LatFamRich$Richness <- NA

f <- FamilyNames[1]
currentFamDF <- FamList[[1]]
LatFamilyRichnessDF<- subset(LatFamRich, Family == f)
rDF <- tally(group_by(currentFamDF, CellID))
LatFamilyRichnessDF$Richness <- rDF$n

for(i in 2:NumberFamilies){
  f <- FamilyNames[i]
  currentFamDF <- FamList[[i]]
  tempbyfam <- subset(LatFamRich, Family == f)
  rDF <- tally(group_by(currentFamDF, CellID))
  tempbyfam$Richness <- rDF$n
  LatFamilyRichnessDF <- bind(LatFamilyRichnessDF, tempbyfam)
}


# 1.3 Make plot!
FamilyLatScatter <- ggplot(data = LatFamilyRichnessDF, aes(Latitude, Richness, color = Family)) +
  #geom_smooth(size = 2, show.legend = FALSE) +
  geom_point(shape = 16, size = 7, alpha=0.6) + 
  ylab("α diversity") + xlab("Latitude") + 
  theme_minimal() + theme(axis.title.y = element_text(size=40), axis.title.x = element_text(size=40),  
                          axis.text = element_text(size=20)) 
FamilyLatScatter

png("Figures/FamilyLatScatter.png", width = 1200, height = 1000, pointsize = 20)
FamilyLatScatter
dev.off()



# 2.0 Families with #species > mean #species

# 2.1 Make a dataframe that contains every cell and the species in each cell----------------------------------
cell <- CellID[1]
famcell <- subset(MostSpecioseFamPres, CellID == cell)
famlist <- unique(famcell$Family)
r <- length(famlist)
MostSpeciesLatFamRich <- data.frame(rep(cell, r))
names(MostSpeciesLatFamRich)[1] <- "CellID"
MostSpeciesLatFamRich$Family <- NA
MostSpeciesLatFamRich$Longitude <- LongLatDF$Longitude[CellID == cell][1]
MostSpeciesLatFamRich$Latitude <- LongLatDF$Latitude[CellID == cell][1]
for(i in 1:length(famlist)){
  famname <- famlist[i]
  MostSpeciesLatFamRich$Family[i] <- famname
}


for(i in 2:length(CellID)){
  cell <- CellID[i]
  famcell <- subset(MostSpecioseFamPres, CellID == cell)
  famlist <- unique(famcell$Family)
  r <- length(famlist)
  tempDF <- data.frame(rep(cell, r))
  names(tempDF)[1] <- "CellID"
  tempDF$Family <- NA
  tempDF$Longitude <- LongLatDF$Longitude[CellID == cell][1]
  tempDF$Latitude <- LongLatDF$Latitude[CellID == cell][1]
  for(j in 1:length(famlist)){
    famname <- famlist[j]
    tempDF$Family[j] <- famname
  }
  MostSpeciesLatFamRich <- bind(MostSpeciesLatFamRich, tempDF)
}


# 2.2 Add species richness data
Families$Number <- c(1:135)
name <- FamMostSpeciesList[1]
MSDF <- subset(Families, Family == name)
  
for(i in 2:length(FamMostSpeciesList)){
  name <- FamMostSpeciesList[i]
  temp <- subset(Families, Family == name)
  MSDF <- bind(MSDF, temp)
}


MostSpeciesLatFamRich$Richness <- NA

#this isn't working and I'm not sure why. Will come back to it.
f <- FamMostSpeciesList[1]
n <- MSDF$Number[MSDF$Family == f]
currentFamDF <- FamList[[n]]
MSLatFamilyRichnessDF<- subset(MostSpeciesLatFamRich, Family == f)
rDF <- tally(group_by(currentFamDF, CellID))
MSLatFamilyRichnessDF$Richness <- rDF$n


for(i in 2:NumberFamilies){
  f <- FamMostSpeciesList[i]
  n <- MSDF$Number[MSDF$Family == f]
  currentFamDF <- FamList[[n]]
  tempbyfam <- subset(MostSpeciesLatFamRich, Family == f)
  rDF <- tally(group_by(currentFamDF, CellID))
  MSLatFamilyRichnessDF$Richness <- rDF$n
  MSLatFamilyRichnessDF <- bind(MSLatFamilyRichnessDF, tempbyfam)
}


# 1.3 Make plot!
FamilyLatScatter <- ggplot(data = LatFamilyRichnessDF, aes(Latitude, Richness, color = Family)) +
  #geom_smooth(size = 2, show.legend = FALSE) +
  #geom_point(shape = 16, size = 7, alpha=0.6) + 
  ylab("α diversity") + xlab("Latitude") + 
  theme_minimal() + theme(axis.title.y = element_text(size=40), axis.title.x = element_text(size=40),  
                          axis.text = element_text(size=20)) 
FamilyLatScatter

png("Figures/FamilyLatScatter.png", width = 1200, height = 1000, pointsize = 20)
FamilyLatScatter
dev.off()

