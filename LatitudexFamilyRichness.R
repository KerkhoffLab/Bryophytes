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
  for(i in 1:length(famlist)){
    famname <- famlist[i]
    tempDF$Family[i] <- famname
  }
  LatFamRich <- bind(LatFamRich, tempDF)
}

i <- 1
