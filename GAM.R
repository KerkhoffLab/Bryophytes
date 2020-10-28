# GAM
# Testing out the moss generalized additive model
# Hailey Napier and Kathryn Dawdy
# October 2020

# 0.0 FIRST -------------------------------------
# 0.1 Load Packages
library(dplyr)
library(raster)

# 0.2 Load Data
MossOrderRichList <- readRDS("Data/MossOrderRichList.rds")
MossOrderNames <- readRDS("Data/MossOrderNames.rds")
RichnessVec <- readRDS("Data/RichnessVec.rds")
BiomeCells <- readRDS("Data/BiomeCellsClean.rds")


# 1.0 Get WorldClim data ------------------------
# Adapted from WorldClim.R
worldclim <- getData('worldclim', var='bio', res=10) 
WC_layers <- do.call(brick, lapply(list.files(path = "wc10/", pattern = "*.bil$", full.names = TRUE), raster)) 
WorldClim <- do.call(crop, c(WC_layers,extent(-175,-22,-56,74)))
Bryophytecrs <- crs(RangeRaster) 
  #creates a raster with resolution of the bryophyte range map
WorldClim <- projectRaster(WorldClim, crs = Bryophytecrs) 
WorldClim <- resample(WorldClim, RangeRaster) 
  #fits WorldClim data on the BIEN 100 km^2 map by averaging variables across each cell

#Precipitation data is in mm
MAP <- getValues(WorldClim$bio12)

#Temperature data is degrees Celsius * 10
MAT <- getValues(WorldClim$bio1)
#Divide by 10 to get degrees Celsius
MAT <- MAT/10
MAT


# 2.0 Make biome vector --------------------------
AllBiomeCells <- data.frame(CellID = rep(1:15038))
AllBiomeCells$Biome <- NA
for(i in BiomeCells$CellID){
  biome <- BiomeCells$Type[which(BiomeCells$CellID == i)]
  AllBiomeCells$Biome[which(AllBiomeCells$CellID == i)] <- biome
}

BiomeVec <- AllBiomeCells$Biome


# 3.0 Create the dataframe ----------------------
# 3.1 Make base dataframe
nrows <- 15038*22
GAMDF <- data.frame(CellID = rep(1:15038, 22))
GAMDF$TotalRichness <- rep(RichnessVec, 22)
GAMDF$OrderName <- NA
GAMDF$OrderRichness <- NA
GAMDF$Biome <- rep(BiomeVec, 22)
GAMDF$MAT <- rep(MAT, 22)
GAMDF$MAP <- rep(MAP, 22)

# 3.2 Add order names and order richness
start <- 1
end <- 15038
for(i in 1:22){
  ordername <- MossOrderNames[i]
  richlist <- MossOrderRichList[[i]]
  GAMDF$OrderName[start:end] <- ordername 
  GAMDF$OrderRichness[start:end] <- richlist
  start = end + 1
  end = start + 15037
}




