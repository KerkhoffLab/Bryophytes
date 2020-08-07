# Bryophytes 2020 Data Processing
# Hailey Napier, August 2020
# Adapted from DataProcessing.R

# Load packages ----------------------------------------------------------------------
require(BIEN)
require(dplyr)
require(sp)
require(ape)
require(vegan)
require(reshape2)
require(raster)

require(rgeos)
require(maptools)
require(rgdal)
require(rworldmap)

# Load data
BryophytePresence <- read.csv("Data/BryophytePresence_7.2.20(2).csv")
BlankRas <-raster("Data/blank_100km_raster.tif")
RichnessRaster <- readRDS("Data/RichnessRaster.rds")
CellVec <- c(1:15038)
LongLatDF <- readRDS("Data/LongLatDF.rds")
RichnessRaster <- readRDS("Data/RichnessRaster.rds")

# Source function
source("Functions/ORange.R")

# 1.0 Bryophyte Presence Manipulation ------------------------------------------------
# Order Names
OrderNames <- unique(BryophytePresence$Order)
OrderNames <- OrderNames[!is.na(OrderNames)]
NumberOrders <- length(OrderNames)

# Tally richness by cell and create richness vector
CellRichness <- tally(group_by(BryophytePresence, CellID))
colnames(CellRichness)[2] <- "Richness"
RichnessVec <- numeric(15038)
RichnessVec[CellRichness$CellID] <- CellRichness$Richness

#Save richness and presence data
saveRDS(OrderNames, file = "Data/OrderNames.rds")
saveRDS(CellRichness, file = "Data/CellRichness.rds")
saveRDS(RichnessVec, file = "Data/RichnessVec.rds")
saveRDS(BryophytePresence, file = "Data/BryophytePresence.rds")


# 2.0 Biome Data ------------------------------------------------------------------------
# 2.1 Load biome data
# Load Biome Files
download.file(url = "https://github.com/susyelo/BIEN_FEE_paper/archive/Trait_phylo.zip", 
              destfile = "./Data/Biomes/BIEN_FEE_paper-Trait_phylo.zip")
dir.create("./Data/Biomes/")
setwd("./Data/Biomes/")
unzip("BIEN_FEE_paper-Trait_phylo.zip")
setwd("../../")

# Move the shapefiles
file.move("./Data/Biomes/BIEN_FEE_paper-Trait_phylo/data/processed/Olson_processed/Biomes_olson_projected.dbf", "./Data/Biomes/")
file.move("./Data/Biomes/BIEN_FEE_paper-Trait_phylo/data/processed/Olson_processed/Biomes_olson_projected.prj", "./Data/Biomes/")
file.move("./Data/Biomes/BIEN_FEE_paper-Trait_phylo/data/processed/Olson_processed/Biomes_olson_projected.shp", "./Data/Biomes/")
file.move("./Data/Biomes/BIEN_FEE_paper-Trait_phylo/data/processed/Olson_processed/Biomes_olson_projected.shx", "./Data/Biomes/")

# Delete the repository folder and .zip file
unlink("./Data/Biomes/BIEN_FEE_paper-Trait_phylo", recursive=TRUE)
unlink("./Data/Biomes/BIEN_FEE_paper-Trait_phylo.zip")

# 2.2 Make LongLatBetaRaster
#Code in section 2.2 written by Jackie O'Malley and Julia Eckberg
#Create occurrence by cell matrix by reshaping dataframe, then convert to presence-absence matrix
SpeciesCellID <- BryophytePresence[,c(1,4)]
melted <- melt(SpeciesCellID, id=c("Species", "CellID"), na.rm = TRUE)

SpeciesCellMatrix <- acast(melted, CellID~Species, margins=FALSE)
SpeciesCellMatrix[SpeciesCellMatrix > 0] <- 1

#Using betadiver to compute B-diversity using Jaccard similarity
#betadiver(help = TRUE) gives you indices
betamat <- betadiver(SpeciesCellMatrix, method = "j", order = FALSE, help = FALSE)

#Save species-cell matrix and beta diversity matrix
saveRDS(SpeciesCellMatrix, file="Data/SpeciesCellMatrix.rds")
saveRDS(betamat, file="Data/BetaMat.rds")

#Load blank raster and cell richness data + extract cell IDs and create vector for all cells
BlankRas <-raster("Data/blank_100km_raster.tif")
BetaMat <- readRDS("Data/BetaMat.rds")
CellID <- CellRichness$CellID

#Identify occupied cells that are adjacent to each occuppied cell + convert to vector
neighbor <- function(CellVec) {(adjacent(BlankRas, CellVec, directions=8, pairs=FALSE, target=CellID, sorted=TRUE, include=FALSE, id=FALSE))}
Neighbors <- lapply(CellVec, neighbor)
names(Neighbors) <- CellVec

bryneighbors <- Neighbors[CellID]
bryneighborvect <- unlist(lapply(bryneighbors, length))

#Make beta diversity matrix for all cells
BetaMat<-as.matrix(BetaMat)
row.names(BetaMat) <- CellID
names(BetaMat) <- CellID

saveRDS(BetaMat, file="Data/BetaMat.rds")
saveRDS(bryneighbors, file = "Data/bryneighbors.rds")
saveRDS(bryneighborvect, file="Data/bryneighborvect.rds")
saveRDS(CellID, file="Data/CellID.rds")

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

LongLatBetaVec <- rep(0, 15038)
LongLatBetaVec[Cell8]<-Beta8Vec
LongLatBetaVec[Cell7]<-Beta7Vec
LongLatBetaVec[LongLatBetaVec==0]<-NA
LongLatBetaVec <- 1-LongLatBetaVec

LongLatBetaRaster <- setValues(BlankRas, LongLatBetaVec)
LongLatBetaPoints<-rasterToPoints(LongLatBetaRaster)
LongLatBetaDF <- data.frame(LongLatBetaPoints)
colnames(LongLatBetaDF) <- c("Longitude", "Latitude", "Beta")

coordinates(LongLatBetaDF) <- ~Longitude+Latitude 
proj4string(LongLatBetaDF) <- CRS("+proj=utm +zone=10") 
BetaLongLat <- spTransform(LongLatBetaDF, CRS("+proj=longlat")) 
LongLatBetaDF <- data.frame(BetaLongLat)
LongLatBetaDF[c("Longitude", "Latitude", "Beta")]
saveRDS(LongLatBetaDF, file = "Data/LongLatBetaDF.rds")

BetaLongLat <- data.frame(BetaLongLat)
colnames(BetaLongLat) <- c("Beta", "Longitude", "Latitude")

saveRDS(LongLatBetaRaster, file="Data/LongLatBetaRaster.rds")

# 2.3 Find cells in each biome (each cell assigned to the biome that covers the center of the cell)
# Subset biome data
Coniferous_Forests <- subset(biomes_shp, biomes == "Coniferous_Forests")
Dry_Forest <- subset(biomes_shp, biomes == "Dry_Forest")
Mediterranean_Woodlands <- subset(biomes_shp, biomes == "Mediterranean_Woodlands")
Moist_Forest <- subset(biomes_shp, biomes == "Moist_Forest")
Savannas <- subset(biomes_shp, biomes == "Savannas")
Taiga <- subset(biomes_shp, biomes == "Taiga")
Temperate_Grasslands <- subset(biomes_shp, biomes == "Temperate_Grasslands")
Temperate_Mixed <- subset(biomes_shp, biomes == "Temperate_Mixed")
Tropical_Grasslands <- subset(biomes_shp, biomes == "Tropical_Grasslands")
Tundra <- subset(biomes_shp, biomes == "Tundra")
Xeric_Woodlands <- subset(biomes_shp, biomes == "Xeric_Woodlands")

# Make dataframes for each biome 
#Coniferous Forests
CenterCovConFor <- raster::extract(LongLatBetaRaster, Coniferous_Forests, df = TRUE, cellnumbers = TRUE, weight = FALSE)
colnames(CenterCovConFor) <- c("Type", "CellID", "Beta")
CenterCovConFor$Type <- "Coniferous_Forests"
CenterCovConForVec <- CenterCovConFor$CellID

#Dry Forest
CenterCovDryFor <- raster::extract(LongLatBetaRaster, Dry_Forest, df = TRUE, cellnumbers = TRUE, weight = FALSE)
colnames(CenterCovDryFor) <- c("Type", "CellID", "Beta")
CenterCovDryFor$Type <- "Dry_Forest"
CenterCovDryForVec <- CenterCovDryFor$CellID

#Mediterranean Woodlands
CenterCovMedWood <- raster::extract(LongLatBetaRaster, Mediterranean_Woodlands, df = TRUE, cellnumbers = TRUE, weight = FALSE)
colnames(CenterCovMedWood) <- c("Type", "CellID", "Beta")
CenterCovMedWood$Type <- "Mediterranean_Woodlands"
CenterCovMedWoodVec <- CenterCovMedWood$CellID

#Moist Forest
CenterCovMoistFor <- raster::extract(LongLatBetaRaster, Moist_Forest, df = TRUE, cellnumbers = TRUE, weight = FALSE)
colnames(CenterCovMoistFor) <- c("Type", "CellID", "Beta")
CenterCovMoistFor$Type <- "Moist_Forest"
CenterCovMoistForVec <- CenterCovMoistFor$CellID

#Savannas
CenterCovSavanna <- raster::extract(LongLatBetaRaster, Savannas, df = TRUE, cellnumbers = TRUE, weight = FALSE)
colnames(CenterCovSavanna) <- c("Type", "CellID", "Beta")
CenterCovSavanna$Type <- "Savannas"
CenterCovSavannaVec <- CenterCovSavanna$CellID

#Taiga
CenterCovTaiga <- raster::extract(LongLatBetaRaster, Taiga, df = TRUE, cellnumbers = TRUE, weight = FALSE)
colnames(CenterCovTaiga) <- c("Type", "CellID", "Beta")
CenterCovTaiga$Type <- "Taiga"
CenterCovTaigaVec <- CenterCovTaiga$CellID

#Temperate Grasslands
CenterCovTempGrass <- raster::extract(LongLatBetaRaster, Temperate_Grasslands, df = TRUE, cellnumbers = TRUE, weight = FALSE)
colnames(CenterCovTempGrass) <- c("Type", "CellID", "Beta")
CenterCovTempGrass$Type <- "Temperate_Grasslands"
CenterCovTempGrassVec <- CenterCovTempGrass$CellID

#Temperate Mixed
CenterCovTempMix <- raster::extract(LongLatBetaRaster, Temperate_Mixed, df = TRUE, cellnumbers = TRUE, weight = FALSE)
colnames(CenterCovTempMix) <- c("Type", "CellID", "Beta")
CenterCovTempMix$Type <- "Temperate_Mixed"
CenterCovTempMixVec <- CenterCovTempMix$CellID

#Tropical Grasslands
CenterCovTropGrass <- raster::extract(LongLatBetaRaster, Tropical_Grasslands, df = TRUE, cellnumbers = TRUE, weight = FALSE)
colnames(CenterCovTropGrass) <- c("Type", "CellID", "Beta")
CenterCovTropGrass$Type <- "Tropical_Grasslands"
CenterCovTropGrassVec <- CenterCovTropGrass$CellID

#Tundra
CenterCovTundra <- raster::extract(LongLatBetaRaster, Tundra, df = TRUE, cellnumbers = TRUE, weight = FALSE)
colnames(CenterCovTundra) <- c("Type", "CellID", "Beta")
CenterCovTundra$Type <- "Tundra"
CenterCovTundraVec <- CenterCovTundra$CellID

#Xeric Woodlands
CenterCovXericWood <- raster::extract(LongLatBetaRaster, Xeric_Woodlands, df = TRUE, cellnumbers = TRUE, weight = FALSE)
colnames(CenterCovXericWood) <- c("Type", "CellID", "Beta")
CenterCovXericWood$Type <- "Xeric_Woodlands"
CenterCovXericWoodVec <- CenterCovXericWood$CellID

#Bind biome dataframes
BiomeBetaCellsCenterCov <- bind_rows(CenterCovConFor, CenterCovDryFor, CenterCovMedWood,
                                     CenterCovMoistFor,CenterCovSavanna, CenterCovTaiga, 
                                     CenterCovTempGrass, CenterCovTempMix,CenterCovTropGrass,
                                     CenterCovTundra, CenterCovXericWood)

#Save dataframe
#weight = false; cells counted in biome only if biome polygon overlaps center of cell
saveRDS(BiomeBetaCellsCenterCov, file = "Data/BiomeBetaCellsCentCov.rds")


# 4.0 Biome Alpha Diversity Data -------------------------------------------------------------
# 4.1 Create dataframes for each biome with cell coordinates and richness
#Coniferous Forests
AlphaConFor <- raster::extract(RichnessRaster, Coniferous_Forests, df = TRUE, cellnumbers = TRUE)
colnames(AlphaConFor) <- c("Type", "CellID", "Alpha")
AlphaConFor$Type <- "Coniferous_Forests"
AlphaConForVec <- AlphaConFor$CellID
AlphaConFor <- merge(AlphaConFor, LongLatDF)
saveRDS(AlphaConForVec, "Data/Coniferous_ForestsVec.rds")

#Dry Forest
AlphaDryFor <- raster::extract(RichnessRaster, Dry_Forest, df = TRUE, cellnumbers = TRUE)
colnames(AlphaDryFor) <- c("Type", "CellID", "Alpha")
AlphaDryFor$Type <- "Dry_Forest"
AlphaDryForVec <- AlphaDryFor$CellID
AlphaDryFor <- merge(AlphaDryFor, LongLatDF)
saveRDS(AlphaDryForVec, "Data/Dry_ForestVec.rds")

#Mediterranean Woodlands
AlphaMedWood <- raster::extract(RichnessRaster, Mediterranean_Woodlands, df = TRUE, cellnumbers = TRUE)
colnames(AlphaMedWood) <- c("Type", "CellID", "Alpha")
AlphaMedWood$Type <- "Mediterranean_Woodlands"
AlphaMedWoodVec <- AlphaMedWood$CellID
AlphaMedWood <- merge(AlphaMedWood, LongLatDF)
saveRDS(AlphaMedWoodVec, "Data/Mediterranean_WoodlandsVec.rds")

#Moist Forest
AlphaMoistFor <- raster::extract(RichnessRaster, Moist_Forest, df = TRUE, cellnumbers = TRUE)
colnames(AlphaMoistFor) <- c("Type", "CellID", "Alpha")
AlphaMoistFor$Type <- "Moist_Forest"
AlphaMoistForVec <- AlphaMoistFor$CellID
AlphaMoistFor <- merge(AlphaMoistFor, LongLatDF)
saveRDS(AlphaMoistForVec, "Data/Moist_ForestVec.rds")

#Savannas
AlphaSavanna <- raster::extract(RichnessRaster, Savannas, df = TRUE, cellnumbers = TRUE)
colnames(AlphaSavanna) <- c("Type", "CellID", "Alpha")
AlphaSavanna$Type <- "Savannas"
AlphaSavannaVec <- AlphaSavanna$CellID
AlphaSavanna <- merge(AlphaSavanna, LongLatDF)
saveRDS(AlphaSavannaVec, "Data/SavannasVec.rds")

#Taiga
AlphaTaiga <- raster::extract(RichnessRaster, Taiga, df = TRUE, cellnumbers = TRUE)
colnames(AlphaTaiga) <- c("Type", "CellID", "Alpha")
AlphaTaiga$Type <- "Taiga"
AlphaTaigaVec <- AlphaTaiga$CellID
AlphaTaiga <- merge(AlphaTaiga, LongLatDF)
saveRDS(AlphaTaigaVec, "Data/TaigaVec.rds")

#Temperate Grasslands
AlphaTempGrass <- raster::extract(RichnessRaster, Temperate_Grasslands, df = TRUE, cellnumbers = TRUE)
colnames(AlphaTempGrass) <- c("Type", "CellID", "Alpha")
AlphaTempGrass$Type <- "Temperate_Grasslands"
AlphaTempGrassVec <- AlphaTempGrass$CellID
AlphaTempGrass <- merge(AlphaTempGrass, LongLatDF)
saveRDS(AlphaTempGrassVec, "Data/Temperate_GrasslandsVec.rds")

#Temperate Mixed
AlphaTempMix <- raster::extract(RichnessRaster, Temperate_Mixed, df = TRUE, cellnumbers = TRUE)
colnames(AlphaTempMix) <- c("Type", "CellID", "Alpha")
AlphaTempMix$Type <- "Temperate_Mixed"
AlphaTempMixVec <- AlphaTempMix$CellID
AlphaTempMix <- merge(AlphaTempMix, LongLatDF)
saveRDS(AlphaTempMixVec, "Data/Temperate_MixedVec.rds")

#Tropical Grasslands
AlphaTropGrass <- raster::extract(RichnessRaster, Tropical_Grasslands, df = TRUE, cellnumbers = TRUE)
colnames(AlphaTropGrass) <- c("Type", "CellID", "Alpha")
AlphaTropGrass$Type <- "Tropical_Grasslands"
AlphaTropGrassVec <- AlphaTropGrass$CellID
AlphaTropGrass <- merge(AlphaTropGrass, LongLatDF)
saveRDS(AlphaTropGrassVec, "Data/Tropical_GrasslandsVec.rds")

#Tundra
AlphaTundra <- raster::extract(RichnessRaster, Tundra, df = TRUE, cellnumbers = TRUE)
colnames(AlphaTundra) <- c("Type", "CellID", "Alpha")
AlphaTundra$Type <- "Tundra"
AlphaTundraVec <- AlphaTundra$CellID
AlphaTundra <- merge(AlphaTundra, LongLatDF)
saveRDS(AlphaTundraVec, "Data/TundraVec.rds")

#Xeric Woodlands
AlphaXericWood <- raster::extract(RichnessRaster, Xeric_Woodlands, df = TRUE, cellnumbers = TRUE)
colnames(AlphaXericWood) <- c("Type", "CellID", "Alpha")
AlphaXericWood$Type <- "Xeric_Woodlands"
AlphaXericWoodVec <- AlphaXericWood$CellID
AlphaXericWood <- merge(AlphaXericWood, LongLatDF)
saveRDS(AlphaXericWoodVec, "Data/Xeric_WoodlandsVec.rds")


# 4.4 Bind biome dataframes
BiomeRichness <- bind_rows(AlphaConFor, AlphaDryFor, AlphaMedWood,
                           AlphaMoistFor,AlphaSavanna, AlphaTaiga, 
                           AlphaTempGrass, AlphaTempMix,AlphaTropGrass,
                           AlphaTundra, AlphaXericWood)
saveRDS(BiomeRichness, file = "Data/BiomeRichness.rds")


# 4.2 Loop through orders and tally richness for each order, store in a list
OrderList <- list()
for(i in 1:NumberOrders){
  ord <- OrderNames[i]
  OrderList[[i]] <- subset(BryophytePresence, Order == ord)
}

OrderRichList <- list()
OrderPresList <- list()
for(i in 1:NumberOrders){
  OrderPresList[[i]] <- tally(group_by(OrderList[[i]], CellID))
  names(OrderPresList[[i]])[2] <- "Richness"
  OrderRichList[[i]] <- numeric(15038)
  OrderRichList[[i]][OrderPresList[[i]]$CellID] <- OrderPresList[[i]]$Richness
  OrderRichList[[i]][which(OrderRichList[[i]]==0)] = NA
}


# 4.3 OrderBiomeDF
# Includes orders and biomes, not separated by continent or hemisphere
o <- OrderNames[1]
b <- BiomeNames[1]

for(i in 2:NumberBiomes){
  b <- BiomeNames[i]
  tempdf <- data.frame(ORange(o, b))
  tempdf$CellID <- c(1:15038)
  tempdf$Biome <- b
  tempdf$Order <- o
  OrderBiomeDF <- bind_rows(OrderBiomeDF, tempdf)
}

for(i in 2:NumberOrders){
  o <- OrderNames[i]
  for(i in 1:NumberBiomes){
    b <- BiomeNames[i]
    tempdf <- data.frame(ORange(o,b))
    tempdf$CellID <- c(1:15038)
    tempdf$Biome <- b
    tempdf$Order <- o
    OrderBiomeDF <- bind_rows(OrderBiomeDF, tempdf)
  }
}
names(OrderBiomeDF)[1] <- "Alpha"
saveRDS(OrderBiomeDF, file = "Data/OrderBiomeDF.rds")


# 4.4 Subset richness data based on max alpha diversity
# max α > 100
OBRAbove100DF <- subset(OrderBiomeDF, 
                        OrderBiomeDF$Order=="Hypnales"|
                          OrderBiomeDF$Order=="Dicranales")
# max α 25-100
OBR25to100DF <- subset(OrderBiomeDF,
                       OrderBiomeDF$Order=="Bartramiales"|
                         OrderBiomeDF$Order=="Bryales"|
                         OrderBiomeDF$Order=="Grimmiales"|
                         OrderBiomeDF$Order=="Hookeriales"|
                         OrderBiomeDF$Order=="Jungermanniales"|
                         OrderBiomeDF$Order=="Orthotrichales"|
                         OrderBiomeDF$Order=="Porellales"|
                         OrderBiomeDF$Order=="Pottiales")
# max α 10-25
OBR10to25DF <- subset(OrderBiomeDF,
                      OrderBiomeDF$Order=="Funariales"|
                        OrderBiomeDF$Order=="Hedwigiales"|
                        OrderBiomeDF$Order=="Marchantiales"|
                        OrderBiomeDF$Order=="Metzgeriales"|
                        OrderBiomeDF$Order=="Polytrichales"|
                        OrderBiomeDF$Order=="Sphagnales")
# max α < 10
OBRBelow10DF <- subset(OrderBiomeDF,
                       OrderBiomeDF$Order!="Hypnales"&
                         OrderBiomeDF$Order!="Dicranales"&
                         OrderBiomeDF$Order!="Bartramiales"&
                         OrderBiomeDF$Order!="Bryales"&
                         OrderBiomeDF$Order!="Grimmiales"&
                         OrderBiomeDF$Order!="Hookeriales"&
                         OrderBiomeDF$Order!="Jungermanniales"&
                         OrderBiomeDF$Order!="Orthotrichales"&
                         OrderBiomeDF$Order!="Porellales"&
                         OrderBiomeDF$Order!="Pottiales"&
                         OrderBiomeDF$Order!="Funariales"&
                         OrderBiomeDF$Order!="Hedwigiales"&
                         OrderBiomeDF$Order!="Marchantiales"&
                         OrderBiomeDF$Order!="Metzgeriales"&
                         OrderBiomeDF$Order!="Polytrichales"&
                         OrderBiomeDF$Order!="Sphagnales")


# 4.5 Save data
saveRDS(OrderRichList, file = "Data/OrderRichList.rds")
saveRDS(BiomeRich, "Data/BiomeRich.rds")
saveRDS(OrderBiomeDF, file = "Data/OrderBiomeDF.rds")

saveRDS(OBRAbove100DF, "Data/OBRAbove100DF.rds")
saveRDS(OBR25to100DF, "Data/OBR25to100DF.rds")
saveRDS(OBR10to25DF, "Data/OBR10to25DF.rds")
saveRDS(OBRBelow10DF, "Data/OBRBelow10DF.rds")


# 5.0 Circle plot data
# 5.1 Create a vector containing all of the biomes indexed by CellID
BiomeCellsVec <- as.vector(rep(NA, 15038))
for(i in 1:length(BiomeNames)){
  biome <- BiomeNames[i]
  biomevec <- ORange(range = biome)
  for(j in 1:length(biomevec)){
    rich <- biomevec[j]
    if(!is.na(rich)){
      BiomeCellsVec[j] <- biome
    }
  }
}
BiomeCellsVec <- as.factor(BiomeCellsVec)

# 5.2 Make BiomeCellsDF
BiomeCellsDF <- data.frame(CellID = 1:15038)
BiomeCellsDF$Biome <- BiomeCellsVec

# 5.3 Filter BryophytePresence by species and find cell with highest richness value
SpeciesNames <- unique(BryophytePresence$Species)
SpeciesBiomes <- data.frame("Species" = SpeciesNames)
SpeciesBiomes$Biome <- NA
for(i in 1:length(SpeciesNames)){
  species <- SpeciesNames[i]
  df <- BryophytePresence %>% 
    filter(BryophytePresence$Species == species)
  spcell <- as.vector(df$CellID)
  biomecells <- BiomeCellsVec[spcell]
  biomecells <- biomecells[complete.cases(biomecells)]
  biome <- names(sort(summary(biomecells)))[1]
  SpeciesBiomes$Biome[SpeciesBiomes$Species == species] <- biome
}

# 5.4 Filter BryophytePresence by species and find total abundance 
SpeciesNames <- unique(BryophytePresence$Species)
SpecAb <- as.vector(rep(NA, length(SpeciesNames)))

SpBiMat <- matrix(NA, length(SpeciesNames), length(BiomeNames))
rownames(SpBiMat) <- SpeciesNames
colnames(SpBiMat) <- BiomeNames

for(i in 1:length(SpeciesNames)){
  species <- SpeciesNames[i]
  df <- BryophytePresence %>% 
    filter(BryophytePresence$Species == species)
  tot <- nrow(df)
  SpecAb[i] <- tot
  for(j in 1:length(BiomeNames)){
    spcell <- as.vector(df$CellID)
    biome <- BiomeNames[j]
    biomedf <- BiomeCellsDF %>%
      filter(BiomeCellsDF$Biome == biome) %>%
      filter(CellID %in% spcell)
    ab <- nrow(biomedf)
    SpBiMat[species, biome] <- ab
  }
}

# 5.5 Make OrderSpeciesList
OrderSpeciesList <- list()
for(i in 1:length(OrderNames)){
  order <- OrderNames[i]
  orddf <- BryophytePresence %>%
    filter(BryophytePresence$Order == order)
  specvec <- unique(orddf$Species)
  OrderSpeciesList[[i]] <- specvec
}

# 5.6 MossOrderSpeciesList
MossOrderSpeciesList <- list()
for(i in 1:length(MossOrderNames)){
  order <- MossOrderNames[i]
  orddf <- MossPresence %>%
    filter(MossPresence$Order == order)
  specvec <- unique(orddf$Species)
  MossOrderSpeciesList[[i]] <- specvec
}

# 5.7 Make matrix for all bryophytes plot
# 5.7.1 Make empty matrix 
CircleMatAll <- matrix(NA, length(OrderNames), length(BiomeNames))
rownames(CircleMatAll) <- OrderNames
colnames(CircleMatAll) <- BiomeNames

# 5.7.2 Fill matrix so it counts species based on top biome (moss)
for(h in 1:length(OrderNames)){
  order <- OrderNames[h]
  speclist <- OrderSpeciesList[[h]]
  valvec <- as.vector(rep(0, length(BiomeNames)))
  for(i in 1:length(speclist)){
    species <- speclist[i]
    biomenumcells <- vector()
    for(j in 1:length(BiomeNames)){
      biome <- BiomeNames[j]
      ab <- SpBiMat[species,biome]
      biomenumcells[j] <- ab
    }
    biomeindex <- which(biomenumcells == max(biomenumcells, na.rm = T))
    valvec[biomeindex] <- valvec[biomeindex] + 1
  }
  for(k in 1:length(valvec)){
    val <- valvec[k]
    biome <- BiomeNames[k]
    CircleMatAll[order, biome] <- val
  }
}

for(i in 1:length(BiomeNames)){
  biome <- BiomeNames[i]
  for(j in 1:length(OrderNames)){
    order <- OrderNames[j]
    speclist <- OrderSpeciesList[[j]]
    valvec <- as.vector(rep(0, length(BiomeNames)))
  }
}

# 5.7.3 Save matrix
saveRDS(CircleMatAll, "Data/CircleMatAll.rds")

# 5.8 Make matrix for moss only plot
# 5.8.1 Make empty matrix 
CircleMatAllMoss <- matrix(NA, length(MossOrderNames), length(BiomeNames))
rownames(CircleMatAllMoss) <- MossOrderNames
colnames(CircleMatAllMoss) <- BiomeNames

# 5.8.2 Fill matrix so it counts species based on top biome (moss)
for(h in 1:length(MossOrderNames)){
  order <- MossOrderNames[h]
  speclist <- MossOrderSpeciesList[[h]]
  valvec <- as.vector(rep(0, length(BiomeNames)))
  for(i in 1:length(speclist)){
    species <- speclist[i]
    biomenumcells <- vector()
    for(j in 1:length(BiomeNames)){
      biome <- BiomeNames[j]
      ab <- SpBiMat[species,biome]
      biomenumcells[j] <- ab
    }
    biomeindex <- which(biomenumcells == max(biomenumcells, na.rm = T))
    valvec[biomeindex] <- valvec[biomeindex] + 1
  }
  for(k in 1:length(valvec)){
    val <- valvec[k]
    biome <- BiomeNames[k]
    CircleMatAllMoss[order, biome] <- val
  }
}

for(i in 1:length(BiomeNames)){
  biome <- BiomeNames[i]
  for(j in 1:length(MossOrderNames)){
    order <- MossOrderNames[j]
    speclist <- MossOrderSpeciesList[[j]]
    valvec <- as.vector(rep(0, length(BiomeNames)))
  }
}

# 5.8.3 Save matrix
saveRDS(CircleMatAllMoss, "Data/CircleMatAllMoss.rds")



######### CONGRATULATIONS! NOW YOU HAVE ALL OF THE DATA YOU NEED TO RUN BRYOPHYTES2020.RMD! #########
