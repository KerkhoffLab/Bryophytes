#Bryophyte data processing
#Julia Eckberg and Jackie O'Malley, Summer 2019
#Additions by Hailey Napier, June 2020, July 2020

#Load packages
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

#Load data and source functions
SpeciesPresence <- read.csv("Data/SpeciesPresence_by_100km_Cell.csv")
ByGroup <- read.csv("Data/BryophytePhylogeny.csv")

source("Functions/BIEN_occurrence_higher_plant_group.R")
source("Functions/BIEN_taxonomy_higher_plant_group.R")

#Table of all bryophyte occurrences
BrySpecies <- BIEN_taxonomy_higher_plant_group("bryophytes", print.query = FALSE)
SpeciesPresence$Species <- gsub("_", " ", SpeciesPresence$Species)
BryophytePresence <- SpeciesPresence[SpeciesPresence$Species %in% unique(BrySpecies$scrubbed_species_binomial), ]

#Subset bryophytes into three groups
Mosses <- subset(ByGroup, Group=="Mosses")
Liverworts <- subset(ByGroup, Group=="Liverworts")
Hornworts <- subset(ByGroup, Group=="Hornworts")

#Rename columns
BrySpecies$Family <- BrySpecies$scrubbed_family
BrySpecies$Genus <- BrySpecies$scrubbed_genus
BrySpecies$Species <- BrySpecies$scrubbed_species_binomial
BrySpecies <- BrySpecies[,c(10,11,12)]
BrySpecies <- distinct(BrySpecies)

#Add group to bryophyte data by family
BrySpecies <- merge(BrySpecies, ByGroup, by = "Family", all.x = TRUE)

#Add group to presence data by species and remove duplicate entries and save
BryophytePresence <- merge(BryophytePresence, BrySpecies, by = "Species", all.x= TRUE)
BryophytePresence <- distinct(BryophytePresence)
saveRDS(BryophytePresence, file = "Data/OLD_BryophytePresence.rds")


####Summer 2020 Additions####
####Updated BryophytePresence (7/2020)####
BryophytePresence <- read.csv("Data/BryophytePresence_7.2.20(2).csv")

#Family & order names
OrderNames <- unique(BryophytePresence$Order)
OrderNames <- OrderNames[!is.na(OrderNames)]
NumberOrders <- length(OrderNames)
saveRDS(OrderNames, file = "Data/OrderNames.rds")

FamilyNames <- unique(BryophytePresence$Family)
FamilyNames <- as.vector(FamilyNames)
FamilyNames <- FamilyNames[!is.na(FamilyNames)]
NumberFamilies <- length(FamilyNames)
saveRDS(FamilyNames, file = "Data/FamilyNames.rds")

#Tally richness by cell and create richness vector and save
CellRichness <- tally(group_by(BryophytePresence, CellID))
colnames(CellRichness)[2] <- "Richness"
RichnessVec <- numeric(15038)
RichnessVec[CellRichness$CellID] <- CellRichness$Richness

#Save richness and presence data
saveRDS(CellRichness, file = "Data/CellRichness.rds")
saveRDS(RichnessVec, file = "Data/RichnessVec.rds")
saveRDS(BryophytePresence, file = "Data/BryophytePresence.rds")


##BryophyteDiversity.R##

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

#Stop for Bryophytes.rmd --------------------------------------------------------------


##Bryophytes.rmd##
#Load blank raster and cell richness data + extract cell IDs and create vector for all cells
#Change file for BetaMat and CellRichness depending on if you want to map bryophytes, mosses, liverworts, etc. 
BlankRas <-raster("Data/blank_100km_raster.tif")
BetaMat <- readRDS("Data/BetaMat.rds")
CellRichness <- readRDS("Data/CellRichness.rds")
CellID <- CellRichness$CellID
CellVec <- c(1:15038)

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


##SpeciesRanges.R##
#Range by species 
Range <- tally(group_by(BryophytePresence, Species))
Range <- merge(Range, BryophytePresence, by = "Species")

SpeciesRange <- subset(Range, select = c("n", "Species")) %>%
  group_by(Species) %>%
  summarize(Avg = median(n))
colnames(SpeciesRange) <- c("Species", "RangeAvg")


CellRange <- tally(group_by(BryophytePresence, Species))
CellRange <- merge(CellRange, BryophytePresence, by = "Species")

CellRange <- subset(CellRange, select = c("n", "CellID")) %>%
  group_by(CellID) %>%
  summarize(Avg = median(n))

CellRangeVec <- CellRange$CellID

BryophyteRange <- numeric(15038)
BryophyteRange[CellRange$CellID] <- CellRange$Avg 
BryophyteRange[which(BryophyteRange==0)]=NA
RangeRaster <- setValues(BlankRas, BryophyteRange)

saveRDS(SpeciesRange, file = "Data/SpeciesRange.rds")
saveRDS(CellRange, file = "Data/CellRange.rds")
saveRDS(RangeRaster, "Data/RangeRaster.rds")

#Stop for NullModelWithWorldClimData.R & SpreadingDye.R-------------------------------


#ONLY NEEDED FOR MAPPING & SHAPEFILE EXTRACTION

##Bryophytes.Rmd##
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

#Stop for MontaneFamilies.R-------------------------------------------------------------


##OneScaleFamRichMaps.R##
#Loop through families and tally richness for each family, store in a list
FamList <- list()
NumberFamilies <- length(FamilyNames)
for(i in 1:NumberFamilies){
  fam <- FamilyNames[i]
  FamList[[i]] <- subset(BryophytePresence, Family == fam)
}

FamRichList <- list()
FamPresList <- list()
for(i in 1:NumberFamilies){
  FamPresList[[i]] <- tally(group_by(FamList[[i]], CellID))
  names(FamPresList[[i]])[2] <- "Richness"
  FamRichList[[i]] <- numeric(15038)
  FamRichList[[i]][FamPresList[[i]]$CellID] <- FamPresList[[i]]$Richness
  FamRichList[[i]][which(FamRichList[[i]]==0)] = NA
}

#Loop through orders and tally richness for each order, store in a list
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

saveRDS(OrderRichList, file = "Data/OrderRichList.rds")
saveRDS(FamRichList, file = "Data/FamRichList.rds")


##Continents.R##
# libs
library(raster)
library(rgeos)
library(maptools)
require(rgdal)
require(rworldmap)

# Downloading data 
## from http://www.naturalearthdata.com/downloads/10m-physical-vectors/10m-physical-labels/
dir.create("./Data/MapOutlines/")

download.file("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_geography_regions_polys.zip",
              destfile = "./Data/MapOutlines/ne_10m_geography_regions_polys.zip")

setwd("./Data/MapOutlines/")
unzip("ne_10m_geography_regions_polys.zip")
setwd("../../")

# Reading data 
# North America
data(wrld_simpl)

subregion21 <- wrld_simpl[which(wrld_simpl@data$SUBREGION == 21), ]
subregion29 <- wrld_simpl[which(wrld_simpl@data$SUBREGION == 29), ]
subregion13 <- wrld_simpl[which(wrld_simpl@data$SUBREGION == 13), ]
subregion21_29_13 <- bind(subregion21,subregion29,subregion13)
plot(subregion21_29_13)
noGRL <- subregion21_29_13[which(subregion21_29_13@data$NAME!='Greenland'), ] 
noIslands <- noGRL[which(noGRL@data$AREA>500), ]
NorthAm <- gBuffer(noIslands,width=0.00001)

# 1.2 South America
wrld_simpl@data
region19 <- wrld_simpl[which(wrld_simpl@data$REGION == 19), ]
region19No21 <- region19[which(region19@data$SUBREGION != 21), ]
region19No21_29 <- region19No21[which(region19No21@data$SUBREGION != 29), ]
region19No21_29_13 <- region19No21_29[which(region19No21_29@data$SUBREGION != 13), ]
plot(region19No21_29_13)
noIslands <- region19No21_29_13[which(region19No21_29_13@data$AREA>500), ]
SouthAm <- gBuffer(noIslands,width=0.00001)

# Cropping to the New world
# North America
plot(NorthAm)
extent(NorthAm)
NorthAm <- crop(NorthAm, extent(-175,-22,-56,74))
plot(NorthAm)

# South America
plot(SouthAm)
extent(SouthAm)
SouthAm <- crop(SouthAm, extent(-175,-22,-56,74))
plot(SouthAm)


# Reprojecting 
## Change the projections to the BIEN ref proj (Lambert Azimuthal Equal Area projection)
crs_ref <- "+proj=laea +lat_0=15 +lon_0=-80 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
NorthAm_proj <- spTransform(NorthAm,crs_ref)
SouthAm_proj <- spTransform(SouthAm, crs_ref)


# Plots
plot(NorthAm_proj)
plot(SouthAm_proj)

# Writing shapefiles
setwd("./Data/MapOutlines/")
dir.create("./Continents/")
setwd("../../")

NorthAm_proj <- as(NorthAm_proj, "SpatialPolygonsDataFrame")
writeOGR(obj=NorthAm_proj, 
         dsn="./Data/MapOutlines/Continents/north_america.shp", 
         layer="north_america", 
         driver="ESRI Shapefile", 
         overwrite = TRUE)

SouthAm_proj <- as(SouthAm_proj, "SpatialPolygonsDataFrame")
writeOGR(obj=SouthAm_proj, 
         dsn="./Data/MapOutlines/Continents/south_america.shp", 
         layer="south_america", 
         driver="ESRI Shapefile", 
         overwrite = TRUE)


# Create raster including CellIDs for each cell
LongLatRaster <- setValues(BlankRas, CellVec)

# Create a vector of CellIDs for North and South America
NorthAm <- shapefile("Data/MapOutlines/Continents/north_america.shp")
NorthACells <- raster::extract(LongLatRaster, NorthAm, df = TRUE, cellnumbers = TRUE, weight = TRUE)
NorthAmericaVec <- NorthACells$cell

SouthAm <- shapefile("Data/MapOutlines/Continents/south_america.shp")
SouthACells <- raster::extract(LongLatRaster, SouthAm, df = T, cellnumbers = T,  weight = T)
SouthAmericaVec <- SouthACells$cell

saveRDS(NorthAmericaVec, "Data/NorthAmericaVec.rds")
saveRDS(SouthAmericaVec, "Data/SouthAmericaVec.rds")


##BiomeContinents.R##
# Load Data 
ContBiomeRichness <- readRDS("Data/BiomeRichness.rds")
BiomeNames <- readRDS("Data/BiomeNames.rds")

# Add Continent column to dataframe
ContBiomeRichness$Cont <- NA

for(i in 1:nrow(ContBiomeRichness)){
  cell = ContBiomeRichness$CellID[i]
  if(cell %in% NorthAmericaVec){
    ContBiomeRichness$Cont[i] <- "NorthAmerica"
  }else if(cell %in% SouthAmericaVec){
    ContBiomeRichness$Cont[i] <- "SouthAmerica"
  }
}

# Filter dataframe by continent and extract cell IDs for each biome, put in a list -------
# North America
NorthAmBiomeList <- list()
for(i in 1:length(BiomeNames)){
  b <- BiomeNames[i]
  vec <- ContBiomeRichness %>%
    filter(ContBiomeRichness$Cont == "NorthAmerica") %>%
    filter(Type == b)
  vec <- as.vector(vec$CellID)
  NorthAmBiomeList[[i]] <- vec
}

# South America
SouthAmBiomeList <- list()
for(i in 1:length(BiomeNames)){
  b <- BiomeNames[i]
  vec <- ContBiomeRichness %>%
    filter(ContBiomeRichness$Cont == "SouthAmerica") %>%
    filter(Type == b)
  vec <- as.vector(vec$CellID)
  SouthAmBiomeList[[i]] <- vec
}

#  Save lists
saveRDS(NorthAmBiomeList, "Data/NorthAmBiomeList.rds")
saveRDS(SouthAmBiomeList, "Data/SouthAmBiomeList.rds")


#Stop for order.alpha.comp.R, fam.alpha.com.R, ORange.R, OrdBiomeBP.R, 
    ##TreeMaps.R, and OrderBiomePlots.R------------------------------------------------


