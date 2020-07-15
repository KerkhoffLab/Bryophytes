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


LongLatBetaVec <- rep(0, 15038)
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

