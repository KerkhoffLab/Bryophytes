# Bryophytes 2020 Data Processing
# Hailey Napier, August 2020
# Adapted from DataProcessing.R

# Load packages ----------------------------------------------------------------
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


# 1.0 Bryophyte Presence Manipulation ------------------------------------------

# Load Data
BryophytePresence <- read.csv("Data/BryophytePresence_7.2.20(2).csv")

# Order Names
OrderNames <- unique(BryophytePresence$Order)
OrderNames <- OrderNames[!is.na(OrderNames)]
NumberOrders <- length(OrderNames)

# Family Names
FamilyNames <- unique(BryophytePresence$Family)
FamilyNames <- as.vector(FamilyNames)
FamilyNames <- FamilyNames[!is.na(FamilyNames)]
NumberFamilies <- length(FamilyNames)

# Tally richness by cell and create richness vector
CellRichness <- tally(group_by(BryophytePresence, CellID))
colnames(CellRichness)[2] <- "Richness"
RichnessVec <- numeric(15038)
RichnessVec[CellRichness$CellID] <- CellRichness$Richness

#Save richness and presence data
saveRDS(OrderNames, file = "Data/OrderNames.rds")
saveRDS(FamilyNames, file = "Data/FamilyNames.rds")
saveRDS(CellRichness, file = "Data/CellRichness.rds")
saveRDS(RichnessVec, file = "Data/RichnessVec.rds")
saveRDS(BryophytePresence, file = "Data/BryophytePresence.rds")


# 2.0 Biome Data ------------------------------------------------------------------
# Load blank raster and richness/presence data 
BlankRas <-raster("Data/blank_100km_raster.tif")
RichnessVec <- readRDS("Data/RichnessVec.rds")
BryophytePresence <- readRDS("Data/BryophytePresence.rds")

LongLatBetaRaster <- readRDS("Data/LongLatBetaRaster.rds")
RichnessRaster <- readRDS("Data/RichnessRaster.rds")
CellVec <- c(1:15038)

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

# 1.3 Delete the repository folder and .zip file
unlink("./Data/Biomes/BIEN_FEE_paper-Trait_phylo", recursive=TRUE)
unlink("./Data/Biomes/BIEN_FEE_paper-Trait_phylo.zip")





