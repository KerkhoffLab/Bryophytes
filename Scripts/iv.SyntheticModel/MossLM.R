# Linear Regression Model
# Hailey Napier
# November 2020

# 0.0 FIRST -------------------------------------
# 0.1 Load Packages
library(dplyr)
library(raster)
library(ggplot2)
library(devtools)
devtools::install_github("cardiomoon/ggiraphExtra", force = T)
library(ggiraphExtra)

# 0.2 Load Data
MossOrderRichList <- readRDS("Data/MossOrderRichList.rds")
  # Find in MossPlotData.R
MossOrderNames <- readRDS("Data/MossOrderNames.rds")
RichnessVec <- readRDS("Data/RichnessVec.rds")
  # Find in DataProcessing.R
BiomeCells <- readRDS("Data/BiomeCellsClean.rds")
  # Find BiomeBetaCellsClean.rds in BiomeBetaCells.R
  # Run: BiomeBetaCellsClean$Beta <- NULL
       # BiomeCells <- BiomeBetaCellsClean
LongLatDF <- readRDS("Data/LongLatDF.rds")
  # Find in DataProcessing2020.R
RangeRaster <- readRDS("Data/RangeRaster.rds")
MossRichnessRaster <- readRDS("Data/MossRichnessRaster.rds")


# 1.0 Get WorldClim data ------------------------
# Adapted from WorldClim.R
worldclim <- raster::getData('worldclim', var='bio', res=10)
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
LMDF <- data.frame(CellID = rep(1:15038, 22))
LMDF$TotalRichness <- rep(RichnessVec, 22)
LMDF$OrderName <- NA
LMDF$OrderRichness <- NA
LMDF$Biome <- rep(BiomeVec, 22)
LMDF$MAT <- rep(MAT, 22)
LMDF$MAP <- rep(MAP, 22)

# 3.2 Add order names and order richness
start <- 1
end <- 15038
for(i in 1:22){
  ordername <- MossOrderNames[i]
  richlist <- MossOrderRichList[[i]]
  LMDF$OrderName[start:end] <- ordername 
  LMDF$OrderRichness[start:end] <- richlist
  start = end + 1
  end = start + 15037
}

# 3.3 Add latitude
Lat <- as.vector(LongLatDF$Latitude)
LMDF$Lat <- rep(Lat, 22)

saveRDS(LMDF, "Data/LMDF.rds")

# 3.4 Extract and add mountain/lowland 
#Extract cells in montane regions
AlphaMountLM <- raster::extract(MossRichnessRaster, nw_mount, df = TRUE, cellnumbers = TRUE)
colnames(AlphaMountLM) <- c("Topo", "CellID", "Alpha")
AlphaMountLM$Topo <-"Montane"
AlphaMountLM$Alpha <- NULL

AlphaMountLM
saveRDS(AlphaMountLM, "Data/AlphaMountLM.rds")

#Get rid of duplicate cells in AlphaMountLM
dupes <- AlphaMountLM$CellID[which(duplicated(AlphaMountLM) == T)]
dupes <- unique(dupes)

for(i in dupes){
  dupedcells <- which(AlphaMountLM$CellID == i)
  if(length(dupedcells) == 2){
    AlphaMountLM$CellID[dupedcells[1]] <- NA
  }else if(length(dupedcells) == 3)
    AlphaMountLM$CellID[dupedcells[2]] <- NA
  AlphaMountLM$CellID[dupedcells[1]] <- NA
}

AlphaMountLM <- AlphaMountLM %>%
  filter(!is.na(AlphaMountLM$CellID))


#Join LMDF and AlphaMountLM by CellID
LMDF2 <- full_join(LMDF, AlphaMountLM, by="CellID")

#Make non-montane cells lowland
LMDF2$Topo[is.na(LMDF2$Topo)] <- "Lowland"
saveRDS(LMDF2, "Data/LMDF2.rds")

#Make richness values of 0 into NAs
LMDF3 <- LMDF2
LMDF3$TotalRichness[which(LMDF3$TotalRichness==0)] <- NA
saveRDS(LMDF3, "Data/LMDF3.rds")


# 4.0 Make a histogram of richness values ----------
# To see if we have a normal distribution of richness
NoZeroRichVec <- RichnessVec[RichnessVec != 0]
NoZeroRichVec
hist(NoZeroRichVec)
# it's not a normal distribution (surprise surprise), so log transform 
hist(log(NoZeroRichVec))


# 5.0 Make linear models ---------------------------
# 5.1 Linear model with MAT + MAP with raw data
mosslm1 <- lm(TotalRichness ~ MAT + MAP, data = LMDF)
summary(mosslm1)
plot(mosslm1)

# 5.2 Linear model with MAT + MAP with log transformed data
mosslm2 <- lm(log1p(TotalRichness) ~ log1p(MAT) + log1p(MAP), data = LMDF)
summary(mosslm2)
plot(mosslm2)

# 5.3 Linear model with log(MAT) + log(MAP) + Biomes
mosslm3 <- lm(log1p(TotalRichness) ~ log1p(MAT) + log1p(MAP) + Biome, data = LMDF)
summary(mosslm3)
plot(mosslm3)

### ^^^ The LMs above use data with 0s instead of NAs, so not super accurate ^^^

### vvv LMs below have NAs instead of 0s vvv

# 5.4 Linear model with log(MAT) + log(MAP) + Biomes
mosslm4 <- lm(log1p(TotalRichness) ~ log1p(MAT) + log1p(MAP) + Biome, data = LMDF3)
summary(mosslm4)

# 5.5 Linear model with interaction effect
mosslm5 <- lm(log1p(TotalRichness) ~ log1p(MAT) + log1p(MAP) + 
                 Biome*log1p(MAT) + Biome*log1p(MAP) +
                 Topo*log1p(MAT) + Topo*log1p(MAP), data = LMDF3)
#so many terms!
summary(mosslm5)
plot(mosslm5)

# 5.6 Linear model with orders ~ MAT (log transform)
mosslm6 <- lm(log1p(OrderRichness) ~ log1p(MAT), data = LMDF3)
summary(mosslm6)

# 5.7 Linear model with orders ~ MAT, MAP, Biomes, Topo (log transform and interaction effect)
mosslm7 <- lm(log1p(OrderRichness) ~ log1p(MAT) + log1p(MAP) + 
                Biome*log1p(MAT) + Biome*log1p(MAP) + 
                Topo*log1p(MAT) + Topo*log1p(MAT), data = LMDF3)
summary(mosslm7)

# 5.8 TotalRichness with MAT and MAP (log transformed)
LMDF4 <- LMDF3 %>%
  select(c(MAT, MAP, TotalRichness))

mosslm8 <- lm(log1p(TotalRichness) ~ log1p(MAT) + log1p(MAP), data = LMDF4)


# 6.0 Try plotting with ggPredict
ggPredict(mosslm1, interactive = T)
