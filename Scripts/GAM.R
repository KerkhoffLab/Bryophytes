# GAM
# Testing out the moss generalized additive model
# Hailey Napier
# October 2020

# 0.0 FIRST -------------------------------------
# 0.1 Load Packages
library(dplyr)
library(raster)
library(mgcv)
library(ggplot2)

# 0.2 Load Data
MossOrderRichList <- readRDS("Data/MossOrderRichList.rds")
MossOrderNames <- readRDS("Data/MossOrderNames.rds")
RichnessVec <- readRDS("Data/RichnessVec.rds")
BiomeCells <- readRDS("Data/BiomeCellsClean.rds")
LongLatDF <- readRDS("Data/LongLatDF.rds")


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

# 3.3 Add latitude
Lat <- as.vector(LongLatDF$Latitude)
GAMDF$Lat <- rep(Lat, 22)


# 4.0 Test GAM function --------------------------
# 4.1 One variable: MAP
mossgam1 <- gam(TotalRichness ~ s(MAP), data = GAMDF, method = "REML")
# number of basis functions
coef(mossgam1)
# smoothing parameter
mossgam1$sp
plot.gam(mossgam1, residuals = T, pch = 1)

# 4.2 Two variables: MAP & MAT
mossgam2 <- gam(TotalRichness~s(MAT) + s(MAP), data = GAMDF, method = "REML")
plot.gam(mossgam2, residuals = T, pch = 1)

summary(mossgam2)

# 4.3 MAP, with MAP NAs removed so it will plot with data
MAPDFnoNA <- GAMDF %>%
  filter(is.na(MAP) == FALSE)
MAPDFnoNA

mossgam3 <- gam(TotalRichness~s(MAP), data = MAPDFnoNA, method = "REML")
plot.gam(mossgam3, residuals = T, pch = 1)


#plot using ggpplot
p <- predict.gam(mossgam3, type="lpmatrix", na.action = na.pass)
beta <- coef(mossgam3)[grepl("MAP", names(coef(mossgam3)))]
s <- p[,grepl("MAP", colnames(p))] %*% beta
ggplot(data=cbind.data.frame(s, MAPDFnoNA$MAP), aes(x=MAPDFnoNA$MAP, y=s)) + geom_line()

 ggplot(data = cbind.data.frame(s,MAPDFnoNA), aes(Lat, TotalRichness)) + 
  geom_point(shape = 16, 
             size = 5, 
             alpha=0.5, 
             color = "gray40") +
  geom_line(aes(y = s)) + 
  ylab("Species Richness") + 
  xlab("Latitude") + 
  theme_minimal() 

 # 4.4 Test MAP&MAT GAM with log link function
mossgam4 <- gam(TotalRichness~s(MAP) + s(MAT), data = GAMDF, family = gaussian(link = "log"))
summary(mossgam4)

