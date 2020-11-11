# Moss Linear Regression Models
# Kathryn Dawdy
# November 2020

# Load packages
library(dplyr)
library(raster)
library(ggplot2)

# Load data
LMDF <- readRDS("Data/LMDF.rds")
LMDF2 <- readRDS("Data/LMDF2.rds")
LMDF3 <- readRDS("Data/LMDF3.rds")
MossRichnessRaster <- readRDS("Data/MossRichnessRaster.rds")
nw_mount <- shapefile("Data/MapOutlines/Mountains/Koeppen-Geiger_biomes.shp")

#Make LMDF with montane/lowland column
#First run MossLM.R (as it is on 11/6/2020 !)
saveRDS(LMDF, "Data/LMDF.rds")
LMDF <- readRDS("Data/LMDF.rds")
  
#Extract cells in montane regions
AlphaMountLM <- raster::extract(MossRichnessRaster, nw_mount, df = TRUE, cellnumbers = TRUE)
colnames(AlphaMountLM) <- c("Topo", "CellID", "Alpha")
AlphaMountLM$Topo <-"Montane"
AlphaMountLM$Alpha <- NULL

#Join LMDF and AlphaMountLM by CellID
LMDF2 <- full_join(LMDF, AlphaMountLM, by="CellID")

#Make non-montane cells lowland
LMDF2$Topo[is.na(LMDF2$Topo)] <- "Lowland"
saveRDS(LMDF2, "Data/LMDF2.rds")

#Make richness values of 0 into NAs
LMDF3 <- LMDF2
LMDF3$TotalRichness[which(LMDF3$TotalRichness==0)] <- NA
saveRDS(LMDF3, "Data/LMDF3.rds")


#Playing around to get comfortable + comparing LMDF2 and LMDF3
#Simple plots of richness as a function of predictor variables
Rich_by_MAT_2 <- plot(TotalRichness~MAT, data=LMDF2)
Rich_by_MAT <- plot(TotalRichness~MAT, data=LMDF3) #Yeah, I'm gonna use LMDF3 since it removes richness values of 0
Rich_by_MAP <- plot(TotalRichness~MAP, data=LMDF3)

#Log transform axes
logRich_by_MAT <- plot(log1p(TotalRichness)~MAT, data=LMDF3)
logRich_by_MAP <- plot(log1p(TotalRichness)~MAP, data=LMDF3)

Rich_by_logMAT <- plot(TotalRichness~log1p(MAT), data=LMDF3)
Rich_by_logMAP <- plot(TotalRichness~log1p(MAP), data=LMDF3)

logRich_by_logMAT <- plot(log1p(TotalRichness)~log1p(MAT), data=LMDF3)
logRich_by_logMAP <- plot(log1p(TotalRichness)~log1p(MAP), data=LMDF3)

#Generate linear regressions for each variable
lm_Rich_by_MAT <- lm(TotalRichness~MAT, data=LMDF3)
summary(lm_Rich_by_MAT)
plot(TotalRichness~MAT, data=LMDF3)
abline(lm_Rich_by_MAT, col="blue")

lm_Rich_by_MAP <- lm(TotalRichness~MAP, data=LMDF3)
summary(lm_Rich_by_MAP)
plot(TotalRichness~MAP, data=LMDF3)
abline(lm_Rich_by_MAP, col="blue")

lm_Rich_by_MAT_MAP <- lm(TotalRichness~MAT + MAP, data=LMDF3)
summary(lm_Rich_by_MAT_MAP) #Adjusted R-squared:  0.07607 

lm_log_Rich_MAT_MAP <- lm(log1p(TotalRichness) ~ log1p(MAT) + log1p(MAP), 
                          data=LMDF3)
summary(lm_log_Rich_MAT_MAP) #Adjusted R-squared:  0.1128 

lm_Biome <- lm(log1p(TotalRichness) ~ log1p(MAT) + log1p(MAP) + Biome,
                   data=LMDF3)
summary(lm_Biome) #Adjusted R-squared:  0.2936

lm_Biome_Topo <- lm(log1p(TotalRichness) ~ log1p(MAT) + log1p(MAP) + Biome + Topo,
                    data=LMDF3)
summary(lm_Biome_Topo) #Adjusted R-squared:  0.3767 

lm_Biome_Topo_multiplied <- lm(log1p(TotalRichness) ~ log1p(MAT) + log1p(MAP) + 
                                 Biome*log1p(MAT) + Biome*log1p(MAP) +
                                 Topo*log1p(MAT) + Topo*log1p(MAP), 
                               data = LMDF3)
summary(lm_Biome_Topo_multiplied) #Adjusted R-squared:  0.5012 

#Now try order richness
lm_OrdRich_by_MAT_MAP <- lm(OrderRichness~MAT + MAP, data=LMDF3)
summary(lm_OrdRich_by_MAT_MAP) #Adjusted R-squared:  0.005792



#Truly not sure if anything below works
#LM with biomes + topo (not interaction terms)
mosslm_1 <- lm(log1p(TotalRichness) ~ log1p(MAT) + log1p(MAP) + Biome + 
                         Topo, data = LMDF2)
summary(mosslm_1)
plot(mosslm_1)

#LM with biomes + topo (interaction terms?)
mosslm_2 <- lm(log1p(TotalRichness) ~ log1p(MAT) + log1p(MAP) + 
                  Biome*log1p(MAT) + Biome*log1p(MAP) +
                  Topo*log1p(MAT) + Topo*log1p(MAP), data = LMDF2)
summary(mosslm_2)
plot(mosslm_2)