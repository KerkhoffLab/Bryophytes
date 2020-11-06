# Moss Linear Regression Models
# Kathryn Dawdy
# November 2020


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

#Truly not sure if anything below works
#LM with biomes + topo (not interaction terms)
mosslm_KD <- lm(log1p(TotalRichness) ~ log1p(MAT) + log1p(MAP) + Biome + 
                         Topo, data = LMDF2)
summary(mosslm_KD)
plot(mosslm_KD)

#LM with biomes + topo (interaction terms?)
mosslm_KD_1 <- lm(log1p(TotalRichness) ~ log1p(MAT) + log1p(MAP) + 
                  Biome*log1p(MAT) + Biome*log1p(MAP) +
                  Topo*log1p(MAT) + Topo*log1p(MAP), data = LMDF2)
summary(mosslm_KD_1)
plot(mosslm_KD_1)