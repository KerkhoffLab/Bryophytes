#Mapping Mosses
#Make sure to run DataLoading.R and DataProcessing.R first

#Load packages
require(BIEN)
require(ape)
require(maps) 
require(dplyr)
require(maptools)
require(raster)
require(dismo)
require(sp)
require(rgdal)
require(mapdata)
require(mapproj)


#Plot bryophyte richness
RichnessVector<-Bryophyte_Richness
RichnessVector[which(RichnessVector==0)]=NA
RichnessRaster <- setValues(BIENblank, RichnessVector)
plot(RichnessRaster, axes = F)

#Tally
require(dplyr)
MossTally <- tally(group_by(MossPresence, CellID))
colnames(MossTally)[2] <- "Richness"

#Moss richness
Moss_Richness <- numeric(15038)
Moss_Richness[MossTally$CellID] <- MossTally$Richness
 
#Plot moss richness
MossRichnessRaster <- setValues(BIENblank, Moss_Richness)
plot(MossRichnessRaster)
MossRichnessVector<-Moss_Richness
MossRichnessVector[which(MossRichnessVector==0)]=NA
MossRichnessRasterNA <- setValues(BIENblank, MossRichnessVector)
plot(MossRichnessRasterNA, axes = F)
 


#Mapping Liverworts

#Tally
require(dplyr)
LiverwortTally <- tally(group_by(LiverwortPresence, CellID))
colnames(LiverwortTally)[2] <- "Richness"
 
#Liverwort richness
Liverwort_Richness <- numeric(15038)
Liverwort_Richness[LiverwortTally$CellID] <- LiverwortTally$Richness

#Plot liverwort richness
LiverwortRichnessRaster <- setValues(BIENblank, Liverwort_Richness)
plot(LiverwortRichnessRaster)
LiverwortRichnessVector<-Liverwort_Richness
LiverwortRichnessVector[which(LiverwortRichnessVector==0)]=NA
LiverwortRichnessRasterNA <- setValues(BIENblank, LiverwortRichnessVector)
plot(LiverwortRichnessRasterNA, axes = F)
 

#Mapping Hornworts and Liverworts

#Tally
require(dplyr)
HLTally <- tally(group_by(HLPresence, CellID))
colnames(HLTally)[2] <- "Richness"
 
#Hornwort and liverwort richness
HL_Richness <- numeric(15038)
HL_Richness[HLTally$CellID] <- HLTally$Richness

#Plot HL richness
HLRichnessRaster <- setValues(BIENblank, HL_Richness)
plot(HLRichnessRaster)
HLRichnessVector<-HL_Richness
HLRichnessVector[which(HLRichnessVector==0)]=NA
HLRichnessRasterNA <- setValues(BIENblank, HLRichnessVector)
plot(HLRichnessRasterNA, axes = F)
 


#Map by group
source('~/Documents/Bryophytes/Functions/group.map.R')
group.map("Mosses")
group.map("Hornworts")
group.map("Liverworts")

#Map by species or family:
source('~/Documents/Bryophytes/Functions/species.map.R')
source('~/Documents/Bryophytes/Functions/family.map.R')
 

#Create dataset showing range
Range <- tally(group_by(BryophytePresence, Species))
Filter <- merge(Range, BryophytePresence, by = "Species")
 
#Find median of CellIDs
Filter2 <- subset(Filter, select = c("n", "CellID")) %>%
  group_by(CellID) %>%
  summarize(Avg = median(n))

#Bryophyte range map
BryophyteRange <- numeric(15038)
BryophyteRange[Filter2$CellID] <- Filter2$Avg
RangeVector<-BryophyteRange
RangeVector[which(RangeVector==0)]=NA
RangeRaster <- setValues(BIENblank, RangeVector)
plot(RangeRaster)
 