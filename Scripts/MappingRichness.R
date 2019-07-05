#Mapping Mosses
#Uses data from DataProcessing.R

#Load packages
require(BIEN)
require(maps) 
require(dplyr)
require(maptools)
require(raster)
require(sp)
require(rgdal)
require(mapdata)
require(mapproj)

require(wesanderson)
require(ggplot2)
require(rasterVis)

#Load blank raster and richness/presence data
BlankRas <-raster("Data/blank_100km_raster.tif")
RichnessVec <- readRDS("Data/RichnessVec.rds")
BryophytePresence <- readRDS("Data/BryophytePresence.rds")

#Set theme and colors for gplots
cols <- rev(wes_palette("Zissou1", 500, type = "continuous"))
theme_set(theme_void())


#BRYOPHYTE RICHNESS
RichnessVec[which(RichnessVec==0)]=NA
RichnessRaster <- setValues(BlankRas, RichnessVec)
gplot(RichnessRaster, maxpixels=15038) + geom_raster(aes(fill = value))+ scale_fill_gradientn(colours=cols, na.value="transparent") +
  coord_equal() 


#MOSS RICHNESS
MossPresence <- subset(BryophytePresence, BryophytePresence$Group=="Mosses")
MossPresence <- tally(group_by(MossPresence, CellID))
colnames(MossPresence)[2] <- "Richness"

MossRichness <- numeric(15038)
MossRichness[MossPresence$CellID] <- MossPresence$Richness
MossRichness[which(MossRichness==0)]=NA

#Plot moss richness
MossRichnessRaster <- setValues(BlankRas, MossRichness)
gplot(MossRichnessRaster, maxpixels=15038) + geom_raster(aes(fill = value))+ scale_fill_gradientn(colours=cols, na.value="transparent") +
  coord_equal()  


#LIVERWORT RICHNESS
LiverwortPresence <- subset(BryophytePresence, BryophytePresence$Group=="Liverworts")
LiverwortPresence <- tally(group_by(LiverwortPresence, CellID))
colnames(LiverwortPresence)[2] <- "Richness"
 
LiverwortRichness <- numeric(15038)
LiverwortRichness[LiverwortPresence$CellID] <- LiverwortPresence$Richness
LiverwortRichness[which(LiverwortRichness==0)]=NA

#Plot liverwort richness
LiverwortRichnessRaster <- setValues(BlankRas, LiverwortRichness)
gplot(LiverwortRichnessRaster, maxpixels=15038) + geom_raster(aes(fill = value))+ scale_fill_gradientn(colours=cols, na.value="transparent") +
  coord_equal()   


#HORNWORT + LIVERWORT RICHNESS
HornwortPresence <- subset(BryophytePresence, BryophytePresence$Group=="Hornworts")
HLPresence <- subset(BryophytePresence, BryophytePresence$Group=="Hornworts"|BryophytePresence$Group=="Liverworts")

HLPresence <- tally(group_by(HLPresence, CellID))
colnames(HLPresence)[2] <- "Richness"
 
HLRichness <- numeric(15038)
HLRichness[HLPresence$CellID] <- HLPresence$Richness
HLRichness[which(HLRichness==0)]=NA

#Plot HL richness
HLRichnessRaster <- setValues(BlankRas, HLRichness)
gplot(RichnessRaster, maxpixels=15038) + geom_raster(aes(fill = value))+ scale_fill_gradientn(colours=cols, na.value="transparent") +
  coord_equal()    


#MAP BY GROUP/SPECIES/FAMILY
source("Functions/group.map.R")
source("Functions/species.map.R")
source("Functions/family.map.R")

group.map("Mosses")
group.map("Hornworts")
group.map("Liverworts")


#BRYOPHYTE RANGE MAP
Range <- tally(group_by(BryophytePresence, Species))
Range <- merge(Range, BryophytePresence, by = "Species")
 
#Find median of CellIDs
Range <- subset(Range, select = c("n", "CellID")) %>%
  group_by(CellID) %>%
  summarize(Avg = median(n))

#Bryophyte range map
BryophyteRange <- numeric(15038)
BryophyteRange[Range$CellID] <- Range$Avg
BryophyteRange[which(BryophyteRange==0)]=NA

RangeRaster <- setValues(BlankRas, BryophyteRange)
gplot(RangeRaster, maxpixels=15038) + geom_raster(aes(fill = value))+ scale_fill_gradientn(colours=cols, na.value="transparent") +
  coord_equal()    
