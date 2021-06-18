#Comparing Phylogenetic Diversity of Liverwortes in the Americas Using 3 Laenen Phylogenies

#Packages
require(betapart)
require(ape)
require(phytools)
require(picante)
require(tm)
require(treebase)
require(geiger)
require(raster)
require(ggplot2)
require(wesanderson)
require(sf)
require(dplyr)
require(gridExtra)

#Load blank raster and colors
BlankRas <-raster("Data/blank_100km_raster.tif")
cols <- rev(wes_palette("Zissou1", 500, type = "continuous"))

#Load mapping files
nw_mount <- shapefile("Data/MapOutlines/Mountains/Koeppen-Geiger_biomes.shp")
nw_bound <- shapefile("Data/MapOutlines/Global_bound/Koeppen-Geiger_biomes.shp")
nw_mount_sf <- st_as_sf(nw_mount)
nw_bound_sf <- st_as_sf(nw_bound)

#Load in presence-absence matix and transform so row = species and column = cell ID
SpeciesCellMatrix <- readRDS("Data/SpeciesCellMatrix.rds")
SpeciesCellMatrix <- t(SpeciesCellMatrix)

###############################################################
### TREE 75502 ###
#######################

Laenen2 <- search_treebase("75502", by="id.tree")
Laenen2 <- Laenen2[[1]]
Laenen2$tip.label <- gsub("_", " ", Laenen2$tip.label)

#Match tree data to presence data to drop any species that don't overlap from the tree and the presence-absence matrix
LiverwortTreeData2 <- treedata(Laenen2, SpeciesCellMatrix)

#Load updated tree and presence-absence matrix with matched species + create vector of matched species names
SpeciesCellMatrix2 <- LiverwortTreeData2$data
SpeciesCellMatrix2 <- t(SpeciesCellMatrix2)
LiverwortTree2 <- LiverwortTreeData2$phy
LiverwortOverlapSpecies2 <- LiverwortTree2$tip.label

#Calculate Faith's Phylogenetic Diversity (PD)
LiverwortPD2 <- pd(SpeciesCellMatrix2, LiverwortTree2, include.root = TRUE)

#Create vector of cell IDs for cells that contain matched species
CellID2 <- row.names(LiverwortPD2)
CellID2 <- as.numeric(CellID2)

#Add cell ID as a column in PD dataframe and remove cells with 0 species
LiverwortPD2 <- cbind(LiverwortPD2, CellID2)
LiverwortPD2 <- LiverwortPD2 %>% filter(SR != 0)

#Create vector of PD values
PDVec2 <- numeric(15038)
PDVec2[LiverwortPD2$CellID2] <- LiverwortPD2$PD
PDVec2[PDVec2==0] <- NA

#Create PD raster and dataframe
PDRaster2 <- setValues(BlankRas, PDVec2)
PDDF2 <- rasterToPoints(PDRaster2)
PDDF2 <- data.frame(PDDF2)
colnames(PDDF2) <- c("Longitude", "Latitude", "PD2")

#Map PD
theme_set(theme_void())
PDMap2 <- ggplot() + geom_tile(data=PDDF2, aes(x=Longitude, y=Latitude, fill=PD2)) +   
  scale_fill_gradientn(name="PD2", colours=cols, na.value="transparent") +
  coord_equal() + geom_sf(data = nw_bound_sf, size = 0.1, fill=NA) + 
  geom_sf(data = nw_mount_sf, size = 0.2, alpha=0.1)
PDMap2

###############################################################
### TREE 75503 ###
#######################

Laenen3 <- search_treebase("75503", by="id.tree")
Laenen3 <- Laenen3[[1]]
Laenen3$tip.label <- gsub("_", " ", Laenen3$tip.label)

#Match tree data to presence data to drop any species that don't overlap from the tree and the presence-absence matrix
LiverwortTreeData3 <- treedata(Laenen3, SpeciesCellMatrix)

#Load updated tree and presence-absence matrix with matched species + create vector of matched species names
SpeciesCellMatrix3 <- LiverwortTreeData3$data
SpeciesCellMatrix3 <- t(SpeciesCellMatrix3)
LiverwortTree3 <- LiverwortTreeData3$phy
LiverwortOverlapSpecies3 <- LiverwortTree3$tip.label

#Calculate Faith's Phylogenetic Diversity (PD)
LiverwortPD3 <- pd(SpeciesCellMatrix3, LiverwortTree3, include.root = TRUE)

#Create vector of cell IDs for cells that contain matched species
CellID3 <- row.names(LiverwortPD3)
CellID3 <- as.numeric(CellID3)

#Add cell ID as a column in PD dataframe and remove cells with 0 species
LiverwortPD3 <- cbind(LiverwortPD3, CellID3)
LiverwortPD3 <- LiverwortPD3 %>% filter(SR != 0)

#Create vector of PD values
PDVec3 <- numeric(15038)
PDVec3[LiverwortPD3$CellID3] <- LiverwortPD3$PD
PDVec3[PDVec3==0] <- NA

#Create PD raster and dataframe
PDRaster3 <- setValues(BlankRas, PDVec3)
PDDF3 <- rasterToPoints(PDRaster3)
PDDF3 <- data.frame(PDDF3)
colnames(PDDF3) <- c("Longitude", "Latitude", "PD3")

#Map PD
theme_set(theme_void())
PDMap3 <- ggplot() + geom_tile(data=PDDF3, aes(x=Longitude, y=Latitude, fill=PD3)) +   
  scale_fill_gradientn(name="PD3", colours=cols, na.value="transparent") +
  coord_equal() + geom_sf(data = nw_bound_sf, size = 0.1, fill=NA) + 
  geom_sf(data = nw_mount_sf, size = 0.2, alpha=0.1)
PDMap3


###############################################################
### MAP RICHNESS ###
##########################

#Create Liverwort richness vector
LiverwortRichness <- numeric(15038)
LiverwortRichness[LiverwortPD2$CellID2] <- LiverwortPD2$SR
LiverwortRichness[which(LiverwortRichness==0)]=NA

#Plot Liverwort richness
LiverwortRichnessRaster <- setValues(BlankRas, LiverwortRichness)

LiverwortRichnessDF <- rasterToPoints(LiverwortRichnessRaster)
LiverwortRichnessDF <- as.data.frame(LiverwortRichnessDF)
colnames(LiverwortRichnessDF) <- c("Longitude", "Latitude", "Richness")

RichnessMap <- ggplot() + geom_tile(data=LiverwortRichnessDF, aes(x=Longitude, y=Latitude, fill=Richness)) +   
  scale_fill_gradientn(name="Richness", colours=cols, na.value="transparent") +
  coord_equal() + geom_sf(data = nw_bound_sf, size = 0.1, fill=NA) + 
  geom_sf(data = nw_mount_sf, size = 0.2, alpha=0.1)
RichnessMap

###############################################################
### MAP RICHNESS AND PD FOR 3 TREES ###
#######################################

grid.arrange(RichnessMap, PDMap2, PDMap3, ncol=3)
