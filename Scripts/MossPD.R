#Comparing Phylogenetic Diversity of Mosses in the Americas Using 3 Laenen Phylogenies

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
require(PhyloMeasures)

#Load blank raster and colors
BlankRas <-raster("Data/blank_100km_raster.tif")
cols <- rev(wes_palette("Zissou1", 500, type = "continuous"))

#Load mapping files
nw_mount <- shapefile("./Outputs/Mountains/Koeppen-Geiger_biomes.shp")
nw_bound <- shapefile("./Outputs/Global_bound/Koeppen-Geiger_biomes.shp")
nw_mount_sf <- st_as_sf(nw_mount)
nw_bound_sf <- st_as_sf(nw_bound)

#Load in presence-absence matix and transform so row = species and column = cell ID
SpeciesCellMatrix <- readRDS("Data/SpeciesCellMatrix.rds")
SpeciesCellMatrix <- t(SpeciesCellMatrix)

###############################################################
### TREE 75504 ###
#######################

Laenen4 <- search_treebase("75504", by="id.tree")
Laenen4 <- Laenen4[[1]]
Laenen4$tip.label <- gsub("_", " ", Laenen4$tip.label)

#Match tree data to presence data to drop any species that don't overlap from the tree and the presence-absence matrix
MossTreeData4 <- treedata(Laenen4, SpeciesCellMatrix)

#Load updated tree and presence-absence matrix with matched species + create vector of matched species names
SpeciesCellMatrix4 <- MossTreeData4$data
SpeciesCellMatrix4 <- t(SpeciesCellMatrix4)
MossTree4 <- MossTreeData4$phy
MossOverlapSpecies4 <- MossTree4$tip.label

#Calculate Faith's Phylogenetic Diversity (PD)
MossPD4 <- pd(SpeciesCellMatrix4, MossTree4, include.root = TRUE)

#Create vector of cell IDs for cells that contain matched species
CellID4 <- row.names(MossPD4)
CellID4 <- as.numeric(CellID4)

#Add cell ID as a column in PD dataframe and remove cells with 0 species
MossPD4 <- cbind(MossPD4, CellID4)
MossPD4 <- MossPD4 %>% filter(SR != 0)

#Create vector of PD values
PDVec4 <- numeric(15038)
PDVec4[MossPD4$CellID4] <- MossPD4$PD
PDVec4[PDVec4==0] <- NA

#Create PD raster and dataframe
PDRaster4 <- setValues(BlankRas, PDVec4)
PDDF4 <- rasterToPoints(PDRaster4)
PDDF4 <- data.frame(PDDF4)
colnames(PDDF4) <- c("Longitude", "Latitude", "PD4")

#Map PD
theme_set(theme_void())
PDMap4 <- ggplot() + geom_tile(data=PDDF4, aes(x=Longitude, y=Latitude, fill=PD4)) +   
  scale_fill_gradientn(name="PD4", colours=cols, na.value="transparent") +
  coord_equal() + geom_sf(data = nw_bound_sf, size = 0.1, fill=NA) + 
  geom_sf(data = nw_mount_sf, size = 0.2, alpha=0.1)
PDMap4

###############################################################
### TREE 75505 ###
#######################

Laenen5 <- search_treebase("75505", by="id.tree")
Laenen5 <- Laenen5[[1]]
Laenen5$tip.label <- gsub("_", " ", Laenen5$tip.label)

#Match tree data to presence data to drop any species that don't overlap from the tree and the presence-absence matrix
MossTreeData5 <- treedata(Laenen5, SpeciesCellMatrix)

#Load updated tree and presence-absence matrix with matched species + create vector of matched species names
SpeciesCellMatrix5 <- MossTreeData5$data
SpeciesCellMatrix5 <- t(SpeciesCellMatrix5)
MossTree5 <- MossTreeData5$phy
MossOverlapSpecies5 <- MossTree5$tip.label

#Calculate Faith's Phylogenetic Diversity (PD)
MossPD5 <- pd(SpeciesCellMatrix5, MossTree5, include.root = TRUE)

#Create vector of cell IDs for cells that contain matched species
CellID5 <- row.names(MossPD5)
CellID5 <- as.numeric(CellID5)

#Add cell ID as a column in PD dataframe and remove cells with 0 species
MossPD5 <- cbind(MossPD5, CellID5)
MossPD5 <- MossPD5 %>% filter(SR != 0)

#Create vector of PD values
PDVec5 <- numeric(15038)
PDVec5[MossPD5$CellID5] <- MossPD5$PD
PDVec5[PDVec5==0] <- NA

#Create PD raster and dataframe
PDRaster5 <- setValues(BlankRas, PDVec5)
PDDF5 <- rasterToPoints(PDRaster5)
PDDF5 <- data.frame(PDDF5)
colnames(PDDF5) <- c("Longitude", "Latitude", "PD5")

#Map PD
theme_set(theme_void())
PDMap5 <- ggplot() + geom_tile(data=PDDF5, aes(x=Longitude, y=Latitude, fill=PD5)) +   
  scale_fill_gradientn(name="PD5", colours=cols, na.value="transparent") +
  coord_equal() + geom_sf(data = nw_bound_sf, size = 0.1, fill=NA) + 
  geom_sf(data = nw_mount_sf, size = 0.2, alpha=0.1)
PDMap5

###############################################################
### TREE 75506 ###
#######################

Laenen6 <- search_treebase("75506", by="id.tree")
Laenen6 <- Laenen6[[1]]
Laenen6$tip.label <- gsub("_", " ", Laenen6$tip.label)

#Match tree data to presence data to drop any species that don't overlap from the tree and the presence-absence matrix
MossTreeData6 <- treedata(Laenen6, SpeciesCellMatrix)

#Load updated tree and presence-absence matrix with matched species + create vector of matched species names
SpeciesCellMatrix6 <- MossTreeData6$data
SpeciesCellMatrix6 <- t(SpeciesCellMatrix6)
MossTree6 <- MossTreeData6$phy
MossOverlapSpecies6 <- MossTree6$tip.label

#Calculate Faith's Phylogenetic Diversity (PD)
MossPD6 <- pd(SpeciesCellMatrix6, MossTree6, include.root = TRUE)

#Create vector of cell IDs for cells that contain matched species
CellID6 <- row.names(MossPD6)
CellID6 <- as.numeric(CellID6)

#Add cell ID as a column in PD dataframe and remove cells with 0 species
MossPD6 <- cbind(MossPD6, CellID6)
MossPD6 <- MossPD6 %>% filter(SR != 0)

#Create vector of PD values
PDVec6 <- numeric(15038)
PDVec6[MossPD6$CellID6] <- MossPD6$PD
PDVec6[PDVec6==0] <- NA

#Create PD raster and dataframe
PDRaster6 <- setValues(BlankRas, PDVec6)
PDDF6 <- rasterToPoints(PDRaster6)
PDDF6 <- data.frame(PDDF6)
colnames(PDDF6) <- c("Longitude", "Latitude", "PD6")

#Map PD
theme_set(theme_void())
PDMap6 <- ggplot() + geom_tile(data=PDDF6, aes(x=Longitude, y=Latitude, fill=PD6)) +   
  scale_fill_gradientn(name="PD6", colours=cols, na.value="transparent") +
  coord_equal() + geom_sf(data = nw_bound_sf, size = 0.1, fill=NA) + 
  geom_sf(data = nw_mount_sf, size = 0.2, alpha=0.1)
PDMap6


###############################################################
### MAP RICHNESS ###
##########################

#Create moss richness vector
MossRichness <- numeric(15038)
MossRichness[MossPD4$CellID4] <- MossPD4$SR
MossRichness[which(MossRichness==0)]=NA

#Plot moss richness
MossRichnessRaster <- setValues(BlankRas, MossRichness)

MossRichnessDF <- rasterToPoints(MossRichnessRaster)
MossRichnessDF <- as.data.frame(MossRichnessDF)
colnames(MossRichnessDF) <- c("Longitude", "Latitude", "Richness")

RichnessMap <- ggplot() + geom_tile(data=MossRichnessDF, aes(x=Longitude, y=Latitude, fill=Richness)) +   
  scale_fill_gradientn(name="Richness", colours=cols, na.value="transparent") +
  coord_equal() + geom_sf(data = nw_bound_sf, size = 0.1, fill=NA) + 
  geom_sf(data = nw_mount_sf, size = 0.2, alpha=0.1)
RichnessMap

###############################################################
### MAP RICHNESS AND PD FOR 3 TREES ###
#######################################

grid.arrange(RichnessMap, PDMap4, PDMap5, PDMap6, ncol=2)


###############################################################
### PhyloMeasures ###
#######################################

pdquery <- pd.query(MossTree4, SpeciesCellMatrix4, standardize = TRUE)

#SES.PD
ses <- ses.pd(SpeciesCellMatrix4, MossTree4, null.model = "taxa.labels", runs = 999, include.root=TRUE)

###############################################################
### PHYLOGENETIC BETA DIVERSITY ###
#######################################

PhyBetaJ <- phylo.beta.pair(SpeciesCellMatrix4, MossTree4, index.family="jaccard")
PhyBetaS <- phylo.beta.pair(SpeciesCellMatrix4, MossTree4, index.family="sorensen")
