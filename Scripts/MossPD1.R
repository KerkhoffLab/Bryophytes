#Calculating Phylogenetic Diversity of Mosses in the Americas

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

#Load mapping files
nw_mount <- shapefile("./Outputs/Mountains/Koeppen-Geiger_biomes.shp")
nw_bound <- shapefile("./Outputs/Global_bound/Koeppen-Geiger_biomes.shp")

nw_mount_sf <- st_as_sf(nw_mount)
nw_bound_sf <- st_as_sf(nw_bound)

#Load in presence-absence matix and transform so row = species and column = cell ID
SpeciesCellMatrix <- readRDS("Data/SpeciesCellMatrix.rds")
SpeciesCellMatrix <- t(SpeciesCellMatrix)

#Search TreeBase by author, then find tree id and load in desired tree
LaenenTrees <- search_treebase("Laenen", by="author")
Laenen <- search_treebase("75506", by="id.tree")
Laenen <- Laenen[[1]]
Laenen$tip.label <- gsub("_", " ", Laenen$tip.label)

#Match tree data to presence data to drop any species that don't overlap from the tree and the presence-absence matrix
MossTreeData <- treedata(Laenen, SpeciesCellMatrix)

#Load updated tree and presence-absence matrix with matched species + create vector of matched species names
SpeciesCellMatrix <- MossTreeData$data
SpeciesCellMatrix <- t(SpeciesCellMatrix)
MossTree <- MossTreeData$phy
MossOverlapSpecies <- MossTree$tip.label

#Calculate Faith's Phylogenetic Diversity (PD)
MossPD <- pd(SpeciesCellMatrix, MossTree, include.root = TRUE)

#Create vector of cell IDs for cells that contain matched species
CellID <- row.names(MossPD)
CellID <- as.numeric(CellID)

#Add cell ID as a column in PD dataframe and remove cells with 0 species
MossPD <- cbind(MossPD, CellID)
MossPD <- MossPD %>% filter(SR != 0)

#Create vector of PD values
PDVec <- numeric(15038)
PDVec[MossPD$CellID] <- MossPD$PD
PDVec[PDVec==0] <- NA

#Load blank raster and colors
BlankRas <-raster("Data/blank_100km_raster.tif")
cols <- rev(wes_palette("Zissou1", 500, type = "continuous"))

#Create PD raster and dataframe
PDRaster <- setValues(BlankRas, PDVec)

PDDF <- rasterToPoints(PDRaster)
PDDF <- data.frame(PDDF)
colnames(PDDF) <- c("Longitude", "Latitude", "PD")

#Map PD
theme_set(theme_void())
PDMap <- ggplot() + geom_tile(data=PDDF, aes(x=Longitude, y=Latitude, fill=PD)) +   
  scale_fill_gradientn(name="PD", colours=cols, na.value="transparent") +
  coord_equal() + geom_sf(data = nw_bound_sf, size = 0.1, fill=NA) + 
  geom_sf(data = nw_mount_sf, size = 0.2, alpha=0.1)
PDMap

#Standardized effect size of PD
MossSESPD <- ses.pd(SpeciesCellMatrix, MossTree, null.model = "taxa.labels", runs=999, include.root = TRUE)



#RICHNESS

#Create moss richness vector
MossRichness <- numeric(15038)
MossRichness[MossPD$CellID] <- MossPD$SR
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


#PLOT SUBSETTED RICHNESS AND PD MAPS SIDE BY SIDE

grid.arrange(RichnessMap, PDMap, ncol=2)
