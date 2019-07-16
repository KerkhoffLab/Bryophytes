#Calculating Phylogenetic Diversity of Bryophytes in the Americas

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
require(rasterVis)
library(dplyr)

#Load in presence-absence matix and transform so row = species and column = cell ID
SpeciesCellMatrix <- readRDS("Data/SpeciesCellMatrix.rds")
SpeciesCellMatrix <- t(SpeciesCellMatrix)

#Search TreeBase by author, then find tree id and load in desired tree
#LaenenTrees <- search_treebase("Laenen", by="author")
Laenen6 <- search_treebase("75506", by="id.tree")
Laenen6 <- Laenen6[[1]]
Laenen6$tip.label <- gsub("_", " ", Laenen6$tip.label)

#Match tree data to presence data to drop any species that don't overlap from the tree and the presence-absence matrix
MossTreeData <- treedata(Laenen6, SpeciesCellMatrix)

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
  coord_equal() 
PDMap

#Standardized effect size of PD
MossSESPD <- ses.pd(SpeciesCellMatrix, MossTree, null.model = "taxa.labels", runs=999, include.root = TRUE)










#RICHNESS
#Load richness and presence data
RichnessVec <- readRDS("Data/RichnessVec.rds")
BryophytePresence <- readRDS("Data/BryophytePresence.rds")

#Subset moss data to only include mosses in the phylogeny
MossPresence <- subset(BryophytePresence, BryophytePresence$Species==MossOverlapSpecies)
######## WE KNOW THESE SPECIES MUST BE MOSSES BECAUSE THEY ARE IN THE MOSS PHYLOGENY BUT THEY DONT HAVE A GROUP LISTED SO WE SHOULD REPLACE NA VALUES IN THE GROUP COLUMN WITH "MOSSES"
### idk why it's not working though-- ask Susy?
MossPresence[is.na("Group")] <- "Mosses"

MossPresence <- tally(group_by(Species, CellID))
colnames(MossPresence)[2] <- "Richness"

#Create moss richness vector
MossRichness <- numeric(15038)
MossRichness[which(MossRichness==0)]=NA
MossRichness[MossPresence$CellID] <- MossPresence$Richness

#Plot moss richness
MossRichnessRaster <- setValues(BlankRas, MossRichness)
MossRichnessDF <- as.data.frame(MossRichnessRaster)
gplot(MossRichnessRaster, maxpixels=15038) + geom_raster(aes(fill = value))+ scale_fill_gradientn(colours=cols, na.value="transparent") +
  coord_equal()  

#PLOT SUBSETTED RICHNESS AND PD MAPS SIDE BY SIDE

