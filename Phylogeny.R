
require(betapart)
require(ape)
require(phytools)
require(tm)
require(treebase)
require(geiger)
require(raster)
require(ggplot2)
require(wesanderson)

#LaenenTrees <- search_treebase("Laenen", by="author")
SpeciesCellMatrix <- t(SpeciesCellMatrix)
Laenen <- search_treebase("75506", by="id.tree")
Laenen <- Laenen[[1]]
Laenen$tip.label <- gsub("_", " ", Laenen$tip.label)


MossDataTree <- treedata(Laenen, SpeciesCellMatrix)
MossDataTree

#Calculate Faith's Phylogenetic Diversity (PD)
require(picante)
matrix <- MossDataTree$data
matrix <- t(matrix)
tree <- MossDataTree$phy
MossOverlap <- tree$tip.label

pd <- pd(matrix, tree, include.root = TRUE)

pdIDs <- row.names(pd)
pdIDs <- as.numeric(pdIDs)
pd <- cbind(pd, pdIDs)

library(dplyr)
pd <- pd %>% filter(SR != 0)

PDvec <- numeric(15038)
PDvec[pd$pdIDs] <- pd$PD
PDvec[PDvec==0] <- NA

BlankRas <-raster("Data/blank_100km_raster.tif")
cols <- rev(wes_palette("Zissou1", 500, type = "continuous"))

PDRaster <- setValues(BlankRas, PDvec)

PDDF <- rasterToPoints(PDRaster)
PDDF <- data.frame(PDDF)

colnames(PDDF) <- c("Longitude", "Latitude", "PD")

theme_set(theme_void())
PDMap <- ggplot() + geom_tile(data=PDDF, aes(x=Longitude, y=Latitude, fill=PD)) +   
  scale_fill_gradientn(name="PD", colours=cols, na.value="transparent") +
  coord_equal() 
PDMap

#Richness
#Load blank raster and richness/presence data
RichnessVec <- readRDS("Data/RichnessVec.rds")
BryophytePresence <- readRDS("Data/BryophytePresence.rds")

#MOSS RICHNESS
MossPresence <- subset(BryophytePresence, BryophytePresence$Group=="Mosses")
MossPresence <- subset(MossPresence, MossPresence$Species==MossOverlap)

MossPresence <- tally(group_by(MossPresence, pdIDs))
colnames(MossPresence)[2] <- "Richness"

MossRichness <- numeric(15038)
MossRichness[MossPresence$CellID] <- MossPresence$Richness
MossRichness[which(MossRichness==0)]=NA

#Plot moss richness
MossRichnessRaster <- setValues(BlankRas, MossRichness)
gplot(MossRichnessRaster, maxpixels=15038) + geom_raster(aes(fill = value))+ scale_fill_gradientn(colours=cols, na.value="transparent") +
  coord_equal()  


#MossData <- na.omit(BrySpecies)
#rownames(MossData) <- MossData$Species
#to_drop <- which(Laenen$tip.label%in%unique(BrySpecies$Species)==FALSE)
#MossTree <- drop.tip(Laenen, to_drop)

#Liu trees (weird formatting of species names)
#Tree25 <- read.tree(file = "Trees/aa-nu-astral-mlbs-FigS25.tre")
#Tree25Label <- Tree25["tip.label"]
#Tree25Label <- gsub("-", " ", Tree25Label)
#removeNumbers(Tree25Label)
