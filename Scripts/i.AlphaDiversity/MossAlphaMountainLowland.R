# MOSS mountain lowland alpha scatterplots
# Essentially MountainLowland.Rmd but with MOSSES
# Adapted from MountainLowland.Rmd, DataProcessing.R
# Kathryn Dawdy, June 2021


# load packages ----
require(sp)
require(raster)
require(ggplot2)
require(gridExtra)
require(rgdal)
require(forcats)


# 0.0 Load data ----------------------------------------------------------------
#Load blank raster and cell richness data + extract cell IDs and create vector for all cells
#Change file for CellRichness depending on if you want to map bryophytes, mosses, liverworts, etc. 
BlankRas <-raster("Data/blank_100km_raster.tif")
CellVec <- c(1:15038)
CellRichness <- readRDS("Data/MossRichness.rds")
CellID <- CellRichness$CellID

# Run beginning of MossBiomeDiversity.R for data
MossPresence <- readRDS("Data/MossPresence.rds")
MossRichnessVec <- readRDS("Data/MossRichnessVec.rds")
MossRichnessRaster <- readRDS("Data/MossRichnessRaster.rds")
LongLatDF <- readRDS("Data/LongLatDF.rds")  #run BiomeDiversity.R or MountainLowland.Rmd for data

# Add continental and mountainous region outlines
nw_mount <- shapefile("Data/MapOutlines/Mountains/Koeppen-Geiger_biomes.shp")
nw_bound <- shapefile("Data/MapOutlines/Global_bound/Koeppen-Geiger_biomes.shp")

# Generate this data below
AlphaMount <- readRDS("Data/MossAlphaMount.rds")
AlphaLowland <- readRDS("Data/MossAlphaLowland.rds")



# 1.0 Make mountainous / lowland ALPHA scatterplots ----------------------------
# 1.1 Alpha diversity montane by latitude scatterplot
AlphaMount <- raster::extract(MossRichnessRaster, nw_mount, df = TRUE, cellnumbers = TRUE)

colnames(AlphaMount) <- c("Type", "CellID", "Alpha")
AlphaMount$Type <- "Mountain"

AlphaBoundVec <- AlphaMount[, "CellID"]
AlphaMount <- merge(AlphaMount, LongLatDF)

AlphaMountScatterplot <- ggplot(AlphaMount, aes(Latitude, Alpha)) + geom_point(shape = 16, size = 5, show.legend = FALSE, alpha=0.5, color = "goldenrod2") + ylab("Mountainous α diversity") + ylim (0, 800) + xlab("Latitude") + theme_minimal() + theme(axis.title.y = element_text(size=32), axis.title.x = element_text(size=32),  axis.text = element_text(size=20))
AlphaMountScatterplot

# 1.2 Beta diversity lowland by latitude scatterplot
AlphaBound <- raster::extract(MossRichnessRaster, nw_bound, df = TRUE, cellnumbers = TRUE)
colnames(AlphaBound) <- c("Type", "CellID", "Alpha")

AlphaMatch <- AlphaBound[AlphaBound$CellID %in% AlphaBoundVec, ]
AlphaMatchVec <- AlphaMatch[, "CellID"]
AlphaLowland <- AlphaBound[!AlphaBound$CellID %in% AlphaMatchVec, ]

AlphaLowland$Type <- "Lowland"

AlphaLowlandVec <- AlphaLowland$CellID
AlphaLowland <- merge(AlphaLowland, LongLatDF)

AlphaLowlandScatterplot <- ggplot(AlphaLowland, aes(Latitude, Alpha)) + geom_point(shape = 16, size = 5, show.legend = FALSE, alpha=0.5, color = "cyan4") + ylab("Lowland α diversity") + ylim(0, 800) + xlab("Latitude") + theme_minimal() + theme(axis.title.y = element_text(size=32), axis.title.x = element_text(size=32),  axis.text = element_text(size=20))
AlphaLowlandScatterplot

# 1.3 Combine alpha mountainous and lowland scatterplots
FullAlpha <- rbind(AlphaMount, AlphaLowland)

AlphaScatter <- ggplot(data = FullAlpha, aes(Latitude, Alpha, color=Type)) + 
  geom_point(shape = 16, size = 3, show.legend=FALSE, alpha=0.8) + 
  ylab("α diversity") + ylim(0, 800) +
  theme_minimal() + theme(axis.title.y = element_text(size=40), axis.title.x = element_blank(),  
                          axis.text = element_text(size=20)) + 
  scale_color_manual(values = c("cyan4", "goldenrod2")) + geom_smooth(size = 2, show.legend = FALSE)
AlphaScatter

# save figure
png("Figures/MossAlphaMountLowScatter.png", width = 1500, height = 1000, pointsize = 20)
AlphaScatter
dev.off()
