# Plotting beta diversity of BRYOPHYTES by biome and continent
# Adapted from BiomeDiversity.R and OrderBiomeDiversity.R
# Kathryn Dawdy, July 2020


# 0.0 FIRST ----------------------------------------------------------------

#Run DataProcessing.R, Continents.R, BiomeContinents.R, BiomeDiversity.R, 
    #ORange.R, OrdBiomeBP.R, TotalAlpha.R, 
    #then OrderBiomeDF.R - unless you can just load the DF (takes a while)
    #then MossPlotData.R - or just load the DF
OrderBiomeDF <- readRDS("Data/OrderBiomeDF.rds")
MossOBC <- readRDS("Data/MossOBC.rds")

#Run OrderRichness.R

# 0.1 Load Packages --------------------------------------------------------
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
require(knitr)
require(latexpdf)
require(vegan)
require(gridExtra)
require(sf)
require(rgeos)
require(rworldmap)
require(filesstrings)
require(forcats)
require(tidyverse)
require(tmap)

# 0.2 Load data ------------------------------------------------------------
BiomeBetaDF <- readRDS("Data/BiomeBetaDF.rds")
View(BiomeBetaDF)

# 0.3 Colors ---------------------------------------------------------------
#From wes_palette() hex numbers on GitHub: karthik/wesanderson
#Color scheme for biomes (in order of BiomeNames (BiomeProcessing.R))
cols7 <- c("#D8B70A", "#972D15", "#A2A475", "#81A88D", "#02401B",
           "#446455", "#FDD262", "#D3DDDC", "#C7B19C",
           "#798E87", "#C27D38")

#Colors used for plots (# corresponds to # of boxplots/length of data)
biome_cols_11 <- c("#D8B70A", "#972D15", "#A2A475", "#81A88D", "#02401B",
                   "#446455", "#FDD262", "#D3DDDC", "#C7B19C", "#798E87", 
                   "#C27D38")


# 1.0 Process data ---------------------------------------------------------
# 1.1 Remove "NA" strings
NoNABiomeBetaDF <- BiomeBetaDF[!grepl("NA", BiomeBetaDF$Beta),]
View(NoNABiomeBetaDF)
    #from 165,418 rows to only 3,844 rows...? doesn't seem like enough cells

# 1.2 Coerce beta values into double-precision vector
typeof(NoNABiomeBetaDF$Biome)
NoNABiomeBetaDF$Beta <- as.double(NoNABiomeBetaDF$Beta)


# 2.0 Beta diversity plot by biome -----------------------------------------
BiomeBetaBV <- ggplot(NoNABiomeBetaDF, aes(x=Biome, y=Beta, fill=Biome, color=Biome)) + 
  geom_boxplot(show.legend = FALSE, fill=biome_cols_11, color="black") +
  guides(x = guide_axis(angle=30)) +
  theme_minimal() +        #un-comment whichever theme you want
  #theme_gray() +
  #theme_light() +
  #theme_bw() +
  geom_violin(scale="count", show.legend=FALSE, fill="gray", alpha=0.35,
              color="gray25") +
  xlab("Biome") +
  ylab("Beta Diversity") +  
  theme(axis.title.y = element_text(size=32), 
        axis.title.x = element_text(size=32),
        axis.text.y = element_text(size=20), 
        axis.text.x = element_text(angle = 30, hjust = 1, size = 12))
BiomeBetaBV
