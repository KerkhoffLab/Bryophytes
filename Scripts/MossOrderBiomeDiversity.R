# Plotting alpha diversity of MOSS orders by biome and continent
# Adapted from BiomeDiversity.R
# Kathryn Dawdy and Hailey Napier, July 2020

# 0.0 FIRST ----------------------------------------------------------------

#Run DataProcessing.R, Continents.R, BiomeContinents.R, BiomeDiversity.R, 
    #ORange.R, OrdBiomeBP.R, TotalAlpha.R, 
    #then MossPlotData.R - or just load the DF here
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
OrderBiomeDF <- readRDS("Data/OrderBiomeDF.rds")

OrdRichAbove100 <- readRDS("Data/OrdRichAbove100.rds")
OrdRich25to100 <- readRDS("Data/OrdRich25to100.rds")
OrdRich10to25 <- readRDS("Data/OrdRich10to25.rds")
OrdRichBelow10 <-readRDS("Data/OrdRichBelow10.rds")

OrderBiomeHemDF <- readRDS("Data/OrderBiomeHemDF.rds")

MossOrdRichAbove100 <- readRDS("Data/MossOrdRichAbove100.rds")
MossOrdRich25to100 <- readRDS("Data/MossOrdRich25to100.rds")
MossOrdRich10to25 <- readRDS("Data/MossOrdRich10to25.rds")
MossOrdRichBelow10 <- readRDS("Data/MossOrdRichBelow10.rds")

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

biome_cols_22 <- c(biome_cols_11, biome_cols_11)

biome_cols_87 <- c(biome_cols_22, biome_cols_11,
                   "#D8B70A", "#972D15", "#A2A475", "#81A88D", "#02401B",
                   "#446455", "#FDD262", "#D3DDDC", "#C7B19C", 
                   "#C27D38",
                   biome_cols_22, biome_cols_22)

biome_cols_66 <- c(biome_cols_22, biome_cols_22, biome_cols_22)

biome_cols_166 <- c(biome_cols_22,
                    c("#D8B70A", "#972D15", "#A2A475", "#81A88D", "#02401B",
                      "#FDD262", "#D3DDDC", "#C7B19C", 
                      "#C27D38"),
                    biome_cols_11,
                    c("#D8B70A", "#972D15", "#A2A475", "#81A88D", "#02401B",
                      "#446455", "#FDD262", "#D3DDDC",
                      "#C27D38"),
                    c("#D8B70A", "#972D15", "#81A88D", "#02401B",
                      "#446455", "#FDD262", "#D3DDDC", "#C7B19C", "#798E87", 
                      "#C27D38"),
                    c("#81A88D",
                      "#FDD262", "#D3DDDC"),
                    c("#D8B70A", "#972D15", "#81A88D",
                      "#446455", "#D3DDDC", "#C7B19C", "#798E87"),
                    c("#D8B70A", "#972D15", "#A2A475", "#81A88D", "#02401B",
                      "#FDD262", "#D3DDDC", "#C7B19C", 
                      "#C27D38"),
                    "#81A88D",
                    c("#972D15", "#A2A475", "#81A88D", "#02401B",
                      "#FDD262", "#D3DDDC", "#C7B19C", 
                      "#C27D38"),
                    c("#D8B70A",
                      "#446455", "#FDD262", "#D3DDDC"),
                    biome_cols_22,
                    c("#D8B70A", "#972D15", "#81A88D",
                      "#D3DDDC", "#C7B19C", 
                      "#C27D38"),
                    c("#D8B70A",
                      "#446455", "#FDD262", "#D3DDDC", "#798E87", 
                      "#C27D38"),
                    c("#A2A475", "#81A88D", "#02401B",
                      "#FDD262", "#D3DDDC", "#C7B19C"),
                    c("#D8B70A", "#972D15", "#A2A475", "#81A88D", "#02401B",
                      "#FDD262", "#D3DDDC", "#C7B19C", 
                      "#C27D38"),
                    biome_cols_11,
                    c("#D8B70A",
                      "#C27D38"),
                    biome_cols_11)

biome_cols_29 <- c(biome_cols_11,
                   c("#D8B70A", "#972D15", "#A2A475", "#81A88D", "#02401B",
                     "#446455", "#FDD262", "#D3DDDC", "#798E87", 
                     "#C27D38"),
                   c("#972D15", "#A2A475", "#81A88D", "#02401B",
                     "#FDD262", "#D3DDDC", "#C7B19C", 
                     "#C27D38"))

biome_cols_18 <- c(c("#D8B70A", "#972D15", "#A2A475", "#81A88D", "#02401B",
                     "#446455", "#FDD262", "#D3DDDC", "#798E87", 
                     "#C27D38"),
                   c("#972D15", "#A2A475", "#81A88D", "#02401B",
                     "#FDD262", "#D3DDDC", "#C7B19C", 
                     "#C27D38"))

biome_cols_65 <- c(biome_cols_22,
                   biome_cols_11,
                   c("#D8B70A", "#972D15", "#A2A475", "#81A88D", "#02401B",
                     "#446455", "#FDD262", "#D3DDDC", "#C7B19C", 
                     "#C27D38"),
                   biome_cols_22)

biome_cols_44 <- c(biome_cols_22,
                   biome_cols_22)

biome_cols_93 <- c(biome_cols_11,
                   c("#D8B70A", "#972D15", "#A2A475", "#81A88D", "#02401B",
                     "#FDD262", "#D3DDDC", "#C7B19C", 
                     "#C27D38"),
                   biome_cols_11,
                   c("#D8B70A", "#972D15", "#A2A475", "#81A88D", "#02401B",
                     "#446455", "#FDD262", "#D3DDDC",
                     "#C27D38"),
                   c("#D8B70A", "#972D15", "#81A88D", "#02401B",
                     "#446455", "#FDD262", "#D3DDDC", "#C7B19C", "#798E87", 
                     "#C27D38"),
                   c("#D8B70A", "#972D15", "#A2A475", "#81A88D", "#02401B",
                     "#FDD262", "#D3DDDC", "#C7B19C", 
                     "#C27D38"),
                   c("#972D15", "#A2A475", "#81A88D", "#02401B",
                     "#FDD262", "#D3DDDC", "#C7B19C", 
                     "#C27D38"),
                   c("#A2A475", "#81A88D", "#02401B",
                     "#FDD262", "#D3DDDC", "#C7B19C"),
                   c("#D8B70A", "#972D15", "#A2A475", "#81A88D", "#02401B",
                     "#FDD262", "#D3DDDC", "#C7B19C", 
                     "#C27D38"),
                   biome_cols_11)


# 1.0 MOSS RICHNESS FACET PLOTS in biomes by order -------------------------
# 1.1 MossOBC made in MossPlotData.R, also in the BryophytesData folder ----
MossOBC <- readRDS("Data/MossOBC.rds")

# 1.2 Facet of moss richness in biomes by all orders -----------------------
MossFacetOBR <- ggplot(MossOBC, 
                       aes(x=Biome, y=Alpha, 
                           #fill=Biome, color=Biome
                       )) + 
  geom_boxplot(show.legend = FALSE, 
               #fill=biome_cols_18, color="black"
  ) +
  guides(x = guide_axis(angle=30)) +
  theme_gray() +
  geom_violin(scale="count", show.legend=FALSE, fill="gray", alpha=0.35, 
              color="gray25") +
  xlab("Biome") +
  ylab("Richness") +  
  theme(axis.title.y = element_text(size=32), 
        axis.title.x = element_text(size=32),
        axis.text.y = element_text(size=15), 
        axis.text.x = element_text(angle = 30, hjust = 1, size = 8))+
  facet_wrap(~Order)
MossFacetOBR

# 1.3 Subset moss order by maximum alpha diversity value -------------------
###Run MossPlotData.R
# max α > 100
MossOrdRichAbove100 <- readRDS("Data/MossOrdRichAbove100.rds")
MossOrdRichAbove100
MossOBRAbove100DF <- subset(MossOBC, 
                            MossOBC$Order=="Hypnales"|
                              MossOBC$Order=="Dicranales")

# max α 25-100
MossOrdRich25to100 <- readRDS("Data/MossOrdRich25to100.rds")
MossOrdRich25to100
MossOBR25to100DF <- subset(MossOBC,
                           MossOBC$Order=="Bartramiales"|
                             MossOBC$Order=="Bryales"|
                             MossOBC$Order=="Grimmiales"|
                             MossOBC$Order=="Hookeriales"|
                             MossOBC$Order=="Orthotrichales"|
                             MossOBC$Order=="Pottiales")

# max α 10-25
MossOrdRich10to25 <- readRDS("Data/MossOrdRich10to25.rds")
MossOrdRich10to25
MossOBR10to25DF <- subset(MossOBC,
                          MossOBC$Order=="Funariales"|
                            MossOBC$Order=="Hedwigiales"|
                            MossOBC$Order=="Polytrichales"|
                            MossOBC$Order=="Sphagnales")

# max α < 10
MossOrdRichBelow10 <- readRDS("Data/MossOrdRichBelow10.rds")
MossOrdRichBelow10
MossOBRBelow10DF <- subset(MossOBC,
                           MossOBC$Order=="Andreaeaeales"|
                             MossOBC$Order=="Archidiales"|
                             MossOBC$Order=="Aulacomniales"|
                             MossOBC$Order=="Bryoxiphiales"|
                             MossOBC$Order=="Buxbaumiales"|
                             MossOBC$Order=="Gigaspermales"|
                             MossOBC$Order=="Hypnodendrales"|
                             MossOBC$Order=="Ptychomniales"|
                             MossOBC$Order=="Rhizogoniales"|
                             MossOBC$Order=="Splachnales")

saveRDS(MossOBRAbove100DF, "Data/MossOBRAbove100DF.rds")
saveRDS(MossOBR25to100DF, "Data/MossOBR25to100DF.rds")
saveRDS(MossOBR10to25DF, "Data/MossOBR10to25DF.rds")
saveRDS(MossOBRBelow10DF, "Data/MossOBRBelow10DF.rds")

# 1.4 Load moss max richness value groupings -------------------------------
MossOBRAbove100DF <- readRDS("Data/MossOBRAbove100DF.rds")
MossOBR25to100DF <- readRDS("Data/MossOBR25to100DF.rds")
MossOBR10to25DF <- readRDS("Data/MossOBR10to25DF.rds")
MossOBRBelow10DF <- readRDS("Data/MossOBRBelow10DF.rds")

# 1.5 MOSS Facets of richness in biomes by orders grouped by max α ---------
# 1.5.1 Max α >100 ---------------------------------------------------------
MossFacetOBRAbove100 <- ggplot(MossOBRAbove100DF, 
                               aes(x=Biome, y=Alpha, fill=Biome, color=Biome)) + 
  geom_boxplot(show.legend = FALSE, fill=biome_cols_22, color="black",
               outlier.size=1) +
  #theme_minimal() +     #un-comment whichever theme you want
  theme_gray() +
  #theme_light() +
  #theme_bw() +
  #theme_dark() +
  #theme_linedraw() +
  geom_violin(scale="count", show.legend=FALSE, fill="gray", alpha=0.35,
              color="gray25") +
  xlab("Biome") +
  ylab("Richness") +  
  theme(axis.title.y = element_text(size=32), 
        axis.title.x = element_text(size=32),
        axis.text.y = element_text(size=15), 
        axis.text.x = element_text(angle = 30, hjust = 1, size = 8))+
  facet_wrap(~Order
             , 
             #ncol=1        #un-comment # of rows you want
             ncol=2
  )
MossFacetOBRAbove100

# 1.5.2 Max α 25-100 -------------------------------------------------------
MossFacetOBR25to100 <- ggplot(MossOBR25to100DF, 
                              aes(x=Biome, y=Alpha, fill=Biome, color=Biome)) + 
  geom_boxplot(show.legend = FALSE, fill=biome_cols_65, color="black",
               outlier.size=0.7) +
  #theme_minimal() +     #un-comment whichever theme you want
  theme_gray() +
  #theme_light() +
  #theme_bw() +
  #theme_dark() +
  #theme_linedraw() +
  geom_violin(scale="count", show.legend=FALSE, fill="gray", alpha=0.35,
              color="gray25") +
  xlab("Biome") +
  ylab("Richness") +  
  theme(axis.title.y = element_text(size=32), 
        axis.title.x = element_text(size=32),
        axis.text.y = element_text(size=15), 
        axis.text.x = element_text(angle = 30, hjust = 1, size = 8))+
  facet_wrap(~Order
             , 
             #ncol=2        #un-comment # of rows you want
             ncol=3
  )
MossFacetOBR25to100

# 1.5.3 Max α 10-25 --------------------------------------------------------
MossFacetOBR10to25 <- ggplot(MossOBR10to25DF, 
                             aes(x=Biome, y=Alpha, fill=Biome, color=Biome)) + 
  geom_boxplot(show.legend = FALSE, fill=biome_cols_44, color="black",
               outlier.size=0.7) +
  #theme_minimal() +     #un-comment whichever theme you want
  theme_gray() +
  #theme_light() +
  #theme_bw() +
  #theme_dark() +
  #theme_linedraw() +
  geom_violin(scale="count", show.legend=FALSE, fill="gray", alpha=0.35, 
              color="gray25") +
  xlab("Biome") +
  ylab("Richness") +  
  theme(axis.title.y = element_text(size=32), 
        axis.title.x = element_text(size=32),
        axis.text.y = element_text(size=15), 
        axis.text.x = element_text(angle = 30, hjust = 1, size = 8))+
  facet_wrap(~Order
             , 
             ncol=2        #un-comment # of rows you want
             #ncol=4
  )
MossFacetOBR10to25

# 1.5.4 Max α <10 ----------------------------------------------------------
MossFacetOBRBelow10 <- ggplot(MossOBRBelow10DF, 
                              aes(x=Biome, y=Alpha, fill=Biome, color=Biome)) + 
  geom_boxplot(show.legend = FALSE, fill=biome_cols_93, color="black",
               outlier.size=0.7) +
  #theme_minimal() +     #un-comment whichever theme you want
  theme_gray() +
  #theme_light() +
  #theme_bw() +
  #theme_dark() +
  #theme_linedraw() +
  #geom_violin(scale="count", show.legend=FALSE, fill="gray", alpha=0.35, color="gray25") +
  xlab("Biome") +
  ylab("Richness") +  
  theme(axis.title.y = element_text(size=32), 
        axis.title.x = element_text(size=32),
        axis.text.y = element_text(size=15), 
        axis.text.x = element_text(angle = 30, hjust = 1, size = 8))+
  facet_wrap(~Order
             ,
             #ncol=6        #un-comment # of rows you want
             ncol=5
  )
MossFacetOBRBelow10


# 2.0 MOSS ORDER CONTINENT FACET PLOTS -------------------------------------
# MossOBC is made in MossPlotData.R, it's also in the BryophytesData folder
MossOBC <- readRDS("Data/MossOBC.rds")

MossFacetContBiomeRich <- ggplot(MossOBC, 
                                 aes(x=Biome, y=Alpha, fill=Biome, color=Biome)) + 
  geom_boxplot(show.legend = FALSE, fill=biome_cols_18, color="black") +
  guides(x = guide_axis(angle=30)) +
  theme_gray() +
  geom_violin(scale="count", show.legend=FALSE, fill="gray", alpha=0.35, 
              color="gray25") +
  xlab("Biome") +
  ylab("Richness") +  
  theme(axis.title.y = element_text(size=32), 
        axis.title.x = element_text(size=32),
        axis.text.y = element_text(size=15), 
        axis.text.x = element_text(angle = 30, hjust = 1, size = 8))+
  facet_wrap(~Cont)
MossFacetContBiomeRich
