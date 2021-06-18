#Plotting alpha diversity of BRYOPHYTE orders by biome and continent
#Adapted from BiomeDiversity.R
#Kathryn Dawdy and Hailey Napier, July 2020

# 0.0 FIRST ----------------------------------------------------------------

#Run DataProcessing.R, Continents.R, BiomeContinents.R, BiomeDiversity.R, 
    #ORange.R, OrdBiomeBP.R, TotalAlpha.R, 
    #then OrderBiomeDF.R - unless you can just load the DF (takes a while)
    #then MossPlotData.R - or just load the DF
OrderBiomeDF <- readRDS("Data/OrderBiomeDF.rds")
OrderBiomeContDF <- readRDS("Data/OrderBiomeContDF.rds")

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


# 1.0 BIOME RICHNESS BY ORDER ----------------------------------------------

#Hailey's function:
source("Functions/OrdBiomeBP.R")

###Run OrdBiomeBP.R
#Use OrdBiomeBP function for box, violin, or layered violin on box plot
  #Enter any order; box, violin, or boxyviolin; cont = "Southern", "Northern", or "both"
OrdBiomeBP("Hypnales", "box")
OrdBiomeBP("Hypnales", "violin")
OrdBiomeBP("Hypnales", "boxyviolin")
OrdBiomeBP("Hypnales", "boxyviolin", cont="South America")
OrdBiomeBP("Hypnales", "boxyviolin", cont="North America")
OrdBiomeBP("Hypnales", "boxyviolin", cont="both")


# 2.0 RICHNESS FACET PLOTS in biomes by order ------------------------------

# 2.1 Facet of richness in biomes by all orders ----------------------------
FacetOrdBiomeRich <- ggplot(OrderBiomeDF, 
                            aes(x=Biome, y=Alpha)) + 
  geom_boxplot(show.legend = FALSE) +
  theme_minimal() +
  xlab("Biome") +
  ylab("Richness") +  
  theme(axis.title.y = element_text(size=32), 
        axis.title.x = element_text(size=32),
        axis.text.y = element_text(size=20), 
        axis.text.x = element_text(angle = 90, hjust = 1, size = 8))+
  facet_wrap(~Order)
FacetOrdBiomeRich

png("Figures/AlphaOrderBiomeFacet.png", width = 1500, height = 1000, pointsize = 20)
FacetOrdBiomeRich
dev.off()

# 2.2 Subset order by maximum alpha diversity value ------------------------
###Run OrderRichness.R
# max α > 100
OrdRichAbove100 <- readRDS("Data/OrdRichAbove100.rds")
OrdRichAbove100
OBRAbove100DF <- subset(OrderBiomeDF, 
                        OrderBiomeDF$Order=="Hypnales"|
                          OrderBiomeDF$Order=="Dicranales")

# max α 25-100
OrdRich25to100 <- readRDS("Data/OrdRich25to100.rds")
OrdRich25to100
OBR25to100DF <- subset(OrderBiomeDF,
                       OrderBiomeDF$Order=="Bartramiales"|
                         OrderBiomeDF$Order=="Bryales"|
                         OrderBiomeDF$Order=="Grimmiales"|
                         OrderBiomeDF$Order=="Hookeriales"|
                         OrderBiomeDF$Order=="Jungermanniales"|
                         OrderBiomeDF$Order=="Orthotrichales"|
                         OrderBiomeDF$Order=="Porellales"|
                         OrderBiomeDF$Order=="Pottiales")

# max α 10-25
OrdRich10to25 <- readRDS("Data/OrdRich10to25.rds")
OrdRich10to25
OBR10to25DF <- subset(OrderBiomeDF,
                      OrderBiomeDF$Order=="Funariales"|
                        OrderBiomeDF$Order=="Hedwigiales"|
                        OrderBiomeDF$Order=="Marchantiales"|
                        OrderBiomeDF$Order=="Metzgeriales"|
                        OrderBiomeDF$Order=="Polytrichales"|
                        OrderBiomeDF$Order=="Sphagnales")

# max α < 10
OrdRichBelow10 <-readRDS("Data/OrdRichBelow10.rds")
OrdRichBelow10
OBRBelow10DF <- subset(OrderBiomeDF,
                       OrderBiomeDF$Order!="Hypnales"&
                         OrderBiomeDF$Order!="Dicranales"&
                         OrderBiomeDF$Order!="Bartramiales"&
                         OrderBiomeDF$Order!="Bryales"&
                         OrderBiomeDF$Order!="Grimmiales"&
                         OrderBiomeDF$Order!="Hookeriales"&
                         OrderBiomeDF$Order!="Jungermanniales"&
                         OrderBiomeDF$Order!="Orthotrichales"&
                         OrderBiomeDF$Order!="Porellales"&
                         OrderBiomeDF$Order!="Pottiales"&
                         OrderBiomeDF$Order!="Funariales"&
                         OrderBiomeDF$Order!="Hedwigiales"&
                         OrderBiomeDF$Order!="Marchantiales"&
                         OrderBiomeDF$Order!="Metzgeriales"&
                         OrderBiomeDF$Order!="Polytrichales"&
                         OrderBiomeDF$Order!="Sphagnales")

# max α < 25
#We ended up not using the <25 grouping, but keeping this just in case...
#OrdRichBelow25
#OBRBelow25DF <- subset(OrderBiomeDF,
#OrderBiomeDF$Order!="Hypnales"&
#OrderBiomeDF$Order!="Dicranales"&
#OrderBiomeDF$Order!="Bartramiales"&
#OrderBiomeDF$Order!="Bryales"&
#OrderBiomeDF$Order!="Grimmiales"&
#OrderBiomeDF$Order!="Hookeriales"&
#OrderBiomeDF$Order!="Jungermanniales"&
#OrderBiomeDF$Order!="Orthotrichales"&
#OrderBiomeDF$Order!="Porellales"&
#OrderBiomeDF$Order!="Pottiales")

saveRDS(OBRAbove100DF, "Data/OBRAbove100DF.rds")
saveRDS(OBR25to100DF, "Data/OBR25to100DF.rds")
saveRDS(OBR10to25DF, "Data/OBR10to25DF.rds")
saveRDS(OBRBelow10DF, "Data/OBRBelow10DF.rds")

# 2.2.5 Subset for cells that don't have centers covered
# max α > 100
OrdRichAbove100 <- readRDS("Data/OrdRichAbove100.rds")
OrdRichAbove100
OBRAbove100DF <- subset(CleanOrderBiomeDF, 
                        CleanOrderBiomeDF$Order=="Hypnales"|
                          CleanOrderBiomeDF$Order=="Dicranales")

# max α 25-100
OrdRich25to100 <- readRDS("Data/OrdRich25to100.rds")
OrdRich25to100
OBR25to100DF <- subset(CleanOrderBiomeDF,
                       CleanOrderBiomeDF$Order=="Bartramiales"|
                         CleanOrderBiomeDF$Order=="Bryales"|
                         CleanOrderBiomeDF$Order=="Grimmiales"|
                         CleanOrderBiomeDF$Order=="Hookeriales"|
                         CleanOrderBiomeDF$Order=="Jungermanniales"|
                         CleanOrderBiomeDF$Order=="Orthotrichales"|
                         CleanOrderBiomeDF$Order=="Porellales"|
                         CleanOrderBiomeDF$Order=="Pottiales")

# max α 10-25
OrdRich10to25 <- readRDS("Data/OrdRich10to25.rds")
OrdRich10to25
OBR10to25DF <- subset(CleanOrderBiomeDF,
                      CleanOrderBiomeDF$Order=="Funariales"|
                        CleanOrderBiomeDF$Order=="Hedwigiales"|
                        CleanOrderBiomeDF$Order=="Marchantiales"|
                        CleanOrderBiomeDF$Order=="Metzgeriales"|
                        CleanOrderBiomeDF$Order=="Polytrichales"|
                        CleanOrderBiomeDF$Order=="Sphagnales")

# max α < 10
OrdRichBelow10 <-readRDS("Data/OrdRichBelow10.rds")
OrdRichBelow10
OBRBelow10DF <- subset(CleanOrderBiomeDF,
                       CleanOrderBiomeDF$Order!="Hypnales"&
                         CleanOrderBiomeDF$Order!="Dicranales"&
                         CleanOrderBiomeDF$Order!="Bartramiales"&
                         CleanOrderBiomeDF$Order!="Bryales"&
                         CleanOrderBiomeDF$Order!="Grimmiales"&
                         CleanOrderBiomeDF$Order!="Hookeriales"&
                         CleanOrderBiomeDF$Order!="Jungermanniales"&
                         CleanOrderBiomeDF$Order!="Orthotrichales"&
                         CleanOrderBiomeDF$Order!="Porellales"&
                         CleanOrderBiomeDF$Order!="Pottiales"&
                         CleanOrderBiomeDF$Order!="Funariales"&
                         CleanOrderBiomeDF$Order!="Hedwigiales"&
                         CleanOrderBiomeDF$Order!="Marchantiales"&
                         CleanOrderBiomeDF$Order!="Metzgeriales"&
                         CleanOrderBiomeDF$Order!="Polytrichales"&
                         CleanOrderBiomeDF$Order!="Sphagnales")

saveRDS(OBRAbove100DF, "Data/CleanOBRAbove100DF.rds")
saveRDS(OBR25to100DF, "Data/CleanOBR25to100DF.rds")
saveRDS(OBR10to25DF, "Data/CleanOBR10to25DF.rds")
saveRDS(OBRBelow10DF, "Data/CleanOBRBelow10DF.rds")

# 2.3 Load max richness value groupings ------------------------------------
OBRAbove100DF <- readRDS("Data/OBRAbove100DF.rds")
OBR25to100DF <- readRDS("Data/OBR25to100DF.rds")
OBR10to25DF <- readRDS("Data/OBR10to25DF.rds")
OBRBelow10DF <- readRDS("Data/OBRBelow10DF.rds")

# 2.4 Facets of richness in biomes by orders grouped by max α --------------
# 2.4.1 Max α >100 ---------------------------------------------------------
FacetOBRAbove100 <- ggplot(OBRAbove100DF, 
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
FacetOBRAbove100

png("Figures/AlphaOrderBiomeAbove100.png", width = 1500, height = 1000, pointsize = 20)
FacetOBRAbove100
dev.off()

# 2.4.2 Max α 25-100 -------------------------------------------------------
FacetOBR25to100 <- ggplot(OBR25to100DF, 
                          aes(x=Biome, y=Alpha, fill=Biome, color=Biome)) + 
  geom_boxplot(show.legend = FALSE, fill=biome_cols_87, color="black",
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
             ncol=4
  )
FacetOBR25to100

png("Figures/AlphaOrderBiome25to100.png", width = 1500, height = 1000, pointsize = 20)
FacetOBR25to100
dev.off()

# 2.4.3 Max α 10-25 --------------------------------------------------------
FacetOBR10to25 <- ggplot(OBR10to25DF, 
                         aes(x=Biome, y=Alpha, fill=Biome, color=Biome
                         )) + 
  geom_boxplot(show.legend = FALSE, fill=biome_cols_66, color="black",
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
             ncol=3        #un-comment # of rows you want
             #ncol=2
  )
FacetOBR10to25

png("Figures/AlphaOrderBiome10to25.png", width = 1500, height = 1000, pointsize = 20)
FacetOBR10to25
dev.off()

# 2.4.4 Max α <10 ----------------------------------------------------------
FacetOBRBelow10 <- ggplot(OBRBelow10DF, 
                          aes(x=Biome, y=Alpha, fill=Biome, color=Biome)) + 
  geom_boxplot(show.legend = FALSE, fill=biome_cols_166, color="black",
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
        axis.text.x = element_text(angle = 90, hjust = 1, size = 8))+
  facet_wrap(~Order
             ,
             ncol=6        #un-comment # of rows you want
             #ncol=5
  )
FacetOBRBelow10

png("Figures/AlphaOrderBiomeBelow10.png", width = 1500, height = 1000, pointsize = 20)
FacetOBRBelow10
dev.off()


# 2.5 Plots with weighted cell biome count
# 2.5.1 Max α >100 ---------------------------------------------------------
CleanFacetOBRAbove100 <- ggplot(CleanOBRAbove100DF, 
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
CleanFacetOBRAbove100

png("Figures/CleanAlphaOrderBiomeAbove100.png", width = 1500, height = 1000, pointsize = 20)
CleanFacetOBRAbove100
dev.off()

# 2.4.2 Max α 25-100 -------------------------------------------------------
CleanFacetOBR25to100 <- ggplot(CleanOBR25to100DF, 
                          aes(x=Biome, y=Alpha, fill=Biome, color=Biome)) + 
  geom_boxplot(show.legend = FALSE, fill=biome_cols_87, color="black",
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
             ncol=4
  )
CleanFacetOBR25to100

png("Figures/CleanAlphaOrderBiome25to100.png", width = 1500, height = 1000, pointsize = 20)
CleanFacetOBR25to100
dev.off()

# 2.4.3 Max α 10-25 --------------------------------------------------------
CleanFacetOBR10to25 <- ggplot(CleanOBR10to25DF, 
                         aes(x=Biome, y=Alpha, fill=Biome, color=Biome
                         )) + 
  geom_boxplot(show.legend = FALSE, fill=biome_cols_66, color="black",
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
             ncol=3        #un-comment # of rows you want
             #ncol=2
  )
CleanFacetOBR10to25

png("Figures/CleanAlphaOrderBiome10to25.png", width = 1500, height = 1000, pointsize = 20)
CleanFacetOBR10to25
dev.off()

# 2.4.4 Max α <10 ----------------------------------------------------------
CleanFacetOBRBelow10 <- ggplot(CleanOBRBelow10DF, 
                          aes(x=Biome, y=Alpha, fill=Biome, color=Biome)) + 
  geom_boxplot(show.legend = FALSE, fill=biome_cols_166, color="black",
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
        axis.text.x = element_text(angle = 90, hjust = 1, size = 8))+
  facet_wrap(~Order
             ,
             ncol=6        #un-comment # of rows you want
             #ncol=5
  )
CleanFacetOBRBelow10

png("Figures/CleanAlphaOrderBiomeBelow10.png", width = 1500, height = 1000, pointsize = 20)
CleanFacetOBRBelow10
dev.off()


# 3.0 CONTINENT FACET PLOTS -----------------------------------------------
FacetContBiomeRich <- ggplot(OrderBiomeContDF, 
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
CleanFacetContBiomeRich

png("Figures/AlphaBiomeContinents.png", width = 1500, height = 1000, pointsize = 20)
FacetContBiomeRich
dev.off()

# 4.0 Orders on x axis, facet of biomes boxplots ---------------------------
FacetRichOrder <- ggplot(OrderBiomeDF, 
                         aes(x=Order, y=Alpha)) + 
  geom_boxplot(show.legend = FALSE) +
  theme_minimal() +
  xlab("Order") +
  ylab("Richness") +  
  theme(axis.title.y = element_text(size=32), 
        axis.title.x = element_text(size=32),
        axis.text.y = element_text(size=20), 
        axis.text.x = element_text(angle = 90, hjust = 1, size = 6))+
  facet_wrap(~Biome)
FacetRichOrder

png("Figures/AlphaOrderBiomeSwapped.png", width = 1500, height = 1000, pointsize = 20)
FacetRichOrder
dev.off()
