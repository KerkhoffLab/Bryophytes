# 03 Lineage Analysis
# Lineage Analysis Code for Moss Manuscript
# Compiled by Hailey Napier, June 2021

# 0.0 Load Data and Packages, Source Functions -----
## 0.1 Load packages ----
#if (!requireNamespace("BiocManager", quietly = TRUE))
  #install.packages("BiocManager")
#BiocManager::install("ggtree")
#BiocManager::install("treeio")
#BiocManager::install("rphast")
install.packages("ggplot2")
install.packages("ggimage")
install.packages("ape")
install.packages("tidyr")
install.packages("dplyr")
install.packages("gridExtra")
install.packages("png")
install.packages("phytools")
install.packages("tools")
install.packages("grid")
library(ggplot2)
library(ggimage)
library(ape)
library(tidyr)
library(dplyr)
library(gridExtra)
library(png)
library(phytools)
library(tools)
library(grid)

## 0.2 Load data (generated in data processing script ***not yet finished***) ----
# From DataProcessing2020.R
MossPresence <- readRDS("Data/MossPresence.rds")
OrderNames <- readRDS("Data/OrderNames.rds")
MossOrderNames <- readRDS("Data/MossOrderNames.rds")
MossOrderRichList <- readRDS("Data/MossOrderRichList.rds")
MossRichnessVec <- readRDS("Data/MossRichnessVec.rds")
MossCellRichness <- readRDS("Data/MossCellRichness.rds")

# From 01_BetaDiversityMS.R
MossBiomeBetaCellsClean <- ("Data/MossBiomeBetaCellsClean.rds")

# ~not sure where this is from~
LongLatDF <- readRDS("Data/LongLatDF.rds")

## 0.3 Source function
source("Functions/ORange.R")


# 1.0 Read and process Liu et al (2019) phologenetic tree ----

## 1.1 Download tree ----
  # Download tree from Dryad via https://datadryad.org/stash/dataset/doi:10.5061/dryad.tj3gd75
  # Put downloaded tree into Data/trees 

## 1.2 Read tree into script (from Data/trees) ----
FigS20 <- read.tree("Data/trees/aa-nu-RAxML-FigS20.tre")

## 1.3 Extract tree data and store as a dataframe ----
FigS20_FOG <- ggtree(FigS20)$data

FigS20_names <- data.frame(FigS20_FOG$label <- gsub("_", " ", FigS20_FOG$label))
names(FigS20_names)[1] <- "name"
FigS20_names <- separate(FigS20_names, name, into = c("genus","other"))
FigS20_names$other <- NULL
FigS20_names$family <- NA
FigS20_names$order <- NA
FigS20_names$group <- NA

## 1.4 Make list of genus names in our data ----
unique(BryophytePresence$Family[which(BryophytePresence$Genus == FigS20_FOG$genus[i])])
GenusNames <- unique(BryophytePresence$Genus)
GenusNames <- GenusNames[complete.cases(GenusNames)]

## 1.5 Clean up dataframe and assign genus and family names ----
for(i in 1:nrow(FigS20_names)){
  n <- FigS20_names$genus[i]
  FigS20_names$genus[i] <- ifelse(n == "sphagnumL" | n == "sphagnumP" | n == "sphagnumR", "Sphagnum", n)
  FigS20_names$genus[i] <- gsub('[[:digit:]]+', '', FigS20_names$genus[i])
  FigS20_names$genus[i] <- toTitleCase(FigS20_names$genus[i])
  test.genus <- FigS20_names$genus[i]
  for(j in 1:length(GenusNames)){
    ref.genus <- GenusNames[j]
    if(test.genus == ref.genus){
      FigS20_names$family[i] <- unique(BryophytePresence$Family[which(BryophytePresence$Genus == test.genus)])
      FigS20_names$order[i] <- unique(BryophytePresence$Order[which(BryophytePresence$Genus == test.genus)])
      FigS20_names$group[i] <- unique(BryophytePresence$Group[which(BryophytePresence$Genus == test.genus)])
      break
    }
  }
}

FigS20_FOG <- bind_cols(FigS20_FOG, FigS20_names)
write.csv(FigS20_FOG, "Data/FamilyTrees/FigS20_FOG.csv")

## 1.6 Group tip labels into families ----
### 1.61 Get family names for each tip label ----
#### Make a dataframe that contains the tip labels and their families for reference
clean20famlabels <- FigS20_FOG %>%
  filter(isTip == "TRUE") %>%
  select(family)
Ref20DF <- data.frame(FigS20$tip.label)
Ref20DF$Family <- clean20famlabels
names(Ref20DF)[1] <- "Species"

### 1.62 Loop to find species in each family ----
#### Creates a vector of species/tip label names for each family name
FigS20fam <- F20Fam$Family
FigS20fam
for(i in 1:length(FigS20fam)){
  fam <- FigS20fam[i]
  tempdf <- Ref20DF %>%
    filter(Family == fam)
  tempvec <- tempdf$Species
  assign(fam, tempvec)
}

## 1.7 Group tip lables into orders ----
### 1.71 Get order names for each tip label ----
#### Get order names
clean20orderlabels <- FigS20_FOG %>%
  filter(FigS20_FOG$isTip == "TRUE") %>%
  select(order)
Ref20DF <- data.frame(FigS20$tip.label)
Ref20DF$Family <- clean20famlabels
Ref20DF$Order <- clean20orderlabels
names(Ref20DF)[1] <- "Species"

### 1.72 Loop to find species in each order ----
#### Creates a vector of species names for each order name
FigS20order <- unique(FigS20_FOG$order)
FigS20order <- FigS20order[complete.cases(FigS20order)]
FigS20order
FigS20orderlist <- list()
for(i in 1:length(FigS20order)){
  order <- FigS20order[i]
  tempdf <- Ref20DF %>%
    filter(Order == order)
  tempvec <- tempdf$Species
  FigS20orderlist[[i]] <- tempvec
}

## 1.8 Manually update BryophytePresence .csv file to reflect family and order species assignments from Liu et al. (2019) ----


# 2.0 Group orders based on max alpha diversity ----

## 2.1 Create a dataframe including each moss order, its alpha diversity in each cell, and the biome associated with that cell ----
### If a cell has more than one biome, then the biome assigned is the one that covers the largest proportion of the cell
nrows <- 3654234 #3639196
MossOrderBiomeDF <- data.frame(rep(NA,nrows))
names(MossOrderBiomeDF)[1] <- "Alpha"
MossOrderBiomeDF$Alpha <- NA
MossOrderBiomeDF$CellID <- NA
MossOrderBiomeDF$Order <- NA
MossOrderBiomeDF$Biome <- NA

start <- 1
end <- 15038
o <- MossOrderNames[1]
b <- BiomeNames[1]
MossOrderBiomeDF$Alpha[start:end] <- ORange(o,b,"both","clean")
MossOrderBiomeDF$CellID[start:end] <- c(1:15038)
MossOrderBiomeDF$Biome[start:end] <- b
MossOrderBiomeDF$Order[start:end] <- o

for(j in 2:length(BiomeNames)){
  b <- BiomeNames[j]
    start <- end + 1
  end <- start + 15037
  MossOrderBiomeDF$Alpha[start:end] <- ORange(o,b,"both","clean")
  MossOrderBiomeDF$CellID[start:end]<- c(1:15038)
  MossOrderBiomeDF$Biome[start:end] <- b
  MossOrderBiomeDF$Order[start:end] <- o
}

for(m in 2:length(MossOrderNames)){
  o <- MossOrderNames[m]
  for(n in 1:length(BiomeNames)){
    b <- BiomeNames[n]
    start <- end + 1
    end <- start + 15037
    MossOrderBiomeDF$Alpha[start:end] <- ORange(o,b,"both","clean")
    MossOrderBiomeDF$CellID[start:end]<- c(1:15038)
    MossOrderBiomeDF$Biome[start:end] <- b
    MossOrderBiomeDF$Order[start:end] <- o
  }
}


## 2.2 Make vectors based on max alpha diversity ----
### Make a dataframe to look at numbers for max alpha diversity 
MossOrderMaxAlpha <- data.frame(tapply(MossOrderBiomeDF$Alpha, MossOrderBiomeDF$Order, max, na.rm = T))
names(MossOrderMaxAlpha)[1]  <- "MaxAlpha"
Names <- sort(unique(MossOrderBiomeDF$Order))
MossOrderMaxAlpha$Names <- Names

### Put order names into vectors based on max alpha diversity 
MossOrdRichAbove100 <- vector()
MossOrdRich26to100 <- vector()
MossOrdRich11to25 <- vector()
MossOrdRich10orBelow <- vector()
for(i in 1:length(Names)){
  name <- Names[i]
  if(MossOrderMaxAlpha$MaxAlpha[i] > 100){
    MossOrdRichAbove100 <- c(MossOrdRichAbove100, name)
  }else if(MossOrderMaxAlpha$MaxAlpha[i] > 25){
    MossOrdRich26to100 <- c(MossOrdRich26to100, name)
  }else if(MossOrderMaxAlpha$MaxAlpha[i] > 10){
    MossOrdRich11to25 <- c(MossOrdRich11to25, name)
  }else if(MossOrderMaxAlpha$MaxAlpha[i] <= 10){
    MossOrdRich10orBelow <- c(MossOrdRich10orBelow, name)
  }
}

saveRDS(MossOrdRichAbove100, "Data/MossOrdRichAbove100.rds")
saveRDS(MossOrdRich26to100, "Data/MossOrdRich26to100.rds")
saveRDS(MossOrdRich11to25, "Data/MossOrdRich11to25.rds")
saveRDS(MossOrdRich10orBelow, "Data/MossOrdRich10orBelow.rds")

### Make a dataframe containing order names and richness group categorization
MossOrdRichGroupsDF <- data.frame(Order = unique(MossPresence$Order), RichGroup = NA)
for(i in 1:nrow(MossOrdRichGroupsDF)){
  order = MossOrdRichGroupsDF$Order[i]
  if(order %in% MossOrdRich10orBelow){
    MossOrdRichGroupsDF$RichGroup = "Least diverse (10 or fewer species)"
  }else if(order %in% MossOrdRich11to25){
    MossOrdRichGroupsDF$RichGroup = "Less diverse (11 - 25 species)"
  }else if(order %in% MossOrdRich26to100){
    MossOrdRichGroupsDF$RichGroup = "More diverse (26 - 100 species)"
  }else if(order %in% MossOrdRichAbove100){
    MossOrdRichGroupsDF$RichGroup = "Most diverse (greater than 100 species)"
  }
}

### Add richness groups to MossOrderBiomeDF and save
MossOrderBiomeDF <- merge(MossOrderBiomeDF, MossOrdRichGroupsDF, by = "Order")
saveRDS(MossOrderBiomeDF, file = "Data/MossOrderBiomeDF.rds")


# 3.0 Latitude Scatterplot ----
## 3.1 Add longitude and latitude to MossOrderBiomeDF ----
MossOrderBiomeLatDF <- merge(MossOrderBiomeDF, LongLatDF, by = "CellID")

## 3.2 Create subsetted dataframes for each richness group ----
MostDiverseMoss <- MossOrderBiomeLatDF %>%
  filter(Group == "Most diverse (greater than 100 species)")
MoreDiverseMoss <- MossOrderBiomeLatDF %>%
  filter(Group == "More diverse (26 - 100 species)")
LessDiverseMoss <- MossOrderBiomeLatDF %>%
  filter(Group == "Less diverse (11 - 25 species)")
LeastDiverseMoss <- MossOrderBiomeLatDF %>%
  filter(Group == "Least diverse (10 or fewer species)")

## 3.3 Plot each order richness group in its own plot with a new y-axis scale ----
### Most diverse orders
MossOrderMostRich <- ggplot(MostDiverseMoss, 
                            aes(Latitude, Alpha, color=Order), 
                            show.legend=TRUE) +
  geom_point(shape=16, size=5, alpha=0.6) +
  xlab("Latitude") +
  ylab("Alpha Diversity") +
  theme_minimal() +
  scale_color_manual(values=c("Hypnales" = mossorderpal[7],
                              "Dicranales" = mossorderpal[10])) + 
  theme(axis.title.y = element_text(size=29),
        axis.title.x = element_text(size=29),
        axis.text = element_text(size=20),
        plot.title = element_text(size=32, hjust=0.5),
        legend.title = element_text(size=29),
        legend.text = element_text(size = 20)) + 
  labs(title = "Most Diverse (greater than 100 species)")
MossOrderMostRich
png("Figures/MossOrderMostRich_AlphaScatter.png", width = 1500, height = 1000,pointsize = 20)
MossOrderMostRich
dev.off()

### More diverse orders
MossOrderMoreRich <- ggplot(MoreDiverseMoss, 
                            aes(Latitude, Alpha, color=Order), 
                            show.legend=TRUE) +
  geom_point(shape=16, size=5, alpha=0.6) +
  xlab("Latitude") +
  ylab("Alpha Diversity") +
  theme_minimal() +
  scale_color_manual(values=c("Bartramiales" = mossorderpal[6],
                              "Bryales" = mossorderpal[7],
                              "Grimmiales" = mossorderpal[8],
                              "Hookeriales" = mossorderpal[9],
                              "Orthotrichales" = mossorderpal[4],
                              "Pottiales" = mossorderpal[2])) +
  theme(axis.title.y = element_text(size=29),
        axis.title.x = element_text(size=29),
        axis.text = element_text(size=20),
        plot.title = element_text(size=32, hjust=0.5),
        legend.title = element_text(size=29),
        legend.text = element_text(size = 20)) +
  labs(title = "More Diverse (26 - 100 species)")
MossOrderMoreRich
png("Figures/MossOrderMoreRich_AlphaScatter.png", width = 1500, height = 1000, pointsize = 20)
MossOrderMoreRich
dev.off()

### Less diverse orders
MossOrderLessRich <- ggplot(LessDiverseMoss, 
                            aes(Latitude, Alpha, color=Order), 
                            show.legend=TRUE) +
  geom_point(shape=16, size=5, alpha=0.6) +
  xlab("Latitude") +
  ylab("Alpha Diversity") +
  #geom_jitter(height = 0.3) +
  theme_minimal() +
  scale_color_manual(values=c("Funariales" = mossorderpal[1],
                              "Hedwigiales" = mossorderpal[2],
                              "Polytrichales" = mossorderpal[3],
                              "Sphagnales" = mossorderpal[11])) +
  theme(axis.title.y = element_text(size=29),
        axis.title.x = element_text(size=29),
        axis.text = element_text(size=20),
        plot.title = element_text(size=32, hjust=0.5),
        legend.title = element_text(size=29),
        legend.text = element_text(size = 20)) +
  labs(title = "Less Diverse (11-25 species)")
MossOrderLessRich
png("Figures/MossOrderLessRich_AlphaScatter.png", width = 1500, height = 1000, pointsize = 20)
MossOrderLessRich
dev.off()

### Least diverse orders
MossOrderLeastRich <- ggplot(LeastDiverseMoss, 
                             aes(Latitude, Alpha, color=Order), 
                             show.legend=TRUE) +
  geom_point(shape=16, size=5, alpha=0.6) +
  xlab("Latitude") +
  ylab("Alpha Diversity") +
  #geom_jitter(0.3) +
  theme_minimal() +
  scale_color_manual(values=c("Andreaeaeales" = mossorderpal[1],
                              "Archidiales" = mossorderpal[2],
                              "Aulacomniales" = mossorderpal[3],
                              "Bryoxiphales" = mossorderpal[4],
                              "Buxbaumiales" = mossorderpal[5],
                              "Gigaspermales" = mossorderpal[6],
                              "Hypnodendrales" = mossorderpal[7],
                              "Ptychomniales" = mossorderpal[8],
                              "Rhizogoniales" = mossorderpal[9],
                              "Splachnales" = mossorderpal[10])) +
  theme(axis.title.y = element_text(size=29),
        axis.title.x = element_text(size=29),
        axis.text = element_text(size=20),
        plot.title = element_text(size=32, hjust=0.5),
        legend.title = element_text(size=29),
        legend.text = element_text(size = 20)) +
  labs(title = "Least Diverse (10 or fewer species)")
MossOrderLeastRich
png("Figures/MossOrderLeastRich_AlphaScatter.png", width = 1500, height = 1000, pointsize = 20)
MossOrderLeastRich
dev.off()

### Arrange the richness group plots into a grid
OrderAlphaScatter_Grid <- grid.arrange(MossOrderMostRich, MossOrderMoreRich, MossOrderLessRich, MossOrderLeastRich, nrow = 2)
png("Figures/OrderAlphaScatter_Grid.png", width = 1500, height = 1000, pointsize = 20)
plot(OrderAlphaScatter_Grid)
dev.off()


# 4.0 Biome Analyses



# UNFINISHED STARTING HERE ==================================

###CHANGE THIS SO IT WORKS WITH THE DATAFRAME I HAVE ALREADY WRITTEN####
# 2.0 Find percentage of cells with biome for biomes in each order --------------
# 2.1 Make a matrix with total cell counts for each order within each biome
NumberBiomes <- 11
BiomeNamesAndTotal <- c(BiomeNames, "Total")

#MossOrderBiome = MOB
MOBMat <- matrix(NA, 22, 12)
rownames(MOBMat) <- MossOrderNames
colnames(MOBMat) <- BiomeNamesAndTotal

for(i in 1:NumberOrders){
  order <- MossOrderNames[i]
  
  DF <- MossOrderBiomeList[[i]]
  DF <- DF %>%
    dplyr::filter(!is.na(DF$Biome))
  
  total <- nrow(DF)
  biome = "Total"
  MOBMat[order, biome] <- total
  
  biomes <- DF %>%
    dplyr::select(Biome)
  biomes <- unique(as.vector(biomes$Biome))
  print(order)
  print(biomes)
  
  for(j in 1:length(biomes)){
    biome = biomes[j]
    nbiome <- DF %>%
      dplyr::filter(DF$Biome == biome)
    nbiome <- nrow(nbiome)
    
    MOBMat[order, biome] <- nbiome
  }
}

#save matrix
saveRDS(MOBMat, "Data/MOBMat.rds")





