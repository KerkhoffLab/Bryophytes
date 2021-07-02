# 03 Lineage Analysis
# Lineage Analysis Code for Moss Manuscript
# Compiled by Hailey Napier, June and July 2021

# 0.0 Load Data and Packages, Source Functions -----
## 0.1 Load packages ----
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
install.packages("circlize")
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
library(circlize)

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


# 4.0 Biome Analyses ----
## 4.1 Make moss order/species dataframe ----
MossOrdSpecBioDF <- data.frame("Species" = rep(NA, 2976), "Order" = rep(NA, 2976))
end <- 0
for(i in 1:NumberOrders){
  order <- MossOrderNames[i]
  species_list <- MossOrderSpeciesList[[i]]
  number_species <- length(species_list)
  start <- end + 1
  end <- end + number_species
  MossOrdSpecBioDF$Species[start:end] <- species_list
  MossOrdSpecBioDF$Order[start:end] <- order
}

## 4.2 Add biome column for the biome where each species is most abundant ----
MossOrdSpecBioDF$Biome <- NA
tempDF <- data.frame(Species = NA, Order = NA, Biome = NA)
for(i in 1:nrow(MossOrdSpecBioDF)){
  order <- MossOrdSpecBioDF$Order[i]
  species <- MossOrdSpecBioDF$Species[i]
  biomenumcells <- vector()
  for(j in 1:length(BiomeNames)){
    biome <- BiomeNames[j]
    ab <- SpBiMat[species,biome]
    if(ab == 0){
      ab = NA
    }
    biomenumcells[j] <- ab
  }
  if(sum(biomenumcells, na.rm = T) > 0){
    biomeindex <- which(biomenumcells == max(biomenumcells, na.rm = T))
    maxbiome <- BiomeNames[biomeindex]
    if(length(maxbiome) == 1){
      MossOrdSpecBioDF$Biome[which(MossOrdSpecBioDF$Species == species)] <- maxbiome
    }else if(length(maxbiome) > 1){
      multimaxone <- maxbiome[1]
      MossOrdSpecBioDF$Biome[which(MossOrdSpecBioDF$Species == species)] <- multimaxone
      multimaxtwo <- maxbiome[2]
      tempDF <- tempDF %>% add_row(Species = species, Order = order, Biome = multimaxtwo)
    }
  }
}

### Add in species that have the same max abundance value in multiple biomes
MossOrdSpecBioDF <- bind_rows(MossOrdSpecBioDF, tempDF)

### Omit species where biome == NA
MossOrdSpecBioDF <- MossOrdSpecBioDF[complete.cases(MossOrdSpecBioDF), ]

### Save MossOrdSpecBioDF
saveRDS(MossOrdSpecBioDF, "Data/MossOrdSpecBioDF.rds")

## 4.3 Make moss biome circle plot ----
### Load colors
biome_cols_11 <- c("#D8B70A", "#972D15", "#A2A475", "#81A88D", "#02401B",
                   "#446455", "#FDD262", "#D3DDDC", "#C7B19C", "#798E87", 
                   "#C27D38")
grid.col <- c(Coniferous_Forests = "#D8B70A", Dry_Forest = "#972D15", 
              Mediterranean_Woodlands = "#A2A475", Moist_Forest = "#81A88D", 
              Savannas = "#02401B", Taiga = "#446455", Temperate_Grasslands = "#FDD262", 
              Temperate_Mixed = "#D3DDDC", Tropical_Grasslands = "#C7B19C", Tundra = "#798E87", 
              Xeric_Woodlands = "#C27D38", Hypnales = "grey", Porellales = "grey",  
              Pottiales = "grey", Hookeriales  = "grey", Bryales = "grey", Jungermanniales = "grey", 
              Andreaeaeales = "grey", Splachnales = "grey", Orthotrichales  =  "grey",  
              Bartramiales = "grey", Metzgeriales = "grey", Dicranales = "grey", Anthocerotales = "grey",
              Funariales = "grey", Treubiales = "grey", Archidiales = "grey", Marchantiales = "grey", 
              Polytrichales = "grey", Aulacomniales = "grey", Fossombroniales = "grey", 
              Grimmiales = "grey", Hedwigiales = "grey", Bryoxiphiales = "grey", Buxbaumiales  = "grey",
              Ptychomniales = "grey", Gigaspermales = "grey", Dendrocerotales  = "grey", 
              Pleuroziales = "grey", Rhizogoniales = "grey", Haplomitriales = "grey", Hypnodendrales  = "grey", 
              Pallaviciniales = "grey",  Pelliales = "grey", Notothyladales = "grey", Ricciales = "grey", 
              Ptilidiales = "grey", Sphaerocarpales = "grey", Sphagnales = "grey")

### Make matrix for plotting
CircleMatAllMoss <- matrix(NA, 22, 11)
rownames(CircleMatAllMoss) <- MossOrderNames
colnames(CircleMatAllMoss) <- BiomeNames
SortedMossOrderNames <- sort(MossOrderNames)
tallytable <- table(MossOrdSpecBioDF$Order, MossOrdSpecBioDF$Biome)

for(j in 1:NumberOrders){
  order <- SortedMossOrderNames[j]
  for(k in 1:NumberBiomes){
    biome <- BiomeNames[k]
    richness <- tallytable[j,k]
    CircleMatAllMoss[order, biome] <-  richness
  }
}

### Save matrix for quantitative analysis
saveRDS(CircleMatAllMoss, "Data/CircleMatAllMoss.rds")

### Plot
circos.clear()
png("Figures/CircleMoss.png", width = 1000, height = 1000, pointsize = 20)
circos.par(start.degree = 0)
chordDiagram(CircleMatAllMoss, grid.col = grid.col, column.col = biome_cols_11, 
             directional = 1, direction.type = "arrows", link.arr.type = "big.arrow", 
             link.arr.length = 0.05, link.largest.ontop = T, annotationTrack = c("grid"), 
             preAllocateTracks = 1, big.gap = 20, small.gap = 2)
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex=0.5)
}, bg.border = NA)
dev.off()


## 4.4 Make null model ----
NullMOBMat <- matrix(NA, 22, 12)
rownames(NullMOBMat) <- MossOrderNames
colnames(NullMOBMat) <- BiomeNamesAndTotal

NumberReps <- 1000

for(i in 1:NumberReps){
  ### Use transform to shuffle order column (assign random order to each species)
  DF <- transform(MossOrdSpecBioDF, Order = sample(Order))
  ### Tally species in each order by biome
  tallytable <- table(DF$Order, DF$Biome)
  ### Store species richness values for each biome by order in a matrix
  for(j in 1:NumberOrders){
    order <- SortedMossOrderNames[j]
    for(k in 1:NumberBiomes){
      biome <- BiomeNames[k]
      richness <- tallytable[j,k]
      NullMOBMat[order, biome] <- sum(NullMOBMat[order, biome], richness, na.rm = T)
    }
  }
}

## 4.5 Calculate null species richness totals for each order ----
totals <- rowSums(NullMOBMat, na.rm = T)
for(i in 1:NumberOrders){
  order <- MossOrderNames[i]
  total <- totals[[i]]
  NullMOBMat[order, "Total"] <- total
}

## 4.6 Make a percentage of total species richness matrix for null data ----
NullMOBPerMat <- NullMOBMat

for(i in 1:NumberOrders){
  order <- MossOrderNames[i]
  total <- NullMOBMat[order, "Total"]
  for(j in 1:NumberBiomes){
    biome <- BiomeNames[j]
    biome_abundance <- NullMOBPerMat[order, biome]
    percent <- biome_abundance/total*100
    NullMOBPerMat[order, biome] <- percent
  }
}


##### NOT FINISHED #####

## 4.7 Null model analysis ----
### Divide each cell by number of reps to get null mean
NullMOBMat <- NullMOBMat/NumberReps
NullMeanTable <- apply(NullMOBMat, 2, mean)

### Observed data -- number of species in each biome
ObsMeanTable <- colMeans(CircleMatAllMoss)
ObsSDTable <- apply(CircleMatAllMoss, 2, sd)

### Make a matrix for comparing observed and null means and SDs
MOBStats <- matrix(NA, 11, 4)
rownames(MOBStats) <- BiomeNames
colnames(MOBStats) <- c("ObsMean", "ObsSD", "NullMean", "ZScore")

for(i in 1:NumberBiomes){
  biome <- BiomeNames[i]
  MOBStats[biome, "ObsMean"] <- ObsMeanTable[[i]]
  MOBStats[biome, "ObsSD"]<- ObsSDTable[[i]]
  MOBStats[biome, "NullMean"]<- NullMeanTable[[i]]
  zscore <- ((MOBStats[biome, "ObsMean"] - MOBStats[biome, "NullMean"])/MOBStats[biome, "ObsSD"])
  MOBStats[biome, "ZScore"] <- zscore
}

### Compare observered and null percentages of species richness
NullPerMeanTable <- apply(NullMOBPerMat, 2, mean)

ObsPerMeanTable <- apply(MOBPerMatSpecies, 2, mean)
ObsPerSDTable <- apply(MOBPerMatSpecies, 2, sd)

PerMOBStats <- matrix(NA, 11, 4)
rownames(PerMOBStats) <- BiomeNames
colnames(PerMOBStats) <- c("ObsMean", "ObsSD", "NullMean", "ZScore")

for(i in 1:NumberBiomes){
  biome <- BiomeNames[i]
  PerMOBStats[biome, "ObsMean"] <- ObsPerMeanTable[[i]]
  PerMOBStats[biome, "ObsSD"]<- ObsPerSDTable[[i]]
  PerMOBStats[biome, "NullMean"]<- NullPerMeanTable[[i]]
  zscore <- ((MOBStats[biome, "ObsMean"] - PerMOBStats[biome, "NullMean"])/PerMOBStats[biome, "ObsSD"])
  PerMOBStats[biome, "ZScore"] <- zscore
}

### Orders
### Is this (single) order overrepresented in this biome?
#### Make a zscore matrix
BiomeSD <- apply(CircleMatAllMoss, 2, sd)

MOBZMatBiomeSD <- CircleMatAllMoss
for(i in 1:NumberOrders){
  order <- MossOrderNames[i]
  for(j in 1:NumberBiomes){
    biome <- BiomeNames[j]
    sd <- BiomeSD[j]
    nullpercent <- NullMOBPerMat[order, biome]
    obspercent <- MOBPerMatSpecies[order, biome]
    zscore <- ((nullpercent - obspercent)/sd)
    MOBZMatBiomeSD[order, biome] <- zscore
  }
}

# 4.8 Plotting Z-Scores
## Make a dataframe for plotting
MOBZScoreDFBiomeSD <- data.frame(Order = rep(NA, 242), Biome = rep(NA, 242), ZScore = rep(NA, 242), Group = rep(NA, 242))
rownumber <- 0
for(i in 1:NumberOrders){
  order <- MossOrderNames[i]
  if(order %in% MossOrdRichBelow10){
    group <- "Least diverse (10 or fewer species)"
  }else if(order %in% MossOrdRich10to25){
    group <- "Less diverse (11 - 25 species)"
  }else if(order %in% MossOrdRich25to100){
    group <- "More diverse (26 - 100 species)"
  }else if(order %in% MossOrdRichAbove100){
    group <- "Most diverse (greater than 100 species)"
  }
  for(j in 1:NumberBiomes){
    biome <- BiomeNames[j]
    zscore <- MOBZMatBiomeSD[order,biome]
    rownumber <- rownumber + 1
    MOBZScoreDFBiomeSD$Order[rownumber] <- order
    MOBZScoreDFBiomeSD$Biome[rownumber] <- biome
    MOBZScoreDFBiomeSD$ZScore[rownumber] <- zscore
    MOBZScoreDFBiomeSD$Species_Richness[rownumber] <- group
  }
}

## Doptplot
ZScoreDotBiomeSD <- ggplot(data = MOBZScoreDFBiomeSD, aes(x = Biome, y = ZScore, fill = Species_Richness)) +
  geom_dotplot(binaxis = "y", stackdir= "center", 
               dotsize = 0.5, alpha = 0.8, binwidth = 0.25) +
  theme(legend.position = "right") +
  theme_minimal() +
  labs(fill = "Species Richness Level") +
  scale_fill_brewer(palette="Greens") +
  theme(axis.text.x = element_text(angle = 45,  hjust = 1, size =11)) +
  theme(axis.title.x = element_text(size = 15)) +
  theme(axis.title.y = element_text(size = 15)) + 
  theme(legend.title = element_text(size = 15)) + 
  theme(legend.text = element_text(size = 11)) +
  geom_hline(yintercept = -2, color = "darkblue", linetype = "dashed")
ZScoreDotBiomeSD
png("Figures/ZScoreDotBiomeSD.png", width = 1500, height = 1000, pointsize = 20)
ZScoreDotBiomeSD
dev.off()













# UNFINISHED STARTING HERE ==================================

## 4.1 Make a matrix with total cell counts for each order within each biome
NumberBiomes <- 11
BiomeNamesAndTotal <- c(BiomeNames, "Total")

### MOB = MossOrderBiome
### Initialize matrix and set row and column names
MOBMat <- matrix(NA, 22, 12)
rownames(MOBMat) <- MossOrderNames
colnames(MOBMat) <- BiomeNamesAndTotal

### Loop through orders
for(i in 1:NumberOrders){
  order <- MossOrderNames[i]
  
  # Create a temporary subsetted dataframe that includes only the specified order and only the cells where alpha diversity is greater than zero (not NA)
  DF <- MossOrderBiomeLatDF
  DF <- DF %>%
    dplyr::filter(!is.na(DF$Alpha)) %>%
    dplyr::filter(Order == order)
  
  # Find the total alpha diversity for all of the biomes and put into the "Total" column in MOBMat
  total <- sum(DF$Alpha)
  biome = "Total"
  MOBMat[order, biome] <- total
  
  # Make a list of the biomes each order occupies
  biomes <- DF %>%
    dplyr::select(Biome)
  biomes <- unique(as.vector(biomes$Biome))
  
  # Loop through each order's biome list
  for(j in 1:length(biomes)){
    biome = biomes[j]
    
    biomeDF <- DF %>%
      dplyr::filter(DF$Biome == biome)
    
    # Calculate the total alpha diversity for each biome
    nbiome <- sum(biomeDF$Alpha)
    
    # Put each biome's total in the matrix
    MOBMat[order, biome] <- nbiome
  }
}

### Save matrix
saveRDS(MOBMat, "Data/MOBMat.rds")

## 4.2 Use MOBMat to calculate percentages and put in a dataframe
MOBPercentMat <- MOBMat
for(i in 1:NumberOrders){
  order <- MossOrderNames[i]
  total <- MOBPercentMat[order, "Total"]
  for(j in 1:NumberBiomes){
    biome <- BiomeNames[j]
    biome_total <- MOBPercentMat[order, biome]
    percent <- biome_total/total*100
    MOBPercentMat[order, biome] <- percent
  }
}









