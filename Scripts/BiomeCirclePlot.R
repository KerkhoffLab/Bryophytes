# Biome order circle plot
# Hailey Napier, July 2020
# DO NOT USE THIS SCRIPT

# 0.1 Load packages -------------------------------------------------------------
library(dplyr)
library(circlize)
library(dendextend)
library(ape)
library(phangorn)
library(phytools)
library(rphast)

# 0.2 Load data -----------------------------------------------------------------
BryophytePresence <- readRDS("Data/BryophytePresence.rds") 
BiomeNames <- readRDS("Data/BiomeNames.rds")
BiomeRichness <- readRDS("Data/BiomeRichness.rds")
OrderNames <- readRDS("Data/OrderNames.rds")

tree20 <- read.tree("Data/trees/aa-nu-RAxML-FigS20.tre")
FigS20_FOG <- read.csv("Data/FamilyTrees/FigS20_FOG.csv")
OrderNodesS20 <- read.csv("Data/OrderNodesS20.csv")

MossPresence <- readRDS("Data/MossPresence.rds") 
# Made in MossPlotData.R
MossOrderNames <- unique(MossPresence$Order)
MossSpeciesNames <- unique(MossPresence$Species)

# 0.3 Source functions ----------------------------------------------------------
source("Functions/ORange.R")

# 0.4 Data processing -----------------------------------------------------------
# Create a vector containing all of the biomes indexed by CellID
BiomeCellsVec <- as.vector(rep(NA, 15038))
for(i in 1:length(BiomeNames)){
  biome <- BiomeNames[i]
  biomevec <- ORange(range = biome)
  for(j in 1:length(biomevec)){
    rich <- biomevec[j]
    if(!is.na(rich)){
      BiomeCellsVec[j] <- biome
    }
  }
}
BiomeCellsVec <- as.factor(BiomeCellsVec)

# Make BiomeCellsDF
BiomeCellsDF <- data.frame(CellID = 1:15038)
BiomeCellsDF$Biome <- BiomeCellsVec

# Filter BryophytePresence by species and find cell with highest richness value
SpeciesNames <- unique(BryophytePresence$Species)
SpeciesBiomes <- data.frame("Species" = SpeciesNames)
SpeciesBiomes$Biome <- NA
for(i in 1:length(SpeciesNames)){
  species <- SpeciesNames[i]
  df <- BryophytePresence %>% 
    filter(BryophytePresence$Species == species)
  spcell <- as.vector(df$CellID)
  biomecells <- BiomeCellsVec[spcell]
  biomecells <- biomecells[complete.cases(biomecells)]
  biome <- names(sort(summary(biomecells)))[1]
  SpeciesBiomes$Biome[SpeciesBiomes$Species == species] <- biome
}

# Filter BryophytePresence by species and find total abundance 
SpeciesNames <- unique(BryophytePresence$Species)
SpecAb <- as.vector(rep(NA, length(SpeciesNames)))

SpBiMat <- matrix(NA, length(SpeciesNames), length(BiomeNames))
rownames(SpBiMat) <- SpeciesNames
colnames(SpBiMat) <- BiomeNames

for(i in 1:length(SpeciesNames)){
  species <- SpeciesNames[i]
  df <- BryophytePresence %>% 
    filter(BryophytePresence$Species == species)
  tot <- nrow(df)
  SpecAb[i] <- tot
  for(j in 1:length(BiomeNames)){
    spcell <- as.vector(df$CellID)
    biome <- BiomeNames[j]
    biomedf <- BiomeCellsDF %>%
      filter(BiomeCellsDF$Biome == biome) %>%
      filter(CellID %in% spcell)
    ab <- nrow(biomedf)
    SpBiMat[species, biome] <- ab
  }
}

# Make OrderSpeciesList
OrderSpeciesList <- list()
for(i in 1:length(OrderNames)){
  order <- OrderNames[i]
  orddf <- BryophytePresence %>%
    filter(BryophytePresence$Order == order)
  specvec <- unique(orddf$Species)
  OrderSpeciesList[[i]] <- specvec
}

# MossOrderSpeciesList
MossOrderSpeciesList <- list()
for(i in 1:length(MossOrderNames)){
  order <- MossOrderNames[i]
  orddf <- MossPresence %>%
    filter(MossPresence$Order == order)
  specvec <- unique(orddf$Species)
  MossOrderSpeciesList[[i]] <- specvec
}

# 0.5 Colors ------------------------------------------------------------------------
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

biome_cols_11 <- c("#D8B70A", "#972D15", "#A2A475", "#81A88D", "#02401B",
                   "#446455", "#FDD262", "#D3DDDC", "#C7B19C", "#798E87", 
                   "#C27D38")



#------------------------------------ PLOTS ----------------------------------------

## PLOT WITH EACH SPECIES COUNTED IN THE BIOME WHERE IT'S MOST ABUNDANT ##
# Find biome where each species is most abundant 
  # to prevent overrepresentation of species, only count each 
  # species in the biome where it is most abundant

# Make dataframe for circle plot ----------------------------------------------------
# This doesn't actually work. It's in alphabetical order, not in order of order richness
    # see PLOT WITH EACH SPECIES COUNTED IN THE BIOME WHERE THEY HAVE THE MOST RICHNESS 
    # for a plot that works
OrderBiomeSpeciesDF <- data.frame(from = rep(NA, 418), to = rep(NA,418), value = rep(NA, 418))
end <- 0
for(i in 1:length(OrderNames)){
  start <- end + 1
  end <- start + 10
  order <- OrderNames[i]
  orderspeciesvec <- BryophytePresence %>%
    filter(BryophytePresence$Order == order)
  orderspeciesvec <- as.vector(orderspeciesvec$Species)
  for(j in start:end){
    if(end <= 11){
      onetoeleven <- 1:11
      biomenum <- onetoeleven[j]
    }else{
      biomenum <- j%%11 + 1
    }
    biome <- BiomeNames[biomenum]
    df <- SpeciesBiomes %>%
      filter(SpeciesBiomes$Biome == biome) %>%
      filter(Species %in% orderspeciesvec)
    vec <- as.vector(df$Species)
    val <- length(unique(vec))
    OrderBiomeSpeciesDF$from[start:end] <- order
    OrderBiomeSpeciesDF$to[j] <- biome
    OrderBiomeSpeciesDF$value[j] <- val
  }
}

# Make matrix for circle plot -----------------------------------------------------
OrderBiomeSpeciesMat <- matrix(NA, 38, 11)
rownames(OrderBiomeSpeciesMat) <- OrderNames
colnames(OrderBiomeSpeciesMat) <- BiomeNames
for(i in 1:length(BiomeNames)){
  biome <- BiomeNames[i]
  df <- OrderBiomeSpeciesDF %>%
    filter(OrderBiomeSpeciesDF$to == biome)
  vec <- as.vector(df$value)
  OrderBiomeSpeciesMat[,biome] <- vec
}
view(OrderBiomeSpeciesMat)

# Plot with dataframe --------------------------------------------------------------
chordDiagram(OrderBiomeSpeciesDF, grid.col = grid.col)

# Plot with  matrix ----------------------------------------------------------------
chordDiagram(OrderBiomeSpeciesMat, grid.col = grid.col, column.col = biome_cols_11, 
             directional = 1, direction.type = "arrows", link.arr.type = "big.arrow", 
             link.arr.length = 0.05, link.largest.ontop = T, annotationTrack = c("grid"), 
             preAllocateTracks = 1)
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex=0.3)
}, bg.border = NA)



## PLOT WITH EACH SPECIES COUNTED IN EVERY BIOME WHERE THEY ARE PRESENT ##
# Make empty matrix ------------------------------------------------------------------
CircleMat <- matrix(NA, length(OrderNames), length(BiomeNames))
rownames(CircleMat) <- OrderNames
colnames(CircleMat) <- BiomeNames

# Loop through and filter BryophytePresence ------------------------------------------
for(i in 1:length(BiomeNames)){
  biome <- BiomeNames[i]
  BiomeVec <- BiomeCellsDF %>%
    filter(BiomeCellsDF$Biome == biome)
  BiomeVec <- as.vector(BiomeVec$CellID)
  BryBiome <- BryophytePresence %>%
    filter(CellID %in% BiomeVec)
  for(j in 1:length(OrderNames)){
    order <- OrderNames[j]
    valvec <- BryBiome %>%
      filter(BryBiome$Order == order)
    val <- length(unique(valvec$Species))
    CircleMat[order,biome] <- val
  }
}

# Plot ---------------------------------------------------------------------------------
circos.clear()

png("Figures/CircleBryTopAb.png", width = 1000, height = 1000, pointsize = 20)

circos.par(start.degree = 0)
chordDiagram(CircleMat, grid.col = grid.col, column.col = biome_cols_11, 
             directional = 1, direction.type = "arrows", link.arr.type = "big.arrow", 
             link.arr.length = 0.05, link.largest.ontop = T, annotationTrack = c("grid"), 
             preAllocateTracks = 1, big.gap = 20, small.gap = 5)
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex=0.5)
}, bg.border = NA)

dev.off()

## PLOT WITH EACH SPECIES COUNTED IN  BIOMES WHERE THEY HAVE AT LEAST 10% ABUNDANCE ##
# Make plot matrix -----------------------------------------------------------------------
  
# Make empty matrix 
CircleMat10 <- matrix(NA, length(OrderNames), length(BiomeNames))
rownames(CircleMat10) <- OrderNames
colnames(CircleMat10) <- BiomeNames

# Fill plot matrix
for(h in 1:length(OrderNames)){
  order <- OrderNames[h]
  speclist <- OrderSpeciesList[[h]]
  for(i in 1:length(BiomeNames)){
    biome <- BiomeNames[i]
    val = 0
    for(j in 1:length(speclist)){
      species <- speclist[j]
      index <- which(SpeciesNames == species)
      tot <- SpecAb[index]
      ab <- SpBiMat[species,biome]
      if(ab > 0){
        if(ab/tot >= .1){
          val = val + 1
        }else{
          val = val
        }
      }else{
          val = val
        }
    }
    CircleMat10[order,biome] <- val
  }
}

# Plot ------------------------------------------------------------------------
circos.clear()

png("Figures/CircleBry10.png", width = 1000, height = 1000, pointsize = 20)

circos.par(start.degree = 0)
chordDiagram(CircleMat10, grid.col = grid.col, column.col = biome_cols_11, 
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

## PLOT WITH EACH SPECIES COUNTED IN  BIOMES WHERE THEY HAVE AT LEAST 25% ABUNDANCE ##
# Make plot matrix ----------------------------------------------------------

# Make empty matrix 
CircleMat25 <- matrix(NA, length(OrderNames), length(BiomeNames))
rownames(CircleMat25) <- OrderNames
colnames(CircleMat25) <- BiomeNames

# Fill plot matrix
for(h in 1:length(OrderNames)){
  order <- OrderNames[h]
  speclist <- OrderSpeciesList[[h]]
  for(i in 1:length(BiomeNames)){
    biome <- BiomeNames[i]
    val = 0
    for(j in 1:length(speclist)){
      species <- speclist[j]
      index <- which(SpeciesNames == species)
      tot <- SpecAb[index]
      ab <- SpBiMat[species,biome]
      if(ab > 0){
        if(ab/tot >= .25){
          val = val + 1
        }else{
          val = val
        }
      }else{
        val = val
      }
    }
    CircleMat25[order,biome] <- val
  }
}

# Plot ------------------------------------------------------------------------
circos.clear()

png("Figures/CircleBry25.png", width = 1000, height = 1000, pointsize = 20)

circos.par(start.degree = 0)
chordDiagram(CircleMat25, grid.col = grid.col, column.col = biome_cols_11, 
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


## PLOT 25% ABUNDANCE WITH MOSSES ONLY  ##
# Make empty matrix 
CircleMat25Moss <- matrix(NA, length(OrderNames), length(BiomeNames))
rownames(CircleMat25Moss) <- OrderNames
colnames(CircleMat25Moss) <- BiomeNames

# Fill plot matrix
for(h in 1:length(MossOrderNames)){
  order <- MossOrderNames[h]
  speclist <- MossOrderSpeciesList[[h]]
  for(i in 1:length(BiomeNames)){
    biome <- BiomeNames[i]
    val = 0
    for(j in 1:length(speclist)){
      species <- speclist[j]
      index <- which(SpeciesNames == species)
      tot <- SpecAb[index]
      ab <- SpBiMat[species,biome]
      if(ab > 0){
        if(ab/tot >= .25){
          val = val + 1
        }else{
          val = val
        }
      }else{
        val = val
      }
    }
    CircleMat25Moss[order,biome] <- val
  }
}

# Plot
circos.clear()

png("Figures/CircleMoss25.png", width = 1000, height = 1000, pointsize = 20)

circos.par(start.degree = 0)
chordDiagram(CircleMat25Moss, grid.col = grid.col, column.col = biome_cols_11, 
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

## PLOT WITH EACH SPECIES COUNTED IN  BIOMES WHERE THEY HAVE AT LEAST 50% ABUNDANCE ##
# Make plot matrix ----------------------------------------------------------

# Make empty matrix 
CircleMat50 <- matrix(NA, length(OrderNames), length(BiomeNames))
rownames(CircleMat50) <- OrderNames
colnames(CircleMat50) <- BiomeNames

# Fill plot matrix
for(h in 1:length(OrderNames)){
  order <- OrderNames[h]
  speclist <- OrderSpeciesList[[h]]
  for(i in 1:length(BiomeNames)){
    biome <- BiomeNames[i]
    val = 0
    for(j in 1:length(speclist)){
      species <- speclist[j]
      index <- which(SpeciesNames == species)
      tot <- SpecAb[index]
      ab <- SpBiMat[species,biome]
      if(ab > 0){
        if(ab/tot >= .5){
          val = val + 1
        }else{
          val = val
        }
      }else{
        val = val
      }
    }
    CircleMat50[order,biome] <- val
  }
}

# Plot ------------------------------------------------------------------------
circos.clear()

png("Figures/CircleBry50.png", width = 1000, height = 1000, pointsize = 20)

circos.par(start.degree = 0)
chordDiagram(CircleMat50, grid.col = grid.col, column.col = biome_cols_11, 
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


## PLOT WITH EACH SPECIES COUNTED IN  BIOMES WHERE THEY HAVE AT LEAST 75% ABUNDANCE ##
# Make plot matrix ----------------------------------------------------------

# Make empty matrix 
CircleMat75 <- matrix(NA, length(OrderNames), length(BiomeNames))
rownames(CircleMat75) <- OrderNames
colnames(CircleMat75) <- BiomeNames

# Fill plot matrix
for(h in 1:length(OrderNames)){
  order <- OrderNames[h]
  speclist <- OrderSpeciesList[[h]]
  for(i in 1:length(BiomeNames)){
    biome <- BiomeNames[i]
    val = 0
    for(j in 1:length(speclist)){
      species <- speclist[j]
      index <- which(SpeciesNames == species)
      tot <- SpecAb[index]
      ab <- SpBiMat[species,biome]
      if(ab > 0){
        if(ab/tot >= .75){
          val = val + 1
        }else{
          val = val
        }
      }else{
        val = val
      }
    }
    CircleMat75[order,biome] <- val
  }
}

# Plot ------------------------------------------------------------------------
circos.clear()

png("Figures/CircleBry75.png", width = 1000, height = 1000, pointsize = 20)

circos.par(start.degree = 0)
chordDiagram(CircleMat75, grid.col = grid.col, column.col = biome_cols_11, 
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


## PLOT WITH EACH SPECIES COUNTED IN  BIOMES WHERE THEY HAVE AT LEAST 75% ABUNDANCE ##
## MOSSES ONLY ##
# Make plot matrix ----------------------------------------------------------

# Make empty matrix 
CircleMat75Moss <- matrix(NA, length(MossOrderNames), length(BiomeNames))
rownames(CircleMat75Moss) <- MossOrderNames
colnames(CircleMat75Moss) <- BiomeNames

# Fill plot matrix
for(h in 1:length(MossOrderNames)){
  order <- MossOrderNames[h]
  speclist <- MossOrderSpeciesList[[h]]
  for(i in 1:length(BiomeNames)){
    biome <- BiomeNames[i]
    val = 0
    for(j in 1:length(speclist)){
      species <- speclist[j]
      index <- which(SpeciesNames == species)
      tot <- SpecAb[index]
      ab <- SpBiMat[species,biome]
      if(ab > 0){
        if(ab/tot >= .75){
          val = val + 1
        }else{
          val = val
        }
      }else{
        val = val
      }
    }
    CircleMat75Moss[order,biome] <- val
  }
}

# Plot ------------------------------------------------------------------------
circos.clear()

png("Figures/CircleMoss75.png", width = 1000, height = 1000, pointsize = 20)

circos.par(start.degree = 0)
chordDiagram(CircleMat75Moss, grid.col = grid.col, column.col = biome_cols_11, 
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



## PLOT WITH EACH SPECIES COUNTED IN THE BIOME WHERE THEY HAVE THE MOST ABUNDANCE ##
# Make plot matrix ----------------------------------------------------------

# Make empty matrix 
CircleMatAll <- matrix(NA, length(OrderNames), length(BiomeNames))
rownames(CircleMatAll) <- OrderNames
colnames(CircleMatAll) <- BiomeNames

# Fill matrix so it counts species based on top biome (moss)
for(h in 1:length(OrderNames)){
  order <- OrderNames[h]
  speclist <- OrderSpeciesList[[h]]
  valvec <- as.vector(rep(0, length(BiomeNames)))
  for(i in 1:length(speclist)){
    species <- speclist[i]
    biomenumcells <- vector()
    for(j in 1:length(BiomeNames)){
      biome <- BiomeNames[j]
      ab <- SpBiMat[species,biome]
      biomenumcells[j] <- ab
    }
    biomeindex <- which(biomenumcells == max(biomenumcells, na.rm = T))
    valvec[biomeindex] <- valvec[biomeindex] + 1
  }
  for(k in 1:length(valvec)){
    val <- valvec[k]
    biome <- BiomeNames[k]
    CircleMatAll[order, biome] <- val
  }
}

for(i in 1:length(BiomeNames)){
  biome <- BiomeNames[i]
  for(j in 1:length(OrderNames)){
    order <- OrderNames[j]
    speclist <- OrderSpeciesList[[j]]
    valvec <- as.vector(rep(0, length(BiomeNames)))
  }
}

# Plot ------------------------------------------------------------------------
circos.clear()

png("Figures/CircleTopAb.png", width = 1000, height = 1000, pointsize = 20)

circos.par(start.degree = 0)
chordDiagram(CircleMatAll, grid.col = grid.col, column.col = biome_cols_11, 
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


## PLOT WITH EACH SPECIES COUNTED IN THE BIOME WHERE THEY HAVE THE MOST ABUNDANCE ##
## MOSSES ONLY ##
# Make plot matrix ----------------------------------------------------------

# Make empty matrix 
CircleMatAllMoss <- matrix(NA, length(MossOrderNames), length(BiomeNames))
rownames(CircleMatAllMoss) <- MossOrderNames
colnames(CircleMatAllMoss) <- BiomeNames

# Fill matrix so it counts species based on top biome (moss)
for(h in 1:length(MossOrderNames)){
  order <- MossOrderNames[h]
  speclist <- MossOrderSpeciesList[[h]]
  valvec <- as.vector(rep(0, length(BiomeNames)))
  for(i in 1:length(speclist)){
    species <- speclist[i]
    biomenumcells <- vector()
    for(j in 1:length(BiomeNames)){
      biome <- BiomeNames[j]
      ab <- SpBiMat[species,biome]
      biomenumcells[j] <- ab
    }
    biomeindex <- which(biomenumcells == max(biomenumcells, na.rm = T))
    valvec[biomeindex] <- valvec[biomeindex] + 1
  }
  for(k in 1:length(valvec)){
    val <- valvec[k]
    biome <- BiomeNames[k]
    CircleMatAllMoss[order, biome] <- val
  }
}

for(i in 1:length(BiomeNames)){
  biome <- BiomeNames[i]
  for(j in 1:length(MossOrderNames)){
    order <- MossOrderNames[j]
    speclist <- MossOrderSpeciesList[[j]]
    valvec <- as.vector(rep(0, length(BiomeNames)))
  }
}

# Save matrix for quantitative analysis
saveRDS(CircleMatAllMoss, "Data/CircleMatAllMoss.rds")


# Plot ------------------------------------------------------------------------
circos.clear()

png("Figures/CircleMossTopAb.png", width = 1000, height = 1000, pointsize = 20)

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




