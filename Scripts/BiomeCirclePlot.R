# Biome order circle plot
# Hailey Napier, July 2020

# Load packages -------------------------------------------------------------
library(dplyr)
library(circlize)
library(dendextend)
library(ape)

# Load data -----------------------------------------------------------------
BryophytePresence <- readRDS("Data/BryophytePresence.rds") 
BiomeNames <- readRDS("Data/BiomeNames.rds")
BiomeRichness <- readRDS("Data/BiomeRichness.rds")
OrderNames <- readRDS("Data/OrderNames.rds")

tree20 <- read.tree("Data/trees/aa-nu-RAxML-FigS20.tre")

# Source functions ----------------------------------------------------------
source("Functions/ORange.R")


# 1.0 Find biome where each species is most abundant -----------------------
  # to prevent overrepresentation of species, only count each 
  # species in the biome where it is most abundant

# 1.1 Create a vector containing all of the biomes indexed by
  # CellID
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

# 1.2 Filter BryophytePresence by species and find cell with 
  # highest richness value
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


# 2.0 Make dataframe for circle plot --------------------------------------
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


# 3.0 Make matrix for circle plot -------------------------------------------
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


##PLOT##

# 1.0 Colors ----------------------------------------------------------------
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


# 2.0 Plot with dataframe ---------------------------------------------------
chordDiagram(OrderBiomeSpeciesDF, grid.col = grid.col)


# 3.0 Plot with  matrix -----------------------------------------------------
chordDiagram(OrderBiomeSpeciesMat, grid.col = grid.col, column.col = biome_cols_11, 
             directional = 1, direction.type = "arrows", link.arr.type = "big.arrow", 
             link.arr.length = 0.05, link.largest.ontop = T, annotationTrack = NULL, 
             preAllocateTracks = 1)
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex=0.3)
}, bg.border = NA)
circos.track(ylim = c(0, 1), bg.border = NA, track.height = 0.3, 
             panel.fun = function(x, y) {
               for(i in seq_len(n)) {
                 circos.text(i-0.5, 0, labels[i], adj = c(0, 0.5), 
                             facing = "clockwise", niceFacing = TRUE,
                             col = ct[labels[i]], cex = 0.5)
               }
             })
dend = color_branches(dend, k = 6, col = 1:6)
dend_height = attr(dend, "height")
circos.track(ylim = c(0, dend_height), bg.border = NA, 
             track.height = 0.4, panel.fun = function(x, y) {
               circos.dendrogram(dend)
             })

