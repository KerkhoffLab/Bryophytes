# Biome order circle plot
# Hailey Napier, July 2020

# Load packages -------------------------------------------------------------
library(dplyr)
library(circlize)
library(dendextend)
library(ape)
library(phangorn)
library(phytools)
library(rphast)

# Load data -----------------------------------------------------------------
BryophytePresence <- readRDS("Data/BryophytePresence.rds") 
BiomeNames <- readRDS("Data/BiomeNames.rds")
BiomeRichness <- readRDS("Data/BiomeRichness.rds")
OrderNames <- readRDS("Data/OrderNames.rds")

tree20 <- read.tree("Data/trees/aa-nu-RAxML-FigS20.tre")
FigS20_FOG <- read.csv("Data/FamilyTrees/FigS20_FOG.csv")
OrderNodesS20 <- read.csv("Data/OrderNodesS20.csv")

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
             link.arr.length = 0.05, link.largest.ontop = T, annotationTrack = c("grid"), 
             preAllocateTracks = 1)
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex=0.3)
}, bg.border = NA)


## ADD PHYLOGENETIC TREE ##


# 4.0 Plot tree with circlize ------------------------------------------------
plot(tree20)
tree20$tip.label

#drop liverworts and outgroups
#Liu et al dropped marchantia, bazzania, and scapania to root tree. I also dropped ptillidium
#Takakia is an outgroup according to Liu et al, dropped for clarity (we have no Takakia data anyway)
drop <- c("marchantia_onekp","bazzania_onekp", "scapania_onekp", "ptillidium_onekp", 
          "Takakia_4343a", "Takakia_ceratophylla_3743", "takakia_onekp")
tree20.drop <- drop.tip(tree20, drop)
is.rooted(tree20.drop)
tree20.ultra <- chronopl(tree20.drop, 0, age.min = 1, age.max = NULL,
                                node = "root", tol = 1e-8,CV = FALSE, 
                                eval.max = 500, iter.max = 500)
tree20.hc <- as.hclust(tree20.ultra)
dend  <- as.dendrogram(tree20.hc)

circos.par(gap.degree = 90)
circlize_dendrogram(dend,labels = F)

circos.clear()
circos.par(gap.degree = 90)
circos.initialize("foo", xlim = c(0, 141))
#circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
  #circos.rect(1:141 - 0.8, rep(0, 141), 1:141 - 0.2, runif(141), col = rand_color(141), border = NA)
#}, bg.border = NA)
circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
  circos.text(1:141 - 0.5, rep(0, 141), labels(dend),
              col = labels_colors(dend),
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex = 0.5
  )
}, bg.border = NA, track.height = 0.1)
dend = color_branches(dend, k = 38, col = 138)
max_height <- attr(dend, "height")
circos.track(ylim = c(0, max_height), panel.fun = function(x, y) {
  circos.dendrogram(dend, max_height = max_height)
}, track.height = 0.5, bg.border = NA)


# 5.0 Reshape both datasets to include only data that is available in both -------
# 5.1 Find the orders included in both datasets
labs <- FigS20_FOG %>%
  select(label, order)
labs <- labs[4:144,]
labs$label <- gsub(" ", "_", labs$label)

#19 orders included in both datasets
laborder <- unique(labs$order[complete.cases(labs$order)])
laborder
#
DROPcirctiplabs <- labs[is.na(labs$order), ]
DROPcirctiplabs <- DROPcirctiplabs$label

# 5.2 Reshape biome data (drop all orders that aren't in tree20)
BryPresCircle <- BryophytePresence %>%
  filter(Order %in% laborder)

SpeciesNames <- unique(BryPresCircle$Species)
SpeciesBiomes <- data.frame("Species" = SpeciesNames)
SpeciesBiomes$Biome <- NA
for(i in 1:length(SpeciesNames)){
  species <- SpeciesNames[i]
  df <- BryPresCircle %>% 
    filter(BryPresCircle$Species == species)
  spcell <- as.vector(df$CellID)
  biomecells <- BiomeCellsVec[spcell]
  biomecells <- biomecells[complete.cases(biomecells)]
  biome <- names(sort(summary(biomecells)))[1]
  SpeciesBiomes$Biome[SpeciesBiomes$Species == species] <- biome
}

# 5.3 Reshape tree data (drop all tips that aren't in bry pres data)
drop <- c(drop, DROPcirctiplabs)
tree20.drop <- drop.tip(tree20, drop)
tree20.ultra <- chronopl(tree20.drop, 0, age.min = 1, age.max = NULL,
                         node = "root", tol = 1e-8,CV = FALSE, 
                         eval.max = 500, iter.max = 500)
tree20.hc <- as.hclust(tree20.ultra)
dend <- as.dendrogram(tree20.hc)

newlabels <- labs$order[complete.cases(labs$order)]
labels(dend) <- newlabels

#this creates a tree that just has orders in the data, and has order names as tip labels
plot(dend)

# I think the only way to use the dendrogram in the circle plot is to get rid of all of the 
    # descendants of the order nodes and label the order nodes with the order name.
    # So each order is labeled with a tip that I can assign a sector. This isn't 
    # very scientifically sound though
    # UPDATE: not even sure I can do it

cull <- OrderNodesS20 %>%
  filter(Order %in% laborder)
cull <- as.vector(cull$Node)

descendants <- vector()
end <- 0
for(i in 1:length(cull)){
  d <- getDescendants(tree20, cull[i])
  start <- end + 1
  end <- start + length(d) - 1
  descendants[start:end] <- d
}

culledtree <- drop.tip(tree20, descendants)
culledtree <- drop.tip(culledtree, drop)
plot(culledtree)
tree20.ultra <- chronopl(culledtree, 0, age.min = 1, age.max = NULL,
                         node = "root", tol = 1e-8,CV = FALSE, 
                         eval.max = 500, iter.max = 500)
tree20.hc <- as.hclust(tree20.ultra)
dend <- as.dendrogram(tree20.hc)
plot(dend)


# 6.0 Manually create circle plot ----------------------------------------
circos.clear()
circos.par(cell.padding = c(0, 0, 0, 0), gap.degree = 5)
circos.initialize(xlim = cbind(c(0, 0)))
circos.track(ylim = c(0, 10), bg.border = NA, panel.fun = function(x, y) {
  sector.index = CELL_META$sector.index
  dend = dend
  
  m2 = m[order.dendrogram(dend), ]
  col_mat = col_fun(m2)
  nr = nrow(m2)
  nc = ncol(m2)
  for(i in 1:nc) {
    circos.rect(1:nr - 1, rep(nc - i, nr), 
                1:nr, rep(nc - i + 1, nr), 
                border = col_mat[, i], col = col_mat[, i])
  }
})
