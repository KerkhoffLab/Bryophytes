# FINAL CIRCLE PLOT CODE FOR ALL MOSSES
# Hailey Napier
# September 2020

# 0.0 Load packages -------------------------------------------------------------
library(dplyr)
library(circlize)

# 1.0 Make matrix for plotting --------------------------------------------------
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

# Save matrix for quantitative analysis
saveRDS(CircleMatAllMoss, "Data/CircleMatAllMoss.rds")

# 2.0 Plot -----------------------------------------------------------------------
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
