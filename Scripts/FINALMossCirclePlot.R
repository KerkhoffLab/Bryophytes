# FINAL CIRCLE PLOT CODE FOR ALL MOSSES

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
