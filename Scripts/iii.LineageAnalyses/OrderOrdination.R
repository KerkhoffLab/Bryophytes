MossOrderNames <- readRDS("Data/MossOrderNames.rds")
OrderNames <- readRDS("Data/OrderNames.rds")
OrderRichList <- readRDS("Data/OrderRichList.rds")
# 1.0 Initialize matrix ----
MossOrderCellMat <-  matrix(NA, 15038, 22)
colnames(MossOrderCellMat) <- MossOrderNames
# 2.0 Fill matrix ----
## Loop through orders
for(i in 1:length(MossOrderNames)){
order <- MossOrderNames[i]
orderindex <- which(OrderNames == order)
### Extract vector of species richness in each cell from OrderRichList
ordercellrichvec <- OrderRichList[[orderindex]]
### Fill order column with cell richness vector
MossOrderCellMat[,order] <- ordercellrichvec
}
# 3.0 Replace NA with zeros and Limit to cells with richness over 199
MossOrderCellMat[which(is.na(MossOrderCellMat))]<-0
row.names(MossOrderCellMat)<-1:nrow(MossOrderCellMat)
MossOrderCellMat200<-MossOrderCellMat[rowSums(MossOrderCellMat)>299,]

# 4.0 Ordinate using NMDS
ord<-metaMDS(MossOrderCellMat200, trymax=100)
ordCells<-as.data.frame(ord$points)
ordOrders<-as.data.frame(ord$species)

# 5.0 Bring in Biome data for visualization
biomeCells<-readRDS("Data/BiomeCellsDF.rds")
biomeCells<-biomeCells[row.names(MossOrderCellMat200),]
ordCells$Biome<-biomeCells$Biome


# 6.0 Visualize ordination
ggplot(na.omit(ordCells), aes(MDS1, MDS2, color=Biome, show.legend=TRUE)) +
  geom_point() +
  scale_color_manual(values=c("Coniferous_Forests"="#D8B70A",
                              "Dry_Forest"="#972D15",
                              "Mediterranean_Woodlands"="#A2A475",
                              "Moist_Forest"="#81A88D",
                              "Savannas"="#02401B",
                              "Taiga"="#446455",
                              "Temperate_Grasslands"="#FDD262",
                              "Temperate_Mixed"="#D3DDDC",
                              "Tropical_Grasslands"="#C7B19C",
                              "Tundra"="#798E87",
                              "Xeric_Woodlands"="#C27D38")) +
  #stat_ellipse() +
  theme_minimal()


  


