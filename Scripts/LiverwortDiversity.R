#Adapted from HLDiversity.R and MossDiversity.R by Kathryn Dawdy

#Load data
BryophytePresence <- readRDS("Data/BryophytePresence.rds")

#Subset liverwort data
LiverwortPresence <- subset(BryophytePresence, BryophytePresence$Group=="Liverworts")

#Create occurrence by cell matrix by reshaping dataframe, then convert to presence-absence matrix
require(reshape2)
LiverwortRichness <- tally(group_by(LiverwortPresence, CellID))
saveRDS(LiverwortRichness, file="Data/LiverwortRichness.rds")

SpeciesCellID <- LiverwortPresence[,c(1,4)]
melted <- melt(SpeciesCellID, id=c("Species", "CellID"), na.rm = TRUE)

CellID <- LiverwortRichness$CellID
cellvector <- c(1:15038)
neighbor <- function(cellvector) {(adjacent(BlankRas, cellvector, directions=8, pairs=FALSE, target=CellID, sorted=TRUE, include=FALSE, id=FALSE))}
neighbors <- lapply(cellvector, neighbor)
names(neighbors) <- cellvector
bryneighbors <- neighbors[CellID]

LiverwortCellMatrix <- acast(SpeciesCellID, CellID~Species, margins=FALSE, fill=0)
LiverwortCellMatrix[LiverwortCellMatrix > 0] <- 1

#Using betadiver to compute B-diversity using Sorensen dissimilarity
#betadiver(help = TRUE) gives you indices
LiverwortBetaMat <- betadiver(LiverwortCellMatrix, method = "sor", order = FALSE, help = FALSE)
saveRDS(LiverwortBetaMat, file="Data/LiverwortBetaMat.rds")
