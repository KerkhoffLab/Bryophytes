#Load data
BryophytePresence <- readRDS("Data/BryophytePresence.rds")

#Subset moss data
MossPresence <- subset(BryophytePresence, BryophytePresence$Group=="Mosses")

#Create occurrence by cell matrix by reshaping dataframe, then convert to presence-absence matrix
require(reshape2)
MossRichness <- tally(group_by(MossPresence, CellID))
saveRDS(MossRichness, file = "Data/MossRichness.rds")

SpeciesCellID <- MossPresence[,c(1,4)]
melted <- melt(SpeciesCellID, id=c("Species", "CellID"), na.rm = TRUE)

CellID <- MossRichness$CellID
cellvector <- c(1:15038)
neighbor <- function(cellvector) {(adjacent(BlankRas, cellvector, directions=8, pairs=FALSE, target=CellID, sorted=TRUE, include=FALSE, id=FALSE))}
neighbors <- lapply(cellvector, neighbor)
names(neighbors) <- cellvector
bryneighbors <- neighbors[CellID]

MossCellMatrix <- acast(melted, CellID~Species, margins=FALSE)
MossCellMatrix[MossCellMatrix > 0] <- 1

#Using betadiver to compute B-diversity using Sorensen dissimilarity
#betadiver(help = TRUE) gives you indices
MossBetaMat <- betadiver(MossCellMatrix, method = "sor", order = FALSE, help = FALSE)
saveRDS(MossBetaMat, file="Data/MossBetaMat.rds")
