#Adapted from HLDiversity.R and MossDiversity.R by Kathryn Dawdy

#Load data
BryophytePresence <- readRDS("Data/BryophytePresence.rds")

#Subset hornwort data
HornwortPresence <- subset(BryophytePresence, BryophytePresence$Group=="Hornworts")

#Create occurrence by cell matrix by reshaping dataframe, then convert to presence-absence matrix
require(reshape2)
HornwortRichness <- tally(group_by(HornwortPresence, CellID))
saveRDS(HornwortRichness, file="Data/HornwortRichness.rds")

SpeciesCellID <- HornwortPresence[,c(1,4)]
melted <- melt(SpeciesCellID, id=c("Species", "CellID"), na.rm = TRUE)

CellID <- HornwortRichness$CellID
cellvector <- c(1:15038)
neighbor <- function(cellvector) {(adjacent(BlankRas, cellvector, directions=8, pairs=FALSE, target=CellID, sorted=TRUE, include=FALSE, id=FALSE))}
neighbors <- lapply(cellvector, neighbor)
names(neighbors) <- cellvector
bryneighbors <- neighbors[CellID]

HornwortCellMatrix <- acast(SpeciesCellID, CellID~Species, margins=FALSE, fill=0)
HornwortCellMatrix[HornwortCellMatrix > 0] <- 1

#Using betadiver to compute B-diversity using Sorensen dissimilarity
#betadiver(help = TRUE) gives you indices
HornwortBetaMat <- betadiver(HornwortCellMatrix, method = "sor", order = FALSE, help = FALSE)
saveRDS(HornwortBetaMat, file="Data/HornwortBetaMat.rds")
