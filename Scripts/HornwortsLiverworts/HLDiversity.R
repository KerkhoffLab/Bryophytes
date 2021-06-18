#Load data
BryophytePresence <- readRDS("Data/BryophytePresence.rds")

#Subset hornwort and liverwort data
HornwortPresence <- subset(BryophytePresence, BryophytePresence$Group=="Hornworts")
LiverwortPresence <- subset(BryophytePresence, BryophytePresence$Group=="Liverworts")
HLPresence <- subset(BryophytePresence, BryophytePresence$Group=="Hornworts"|BryophytePresence$Group=="Liverworts")

#Create occurrence by cell matrix by reshaping dataframe, then convert to presence-absence matrix
require(reshape2)
HLRichness <- tally(group_by(HLPresence, CellID))
saveRDS(HLRichness, file="Data/HLRichness.rds")

SpeciesCellID <- HLPresence[,c(1,4)]
melted <- melt(SpeciesCellID, id=c("Species", "CellID"), na.rm = TRUE)

CellID <- HLRichness$CellID
cellvector <- c(1:15038)
neighbor <- function(cellvector) {(adjacent(BlankRas, cellvector, directions=8, pairs=FALSE, target=CellID, sorted=TRUE, include=FALSE, id=FALSE))}
neighbors <- lapply(cellvector, neighbor)
names(neighbors) <- cellvector
bryneighbors <- neighbors[CellID]

HLCellMatrix <- acast(SpeciesCellID, CellID~Species, margins=FALSE, fill=0)
HLCellMatrix[HLCellMatrix > 0] <- 1

#Using betadiver to compute B-diversity using Sorensen dissimilarity
#betadiver(help = TRUE) gives you indices
HLBetaMat <- betadiver(HLCellMatrix, method = "sor", order = FALSE, help = FALSE)
saveRDS(HLBetaMat, file="Data/HLBetaMat.rds")

