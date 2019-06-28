#Load data
BryophytePresence <- readRDS("Data/BryophytePresence.rds")

#Subset moss data
MossPresence <- subset(BryophytePresence, BryophytePresence$Group=="Mosses")

#Create occurrence by cell matrix by reshaping dataframe, then convert to presence-absence matrix
require(reshape2)
SpeciesCellID <- MossPresence[,c(1,4)]
melted <- melt(SpeciesCellID, id=c("Species", "CellID"), na.rm = TRUE)

MossCellMatrix <- acast(melted, CellID~Species, margins=FALSE)
MossCellMatrix[MossCellMatrix > 0] <- 1

#Using betadiver to compute B-diversity using Sorensen dissimilarity
#betadiver(help = TRUE) gives you indices
BetaMat <- betadiver(SpeciesCellMatrix, method = "sor", order = FALSE, help = FALSE)
saveRDS(BetaMat, file="Data/BetaMat.rds")