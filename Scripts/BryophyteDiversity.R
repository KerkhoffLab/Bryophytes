#Load data
BryophytePresence <- readRDS("Data/BryophytePresence.rds")

#Create occurrence by cell matrix by reshaping dataframe, then convert to presence-absence matrix
require(reshape2)
SpeciesCellID <- BryophytePresence[,c(1,4)]
melted <- melt(SpeciesCellID, id=c("Species", "CellID"), na.rm = TRUE)

SpeciesCellMatrix <- acast(melted, CellID~Species, margins=FALSE)
SpeciesCellMatrix[SpeciesCellMatrix > 0] <- 1

#Using betadiver to compute B-diversity using Jaccard similarity
#betadiver(help = TRUE) gives you indices
require(vegan)
betamat <- betadiver(SpeciesCellMatrix, method = "j", order = FALSE, help = FALSE)

#Save species-cell matrix and beta diversity matrix
saveRDS(SpeciesCellMatrix, file="Data/SpeciesCellMatrix.rds")
saveRDS(betamat, file="Data/BetaMat.rds")
