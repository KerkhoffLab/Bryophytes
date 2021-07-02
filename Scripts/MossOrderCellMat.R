# MossOrderCellMat.R
# 7/2/2021
# Hailey Napier

# This script makes one matrix:
## MossOrderCellMat: Each column is an order and each row is a cell. Element values specify 
  ## the number of species from each order in each cell.

# 0.0 Data ----
## From 03_LineageAnalysisMS.R
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

# 3.0 Save matrix ----
saveRDS(MossOrderCellMat, "Data/MossOrderCellMat.rds")
