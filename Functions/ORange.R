#Function to make a vector of alpha diversity values for cellIDs only contained in a specific shapefile (biome/mountain range)
#Input: str, order name; str,  biome/mountain range
#Output: vector of length 15038 containing only alpha diversity values for cells inside specified biome/mountain range area, else NA
#Hailey Napier
#July 16, 2020

ORange <- function(order, range,...){
  #load data
  OrderNames <- readRDS("Data/OrderNames.rds")
  OrderRichList <- readRDS("Data/OrderRichList.rds")
  
  #load file containing CellIDs in the range/biome of interest
  file <- paste("Data/", range, "Vec.rds", sep = "")
  RangeVec <- readRDS(file)
  
  #Make a new vector that contains only the cells that aren't in the range/biome of interest
  NotRangeCells <- c(1:15038)
  NotRangeCells[RangeVec] <- NA
  NotRangeCells <- complete.cases(NotRangeCells)
  
  #Find the index for the order of interest in order to access the richness list in OrderRichList
  orderindex <- which(OrderNames == order)
  
  #Set all of the cells that aren't in the range/biome of interest to NA
  orange <- OrderRichList[[orderindex]]
  orange[NotRangeCells] <- NA
  
  orange
  }
