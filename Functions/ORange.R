#Function to make a vector of alpha diversity values for cellIDs only contained in a specific shapefile (biome/mountain range)
#Input: taxa = str, taxonomic level; default is all "bryophytes" (currently, the only other option is "mosses")
#Input: order =  str, order name; default is all orders
#Input: range = str,  biome/mountain range
#Input: cont =  str, continent ("both", "North America", "South America"); default is both
#Input: cells = str, cell count type; "center", "clean", "weight"; default is clean
        # center counts the cell if the biome overlaps the center of the cell
        # clean counts the cell in the biome that covers the majority of the cell (weighted, but selects one biome per cell)
        # weight gives the proportion of the cell the biome polygon covers
#Output: vector of length 15038 containing only alpha diversity values for cells inside specified biome/mountain range area, else NA
#Hailey Napier
#July 16, 2020

ORange <- function(taxa = "bryophytes", order = "all", range, cont = "both", cells = "center"){
  #load data (comment out load data commands and add necessary data at top of code for loops with large datasets)
  BiomeNames <- readRDS("Data/BiomeNames.rds")
  
  #load file containing CellIDs in the range/biome of interest
  RangeVec <- vector()
  if(taxa == "bryophytes"){
    if(cells == "center"){
      if(cont == "both"){
        file <- paste("Data/", range, "Vec.rds", sep = "")
        RangeVec <- readRDS(file)
      }else if(cont == "North America"){
        NorthAmBiomeRichList <- readRDS("Data/NorthAmBiomeList.rds")
        index <- which(BiomeNames == range)
        RangeVec <- NorthAmBiomeList[[index]]
      }else if(cont == "South America"){
        SouthAmBiomeList <- readRDS("Data/SouthAmBiomeList.rds")
        index <- which(BiomeNames == range)
        RangeVec <- SouthAmBiomeList[[index]]
      }
    }else if(cells == "clean"){
      #will have to add code for continents with clean weighted cells if needed later
      if(cont == "both"){
        file <- paste("Data/", range, "CleanVec.rds", sep = "")
        RangeVec <- readRDS(file)
      }
    }
  }else if(taxa == "mosses"){
    if(cells == "center"){
      if(cont == "both"){
        file <- paste("Data/Moss", range, "Vec.rds", sep = "")
        RangeVec <- readRDS(file)
      }else if(cont == "North America"){
        NorthAmBiomeRichList <- readRDS("Data/NorthAmBiomeList.rds")
        index <- which(BiomeNames == range)
        RangeVec <- NorthAmBiomeList[[index]]
      }else if(cont == "South America"){
        SouthAmBiomeList <- readRDS("Data/SouthAmBiomeList.rds")
        index <- which(BiomeNames == range)
        RangeVec <- SouthAmBiomeList[[index]]
      }
    }else if(cells == "clean"){
      #will have to add code for continents with clean weighted cells if needed later
      if(cont == "both"){
        file <- paste("Data/Moss_", range, "_Clean_Vec.rds", sep = "")
        RangeVec <- readRDS(file)
      }
    }
  }
  
  #Make a new vector that contains only the cells that aren't in the range/biome of interest
  NotRangeCells <- c(1:15038)
  NotRangeCells[RangeVec] <- NA
  NotRangeCells <- complete.cases(NotRangeCells)
  
  #Default is total richness of all bryophyte orders
  if(taxa == "bryophytes"){
    if(order == "all"){
      orange <- readRDS("Data/RichnessVec.rds")
      orange[NotRangeCells] <- NA
    }else{
      #Load data
      OrderNames <- readRDS("Data/OrderNames.rds")
      OrderRichList <- readRDS("Data/OrderRichList.rds")
      
      #Find the index for the order of interest in order to access the richness list in OrderRichList
      orderindex <- which(OrderNames == order)
      
      #Set all of the cells that aren't in the range/biome of interest to NA
      orange <- OrderRichList[[orderindex]]
      orange[NotRangeCells] <- NA
    }
  }else if(taxa == "mosses")
    if(order == "all"){
      orange <- readRDS("Data/MossRichnessVec.rds")
      orange[NotRangeCells] <- NA
    }else{
      #Load data
      MossOrderNames <- readRDS("Data/MossOrderNames.rds")
      MossOrderRichList <- readRDS("Data/MossOrderRichList.rds")
      
      #Find the index for the order of interest in order to access the richness list in MossOrderRichList
      orderindex <- which(MossOrderNames == order)
      
      #Set all of the cells that aren't in the range/biome of interest to NA
      orange <- MossOrderRichList[[orderindex]]
      orange[NotRangeCells] <- NA
    }
    
  return(orange)
}

