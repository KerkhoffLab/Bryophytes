#Spreading Dye Algorithm for Null Modeling
#Hailey Napier Spring 2020

linearRanges <- c(1:4802) #vector of all the ranges 1-4802 to use as an identifier because some ranges have the same size

#make a matrix to store which cells are in each range
ranges.vector <- rep(0, length(ranges)) 
CellID.vector <- rep(0, length(have_neighbors))
range_cells <- array(c(ranges.vector,CellID.vector),dim = c(length(ranges),length(have_neighbors)),dimnames = list(linearRanges,
                                                                                                                   have_neighbors))

#set everything at zero
LandCells$Occupied <- 0
LandCells$Total_Overlap <- 0
focal_neighbors <- NULL
total_neighbors <- NULL

for(i in 1:4802){
  #total_neighbors is a list of all of the neighbors of each of the cells in the range, so it resets with each new range
  total_neighbors <- NULL
  #occupancy reference for each cell in the range has to be set so all of the cells are empty when you start a new range
  LandCells$Occupied <- 0
  
  if(length(linearRanges) > 1){
    #choose a random range using the ID number
    linearID_test_range = sample(linearRanges, 1)
    #take the randomly selected range out of the list of ranges so it doesn't get redrawn
    linearRanges <- linearRanges[linearRanges != linearID_test_range]
  }else if(length(linearRanges) == 1){
    #before it gets to zero the sample will start drawing random ranges to keep from getting to zero, so make sure it takes the last range and not a random one
    linearID_test_range = linearRanges
    linearRanges = NA
  }
  
  #set the range size based on the ID
  test_range = ranges[linearID_test_range]
  
  #choose a random cell from the list of cells that have neighbors
  focal_cell <- sample(have_neighbors, 1)
  
  for(i in 1:test_range){
    #mark the current cell as occupied
    LandCells$Occupied[which(LandCells$CellID == focal_cell)] <- 1
    #add one to the total number of times this cell was included in any range
    LandCells$Total_Overlap[which(LandCells$CellID == focal_cell)] <- LandCells$Total_Overlap[which(LandCells$CellID == focal_cell)] + 1
    #set the linear ID number for the focal cell to mark it in the matrix
    linear_focal_cell <- match(focal_cell, have_neighbors)
    #mark the focal cell in the current range in the matrix
    range_cells[linearID_test_range, linear_focal_cell] = 1
    #make a list of the focal cell's neighbors
    focal_neighbors <- bryneighbors[CellID == focal_cell]
    focal_neighbors <- focal_neighbors[[1]][]
    #add the focal cell's neighbors to the list of all of the neighbors of every cell in the range
    total_neighbors <- append(total_neighbors, focal_neighbors)
    
    repeat{
      #if the focal cell has more than one neighbor, choose a neighbor at random to be the next focal cell
      if(length(focal_neighbors) > 1){
        new_focal_cell <- sample(focal_neighbors, 1)
        #if the cell has exactly one neighbor, choose that neighbor to be the next focal cell
      }else if(length(focal_neighbors) == 1){
        new_focal_cell <- focal_neighbors
        #if the cell has no neighbors, but there is more than one neighbor of any cell in the range, choose one of the total range neighbors at random
      }else if(length(total_neighbors) > 1){
        new_focal_cell <- sample(total_neighbors, 1)
        #if there is exactly one neighbor in the full range, choose that neighbor to be the next focal cell
      }else if(length(total_neighbors) == 1){
        new_focal_cell <- total_neighbors
        #if the focal cell has no neighbors, and the total range has no neighbors, choose a random unoccupied cell to be the next focal cell
      }else{
        new_focal_cell <- sample(LandCells$CellID[LandCells$Occupied == 0], 1)
      }
      if(LandCells$Occupied[which(LandCells$CellID == new_focal_cell)] == 1){
        #remove the occupied cells from both lists of neighbors
        total_neighbors <- total_neighbors[total_neighbors != new_focal_cell]
        focal_neighbors <- focal_neighbors[focal_neighbors != new_focal_cell]
        #repeat choosing the new focal cell until the new focal cell is unoccupied
      }else{
        break
      }
    }
    #if there are more than zero ranges left, set the new unoccupied focal cell is the focal cell
    if(length(linearRanges) >= 1){
      focal_cell <- new_focal_cell
    }else{
      break
    }
  }
}