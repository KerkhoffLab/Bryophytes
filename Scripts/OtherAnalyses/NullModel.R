##Null Model to test the relationship between precipitation and range sizes
#Hailey Napier 
#Spring 2020 
#updates added for data aggregation Summer 2020

#Packages
require(sp)
require(raster)
require(rasterVis)
require(ggplot2)
require(wesanderson)
require(rgdal)
install.packages("dplyr")
require(sf)
require(dpylr)

###RUN *DataProcessing.R* FIRST###

#Omit cells that have no neighbors
have_neighbors <- CellID[which(bryneighborvect!=0)]
LandCells <- data.frame(have_neighbors)
LandCells$CellID <- LandCells$have_neighbors
LandCells$have_neighbors <- NULL


#Make a list of ranges based on species
ranges <- as.vector(SpeciesRange$RangeAvg)
ranges <- sort(ranges)


####Spreading Dye Algorithm####
linearRanges <- c(1:length(ranges)) #vector of all the ranges 1-4802 to use as an identifier because some ranges have the same size

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

for(i in 1:legnth(ranges)){
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
######




#Create a null vector for mapping
cell <- NULL
NullVec <- rep(NA, 15038)

for(i in 1:length(have_neighbors)){
  cell <- have_neighbors[i]
  NullVec[cell] <- LandCells$Total_Overlap[LandCells$CellID == cell]
}


#Map null richness
NullRaster <- setValues(BlankRas, NullVec)
saveRDS(NullRaster, "Data/NullRaster.rds")

NullDF <- rasterToPoints(NullRaster)
NullDF <- data.frame(NullDF)
colnames(NullDF) <- c("Longitude", "Latitude", "Null")

NullMap <- ggplot() + geom_tile(data=NullDF, aes(x=Longitude, y=Latitude, fill=Null)) +   
  scale_fill_gradientn(name="Null α diversity", colours=cols, na.value="transparent", limits=c(0,700)) +
  coord_equal() +
  geom_sf(data = nw_bound_sf, size = 0.5, fill=NA) + 
  geom_sf(data = nw_mount_sf, size = 0.5, alpha=0.1) + theme_void() + 
  theme(legend.text=element_text(size=20), legend.title=element_text(size=32))
NullMap

png("Figures/null1.png", width = 1000, height = 1000, pointsize = 30)
NullMap
dev.off()


#Convert null richness to long-lat
coordinates(NullDF) <- ~Longitude+Latitude 
proj4string(NullDF) <- CRS("+proj=utm +zone=10") 
NullLongLat <- spTransform(NullDF, CRS("+proj=longlat")) 
NullLongLat <- data.frame(NullLongLat)
colnames(NullLongLat) <- c("Null", "Longitude", "Latitude")


#Null richness by latitude
theme_set(theme_gray())
NullScatterplot <- ggplot(NullLongLat, aes(Latitude, Null)) + 
  geom_point(shape = 16, size = 5, show.legend = FALSE, alpha=0.5, color = "goldenrod1") + 
  ylab("Null α diversity") + 
  xlab("Latitude") + 
  theme_minimal() + 
  theme(axis.title.y = element_text(size=32), axis.text.y = element_text(size=20), axis.text.x = element_text(size=20), axis.title.x = element_text(size = 32))
NullScatterplot 

png("Figures/NullScatter1.png", width = 1000, height = 1000, pointsize = 20)
NullScatterplot
dev.off()


#####Adding climate data to null model#####
#Initial WorldClim Data Code - getting ALL the data from WorldClim, puts it into a brick, crops to only include the Americas 
#and creates raster of the proper resolution that matches the RangeRaster with bryophyte range data 
CellRange <- readRDS("Data/CellRange.rds")
CellRangeVec <- CellRange$CellID

RangeRaster <- readRDS("Data/RangeRaster.rds")
Bryophytecrs <- crs(RangeRaster)

worldclim <- getData('worldclim', var='bio', res=10) 
WC_layers <- do.call(brick, lapply(list.files(path = "wc10/", pattern = "*.bil$", full.names = TRUE), raster)) 
WorldClim <- do.call(crop, c(WC_layers,extent(-175,-22,-56,74)))
WorldClim <- projectRaster(WorldClim, crs = Bryophytecrs) 
WorldClim <- resample(WorldClim, RangeRaster) #fits WorldClim data on the BIEN 100 km^2 map by averaging variables across each cell


#Make WorldClim dataframes

#Mean Annual Temperature (bio1)
Bio1DF<- data.frame(bio=getValues(WorldClim$bio1), CellID = as.character(1:15038))
colnames(Bio1DF) <- c("MeanAnnualTemp", "CellID")
Bio1DF <- Bio1DF[Bio1DF$CellID %in% CellRangeVec, ] #subsets it to only include cells with bryophyte range data

#Mean diurnal range (mean of monthly(max temp - min temp)) (bio2)
Bio2DF<- data.frame(bio=getValues(WorldClim$bio2), CellID = as.character(1:15038))
colnames(Bio2DF) <- c("MedDiurnalRange", "CellID")
Bio2DF <- Bio2DF[Bio2DF$CellID %in% CellRangeVec, ] #subsets it to only include cells with bryophyte range data

#Isothermality (Bio2/Bio7)x100 (bio3)
Bio3DF<- data.frame(bio=getValues(WorldClim$bio3), CellID = as.character(1:15038))
colnames(Bio3DF) <- c("Isothermality", "CellID")
Bio3DF <- Bio3DF[Bio2DF$CellID %in% CellRangeVec, ] #subsets it to only include cells with bryophyte range data

#Temperature seasonality (sd x 100) (bio4)
Bio4DF<- data.frame(bio=getValues(WorldClim$bio4), CellID = as.character(1:15038))
colnames(Bio4DF) <- c("TempSeason", "CellID")
Bio4DF <- Bio4DF[Bio4DF$CellID %in% CellRangeVec, ] #subsets it to only include cells with bryophyte range data

#Max temperature of warmest month (bio5)
Bio5DF<- data.frame(bio=getValues(WorldClim$bio5), CellID = as.character(1:15038))
colnames(Bio5DF) <- c("MaxTempWarmM", "CellID")
Bio5DF <- Bio5DF[Bio5DF$CellID %in% CellRangeVec, ] #subsets it to only include cells with bryophyte range data

#Min temperature of coldest moonth (bio6)
Bio6DF<- data.frame(bio=getValues(WorldClim$bio6), CellID = as.character(1:15038))
colnames(Bio6DF) <- c("MinTempColdM", "CellID")
Bio6DF <- Bio6DF[Bio6DF$CellID %in% CellRangeVec, ] #subsets it to only include cells with bryophyte range data

#Temperature annual range (Bio5 - Bio6) (bio7)
Bio7DF<- data.frame(bio=getValues(WorldClim$bio7), CellID = as.character(1:15038))
colnames(Bio7DF) <- c("TempAnnualRange", "CellID")
Bio2DF <- Bio7DF[Bio7DF$CellID %in% CellRangeVec, ] #subsets it to only include cells with bryophyte range data

#Mean temperature of wettest quarter (bio8)
Bio8DF<- data.frame(bio=getValues(WorldClim$bio8), CellID = as.character(1:15038))
colnames(Bio8DF) <- c("MeanTempWetQ", "CellID")
Bio8DF <- Bio8DF[Bio8DF$CellID %in% CellRangeVec, ] #subsets it to only include cells with bryophyte range data

#Mean temperature of driest quarter (bio9)
Bio9DF<- data.frame(bio=getValues(WorldClim$bio9), CellID = as.character(1:15038))
colnames(Bio9DF) <- c("MeanTempDriestQ", "CellID")
Bio9DF <- Bio9DF[Bio9DF$CellID %in% CellRangeVec, ] #subsets it to only include cells with bryophyte range data

#Mean temperature of warmest quarter (bio 10)
Bio10DF<- data.frame(bio=getValues(WorldClim$bio10), CellID = as.character(1:15038))
colnames(Bio10DF) <- c("MeanTempWarmQ", "CellID")
Bio10DF <- Bio10DF[Bio10DF$CellID %in% CellRangeVec, ] #subsets it to only include cells with bryophyte range data

#Mean temperature of coldest quarter (bio 11)
Bio11DF<- data.frame(bio=getValues(WorldClim$bio11), CellID = as.character(1:15038))
colnames(Bio11DF) <- c("MTempColdQ", "CellID")
Bio11DF <- Bio11DF[Bio11DF$CellID %in% CellRangeVec, ] #subsets it to only include cells with bryophyte range data

#Annual precipitation (bio12)
Bio12DF<- data.frame(bio=getValues(WorldClim$bio12), CellID = as.character(1:15038))
colnames(Bio12DF) <- c("AnnualPrecip", "CellID")
Bio12DF <- Bio12DF[Bio12DF$CellID %in% CellRangeVec, ] #subsets it to only include cells with bryophyte range data

#Precipitation of wettest month (bio13)
Bio13DF<- data.frame(bio=getValues(WorldClim$bio13), CellID = as.character(1:15038))
colnames(Bio13DF) <- c("WettestMPrecip", "CellID")
Bio13DF <- Bio13DF[Bio13DF$CellID %in% CellRangeVec, ] #subsets it to only include cells with bryophyte range data

#Precipitation in driest month (bio14)
Bio14DF<- data.frame(bio=getValues(WorldClim$bio14), CellID = as.character(1:15038))
colnames(Bio14DF) <- c("DriestMPrecip", "CellID")
Bio14DF <- Bio14DF[Bio14DF$CellID %in% CellRangeVec, ] #subsets it to only include cells with bryophyte range data

#Precipitation seasonality (coefficient of variation) (bio15)
Bio15DF<- data.frame(bio=getValues(WorldClim$bio15), CellID = as.character(1:15038))
colnames(Bio15DF) <- c("PrecipSeason", "CellID")
Bio15DF <- Bio15DF[Bio15DF$CellID %in% CellRangeVec, ] #subsets it to only include cells with bryophyte range data

#Precipitation of wettest quarter (bio16)
Bio16DF<- data.frame(bio=getValues(WorldClim$bio16), CellID = as.character(1:15038))
colnames(Bio16DF) <- c("WettestQPrecip", "CellID")
Bio16DF <- Bio16DF[Bio16DF$CellID %in% CellRangeVec, ] #subsets it to only include cells with bryophyte range data

#Precipitation in driest quarter (bio17)
Bio17DF<- data.frame(bio=getValues(WorldClim$bio17), CellID = as.character(1:15038))
colnames(Bio17DF) <- c("DriestQPrecip", "CellID")
Bio17DF <- Bio17DF[Bio17DF$CellID %in% CellRangeVec, ] #subsets it to only include cells with bryophyte range data

#Precipitation of warmest quarter (bio18)
Bio18DF<- data.frame(bio=getValues(WorldClim$bio18), CellID = as.character(1:15038))
colnames(Bio18DF) <- c("WarmestQPrecip", "CellID")
Bio18DF <- Bio18DF[Bio18DF$CellID %in% CellRangeVec, ] #subsets it to only include cells with bryophyte range data

#Precipitation of coldest quarter (bio19)
Bio19DF<- data.frame(bio=getValues(WorldClim$bio19), CellID = as.character(1:15038))
colnames(Bio19DF) <- c("ColdestQPrecip", "CellID")
Bio19DF <- Bio19DF[Bio19DF$CellID %in% CellRangeVec, ] #subsets it to only include cells with bryophyte range data

######
#Create new folder
dir.create("Data/NullModelData")
#Save dataframes
saveRDS(Bio1DF, file = "Data/NullModelData/Bio1DF.rds")
saveRDS(Bio2DF, file = "Data/NullModelData/Bio2DF.rds")
saveRDS(Bio3DF, file = "Data/NullModelData/Bio3DF.rds")
saveRDS(Bio4DF, file = "Data/NullModelData/Bio4DF.rds")
saveRDS(Bio5DF, file = "Data/NullModelData/Bio5DF.rds")
saveRDS(Bio6DF, file = "Data/NullModelData/Bio6DF.rds")
saveRDS(Bio7DF, file = "Data/NullModelData/Bio7DF.rds")
saveRDS(Bio8DF, file = "Data/NullModelData/Bio8DF.rds")
saveRDS(Bio9DF, file = "Data/NullModelData/Bio9DF.rds")
saveRDS(Bio10DF, file = "Data/NullModelData/Bio10DF.rds")
saveRDS(Bio11DF, file = "Data/NullModelData/Bio11DF.rds")
saveRDS(Bio12DF, file = "Data/NullModelData/Bio12DF.rds")
saveRDS(Bio13DF, file = "Data/NullModelData/Bio13DF.rds")
saveRDS(Bio14DF, file = "Data/NullModelData/Bio14DF.rds")
saveRDS(Bio15DF, file = "Data/NullModelData/Bio15DF.rds")
saveRDS(Bio16DF, file = "Data/NullModelData/Bio16DF.rds")
saveRDS(Bio17DF, file = "Data/NullModelData/Bio17DF.rds")
saveRDS(Bio18DF, file = "Data/NullModelData/Bio18DF.rds")
saveRDS(Bio19DF, file = "Data/NullModelData/Bio19DF.rds")

