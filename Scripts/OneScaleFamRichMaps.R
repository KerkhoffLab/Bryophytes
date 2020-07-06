#Single Scale Family Richness Maps
#Hailey Napier and Kathryn Dawdy
#July 2, 2020

# 1.0 Plot species richness for each family (separate plot for each family)-----------------------------------

# 1.1 Loop through family names and subset BryophtePresence for each family, store them in a list
FamList <- list()
for(i in 1:NumberFamilies){
  fam <- FamilyNames[i]
  FamList[[i]] <- subset(BryophytePresence, Family == fam)
}


# 1.2 Loop through families and tally richness for each family, store in a list
FamRichList <- list()
FamPresList <- list()
for(i in 1:NumberFamilies){
  FamPresList[[i]] <- tally(group_by(FamList[[i]], CellID))
  names(FamPresList[[i]])[2] <- "Richness"
  FamRichList[[i]] <- numeric(15038)
  FamRichList[[i]][FamPresList[[i]]$CellID] <- FamPresList[[i]]$Richness
  FamRichList[[i]][which(FamRichList[[i]]==0)] = NA
}

# 1.3 Mapping details
# 1.31 Find the highest richness value in the data for an individual cell
# loop through all families and store richness values for each cell in a vector, then find max value
for(i in 1:NumberFamilies){
  FamCellMaxRichness[i] <- max(FamRichList[[i]], na.rm=T)
}
max(FamCellMaxRichness, na.rm = T)

# 1.32 Make a dataframe with families and associated orders
FamilyOrderGroup <- BryophytePresence %>%
  dplyr::select(Family, Order, Group)
FamilyOrderGroup <- FamilyOrderGroup[!duplicated(FamilyOrderGroup$Family),]
#The last row is all NAs, so get rid of that
FamilyOrderGroup <- FamilyOrderGroup[complete.cases(FamilyOrderGroup),]


# 1.4 Make a new folder for richness maps w/in each family
#If you run this and you've already made the folder it will delete everything in it
#setwd("./Figures")
#dir.create("./OneScale_RichByFamMaps")


# 1.5 Loop through families and map all species in each family (one map for each family)

#make sure to set working directory to default

for(i in 1:NumberFamilies){
  TempFamRichnessRaster <- setValues(BlankRas, FamRichList[[i]])
  TempFamDF <- rasterToPoints(TempFamRichnessRaster)
  TempFamDF <- data.frame(TempFamDF)
  colnames(TempFamDF) <- c("Longitude", "Latitude", "Alpha")
  
  
  Map <- ggplot() + geom_tile(data=TempFamDF, aes(x=Longitude, y=Latitude, fill=Alpha)) +   
    scale_fill_gradientn(name="α diversity", colours=cols, na.value="transparent", limits = c(0, 100)) +
    coord_equal() +
    geom_sf(data = nw_bound_sf, size = 0.5, fill=NA) + 
    geom_sf(data = nw_mount_sf, size = 0.5, alpha=0.1) + theme_void() + 
    theme(legend.text=element_text(size=20), legend.title=element_text(size=32))
  
  filename <- paste("./Figures/OneScale_RichByFamMaps/", unique(FamilyOrderGroup$Group[which(FamilyOrderGroup$Family == FamilyNames[i])]), "/", FamilyOrder$Order[which(FamilyOrder$Family == FamilyNames[i])], "/OneScaleRichMap_", FamilyNames[i], ".png", sep = "")
  png(filename, width= 1000, height = 1000, pointsize = 30)
  print({Map})
  dev.off()
}

# 2.0 Make order richness maps the same way (one map for each order) just because I'm curious-----------------
# 2.1 Loop through order names and subset BryophtePresence for each order, store them in a list
OrderNames <- unique(BryophytePresence$Order)
OrderNames <- OrderNames[!is.na(OrderNames)]
NumberOrders <- length(OrderNames)
OrderList <- list()
for(i in 1:NumberOrders){
  ord <- OrderNames[i]
  OrderList[[i]] <- subset(BryophytePresence, Order == ord)
}


# 2.2 Loop through orders and tally richness for each order, store in a list
OrderRichList <- list()
OrderPresList <- list()
for(i in 1:NumberOrders){
  OrderPresList[[i]] <- tally(group_by(OrderList[[i]], CellID))
  names(OrderPresList[[i]])[2] <- "Richness"
  OrderRichList[[i]] <- numeric(15038)
  OrderRichList[[i]][OrderPresList[[i]]$CellID] <- OrderPresList[[i]]$Richness
  OrderRichList[[i]][which(OrderRichList[[i]]==0)] = NA
}

# 2.3 Find the highest richness value in the data for an individual cell
# loop through all orders and store richness values for each cell in a vector
OrderCellMaxRichness <- c()

for(i in 1:NumberOrders){
  OrderCellMaxRichness[i] <- max(OrderRichList[[i]], na.rm=T)
}
max(OrderCellMaxRichness, na.rm = T)

# 2.4 Make a new folder for richness maps w/in each order
#If you run this it wil delete everything in the folder if you've already made it
#setwd("./Figures")
#dir.create("./RichByOrderMaps")


# 2.5 Loop through families and map all species in each order (one map for each order)

#make sure to set working directory to default

for(i in 1:NumberOrders){
  TempOrderRichnessRaster <- setValues(BlankRas, FamRichList[[i]])
  TempOrderDF <- rasterToPoints(TempOrderRichnessRaster)
  TempOrderDF <- data.frame(TempOrderDF)
  colnames(TempOrderDF) <- c("Longitude", "Latitude", "Alpha")
  
  
  Map <- ggplot() + geom_tile(data=TempOrderDF, aes(x=Longitude, y=Latitude, fill=Alpha)) +   
    scale_fill_gradientn(name="α diversity", colours=cols, na.value="transparent", limits = c(0, 200)) +
    coord_equal() +
    geom_sf(data = nw_bound_sf, size = 0.5, fill=NA) + 
    geom_sf(data = nw_mount_sf, size = 0.5, alpha=0.1) + theme_void() + 
    theme(legend.text=element_text(size=20), legend.title=element_text(size=32))
  
  filename <- paste("./Figures/OneScale_RichByFamMaps/",unique(FamilyOrderGroup$Group[which(FamilyOrderGroup$Order == OrderNames[i])]), "/", OrderNames[i], "/ORDEROneScaleRichMap_", OrderNames[i], ".png", sep = "")
  png(filename, width= 1000, height = 1000, pointsize = 30)
  print({Map})
  dev.off()
}

