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

# 1.3 Find the highest richness value in the data for an individual cell
# loop through all families and store richness values for each cell in a vector
for(i in 1:NumberFamilies){
  FamCellMaxRichness[i] <- max(FamRichList[[i]], na.rm=T)
}
max(FamCellMaxRichness, na.rm = T)


# 1.3 Make a new folder for richness maps w/in each family
setwd("./Figures")
dir.create("./OneScale_RichByFamMaps")


# 1.4 Loop through families and map all species in each family (one map for each family)

#make sure to set working directory to default

for(i in 1:NumberFamilies){
  i = 1
  
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
  
  filename <- paste("./Figures/RichnessByFamilyMaps/OneScaleRichMap_", FamilyNames[i], ".png", sep = "")
  png(filename, width= 1000, height = 1000, pointsize = 30)
  print({Map})
  dev.off()
}
