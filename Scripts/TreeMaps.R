#Tree Maps
  #family maps in one folder, with just the family/order name as the title 
#Hailey  Napier
#July 7, 2020

#may have to run OneScaleFamRichMaps for prerequisite code

# 1.0 Make TreeMaps folder --------
setwd("./Figures")
dir.create("./TreeMaps")
#reset working directory to default


# 2.0 ORDERS ----------------------
for(i in 1:NumberOrders){
  TempOrderRichnessRaster <- setValues(BlankRas, OrderRichList[[i]])
  TempOrderDF <- rasterToPoints(TempOrderRichnessRaster)
  TempOrderDF <- data.frame(TempOrderDF)
  colnames(TempOrderDF) <- c("Longitude", "Latitude", "Alpha")
  
  
  Map <- ggplot() + geom_tile(data=TempOrderDF, aes(x=Longitude, y=Latitude, fill=Alpha)) +   
    scale_fill_gradientn(name="α diversity", colours=cols, na.value="transparent", limits = c(0, 200)) +
    coord_equal() +
    geom_sf(data = nw_bound_sf, size = 0.5, fill=NA) + 
    geom_sf(data = nw_mount_sf, size = 0.5, alpha=0.1) + theme_void() + 
    theme(legend.text=element_text(size=20), legend.title=element_text(size=32)) + 
    theme(legend.position = "none")
  
  filename <- paste("./Figures/TreeMaps/", OrderNames[i], ".png",  sep = "")
  png(filename, width= 1000, height = 1000, pointsize = 30)
  print({Map})
  dev.off()
}


# 3.0 FAMILIES ----------------------
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
    theme(legend.text=element_text(size=20), legend.title=element_text(size=32)) + 
    theme(legend.position = "none")
  
  filename <- paste("./Figures/TreeMaps/", FamilyNames[i], ".png", sep = "")
  png(filename, width= 1000, height = 1000, pointsize = 30)
  print({Map})
  dev.off()
}
