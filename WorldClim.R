#Looking at climatic variables using WorldClim data - see if bryophytes that are in locations of higher drought stress have wider ranges

#Initial WorldClim Data Code - getting ALL the data from WorldClim, puts it into a brick, crops to only include the Americas 
#and creates raster of the proper resolution that matches the RangeRaster with bryophyte range data 
worldclim <- getData('worldclim', var='bio', res=10) 
WC_layers <- do.call(brick, lapply(list.files(path = "wc10/", pattern = "*.bil$", full.names = TRUE), raster)) 
WorldClim <- do.call(crop, c(WC_layers,extent(-175,-22,-56,74)))
Bryophytecrs <- crs(RangeRaster) #creates a raster with resolution of the C4 map (100 km^2 cells)
WorldClim <- projectRaster(WorldClim, crs = Bryophytecrs) 
WorldClim <- resample(WorldClim, RangeRaster) #fits WorldClim data on the BIEN 100 km^2 map by averaging variables across each cell
plot(WorldClim$bio14) #bio14 = precipitation in the driest month
plot(WorldClim$bio17) #bio17 = precipitation in the driest quarter 

#Precipitation in the Driest Quarter (1970 - 2000)
# Plot Bio17 (precipitation in the driest quarter) divide by 10 because Worldclim stores integer T*10
QPrecipitationDF<- data.frame(bio=getValues(WorldClim$bio17), CellID = as.character(1:15038))
colnames(QPrecipitationDF) <- c("Precipitation", "CellID")
QPrecipitationDF <- QPrecipitationDF[QPrecipitationDF$CellID %in% CellRangeVec, ] #subsets it to only include cells with bryophyte range data

Bio17DF <- cbind(QPrecipitationDF, CellRange, by= "CellID")
colnames(Bio17DF) <- c("Precipitation", "CellID", "Cell", "Avg", "by")
Bio17DF = subset(Bio17DF, select = -c(Cell,by))

#Rough plot 
xyplot(Bio17DF$Avg ~ Bio17DF$Precipitation, auto.key=TRUE, na.rm=TRUE, lwd=2, xlab = "Precipitation in the Driest Quarter", ylab="Median Range Size")
#Smooth plot 
xyplot(Bio17DF$Avg ~ Bio17DF$Precipitation, auto.key=TRUE, na.rm=TRUE, lwd=2, xlab = "Precipitation in the Driest Quarter", ylab="Median Range Size", type = "smooth")

