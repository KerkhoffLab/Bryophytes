#Looking at climatic variables using WorldClim data - see if bryophytes that are in locations of higher drought stress have wider ranges

#Packages
require(knitr)
require(latexpdf)
require(sp)
require(vegan)
require(raster)
require(rasterVis)
require(wesanderson)
require(ggplot2)
require(gridExtra)
require(sf)
require(rgdal)
require(dplyr)

#Load in data 
RangeRaster <- readRDS("Data/RangeRaster.rds")
CellRange <- readRDS("Data/CellRange.rds")
CellRangeVec <- CellRange$CellID

#Initial WorldClim Data Code - getting ALL the data from WorldClim, puts it into a brick, crops to only include the Americas 
#and creates raster of the proper resolution that matches the RangeRaster with bryophyte range data 
worldclim <- getData('worldclim', var='bio', res=10) 
WC_layers <- do.call(brick, lapply(list.files(path = "wc10/", pattern = "*.bil$", full.names = TRUE), raster)) 
WorldClim <- do.call(crop, c(WC_layers,extent(-175,-22,-56,74)))
Bryophytecrs <- crs(RangeRaster) #creates a raster with resolution of the bryophyte range map
WorldClim <- projectRaster(WorldClim, crs = Bryophytecrs) 
WorldClim <- resample(WorldClim, RangeRaster) #fits WorldClim data on the BIEN 100 km^2 map by averaging variables across each cell
WorldClim17 <- subset(WorldClim, "bio17", drop = TRUE)
WorldClim17DF <- rasterToPoints(WorldClim17)
plot(WorldClim$bio14) #bio14 = precipitation in the driest month
plot(WorldClim$bio17) #bio17 = precipitation in the driest quarter 
#I can't figure out how to make this graph in ggplot, maybe one day

#Precipitation in the Driest Quarter (1970 - 2000)
# Plot Bio17 (precipitation in the driest quarter) divide by 10 because Worldclim stores integer T*10 (I didn't but Hailey did)
QPrecipitationDF<- data.frame(bio=getValues(WorldClim$bio17), CellID = as.character(1:15038))
colnames(QPrecipitationDF) <- c("QuarterlyPrecip", "CellID")
QPrecipitationDF <- QPrecipitationDF[QPrecipitationDF$CellID %in% CellRangeVec, ] #subsets it to only include cells with bryophyte range data

Bio17DF <- cbind(QPrecipitationDF, CellRange, by= "CellID")
colnames(Bio17DF) <- c("Precipitation", "CellID", "Cell", "Avg", "by")
Bio17DF = subset(Bio17DF, select = -c(Cell,by))

Bio17Scatterplot <- ggplot(Bio17DF, aes(Precipitation, Avg)) + 
  geom_point(shape = 16, size = 5, show.legend = FALSE, alpha=0.5, color = "cyan4") + ylab("Median Range Size") + 
  xlab("Precipitation in the Driest Quarter") + theme_minimal() + 
  theme(axis.title.y = element_text(size=32), axis.title.x = element_text(size=32), axis.text = element_text(size=20))
Bio17Scatterplot

png("Figures/Bio17Scatterplot.png", width = 1000, height = 1000, pointsize = 30)
Bio17Scatterplot
dev.off()

#Bio17Map <- ggplot() + geom_tile(data=Bio17DF, aes(fill=Precipitation)) +   
  #scale_fill_gradientn(name="Precipitation", colours=cols, na.value="transparent") +
  #coord_equal() +
  #geom_sf(data = nw_bound_sf, size = 0.5, fill=NA) + 
  #geom_sf(data = nw_mount_sf, size = 0.5, alpha=0.1) + theme_void() + 
  #theme(legend.text=element_text(size=20), legend.title=element_text(size=32))

#Precipitation in the driest month (bio14) 
MPrecipitationDF<- data.frame(bio=getValues(WorldClim$bio14), CellID = as.character(1:15038))
colnames(MPrecipitationDF) <- c("Precipitation", "CellID")
MPrecipitationDF <- MPrecipitationDF[MPrecipitationDF$CellID %in% CellRangeVec, ] #subsets it to only include cells with bryophyte range data

Bio14DF <- cbind(MPrecipitationDF, CellRange, by= "CellID")
colnames(Bio14DF) <- c("Precipitation", "CellID", "Cell", "Avg", "by")
Bio14DF = subset(Bio14DF, select = -c(Cell,by))

Bio14Scatterplot <- ggplot(Bio14DF, aes(Precipitation, Avg)) + 
  geom_point(shape = 16, size = 5, show.legend = FALSE, alpha=0.5, color = "cyan4") + ylab("Median Range Size") + 
  xlab("Precipitation in the Driest Month") + theme_minimal() + 
  theme(axis.title.y = element_text(size=32), axis.title.x = element_text(size=32), axis.text = element_text(size=20))
Bio14Scatterplot

png("Figures/Bio14Scatterplot.png", width = 1000, height = 1000, pointsize = 30)
Bio14Scatterplot
dev.off()

#Make a dataframe with cellID, species, driest quarter precipitation values
BryophytePresence <- readRDS("Data/BryophytePresence.rds")
SpeciesCellDF <- BryophytePresence[,1:2]

QPrecipSpeciesDF <- merge(SpeciesCellDF, QPrecipitationDF, by = "CellID")

#With this new dataframe that has species, cellID, and quarterly precipitation, calculate the median precipitation of each species
#Add in species range values 
SpeciesRange <- readRDS("Data/SpeciesRange.rds")

QPrecipAvg <- subset(QPrecipSpeciesDF, select = c("QuarterlyPrecip", "Species")) %>%
  group_by(Species) %>%
  summarize(Avg = median(QuarterlyPrecip, na.rm=T))

QPrecipAvg <- merge(QPrecipAvg, SpeciesRange, by = "Species")

#Quarterly precipitation plot quarterly precipitation by range
Bio17SpeciesRangeScatterplot <- ggplot(QPrecipAvg, aes(Avg, RangeAvg)) + 
  geom_point(shape = 16, size = 5, show.legend = FALSE, alpha=0.5, color = "cyan4") + ylab("Bryophyte Species Median Range Size") + 
  xlab("Median Precipitation in the Driest Quarter") + theme_minimal() + ylim(0,1700) + geom_smooth(size = 2,color = "gray63") +
  theme(axis.title.y = element_text(size=28), axis.title.x = element_text(size=28), axis.text.x = element_text(size=20), axis.text.y = element_text(size = 20))
Bio17SpeciesRangeScatterplot

png("Figures/Bio17SpeciesRangeScatterplot.png", width = 1000, height = 1000, pointsize = 30)
Bio17SpeciesRangeScatterplot
dev.off()

#Now make a dataframe with cellID, species, and driest month precipitation values (this version also has range values, which are the Avg column)
#and calculate median precipitation 
MPrecipSpeciesDF <- merge(SpeciesCellDF, Bio14DF, by = "CellID")
colnames(MPrecipSpeciesDF) <- c("CellID", "Species", "Precipitation", "RangeAvg")

MPrecipAvg <- subset(MPrecipSpeciesDF, select = c("Precipitation", "Species")) %>%
  group_by(Species) %>%
  summarize(Avg = median(Precipitation, na.rm=T))

MPrecipAvg <- merge(MPrecipAvg, SpeciesRange, by = "Species")

Bio14SpeciesRangeScatterplot <- ggplot(MPrecipAvg, aes(Avg, RangeAvg)) + 
  geom_point(shape = 16, size = 5, show.legend = FALSE, alpha=0.5, color = "cyan4") + ylab("Bryophyte Species Median Range Size") + 
  xlab("Median Precipitation in the Driest Month") + theme_minimal() + ylim(0,1600) + geom_smooth(size = 2,color = "gray63") +
  theme(axis.title.y = element_text(size=28), axis.title.x = element_text(size=28), axis.text.x = element_text(size=20), axis.text.y = element_text(size = 20))
Bio14SpeciesRangeScatterplot

png("Figures/Bio14SpeciesRangeScatterplot.png", width = 1000, height = 1000, pointsize = 30)
Bio14SpeciesRangeScatterplot
dev.off()
