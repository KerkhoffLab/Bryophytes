#Families in Montane Regions
#Hailey Napier Summer 2020
#Adapted from MapOutlines.r

# libs --------------------------------------------------------------------
library(raster)
library(rgeos)
library(maptools)
require(rgdal)
require(rworldmap)

# 0. Downloading data  ----------------------------------------------------
## from http://www.naturalearthdata.com/downloads/10m-physical-vectors/10m-physical-labels/
dir.create("./Data/MapOutlines/")

download.file("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_geography_regions_polys.zip",
              destfile = "./Data/MapOutlines/ne_10m_geography_regions_polys.zip")

setwd("./Data/MapOutlines/")
unzip("ne_10m_geography_regions_polys.zip")
setwd("../")

#  1. Reading data --------------------------------------------------------
# 1.1. Mountain ranges
mount_shp <- shapefile("./Data/MapOutlines/ne_10m_geography_regions_polys.shp")
mount <- subset(mount_shp, featurecla=="Range/mtn")

#list of mountain ranges
sort(mount@data[["name"]])

Andes <- subset(mount, name == "ANDES")
Appalachians <- subset(mount, name == "APPALACHIAN MTS.")
Rocky <- subset(mount, name == "ROCKY MOUNTAINS")


# 1.2 Global boundaries
data(wrld_simpl)
noGRL <- wrld_simpl[which(wrld_simpl@data$NAME!='Greenland'), ] 
noIslands <- noGRL[which(noGRL@data$AREA>500), ]
wbuf <- gBuffer(noIslands,width=0.00001)

# 2. Cropping to the New world --------------------------------------------
plot(wbuf)
extent(wbuf)
mount_NW <- crop(mount, extent(-175,-22,-56,74))
wbuf_NW <- crop(wbuf, extent(-175,-22,-56,74))


# 3. Reprojecting ---------------------------------------------------------
## Change the projections to the BIEN ref proj (Lambert Azimuthal Equal Area projection)
crs_ref <- "+proj=laea +lat_0=15 +lon_0=-80 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
mount_NW_proj <- spTransform(mount_NW,crs_ref)
wbuf_NW_proj <- spTransform(wbuf_NW,crs_ref)

andes_NW_proj <- spTransform(Andes, crs_ref)
appalachians_NW_proj <- spTransform(Appalachians, crs_ref)
rocky_NW_proj <- spTransform(Rocky, crs_ref)

# 4. Plots ----------------------------------------------------------------
#plot(wbuf_NW_proj)
#plot(mount_NW_proj, add = T)

plot(wbuf_NW_proj)
plot(andes_NW_proj, add = T)
plot(appalachians_NW_proj, add = T)
plot(rocky_NW_proj, add = T)

# 5. Writing shapefiles ---------------------------------------------------
setwd("./Data/MapOutlines/Mountains/")
dir.create("./Ranges/")

wbuf_NW_proj <- as(wbuf_NW_proj, "SpatialPolygonsDataFrame")
writeOGR(obj=wbuf_NW_proj, 
         dsn="./Global_bound/", 
         layer="mountain_ranges", 
         driver="ESRI Shapefile", 
         overwrite = TRUE)

writeOGR(obj= andes_NW_proj,
         dsn = "./Ranges/", 
         layer = "mountain_ranges", 
         driver = "ESRI Shapefile", 
         overwrite = TRUE)

writeOGR(obj = appalachians_NW_proj,
         dsn = "./Ranges/",
         layer = "mountain_ranges",
         driver = "ESRI Shapefile", 
         overwrite = TRUE)

writeOGR(obj = rocky_NW_proj, 
         dsn = "./Ranges/",
         layer = "mountain_ranges", 
         driver = "ESRI Shapefile", 
         overwrite = TRUE)


setwd("./Data/MapOutlines/Mountains/Ranges/")
dir.create("./Andes")
dir.create("./Appalachians")
dir.create("./Rockies")

writeOGR(obj = andes_NW_proj,
         dsn = "./Andes",
         layer = "andes", 
         driver = "ESRI Shapefile", 
         overwrite = TRUE)

writeOGR(obj = appalachians_NW_proj,
         dsn = "./Appalachians",
         layer = "appalachians", 
         driver = "ESRI Shapefile", 
         overwrite = TRUE)

writeOGR(obj = rocky_NW_proj,
         dsn = "./Rockies",
         layer = "rockies", 
         driver = "ESRI Shapefile", 
         overwrite = TRUE)


# 6. Add continental and mountainous regional outlines ---------------------
#set working directory as home directory
nw_mount <- shapefile("./Data/MapOutlines/Mountains/Koeppen-Geiger_biomes.shp")
nw_bound <- shapefile("./Data/MapOutlines/Global_bound/Koeppen-Geiger_biomes.shp")

nw_mount_ranges <- shapefile("./Data/MapOutlines/Mountains/Ranges/mountain_ranges.shp")

andes_range <- shapefile("./Data/MapOutlines/Mountains/Ranges/Andes/andes.shp")
appalachian_range <- shapefile("./Data/MapOutlines/Mountains/Ranges/Appalachians/appalachians.shp")
rocky_range <- shapefile("./Data/MapOutlines/Mountains/Ranges/Rockies/rockies.shp")


# 7. Creates dataframe including coordinates for each cell ------------------
LongLatRaster <- setValues(BlankRas, CellVec)
LongLatPoints<-rasterToPoints(LongLatRaster)
LongLatDF <- data.frame(LongLatPoints)
colnames(LongLatDF) <- c("Longitude", "Latitude", "CellID")

coordinates(LongLatDF) <- ~Longitude+Latitude 
proj4string(LongLatDF) <- CRS("+proj=utm +zone=10") 
LongLat <- spTransform(LongLatDF, CRS("+proj=longlat")) 
LongLatDF <- data.frame(LongLat)
LongLatDF[c("Longitude", "Latitude", "CellID")]
LongLatDF <- subset(LongLatDF, select = -c(optional))
saveRDS(LongLatDF, "Data/LongLatDF.rds")


# 8. Creates a dataframe w/beta diversity grouped by montane region------------
LongLatDF <- readRDS("Data/LongLatDF.rds")

AndesBeta <- extract(LongLatBetaRaster, andes_range, df = TRUE, cellnumbers = TRUE)
colnames(AndesBeta) <- c("Range", "CellID", "Beta")
AndesBeta$Range <- "Andes"
AndesVec <- AndesBeta$CellID
AndesBeta <- merge(AndesBeta, LongLatDF)

AppalachianBeta <- extract(LongLatBetaRaster, appalachian_range, df = TRUE, cellnumbers = TRUE)
colnames(AppalachianBeta) <- c("Range", "CellID", "Beta")
AppalachianBeta$Range <- "Appalachians"
AppVec <- AppalachianBeta$CellID
AppalachianBeta <- merge(AppalachianBeta, LongLatDF)

RockyBeta <- extract(LongLatBetaRaster, rocky_range, df = TRUE, cellnumbers = TRUE)
colnames(RockyBeta) <- c("Range", "CellID", "Beta")
RockyBeta$Range <- "Rockies"
RockyVec <- RockyBeta$CellID
RockyBeta <- merge(RockyBeta, LongLatDF)


# 9. Creates a DF w/alpha&beta diversity grouped by montane region--------------
AndesAlphaBeta <- left_join(AndesBeta, CellRichness, by = "CellID")
AppAlphaBeta <- left_join(AppalachianBeta, CellRichness, by = "CellID")
RockyAlphaBeta <- left_join(RockyBeta, CellRichness, by = "CellID")


