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


# 4. Plots ----------------------------------------------------------------
plot(wbuf_NW_proj)
plot(mount_NW_proj, add = T)

# 5. Writing shapefiles ---------------------------------------------------
setwd("./Data/MapOutlines/")
dir.create("./Global_bound/")
dir.create("./Mountains/")

wbuf_NW_proj <- as(wbuf_NW_proj, "SpatialPolygonsDataFrame")
writeOGR(obj=wbuf_NW_proj, 
         dsn="./Global_bound/", 
         layer="Koeppen-Geiger_biomes", 
         driver="ESRI Shapefile", 
         overwrite = TRUE)

writeOGR(obj=mount_NW_proj, 
         dsn="./Mountains/", 
         layer="Koeppen-Geiger_biomes", 
         driver="ESRI Shapefile", 
         overwrite = TRUE)

