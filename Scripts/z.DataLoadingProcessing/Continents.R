# North America and South America Shapefiles
# Hailey Napier, July 2020
# Adapted from MapOutlines.r and MountainRanges.R

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
setwd("../../")

#  1. Reading data ---------------------------------------------------------
# 1.1. North America
data(wrld_simpl)

subregion21 <- wrld_simpl[which(wrld_simpl@data$SUBREGION == 21), ]
subregion29 <- wrld_simpl[which(wrld_simpl@data$SUBREGION == 29), ]
subregion13 <- wrld_simpl[which(wrld_simpl@data$SUBREGION == 13), ]
subregion21_29_13 <- bind(subregion21,subregion29,subregion13)
plot(subregion21_29_13)
noGRL <- subregion21_29_13[which(subregion21_29_13@data$NAME!='Greenland'), ] 
noIslands <- noGRL[which(noGRL@data$AREA>500), ]
NorthAm <- gBuffer(noIslands,width=0.00001)

# 1.2 South America
wrld_simpl@data
region19 <- wrld_simpl[which(wrld_simpl@data$REGION == 19), ]
region19No21 <- region19[which(region19@data$SUBREGION != 21), ]
region19No21_29 <- region19No21[which(region19No21@data$SUBREGION != 29), ]
region19No21_29_13 <- region19No21_29[which(region19No21_29@data$SUBREGION != 13), ]
plot(region19No21_29_13)
noIslands <- region19No21_29_13[which(region19No21_29_13@data$AREA>500), ]
SouthAm <- gBuffer(noIslands,width=0.00001)

# 2. Cropping to the New world ----------------------------------------------
# 2.1 North America
plot(NorthAm)
extent(NorthAm)
NorthAm <- crop(NorthAm, extent(-175,-22,-56,74))
plot(NorthAm)

# 2.2 South America
plot(SouthAm)
extent(SouthAm)
SouthAm <- crop(SouthAm, extent(-175,-22,-56,74))
plot(SouthAm)


# 3. Reprojecting ---------------------------------------------------------
## Change the projections to the BIEN ref proj (Lambert Azimuthal Equal Area projection)
crs_ref <- "+proj=laea +lat_0=15 +lon_0=-80 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
NorthAm_proj <- spTransform(NorthAm,crs_ref)
SouthAm_proj <- spTransform(SouthAm, crs_ref)


# 4. Plots ----------------------------------------------------------------
plot(NorthAm_proj)
plot(SouthAm_proj)


# 5. Writing shapefiles ---------------------------------------------------
setwd("./Data/MapOutlines/")
dir.create("./Continents/")
setwd("../../")

NorthAm_proj <- as(NorthAm_proj, "SpatialPolygonsDataFrame")
writeOGR(obj=NorthAm_proj, 
         dsn="./Data/MapOutlines/Continents/north_america.shp", 
         layer="north_america", 
         driver="ESRI Shapefile", 
         overwrite = TRUE)

SouthAm_proj <- as(SouthAm_proj, "SpatialPolygonsDataFrame")
writeOGR(obj=SouthAm_proj, 
         dsn="./Data/MapOutlines/Continents/south_america.shp", 
         layer="south_america", 
         driver="ESRI Shapefile", 
         overwrite = TRUE)


# 7. Creating raster including CellIDs for each cell -------------------
BlankRas <- raster("Data/blank_100km_raster.tif")
CellVec <- c(1:15038)
LongLatRaster <- setValues(BlankRas, CellVec)


# 8. Creating a vector of CellIDs for North and South America ----------
NorthAm <- shapefile("Data/MapOutlines/Continents/north_america.shp")
NorthACells <- raster::extract(LongLatRaster, NorthAm, df = TRUE, cellnumbers = TRUE, weight = TRUE)
NorthAmericaVec <- NorthACells$cell

SouthAm <- shapefile("Data/MapOutlines/Continents/south_america.shp")
SouthACells <- raster::extract(LongLatRaster, SouthAm, df = T, cellnumbers = T,  weight = T)
SouthAmericaVec <- SouthACells$cell

saveRDS(NorthAmericaVec, "Data/NorthAmericaVec.rds")
saveRDS(SouthAmericaVec, "Data/SouthAmericaVec.rds")
