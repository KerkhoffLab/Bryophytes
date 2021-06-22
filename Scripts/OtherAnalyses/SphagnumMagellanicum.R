#Sphagnum magellanicum map for Chris Goodall

require(BIEN)
require(sp)
require(dplyr)
require(raster)
require(ggplot2)
require(sf)
require(maps)

#Map using range map shapefile from BIEN

BIEN_ranges_species("Sphagnum magellanicum", directory = NULL, matched = TRUE,
                    match_names_only = FALSE, include.gid = FALSE)

SphagnumMagellanicum <- st_read("Sphagnum_magellanicum.shp")

SphagnumMap <- ggplot() + geom_sf(data = SphagnumMagellanicum, size = 1, color = "cyan4", fill = "cyan4") + theme_minimal() +
  scale_x_continuous(limits = c(-175, -25)) +
  coord_sf()

png("SphagnumMagellanicumRange.png", width = 1000, height = 1000, pointsize = 30)
SphagnumMap
dev.off()

SphagnumMap


#Map using species distribution data from BIEN
BryophytePresence <- readRDS("Data/BryophytePresence.rds")
Sphagnum_magellanicum <- subset(BryophytePresence, BryophytePresence$Species== "Sphagnum magellanicum")

tally<-tally(group_by(Sphagnum_magellanicum, CellID))
colnames(tally)[2] <- "Richness"

BIENblank <-raster("Data/blank_100km_raster.tif")
vec <- numeric(15038)
vec[tally$CellID] <- tally$Richness
vec[which(vec==0)]=NA

Raster <- setValues(BIENblank, vec)
Points<-rasterToPoints(Raster)
SphagnumDF <- data.frame(Points)


theme_set(theme_void())
Sphagnum_magellanicum_Map <- ggplot(SphagnumDF) + geom_tile(aes(x=x,y=y, fill=blank_100km_raster), color="cyan4", show.legend=FALSE)  +
  scale_fill_gradientn(colours="cyan4", na.value="transparent") +
  coord_equal()
Sphagnum_magellanicum_Map

png("SphagnumMagellanicum.png", width = 1000, height = 1000, pointsize = 30)
Sphagnum_magellanicum_Map
dev.off()

#Download occurrences (takes a long time)
###SphagnumMagellanicumOccurence <- BIEN_occurrence_species("Sphagnum magellanicum", cultivated = FALSE, only.new.world = FALSE,
                                                       #  all.taxonomy = FALSE, native.status = FALSE, natives.only = TRUE,
                                                        # observation.type = FALSE, political.boundaries = FALSE,
                                                         # collection.info = FALSE)
