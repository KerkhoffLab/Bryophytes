#Bryophyte data processing
#Make sure to run DataLoading.R first

#Load packages
require(BIEN)
require(ape)
require(maps) 
require(dplyr)
require(maptools)
require(raster)
require(dismo)
require(sp)
require(rgdal)
require(mapdata)
require(mapproj)

#Table of all bryophyte occurrences
BrySpecies <- BIEN_taxonomy_higher_plant_group("bryophytes", print.query = FALSE)
SpeciesPresence$Species <- gsub("_", " ", SpeciesPresence$Species)
BryophytePresence <- SpeciesPresence[SpeciesPresence$Species %in% unique(BrySpecies$scrubbed_species_binomial), ]

#Tally richness by cell and create richness vector and save
CellRichness <- tally(group_by(BryophytePresence, CellID))
colnames(BryRichness)[2] <- "Richness"
RichnessVec <- numeric(15038)
RichnessVec[BryRichness$CellID] <- BryRichness$Richness

saveRDS(CellRichness, file = "Data/CellRichness.rds")

#Subset bryophytes into three groups
Mosses <- subset(ByGroup, Group=="Mosses")
Liverworts <- subset(ByGroup, Group=="Liverworts")
Hornworts <- subset(ByGroup, Group=="Hornworts")

#Rename columns
BrySpecies$Family <- BrySpecies$scrubbed_family
BrySpecies$Genus <- BrySpecies$scrubbed_genus
BrySpecies$Species <- BrySpecies$scrubbed_species_binomial
BrySpecies <- BrySpecies[,c(10,11,12)]

#Add group to bryophyte data by family
BrySpecies <- merge(BrySpecies, ByGroup, by = "Family", all.x = TRUE)

#Add group to presence data by species and remove duplicate entries and save
BryophytePresence <- merge(BryophytePresence, BrySpecies, by = "Species", all.x= TRUE)
BryophytePresence <- distinct(BryophytePresence)

saveRDS(BryophytePresence, file = "Data/BryophytePresence.rds")
