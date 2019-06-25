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
allbryophytes <- BIEN_taxonomy_higher_plant_group("bryophytes", print.query = FALSE)
SpeciesPresence$Species <- gsub("_", " ", SpeciesPresence$Species)
BryophytePresence=SpeciesPresence[SpeciesPresence$Species %in% unique(allbryophytes$scrubbed_species_binomial), ]

#Tally richness by cell
tally <- tally(group_by(BryophytePresence, CellID))
colnames(tally)[2] <- "Richness"

#Bryophyte richness by cell
Bryophyte_Richness <- numeric(15038)
Bryophyte_Richness[tally$CellID] <- tally$Richness

#Subset bryophytes into three groups
Mosses <- subset(bryphy, Group=="Mosses")
Liverworts <- subset(bryphy, Group=="Liverworts")
Hornworts <- subset(bryphy, Group=="Hornworts")

#Rename columns
allbryophytes$Family <- allbryophytes$scrubbed_family
allbryophytes$Genus <- allbryophytes$scrubbed_genus
allbryophytes$Species <- allbryophytes$scrubbed_species_binomial

##Data manipulation
newbryophyte <- allbryophytes[,c(10,11,12)]

#Add group to bryophyte data by family
newbryophyte <- merge(newbryophyte, bryphy, by = "Family", all.x = TRUE)

#Add group to occurrence data by species
BryophytePresence <- merge(BryophytePresence, newbryophyte, by = "Species", all.x= TRUE)

#Subset occurrence data by group
MossPresence <- subset(BryophytePresence, BryophytePresence$Group=="Mosses")
HornwortPresence <- subset(BryophytePresence, BryophytePresence$Group=="Hornworts")
LiverwortPresence <- subset(BryophytePresence, BryophytePresence$Group=="Liverworts")
HLPresence <- subset(BryophytePresence, BryophytePresence$Group=="Hornworts"|BryophytePresence$Group=="Liverworts")

