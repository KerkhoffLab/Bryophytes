# Family Richness Hotspots
# Hailey Napier
# June 18, 2020

# 0.0 Load Packages--------------------------------------------------------------------------------
require(dplyr)


# 1.0 Make RichnessHotspots dataframe--------------------------------------------------------------
#includes Order, Family, Number of species, and presence/absence columns for areas
BryPres <- BryophytePresence %>%
  dplyr::select(Family, Order, Group)
BryPres <- BryPres[!duplicated(BryPres$Family),]

#RichnessHotspots <- left_join(BryPres, FamNSpecies, by = "Family")

#RichnessHotspots$Widespread <- 0
#RichnessHotspots$SouthA <- 0
#RichnessHotspots$NorthA <- 0
#RichnessHotspots$CentralA <- 0
#RichnessHotspots$NENorthAmerica <- 0
#RichnessHotspots$NWNorthAmerica <- 0
#RichnessHotspots$Alaska <- 0
#RichnessHotspots$Andes <- 0
#RichnessHotspots$Amazon <- 0
#RichnessHotspots$AtlRnFrst <- 0


# 2.0 Manually look at maps and update richness hotspot varibles-----------------------------------
RichnessHotspots$Family[i]

RichnessHotspots$Widespread[i] <- 1
RichnessHotspots$SouthA[i] <- 1
RichnessHotspots$NorthA[i] <- 1
RichnessHotspots$CentralA[i] <- 1
RichnessHotspots$NENorthAmerica[i] <- 1
RichnessHotspots$NWNorthAmerica[i] <- 1
RichnessHotspots$Alaska[i] <- 1
RichnessHotspots$Andes[i] <- 1
RichnessHotspots$Amazon[i] <- 1
RichnessHotspots$AtlRnFrst[i] <- 1
i <- i + 1


# 3.0 Save RichnessHotspots------------------------------------------------------------------------
saveRDS(RichnessHotspots, file = "Data/RichnessHotspots.rds")


# 4.0 Fix with new BryophytePresence---------------------------------------------------------------


# 4.1 Load Old_BryophytePresence
#Load data and source functions
SpeciesPresence <- read.csv("Data/SpeciesPresence_by_100km_Cell.csv")
ByGroup <- read.csv("Data/BryophytePhylogeny.csv")

source("Functions/BIEN_occurrence_higher_plant_group.R")
source("Functions/BIEN_taxonomy_higher_plant_group.R")

#Table of all bryophyte occurrences
BrySpecies <- BIEN_taxonomy_higher_plant_group("bryophytes", print.query = FALSE)
SpeciesPresence$Species <- gsub("_", " ", SpeciesPresence$Species)
BryophytePresence <- SpeciesPresence[SpeciesPresence$Species %in% unique(BrySpecies$scrubbed_species_binomial), ]

#Subset bryophytes into three groups
Mosses <- subset(ByGroup, Group=="Mosses")
Liverworts <- subset(ByGroup, Group=="Liverworts")
Hornworts <- subset(ByGroup, Group=="Hornworts")

#Rename columns
BrySpecies$Family <- BrySpecies$scrubbed_family
BrySpecies$Genus <- BrySpecies$scrubbed_genus
BrySpecies$Species <- BrySpecies$scrubbed_species_binomial
BrySpecies <- BrySpecies[,c(10,11,12)]
BrySpecies <- distinct(BrySpecies)

#Add group to bryophyte data by family
BrySpecies <- merge(BrySpecies, ByGroup, by = "Family", all.x = TRUE)

#Add group to presence data by species and remove duplicate entries and save
BryophytePresence <- merge(BryophytePresence, BrySpecies, by = "Species", all.x= TRUE)
Old_BryophytePresence <- distinct(BryophytePresence)


# 4.3 Load New_BryophytePresence
New_BryophytePresence <- read.csv("Data/BryophytePresence6:8:20.csv")


# 4.4 Find differences
length(unique(Old_BryophytePresence$Order))
length(unique(New_BryophytePresence$Family))

New_Fam <- New_BryophytePresence$Family
Old_Fam <- Old_BryophytePresence$Family

changedFam <- setdiff(Old_Fam, New_Fam)

# 4.5 Alter RichnessHotspots
RHDF <- RichnessHotspots

#remove the families that have been removed in New_BryophytePresence
for(i in 1:length(changedFam)){
  rf <- changedFam[i]
  RHDF <- subset(RHDF, Family != rf)
}

#remove Order, Group, and NumberSpecies columns
RHDF <- RHDF %>%
  dplyr::select(!c(Order, Group, NumberSpecies))

#add redone Order, Group, and NumberSpecies columns back
FOG <- BryophytePresence %>%
  dplyr::select(c(Family, Order, Group))

RHDF <- full_join(RHDF, FamNSpecies, by = "Family")
RHDF <-full_join(RHDF, FOG, by = "Family")


#4.6 Save as .csv
write.csv(RHDF, "Data/RichnessHotspotsDF.csv")













