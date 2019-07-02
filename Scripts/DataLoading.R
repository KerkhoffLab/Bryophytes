#Load data and functions for bryophytes

SpeciesPresence <- read.csv("Data/SpeciesPresence_by_100km_Cell.csv")

ByGroup <- read.csv("Data/BryophytePhylogeny.csv")

source("Functions/BIEN_occurrence_higher_plant_group.R")
source("Functions/BIEN_taxonomy_higher_plant_group.R")

source("Functions/group.map.R")
source("Functions/species.map.R")
source("Functions/family.map.R")
