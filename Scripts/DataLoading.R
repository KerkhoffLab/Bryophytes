#Load data and functions for bryophytes

source('~/Documents/Bryophytes/Functions/BIEN_occurrence_higher_plant_group.R')
source('~/Documents/Bryophytes/Functions/BIEN_taxonomy_higher_plant_group.R')

BIENblank <-raster("Data/blank_100km_raster.tif")

SpeciesPresence <- read.csv("Data/SpeciesPresence_by_100km_Cell.csv")

bryphy <- read.csv("Data/BryophytePhylogeny.csv")

source('~/Documents/Bryophytes/Functions/group.map.R')
source('~/Documents/Bryophytes/Functions/species.map.R')
source('~/Documents/Bryophytes/Functions/family.map.R')

source("~/Documents/Bryophytes/Functions/betagrid.R")
source("~/Documents/Bryophytes/Functions/betagrid2.R")
