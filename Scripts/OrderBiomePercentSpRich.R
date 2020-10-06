# Order/Biome Quantitative Analysis for Presentation
# Hailey Napier
# September 2020

# Make a dataframe with each order and the percent range area for each biome
## Also include a measure of variablilty if possible (not sure what yet)

# Load Packages
library(dplyr)
library(ggplot2)

# 0.0 Load Data ------------------------------------------------------------
#From DataProcessing2020.R
MossPresence <- readRDS("Data/MossPresence.rds")
MossOrderNames <- readRDS("Data/MossOrderNames.rds")
BiomeNames <- readRDS("Data/BiomeNames.rds")
# From BiomeBetaCellsClean.R
BiomeBetaCellsClean <- readRDS("Data/BiomeBetaCellsClean.rds")
# From BiomeCirclePlot.R
CircleMatAllMoss <- readRDS("Data/CircleMatAllMoss.rds")

MossOrderSpeciesList <- readRDS("Data/MossOrderSpeciesList.rds")

MossOrdRich25to100 <- readRDS("Data/MossOrdRich25to100.rds")
MossOrdRichAbove100 <- readRDS("Data/MossOrdRichAbove100.rds")
MossOrdRich10to25 <- readRDS("Data/MossOrdRich10to25.rds")
MossOrdRichBelow10 <- readRDS("Data/MossOrdRichBelow10.rds")


# 1.0 Make CellID/biome data frames for each order -------------------------
NumberOrders <- length(MossOrderNames)
MossOrderBiomeList <- list()

for(i in 1:NumberOrders){
  order <- MossOrderNames[i]
  CellID <- MossPresence %>%
    dplyr::filter(Order == order) %>%
    dplyr::select(CellID)
  CellID <- unique(as.vector(CellID$CellID))
  DF <- data.frame(CellID)
  DF$Biome <- NA
  for(j in CellID){
    cell <- j
    biome <- BiomeCellsClean$Type[which(BiomeBetaCellsClean$CellID == cell)]
    if(length(biome) > 0){
      DF$Biome[which(DF$CellID == cell)] <- biome
    }
  }
  DF$Order <- order
  MossOrderBiomeList[[i]] <- DF
}


# 2.0 Find percentage of cells with biome for biomes in each order --------------
# 2.1 Make a matrix with total cell counts for each order within each biome
NumberBiomes <- 11
BiomeNamesAndTotal <- c(BiomeNames, "Total")

#MossOrderBiome = MOB
MOBMat <- matrix(NA, 22, 12)
rownames(MOBMat) <- MossOrderNames
colnames(MOBMat) <- BiomeNamesAndTotal

for(i in 1:NumberOrders){
  order <- MossOrderNames[i]
  
  DF <- MossOrderBiomeList[[i]]
  DF <- DF %>%
    dplyr::filter(!is.na(DF$Biome))
  
  total <- nrow(DF)
  biome = "Total"
  MOBMat[order, biome] <- total
  
  biomes <- DF %>%
    dplyr::select(Biome)
  biomes <- unique(as.vector(biomes$Biome))
  print(order)
  print(biomes)
  
  for(j in 1:length(biomes)){
    biome = biomes[j]
    nbiome <- DF %>%
      dplyr::filter(DF$Biome == biome)
    nbiome <- nrow(nbiome)
    
    MOBMat[order, biome] <- nbiome
  }
}

#save matrix
saveRDS(MOBMat, "Data/MOBMat.rds")

# 2.2 Find percentages and put into a DF
MOBPercentMat <- MOBMat
for(i in 1:NumberOrders){
  order <- MossOrderNames[i]
  total <- MOBPercentMat[order, "Total"]
  for(j in 1:NumberBiomes){
    biome <- BiomeNames[j]
    biome_total <- MOBPercentMat[order, biome]
    percent <- biome_total/total*100
    MOBPercentMat[order, biome] <- percent
  }
}


# 3.0 Percent matrix for species composition (not range area) -----------------
# Use circle plot matrix
MOBPerMatSpecies <- CircleMatAllMoss
MOBPerMatSpecies <- cbind(MOBPerMatSpecies, NA)
colnames(MOBPerMatSpecies)[12] <- "Total"

totals <- rowSums(MOBPerMatSpecies, na.rm = T)

for(i in 1:NumberOrders){
  order <- MossOrderNames[i]
  total <- totals[[i]]
  MOBPerMatSpecies[order, "Total"] <- total
}

for(i in 1:NumberOrders){
  order <- MossOrderNames[i]
  total <- MOBPerMatSpecies[order, "Total"]
  for(j in 1:NumberBiomes){
    biome <- BiomeNames[j]
    biome_abundance <- MOBPerMatSpecies[order, biome]
    percent <- biome_abundance/total*100
    MOBPerMatSpecies[order, biome] <- percent
  }
}


# 4.0 Null model for stat analysis -------------------------------------------------
# Assign each species a new order
# Repeat 100-500 times depending on how long it takes

# 4.1.1 Make moss order/species dataframe
MossOrdSpecBioDF <- data.frame("Species" = rep(NA, 2976), "Order" = rep(NA, 2976))
end <- 0
for(i in 1:NumberOrders){
  order <- MossOrderNames[i]
  species_list <- MossOrderSpeciesList[[i]]
  number_species <- length(species_list)
  start <- end + 1
  end <- end + number_species
  MossOrdSpecBioDF$Species[start:end] <- species_list
  MossOrdSpecBioDF$Order[start:end] <- order
}

# 4.1.2 Add biome column for the biome where each species is most abundant
MossOrdSpecBioDF$Biome <- NA
tempDF <- data.frame(Species = NA, Order = NA, Biome = NA)
for(i in 1:nrow(MossOrdSpecBioDF)){
  order <- MossOrdSpecBioDF$Order[i]
  species <- MossOrdSpecBioDF$Species[i]
  biomenumcells <- vector()
  for(j in 1:length(BiomeNames)){
    biome <- BiomeNames[j]
    ab <- SpBiMat[species,biome]
    if(ab == 0){
      ab = NA
    }
    biomenumcells[j] <- ab
  }
  if(sum(biomenumcells, na.rm = T) > 0){
    biomeindex <- which(biomenumcells == max(biomenumcells, na.rm = T))
    maxbiome <- BiomeNames[biomeindex]
    if(length(maxbiome) == 1){
      MossOrdSpecBioDF$Biome[which(MossOrdSpecBioDF$Species == species)] <- maxbiome
    }else if(length(maxbiome) > 1){
      print(species)
      multimax <- maxbiome[1]
      MossOrdSpecBioDF$Biome[which(MossOrdSpecBioDF$Species == species)] <- multimaxone
      multimaxtwo <- maxbiome[2]
      tempDF <- tempDF %>% add_row(Species = species, Order = order, Biome = multimaxtwo)
    }
  }
}

#Add in species that have the same max abundance value in multiple biomes
MossOrdSpecBioDF <- bind_rows(MossOrdSpecBioDF, tempDF)

#Omit species where biome == NA
MossOrdSpecBioDF <- MossOrdSpecBioDF[complete.cases(MossOrdSpecBioDF), ]

# 4.2 Make null model to repeat
NullMOBMat <- matrix(NA, 22, 12)
rownames(NullMOBMat) <- MossOrderNames
colnames(NullMOBMat) <- BiomeNamesAndTotal

SortedMossOrderNames <- sort(MossOrderNames)
NumberReps <- 1000

for(i in 1:NumberReps){
  # 4.2.1: Use transform to shuffle order column (assign random order to each species)
  DF <- transform(MossOrdSpecBioDF, Order = sample(Order))
  # 4.2.2 Tally species in each order by biome
  tallytable <- table(DF$Order, DF$Biome)
  # 4.2.3 Store species richness values for each biome by order in a matrix
  for(j in 1:NumberOrders){
    order <- SortedMossOrderNames[j]
    for(k in 1:NumberBiomes){
      biome <- BiomeNames[k]
      richness <- tallytable[j,k]
      NullMOBMat[order, biome] <- sum(NullMOBMat[order, biome], richness, na.rm = T)
    }
  }
}

# 4.2.4 Calculate totals for each order 
totals <- rowSums(NullMOBMat, na.rm = T)
for(i in 1:NumberOrders){
  order <- MossOrderNames[i]
  total <- totals[[i]]
  NullMOBMat[order, "Total"] <- total
}

# 4.3 Make a percentage matrix for null data
NullMOBPerMat <- NullMOBMat

for(i in 1:NumberOrders){
  order <- MossOrderNames[i]
  total <- NullMOBMat[order, "Total"]
  for(j in 1:NumberBiomes){
    biome <- BiomeNames[j]
    biome_abundance <- NullMOBPerMat[order, biome]
    percent <- biome_abundance/total*100
    NullMOBPerMat[order, biome] <- percent
  }
}


# 5.0 Analysis ---------------------------------------------------------------------
# 5.1 Biomes
# 5.1.1 Raw Richness Numbers
# Divide each cell by number of reps to get null mean
NullMOBMat <- NullMOBMat/NumberReps
NullMeanTable <- apply(NullMOBMat, 2, mean)

# Observed data -- number of species in each biome
ObsMeanTable <- colMeans(CircleMatAllMoss)
ObsSDTable <- apply(CircleMatAllMoss, 2, sd)

# Make a matrix for comparing means and SDs
MOBStats <- matrix(NA, 11, 4)
rownames(MOBStats) <- BiomeNames
colnames(MOBStats) <- c("ObsMean", "ObsSD", "NullMean", "ZScore")

for(i in 1:NumberBiomes){
  biome <- BiomeNames[i]
  MOBStats[biome, "ObsMean"] <- ObsMeanTable[[i]]
  MOBStats[biome, "ObsSD"]<- ObsSDTable[[i]]
  MOBStats[biome, "NullMean"]<- NullMeanTable[[i]]
  zscore <- ((MOBStats[biome, "ObsMean"] - MOBStats[biome, "NullMean"])/MOBStats[biome, "ObsSD"])
  MOBStats[biome, "ZScore"] <- zscore
}

# 5.1.2 Percentages
NullPerMeanTable <- apply(NullMOBPerMat, 2, mean)

ObsPerMeanTable <- apply(MOBPerMatSpecies, 2, mean)
ObsPerSDTable <- apply(MOBPerMatSpecies, 2, sd)

PerMOBStats <- matrix(NA, 11, 4)
rownames(PerMOBStats) <- BiomeNames
colnames(PerMOBStats) <- c("ObsMean", "ObsSD", "NullMean", "ZScore")

for(i in 1:NumberBiomes){
  biome <- BiomeNames[i]
  PerMOBStats[biome, "ObsMean"] <- ObsPerMeanTable[[i]]
  PerMOBStats[biome, "ObsSD"]<- ObsPerSDTable[[i]]
  PerMOBStats[biome, "NullMean"]<- NullPerMeanTable[[i]]
  zscore <- ((MOBStats[biome, "ObsMean"] - PerMOBStats[biome, "NullMean"])/PerMOBStats[biome, "ObsSD"])
  PerMOBStats[biome, "ZScore"] <- zscore
}

# 5.2 Orders
#Is this (single) order overrepresented in this biome?
#Make a zscore matrix
#Not sure which SD to use, so I did two iterations here
BiomeSD <- apply(CircleMatAllMoss, 2, sd)

MOBZMatBiomeSD <- CircleMatAllMoss
for(i in 1:NumberOrders){
  order <- MossOrderNames[i]
  for(j in 1:NumberBiomes){
    biome <- BiomeNames[j]
    sd <- BiomeSD[j]
    nullpercent <- NullMOBPerMat[order, biome]
    obspercent <- MOBPerMatSpecies[order, biome]
    zscore <- ((nullpercent - obspercent)/sd)
    MOBZMatBiomeSD[order, biome] <- zscore
  }
}


MOBZMatAllSD <- CircleMatAllMoss
obssd <- sd(MOBPerMatSpecies)
for(i in 1:NumberOrders){
  order <- MossOrderNames[i]
  for(j in 1:NumberBiomes){
    biome <- BiomeNames[j]
    nullpercent <- NullMOBPerMat[order, biome]
    obspercent <- MOBPerMatSpecies[order, biome]
    zscore <- ((nullpercent - obspercent)/obssd)
    MOBZMatAllSD[order, biome] <- zscore
  }
}

# 6.0 Plotting Z-Scores -------------------------------------------------------------
# 6.1 Make a dataframe for plotting
MOBZScoreDFBiomeSD <- data.frame(Order = rep(NA, 242), Biome = rep(NA, 242), ZScore = rep(NA, 242), Group = rep(NA, 242))
rownumber <- 0
for(i in 1:NumberOrders){
  order <- MossOrderNames[i]
  if(order %in% MossOrdRichBelow10){
    group <- "Least diverse (10 or fewer species)"
  }else if(order %in% MossOrdRich10to25){
    group <- "Less diverse (11 - 25 species)"
  }else if(order %in% MossOrdRich25to100){
    group <- "More diverse (26 - 100 species)"
  }else if(order %in% MossOrdRichAbove100){
    group <- "Most diverse (greater than 100 species)"
  }
  for(j in 1:NumberBiomes){
    biome <- BiomeNames[j]
    zscore <- MOBZMatBiomeSD[order,biome]
    rownumber <- rownumber + 1
    MOBZScoreDFBiomeSD$Order[rownumber] <- order
    MOBZScoreDFBiomeSD$Biome[rownumber] <- biome
    MOBZScoreDFBiomeSD$ZScore[rownumber] <- zscore
    MOBZScoreDFBiomeSD$Species_Richness[rownumber] <- group
  }
}

MOBZScoreDFAllSD <- data.frame(Order = rep(NA, 242), Biome = rep(NA, 242), ZScore = rep(NA, 242), Group = rep(NA, 242))
rownumber <- 0
for(i in 1:NumberOrders){
  order <- MossOrderNames[i]
  if(order %in% MossOrdRichBelow10){
    group <- "Least diverse (10 or fewer species)"
  }else if(order %in% MossOrdRich10to25){
    group <- "Less diverse (11 - 25 species)"
  }else if(order %in% MossOrdRich25to100){
    group <- "More diverse (26 - 100 species)"
  }else if(order %in% MossOrdRichAbove100){
    group <- "Most diverse (greater than 100 species)"
  }
  for(j in 1:NumberBiomes){
    biome <- BiomeNames[j]
    zscore <- MOBZMat[order,biome]
    rownumber <- rownumber + 1
    MOBZScoreDFAllSD$Order[rownumber] <- order
    MOBZScoreDFAllSD$Biome[rownumber] <- biome
    MOBZScoreDFAllSD$ZScore[rownumber] <- zscore
    MOBZScoreDFAllSD$Species_Richness[rownumber] <- group
  }
}

# 6.2 Doptplot
ZScoreDotBiomeSD <- ggplot(data = MOBZScoreDFBiomeSD, aes(x = Biome, y = ZScore, fill = Species_Richness)) +
  geom_dotplot(binaxis = "y", stackdir= "center", 
               dotsize = 0.5, alpha = 0.8, binwidth = 0.25) +
  theme(legend.position = "right") +
  theme_minimal() +
  labs(fill = "Species Richness Level") +
  scale_fill_brewer(palette="Greens") +
  theme(axis.text.x = element_text(angle = 45,  hjust = 1, size =11)) +
  theme(axis.title.x = element_text(size = 15)) +
  theme(axis.title.y = element_text(size = 15)) + 
  theme(legend.title = element_text(size = 15)) + 
  theme(legend.text = element_text(size = 11)) +
  geom_hline(yintercept = -2, color = "darkblue", linetype = "dashed")
ZScoreDotBiomeSD


ZScoreDotAllSD <- ggplot(data = MOBZScoreDFAllSD, aes(x = Biome, y = ZScore, fill = Species_Richness)) +
  geom_dotplot(binaxis = "y", stackdir= "center", 
               dotsize = 0.5, alpha = 0.8, binwidth = 0.05) +
  theme(legend.position = "right") +
  scale_fill_brewer(palette="Greens") +
  theme(axis.text.x = element_text(angle = 45,  hjust = 1, size =11)) +
  theme(axis.title.x = element_text(size = 15)) +
  theme(axis.title.y = element_text(size = 15)) + 
  theme(legend.title = element_text(size = 15)) + 
  theme(legend.text = element_text(size = 11)) 
  
ZScoreDotAllSD




