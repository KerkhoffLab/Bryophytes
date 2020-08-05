#Clean script for reading Liu et al.(2019) supplementary figure 20 tree & processing data
#Hailey Napier & Kathryn Dawdy
#July 20, 2020

# 0.1 Load Packages ------------------------------------------------------
library(ape)
library(phytools)

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("ggtree")
BiocManager::install("treeio")
BiocManager::install("rphast")
library(ggtree)
library(treeio)
library(ggplot2)
library(ggimage)

library(tidyr)
library(dplyr)
library(tools)

library(grid)
library(gridExtra)

library(png)


# 0.2 Download tree from Dryad via https://datadryad.org/stash/dataset/doi:10.5061/dryad.tj3gd75


# 1.0 Read tree from Liu et al., 2019 data ------------------------------
FigS20 <- read.tree("Data/trees/aa-nu-RAxML-FigS20.tre")

# 1.1 Extract tree data and store as a dataframe 
FigS20_FOG <- ggtree(FigS20)$data

FigS20_names <- data.frame(FigS20_FOG$label <- gsub("_", " ", FigS20_FOG$label))
names(FigS20_names)[1] <- "name"
FigS20_names <- separate(FigS20_names, name, into = c("genus","other"))
FigS20_names$other <- NULL
FigS20_names$family <- NA
FigS20_names$order <- NA
FigS20_names$group <- NA

# 1.2 #Make list of genus names in our data
unique(BryophytePresence$Family[which(BryophytePresence$Genus == FigS20_FOG$genus[i])])
GenusNames <- unique(BryophytePresence$Genus)
GenusNames <- GenusNames[complete.cases(GenusNames)]

# 1.2 Clean up dataframe and assign genus and family names
for(i in 1:nrow(FigS20_names)){
  n <- FigS20_names$genus[i]
  FigS20_names$genus[i] <- ifelse(n == "sphagnumL" | n == "sphagnumP" | n == "sphagnumR", "Sphagnum", n)
  FigS20_names$genus[i] <- gsub('[[:digit:]]+', '', FigS20_names$genus[i])
  FigS20_names$genus[i] <- toTitleCase(FigS20_names$genus[i])
  test.genus <- FigS20_names$genus[i]
  for(j in 1:length(GenusNames)){
    ref.genus <- GenusNames[j]
    if(test.genus == ref.genus){
      FigS20_names$family[i] <- unique(BryophytePresence$Family[which(BryophytePresence$Genus == test.genus)])
      FigS20_names$order[i] <- unique(BryophytePresence$Order[which(BryophytePresence$Genus == test.genus)])
      FigS20_names$group[i] <- unique(BryophytePresence$Group[which(BryophytePresence$Genus == test.genus)])
      break
    }
  }
}

FigS20_FOG <- bind_cols(FigS20_FOG, FigS20_names)
write.csv(FigS20_FOG, "Data/FamilyTrees/FigS20_FOG.csv")


# 2.0 Group tip labels into families -------------------------------------

# 2.1 Get family names for each tip label
#I made this program in a different script than part 1, so the name of the tree is different, but it's the same tree
tree20 <- FigS20
#Make a dataframe that contains the tip labels and their families for reference
clean20famlabels <- FigS20_FOG %>%
  filter(isTip == "TRUE") %>%
  select(family)
Ref20DF <- data.frame(tree20$tip.label)
Ref20DF$Family <- clean20famlabels
names(Ref20DF)[1] <- "Species"

# 3.2 Loop to find species in each family
    ##creates a vector of species/tip label names for each family name
tree20fam <- F20Fam$Family
tree20fam
for(i in 1:length(tree20fam)){
  fam <- tree20fam[i]
  tempdf <- Ref20DF %>%
    filter(Family == fam)
  tempvec <- tempdf$Species
  assign(fam, tempvec)
}


# 3.0 Group tip labels into orders -------------------------------------

# 3.1 Program that assigns an order to each species in the data
#Get order names
clean20orderlabels <- FigS20_FOG %>%
  filter(FigS20_FOG$isTip == "TRUE") %>%
  select(order)
Ref20DF <- data.frame(tree20$tip.label)
Ref20DF$Family <- clean20famlabels
Ref20DF$Order <- clean20orderlabels
names(Ref20DF)[1] <- "Species"

# 3.2 Loop to find species in each order -- creates a vector of species names for each order name
tree20order <- unique(FigS20_FOG$order)
tree20order <- tree20order[complete.cases(tree20order)]
tree20order
tree20orderlist <- list()
for(i in 1:length(tree20order)){
  order <- tree20order[i]
  tempdf <- Ref20DF %>%
    filter(Order == order)
  tempvec <- tempdf$Species
  tree20orderlist[[i]] <- tempvec
}



#Make maps with TreeMaps.R
#Make biome plots with OrderBiomePlots.R


