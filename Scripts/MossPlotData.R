# MossPlotData.R
# Subset bryophyte data to only include mosses for bwplots and circle plots
# Hailey Napier, July 2020

# Load packages ------------------------------------------------------------------------
library(dplyr)

# Load data ----------------------------------------------------------------------------
BryophytePresence <- readRDS("Data/BryophytePresence.rds")

BiomeNames <- readRDS("Data/BiomeNames.rds")
NumberBiomes <- length(BiomeNames)
ContinentNames <- c("North America", "South America")
NumberContinents <- 2


# 1.0 Subset BrophytePresence to only include only mosses ------------------------------
MossPresence <- BryophytePresence %>%
  filter(Group == "Moss" | Group == "Mosses")

MossOrderNames <- unique(MossPresence$Order)

saveRDS(MossPresence, "Data/MossPresence.rds")
saveRDS(MossOrderNames, "Data/MossOrderNames.rds")


# 2.0 Update data for bwplots  ----------------------------------------------------------
# 2.1 Make MossOrderRichList
MossOrderList <- list()
for(i in 1:length(MossOrderNames)){
  ord <- MossOrderNames[i]
  MossOrderList[[i]] <- subset(BryophytePresence, Order == ord)
}

MossOrderRichList <- list()
MossOrderPresList <- list()
for(i in 1:length(MossOrderNames)){
  MossOrderPresList[[i]] <- tally(group_by(MossOrderList[[i]], CellID))
  names(MossOrderPresList[[i]])[2] <- "Richness"
  MossOrderRichList[[i]] <- numeric(15038)
  MossOrderRichList[[i]][MossOrderPresList[[i]]$CellID] <- MossOrderPresList[[i]]$Richness
  MossOrderRichList[[i]][which(MossOrderRichList[[i]]==0)] = NA
}

saveRDS(MossOrderRichList, file = "Data/MossOrderRichList.rds")

# 2.2 Make MossOBC
# OBC = Order Biome Continent
nrows <- 7278392
MossOBC <- data.frame(rep(NA,nrows))
names(MossOBC)[1] <- "Alpha"
MossOBC$Alpha <- NA
MossOBC$CellID <- NA
MossOBC$Order <- NA
MossOBC$Cont <- NA
MossOBC$Biome <- NA

start <- 1
end <- 15038
o <- MossOrderNames[1]
b <- BiomeNames[1]
c <- ContinentNames[1]
MossOBC$Alpha[start:end] <- ORange(o,b,c)
MossOBC$CellID[start:end] <- c(1:15038)
MossOBC$Biome[start:end] <- b
MossOBC$Order[start:end] <- o
MossOBC$Cont[start:end] <- c

start <- end + 1
end <- start + 15037
c <- ContinentNames[2]
MossOBC$Alpha[start:end] <- ORange(o, b, c)
MossOBC$CellID[start:end]<- c(1:15038)
MossOBC$Biome[start:end] <- b
MossOBC$Order[start:end] <- o
MossOBC$Cont[start:end] <- c

for(j in 2:length(BiomeNames)){
  b <- BiomeNames[j]
  for(k in 1:length(ContinentNames)){
    start <- end + 1
    end <- start + 15037
    c <- ContinentNames[k]
    MossOBC$Alpha[start:end] <- ORange(o, b, c)
    MossOBC$CellID[start:end]<- c(1:15038)
    MossOBC$Biome[start:end] <- b
    MossOBC$Order[start:end] <- o
    MossOBC$Cont[start:end] <- c
  }
}

for(m in 2:length(MossOrderNames)){
  o <- MossOrderNames[m]
  for(n in 1:length(BiomeNames)){
    b <- BiomeNames[n]
    for(p in 1:NumberContinents){
      start <- end + 1
      end <- start + 15037
      c <- ContinentNames[p]
      MossOBC$Alpha[start:end] <- ORange(o, b, c)
      MossOBC$CellID[start:end]<- c(1:15038)
      MossOBC$Biome[start:end] <- b
      MossOBC$Order[start:end] <- o
      MossOBC$Cont[start:end] <- c
    }
  }
}

saveRDS(MossOBC, file = "Data/MossOBC.rds")


# 2.3 Make vectors based on max alpha diversity
#Make a dataframe to look at numbers for max alpha diversity 
MossOrderMaxAlpha <- data.frame(tapply(MossOBC$Alpha, MossOBC$Order, max, na.rm = T))
names(MossOrderMaxAlpha)[1]  <- "MaxAlpha"
Names <- sort(unique(MossOBC$Order))
MossOrderMaxAlpha$Names <- Names

# Put order names into vectors based on max alpha diversity 
MossOrdRichAbove100 <- vector()
MossOrdRich25to100 <- vector()
MossOrdRich10to25 <- vector()
MossOrdRichBelow10 <- vector()
for(i in 1:length(Names)){
  name <- Names[i]
  if(MossOrderMaxAlpha$MaxAlpha[i] > 100){
    MossOrdRichAbove100 <- c(MossOrdRichAbove100, name)
  }else if(MossOrderMaxAlpha$MaxAlpha[i] >= 25){
    MossOrdRich25to100 <- c(MossOrdRich25to100, name)
  }else if(MossOrderMaxAlpha$MaxAlpha[i] >= 10){
    MossOrdRich10to25 <- c(MossOrdRich10to25, name)
  }else if(MossOrderMaxAlpha$MaxAlpha[i] < 10){
    MossOrdRichBelow10 <- c(MossOrdRichBelow10, name)
  }
}

saveRDS(MossOrdRichAbove100, "Data/MossOrdRichAbove100.rds")
saveRDS(MossOrdRich25to100, "Data/MossOrdRich25to100.rds")
saveRDS(MossOrdRich10to25, "Data/MossOrdRich10to25.rds")
saveRDS(MossOrdRichBelow10, "Data/MossOrdRichBelow10.rds")



