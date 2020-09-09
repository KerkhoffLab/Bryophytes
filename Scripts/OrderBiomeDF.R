#Order biome data manipulation
#Hailey Napier
#July 21, 2020

# Load data ------------------------------------------------------------
OrderRichList <- readRDS("Data/OrderRichList.rds")
NorthAmBiomeList <- readRDS("Data/NorthAmBiomeList.rds")
SouthAmBiomeList <- readRDS("Data/SouthAmBiomeList.rds")

OrderNames <- readRDS("Data/OrderNames.rds")
NumberOrders <- length(OrderNames)
BiomeNames <- readRDS("Data/BiomeNames.rds")
NumberBiomes <- length(BiomeNames)
ContinentNames <- c("North America", "South America")
NumberContinents <- 2

# Source function -------------------------------------------------------
source("Functions/ORange.R")

# OrderBiomeDF ----------------------------------------------------------
# Includes orders and biomes, not separated by continent or hemisphere
o <- OrderNames[1]

for(i in 1:NumberBiomes){
  b <- BiomeNames[i]
  tempdf <- data.frame(ORange(o, b))
  tempdf$CellID <- c(1:15038)
  tempdf$Biome <- b
  tempdf$Order <- o
  OrderBiomeDF <- bind_rows(OrderBiomeDF, tempdf)
}

for(i in 2:NumberOrders){
  o <- OrderNames[i]
  for(i in 1:NumberBiomes){
    b <- BiomeNames[i]
    tempdf <- data.frame(ORange(o,b))
    tempdf$CellID <- c(1:15038)
    tempdf$Biome <- b
    tempdf$Order <- o
    OrderBiomeDF <- bind_rows(OrderBiomeDF, tempdf)
  }
}
names(OrderBiomeDF)[1] <- "Alpha"
saveRDS(OrderBiomeDF, file = "Data/OrderBiomeDF.rds")


# CleanOrderBiomeDF ----------------------------------------------------------
# Includes orders and biomes, not separated by continent or hemisphere
CleanOrderBiomeDF <- data.frame()
o <- OrderNames[1]

for(i in 1:NumberBiomes){
  b <- BiomeNames[i]
  tempdf <- data.frame(ORange(o, b, "both", "clean"))
  tempdf$CellID <- c(1:15038)
  tempdf$Biome <- b
  tempdf$Order <- o
  CleanOrderBiomeDF <- bind_rows(CleanOrderBiomeDF, tempdf)
}

for(i in 2:NumberOrders){
  o <- OrderNames[i]
  for(i in 1:NumberBiomes){
    b <- BiomeNames[i]
    tempdf <- data.frame(ORange(o,b,"both","clean"))
    tempdf$CellID <- c(1:15038)
    tempdf$Biome <- b
    tempdf$Order <- o
    CleanOrderBiomeDF <- bind_rows(CleanOrderBiomeDF, tempdf)
  }
}
names(CleanOrderBiomeDF)[1] <- "Alpha"
saveRDS(CleanOrderBiomeDF, file = "Data/CleanOrderBiomeDF.rds")


# OrderBiomeContDF -----------------------------------------------------
# Separated by order, biome, and continent
# Rewrote to replace append function and tempdf (maybe runs faster because it doesn't remake the dataframe everytime)

nrows <- 12571768
OrderBiomeContDF <- data.frame(rep(NA,nrows))
names(OrderBiomeContDF)[1] <- "Alpha"
OrderBiomeContDF$Alpha <- NA
OrderBiomeContDF$CellID <- NA
OrderBiomeContDF$Order <- NA
OrderBiomeContDF$Cont <- NA
OrderBiomeContDF$Biome <- NA

start <- 1
end <- 15038
o <- OrderNames[1]
b <- BiomeNames[1]
c <- ContinentNames[1]
OrderBiomeContDF$Alpha[start:end] <- ORange(o,b,c)
OrderBiomeContDF$CellID[start:end] <- c(1:15038)
OrderBiomeContDF$Biome[start:end] <- b
OrderBiomeContDF$Order[start:end] <- o
OrderBiomeContDF$Cont[start:end] <- c

start <- end + 1
end <- start + 15037
c <- ContinentNames[2]
OrderBiomeContDF$Alpha[start:end] <- ORange(o, b, c)
OrderBiomeContDF$CellID[start:end]<- c(1:15038)
OrderBiomeContDF$Biome[start:end] <- b
OrderBiomeContDF$Order[start:end] <- o
OrderBiomeContDF$Cont[start:end] <- c

for(j in 2:NumberBiomes){
  b <- BiomeNames[j]
  for(k in 1:NumberContinents){
    start <- end + 1
    end <- start + 15037
    c <- ContinentNames[k]
    OrderBiomeContDF$Alpha[start:end] <- ORange(o, b, c)
    OrderBiomeContDF$CellID[start:end]<- c(1:15038)
    OrderBiomeContDF$Biome[start:end] <- b
    OrderBiomeContDF$Order[start:end] <- o
    OrderBiomeContDF$Cont[start:end] <- c
  }
}

for(m in 2:NumberOrders){
  o <- OrderNames[m]
  for(n in 1:NumberBiomes){
    b <- BiomeNames[n]
    for(p in 1:NumberContinents){
      start <- end + 1
      end <- start + 15037
      c <- ContinentNames[p]
      OrderBiomeContDF$Alpha[start:end] <- ORange(o, b, c)
      OrderBiomeContDF$CellID[start:end]<- c(1:15038)
      OrderBiomeContDF$Biome[start:end] <- b
      OrderBiomeContDF$Order[start:end] <- o
      OrderBiomeContDF$Cont[start:end] <- c
    }
  }
}

saveRDS(OrderBiomeContDF, file = "Data/OrderBiomeContDF.rds")

