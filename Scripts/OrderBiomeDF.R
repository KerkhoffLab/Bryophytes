#Order biome data manipulation
#Hailey Napier
#July 21, 2020

#Load and make data ------------------------------------------------
OrderNames <- readRDS("Data/OrderNames.rds")
NumberOrders <- length(OrderNames)
BiomeNames <- readRDS("Data/BiomeNames.rds")
NumberBiomes <- length(BiomeNames)
HemisphereNames <- c("Both", "Northern", "Southern")
NumberHems <- length(HemisphereNames)

o <- OrderNames[1]
b <- BiomeNames[1]
for(i in 1:NumberHems){
  h <- HemisphereNames[1]
  OrderBiomeDF <- data.frame(ORange(o, b, h))
  OrderBiomeDF$CellID <- c(1:15038)
  OrderBiomeDF$Biome <- b
  OrderBiomeDF$Order <- o
  OrderBiomeDF$Hem <- h
}

for(i in 2:NumberBiomes){
  b <- BiomeNames[i]
  for(i in 1:NumberHems){
    h <- HemisphereNames[i]
    tempdf <- data.frame(ORange(o, b, h))
    tempdf$CellID <- c(1:15038)
    tempdf$Biome <- b
    tempdf$Order <- o
    tempdf$Hem <- h
    OrderBiomeDF <- bind_rows(OrderBiomeDF, tempdf)
  }
}

for(i in 2:NumberOrders){
  o <- OrderNames[i]
  for(i in 1:NumberBiomes){
    b <- BiomeNames[i]
    for(i in 1:NumberHems){
      h <- HemisphereNames[i]
      tempdf <- data.frame(ORange(o,b))
      tempdf$CellID <- c(1:15038)
      tempdf$Biome <- b
      tempdf$Order <- o
      tempdf$Hem <- h
      OrderBiomeDF <- bind_rows(OrderBiomeDF, tempdf)
    }
  }
}
names(OrderBiomeDF)[1] <- "Alpha"
saveRDS(OrderBiomeDF, file = "Data/OrderBiomeDF.rds")
