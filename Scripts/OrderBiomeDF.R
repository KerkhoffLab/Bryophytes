#Order biome data manipulation
#Hailey Napier
#July 21, 2020

o <- OrderNames[1]
b <- BiomeNames[1]
OrderBiomeDF <- data.frame(ORange(o, b))
OrderBiomeDF$CellID <- c(1:15038)
OrderBiomeDF$Biome <- b
OrderBiomeDF$Order <- o
for(i in 2:NumberBiomes){
  b <- BiomeNames[i]
  tempdf <- data.frame(ORange(o,b))
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
