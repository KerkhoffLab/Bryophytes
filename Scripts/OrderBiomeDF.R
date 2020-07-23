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


#This way is maybe slightly faster ----------------------------------
#Rewrote to replace append function and tempdf

nrows <- 18857652
OrderBiomeHemDF <- data.frame(rep(NA,nrows))
names(OrderBiomeHemDF)[1] <- "alpha"
OrderBiomeHemDF$Alpha <- NA
OrderBiomeHemDF$CellID <- NA
OrderBiomeHemDF$Order <- NA
OrderBiomeHemDF$Hem <- NA
OrderBiomeHemDF$Biome <- NA

o <- OrderNames[1]
b <- BiomeNames[1]
h <- HemisphereNames[1]
start <- 1
end <- 15038
OrderBiomeHemDF$Alpha[start:end] <- ORange(o,b,h)
OrderBiomeHemDF$CellID[start:end] <- c(1:15038)
OrderBiomeHemDF$Biome[start:end] <- b
OrderBiomeHemDF$Order[start:end] <- o
OrderBiomeHemDF$Hem[start:end] <- h

for(i in 2:NumberHems){
  start <- end + 1
  end <- start + 15037
  h <- HemisphereNames[i]
  OrderBiomeHemDF$Alpha[start:end] <- (ORange(o, b, h))
  OrderBiomeHemDF$CellID[start:end]<- c(1:15038)
  OrderBiomeHemDF$Biome[start:end] <- b
  OrderBiomeHemDF$Order[start:end] <- o
  OrderBiomeHemDF$Hem[start:end] <- h
}

for(j in 2:NumberBiomes){
  b <- BiomeNames[j]
  for(k in 1:NumberHems){
    start <- end + 1
    end <- start + 15037
    h <- HemisphereNames[k]
    OrderBiomeHemDF$Alpha[start:end] <- (ORange(o, b, h))
    OrderBiomeHemDF$CellID[start:end]<- c(1:15038)
    OrderBiomeHemDF$Biome[start:end] <- b
    OrderBiomeHemDF$Order[start:end] <- o
    OrderBiomeHemDF$Hem[start:end] <- h
  }
}

for(m in 2:NumberOrders){
  o <- OrderNames[m]
  for(n in 1:NumberBiomes){
    b <- BiomeNames[n]
    for(p in 1:NumberHems){
      start <- end + 1
      end <- start + 15037
      h <- HemisphereNames[p]
      OrderBiomeHemDF$Alpha[start:end] <- (ORange(o, b, h))
      OrderBiomeHemDF$CellID[start:end]<- c(1:15038)
      OrderBiomeHemDF$Biome[start:end] <- b
      OrderBiomeHemDF$Order[start:end] <- o
      OrderBiomeHemDF$Hem[start:end] <- h
    }
  }
  print(o)
}

saveRDS(OrderBiomeHemDF, file = "Data/OrderBiomeHemDF.rds")
