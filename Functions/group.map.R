#Map by group

group.map <- function(x, ...){
  k <- subset(BryophytePresence, BryophytePresence$Group== x)
  b <- tally(group_by(k, CellID))
  colnames(b)[2] <- "Richness"
  BIENblank <- raster("Data/blank_100km_raster.tif")
  f <- numeric(15038)
  f[b$CellID] <- b$Richness
  c <- setValues(BIENblank, f)
  plot(c)
  g<-f
  g[which(g==0)]=NA
  h <- setValues(BIENblank, g)
  plot(h, axes = F)
}