# Scatterplot of Order Alpha Diversity by Latitude
# Hailey Napier
# September 2020


# 0.0 Loading --------------------------------------------------------------
# 0.1 Load Packages
library(ggplot2)
library(dplyr)
library(wesanderson)

# 0.2 Load Data
#From DataProcessing2020.R
MossPresence <- readRDS("Data/MossPresence.rds")
OrderNames <- readRDS("Data/OrderNames.rds")
MossOrderNames <- readRDS("Data/MossOrderNames.rds")
#From OneScaleFamRichMaps.R
OrderRichList <- readRDS("Data/OrderRichList.rds")

LongLatDF <- readRDS("Data/LongLatDF.rds")

# 1.0 Data Manipulation ----------------------------------------------------
# 1.1 Select mosses from OrderRichList
mossindex <- match(MossOrderNames, OrderNames)
MossOrderRichList <-list()
for(i in 1:length(MossOrderNames)){
  index <- mossindex[i]
  list <- OrderRichList[index]
  MossOrderRichList[i] <- list
}

# 1.3 Make dataframe for plotting
Lat <- as.vector(LongLatDF$Latitude)
MossOrdLogAlphaDF <- data.frame("Order" = rep(NA, 330836), 
                                "Alpha" = rep(NA, 330836),
                                "LogAlpha" = rep(NA, 330836),
                                "LogTen" = rep(NA, 330836),
                                "CellID" = rep((1:15038), 22), 
                                "Latitude" = rep(Lat, 22))

for(i in 1:length(MossOrderNames)){
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
  list <- MossOrderRichList[[i]]
  start <- (i-1) * 15038 + 1
  end <- start + 15037
  MossOrdLogAlphaDF$Order[start:end] <- order
  MossOrdLogAlphaDF$Group[start:end] <- group
  for(j in 1:15038){
    index <- (i-1)*15038+j
    alpha <- as.numeric(list[j])
    log <- log(alpha)
    logten <- log10(alpha)
    MossOrdLogAlphaDF$LogAlpha[index] <- log
    MossOrdLogAlphaDF$Alpha[index] <- alpha
    MossOrdLogAlphaDF$LogTen[index] <- logten
  }
}

# 2.0 Plot -----------------------------------------------------------------
#Log
MossOrderLogRichScatter <- ggplot(MossOrdLogAlphaDF, aes(Latitude, LogAlpha, color=Order), show.legend=TRUE) +
  geom_point(shape=16, size=2.5, alpha=0.5) +
  xlab("Latitude") +
  ylab("Natural Log Order Alpha Diversity") +
  theme_minimal() +
  theme(axis.title.y = element_text(size=32),
        axis.title.x = element_text(size=32),
        axis.text = element_text(size=20))
MossOrderLogRichScatter

#Log base 10
MossOrderLog10RichScatter <- ggplot(MossOrdLogAlphaDF, aes(Latitude, LogTen, color=Order), show.legend=TRUE) +
  geom_point(shape=16, size=2.5, alpha=0.5) +
  xlab("Latitude") +
  ylab("Log Base 10 Order Alpha Diversity") +
  theme_minimal() +
  theme(axis.title.y = element_text(size=32),
        axis.title.x = element_text(size=32),
        axis.text = element_text(size=20))
MossOrderLog10RichScatter

#Raw alpha diversity
MossOrderRichScatter <- ggplot(MossOrdLogAlphaDF, aes(Latitude, Alpha, color=Order), show.legend=TRUE) +
  geom_point(shape=16, size=2.5, alpha=0.5) +
  xlab("Latitude") +
  ylab("Order Alpha Diversity") +
  theme_minimal() +
  theme(axis.title.y = element_text(size=32),
        axis.title.x = element_text(size=32),
        axis.text = element_text(size=20))
MossOrderRichScatter

#Log plot colored by richness level
MossOrderLogRichLevelScatter <- ggplot(MossOrdLogAlphaDF, aes(Latitude, LogAlpha, color=Group), show.legend=TRUE) +
  geom_point(shape=16, size=2.5, alpha=0.5) +
  scale_color_manual(values = wes_palette(n = 4, name = "Darjeeling1")) +
  xlab("Latitude") +
  ylab("Natural Log Order Alpha Diversity") +
  theme_minimal() +
  theme(axis.title.y = element_text(size=32),
        axis.title.x = element_text(size=32),
        axis.text = element_text(size=20))
MossOrderLogRichLevelScatter


