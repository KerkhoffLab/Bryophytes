# Scatterplot of Order Alpha Diversity by Latitude
# Hailey Napier
# September 2020


# 0.0 FIRST --------------------------------------------------------------
# 0.1 Load Packages
library(ggplot2)
library(dplyr)
library(wesanderson)
library(grid)
library(gridExtra)
library(RColorBrewer)

# 0.2 Load Data
#From DataProcessing2020.R
MossPresence <- readRDS("Data/MossPresence.rds")
OrderNames <- readRDS("Data/OrderNames.rds")
MossOrderNames <- readRDS("Data/MossOrderNames.rds")
#From OneScaleFamRichMaps.R
OrderRichList <- readRDS("Data/OrderRichList.rds")
#From OrderBiomePercentSpRich.R
MOBPerMatSpecies <- readRDS("Data/MOBPerMatSpecies.rds")

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
                                "Percent"  = rep(NA, 330836),
                                "CellID" = rep((1:15038), 22), 
                                "Latitude" = rep(Lat, 22))

for(i in 1:length(MossOrderNames)){
  order <- MossOrderNames[i]
  totalrich <- MOBMat[order, "Total"]
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
    percent <- (alpha/totalrich) * 100
    MossOrdLogAlphaDF$LogAlpha[index] <- log
    MossOrdLogAlphaDF$Alpha[index] <- alpha
    MossOrdLogAlphaDF$LogTen[index] <- logten
    MossOrdLogAlphaDF$Percent[index] <- percent
  }
}

MossOrdLogAlphaDF$Percet <- NULL

# Add percentage
MOLA_Percent <- MossOrdLogAlphaDF
for(i in 1:nrow(MOLA_Percent)){
  order <- MOLA_Percent$Order[i]
  totalrich <- MOBPerMatSpecies[order, "Total"]
  alpha <- MOLA_Percent$Alpha[i]
  percent <- (alpha/totalrich) * 100
  MOLA_Percent$Percent[i] <- percent
}


# 1.4 Omit NA Values
MOLADF_NoNA <- MossOrdLogAlphaDF
MOLADF_NoNA <- MOLADF_NoNA[complete.cases(MOLADF_NoNA[ , 2]),]


# 2.0 Plot -----------------------------------------------------------------
# 2.0.1 Make color palette
mostdivcols <- c("#2c8200","#edd13f")
moredivcols <- c("#004fab",
                  "#2d9e66",
                  "#3a3177",
                  "#5398ff",
                  "#003d81",
                  "#b09eec")
lessdivcols <- c("#6c006d","#c894ff","#00246a","#d85f8a")
leastdivcols <- c("#732832","#da7533","#d73f65","#a17235",
                  "#dca33a","#dbab83","#724529","#d37f80",
                  "#f84a11","#c14234")



pal1 <- wes_palette("Darjeeling1")
pal2 <- wes_palette("Chevalier1")
mossorderpal <- c(pal1, pal2, wes_palette("IsleofDogs1")[1],
                  wes_palette("Cavalcanti1")[5])

# 2.1 Log Plots
  ##Can't get smooth to work on log plots, 
  ##and log leaves bands that make the patterns hard to see
# 2.1.1 Natural Log
MossOrderLogRichScatter <- ggplot(MossOrdLogAlphaDF, aes(Latitude, LogAlpha, color=Order), show.legend=TRUE) +
  #geom_point(shape=16, size=2.5, alpha=0.25) +
  xlab("Latitude") +
  ylab("Natural Log Order Alpha Diversity") +
  theme_minimal() +
  geom_smooth(na.rm = T) +
  theme(axis.title.y = element_text(size=32),
        axis.title.x = element_text(size=32),
        axis.text = element_text(size=20))
MossOrderLogRichScatter

# 2.1.2 Natural Log plot colored by richness level
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

# 2.1.3 Natural Log with No_NA dataframe
MossOrderLogRichNoNAScatter <- ggplot(MOLADF_NoNA, aes(Latitude, LogAlpha, color=Order), show.legend=TRUE) +
  #geom_point(shape=16, size=2.5, alpha=0.25) +
  xlab("Latitude") +
  ylab("Natural Log Order Alpha Diversity") +
  theme_minimal() +
  geom_smooth() +
  theme(axis.title.y = element_text(size=32),
        axis.title.x = element_text(size=32),
        axis.text = element_text(size=20))
MossOrderLogRichNoNAScatter

# 2.1.4 Natural Log base 10
MossOrderLog10RichScatter <- ggplot(MossOrdLogAlphaDF, aes(Latitude, LogTen, color=Order), show.legend=TRUE) +
  geom_point(shape=16, size=2.5, alpha=0.5) +
  xlab("Latitude") +
  ylab("Log Base 10 Order Alpha Diversity") +
  theme_minimal() +
  theme(axis.title.y = element_text(size=32),
        axis.title.x = element_text(size=32),
        axis.text = element_text(size=20))
MossOrderLog10RichScatter

# 2.2 Alpha Diversity Plots
  ## Raw alpha is really busy, and it's tough to see the trends for the less diverse orders
# 2.2.1 Raw alpha diversity
MossOrderRichScatter <- ggplot(MossOrdLogAlphaDF, aes(Latitude, Alpha, color=Order), show.legend=TRUE) +
  geom_point(shape=16, size=2.5, alpha=0.5) +
  xlab("Latitude") +
  ylab("Order Alpha Diversity") +
  theme_minimal() +
  scale_color_manual(values=c("Andreaeaeales" = mossorderpal[1],
                              "Archidiales" = mossorderpal[2],
                              "Aulacomniales" = mossorderpal[3],
                              "Bryoxiphales" = mossorderpal[4],
                              "Buxbaumiales" = mossorderpal[5],
                              "Gigaspermales" = mossorderpal[6],
                              "Hypnodendrales" = mossorderpal[7],
                              "Ptychomniales" = mossorderpal[8],
                              "Rhizogoniales" = mossorderpal[9],
                              "Splachnales" = mossorderpal[10],
                              "Hypnales" = "darkslategray",
                              "Dicranales" = "green4",
                              "Funariales" = lessdivcols[1],
                              "Hedwigiales" = lessdivcols[2],
                              "Polytrichales" = lessdivcols[3],
                              "Sphagnales" = lessdivcols[4],
                              "Bartramiales" = moredivcols[1],
                              "Bryales" = "springgreen4",
                              "Grimmiales" = moredivcols[3],
                              "Hookeriales" = moredivcols[4],
                              "Orthotrichales" = moredivcols[5],
                              "Pottiales" = moredivcols[6])) +
  theme(axis.title.y = element_text(size=25),
        axis.title.x = element_text(size=25),
        axis.text = element_text(size=20), 
        legend.title = element_text(size=17),
        legend.text = element_text(size=13))
MossOrderRichScatter

# 2.2.2 Panel raw alpha diversity plots by richness level, one plot w/one y-axis scale
MossOrderRichFacet <- ggplot(MossOrdLogAlphaDF, 
                               aes(Latitude, Alpha, color=Order), 
                               show.legend=TRUE) +
  geom_point(shape=16, size=2.5, alpha=0.8) +
  xlab("Latitude") +
  ylab("Order Alpha Diversity") +
  theme_minimal() +
  theme(axis.title.y = element_text(size=32),
        axis.title.x = element_text(size=32),
        axis.text = element_text(size=20)) +
  facet_wrap(~Group)
MossOrderRichFacet

# 2.2.3 Plot each order richness group in its own plot with a new y-axis scale
#Quick data manipulation
MostDiverseMoss <- MossOrdLogAlphaDF %>%
  filter(Group == "Most diverse (greater than 100 species)")

MoreDiverseMoss <- MossOrdLogAlphaDF %>%
  filter(Group == "More diverse (26 - 100 species)")

LessDiverseMoss <- MossOrdLogAlphaDF %>%
  filter(Group == "Less diverse (11 - 25 species)")

LeastDiverseMoss <- MossOrdLogAlphaDF %>%
  filter(Group == "Least diverse (10 or fewer species)")

# Most diverse orders
MossOrderMostRich <- ggplot(MostDiverseMoss, 
                             aes(Latitude, Alpha, color=Order), 
                             show.legend=TRUE) +
  geom_point(shape=16, size=1.5, alpha=0.6) +
  xlab("Latitude") +
  ylab("Alpha Diversity") +
  theme_minimal() +
  scale_color_manual(values=c("Hypnales" = mossorderpal[7],
                              "Dicranales" = mossorderpal[10])) + 
  theme(axis.title.y = element_text(size=16),
        axis.title.x = element_text(size=16),
        axis.text = element_text(size=15),
        plot.title = element_text(size=17, hjust=0.5),
        legend.title = element_text(size=13),
        legend.text = element_text(size = 10)) + 
  labs(title = "Most Diverse (greater than 100 species)")
MossOrderMostRich

# More diverse orders
MossOrderMoreRich <- ggplot(MoreDiverseMoss, 
                            aes(Latitude, Alpha, color=Order), 
                            show.legend=TRUE) +
  geom_point(shape=16, size=1.5, alpha=0.6) +
  xlab("Latitude") +
  ylab("Alpha Diversity") +
  theme_minimal() +
  scale_color_manual(values=c("Bartramiales" = mossorderpal[6],
                              "Bryales" = mossorderpal[7],
                              "Grimmiales" = mossorderpal[8],
                              "Hookeriales" = mossorderpal[9],
                              "Orthotrichales" = mossorderpal[4],
                              "Pottiales" = mossorderpal[2])) +
  theme(axis.title.y = element_text(size=16),
        axis.title.x = element_text(size=16),
        axis.text = element_text(size=15),
        plot.title = element_text(size=17, hjust=0.5),
        legend.title = element_text(size=13),
        legend.text = element_text(size = 10)) +
  labs(title = "More Diverse (26 - 100 species)")
MossOrderMoreRich

#Less diverse orders
MossOrderLessRich <- ggplot(LessDiverseMoss, 
                            aes(Latitude, Alpha, color=Order), 
                            show.legend=TRUE) +
  geom_point(shape=16, size=1.5, alpha=0.6) +
  xlab("Latitude") +
  ylab("Alpha Diversity") +
  #geom_jitter(height = 0.3) +
  theme_minimal() +
  scale_color_manual(values=c("Funariales" = mossorderpal[1],
                              "Hedwigiales" = mossorderpal[2],
                              "Polytrichales" = mossorderpal[3],
                              "Sphagnales" = mossorderpal[11])) +
  theme(axis.title.y = element_text(size=16),
        axis.title.x = element_text(size=16),
        axis.text = element_text(size=15),
        plot.title = element_text(size=17, hjust=0.5),
        legend.title = element_text(size=13),
        legend.text = element_text(size = 10)) +
  labs(title = "Less Diverse (11-25 species)")
MossOrderLessRich

#Least diverse orders
MossOrderLeastRich <- ggplot(LeastDiverseMoss, 
                                aes(Latitude, Alpha, color=Order), 
                                show.legend=TRUE) +
  geom_point(shape=16, size=1.5, alpha=0.6) +
  xlab("Latitude") +
  ylab("Alpha Diversity") +
  #geom_jitter(0.3) +
  theme_minimal() +
  scale_color_manual(values=c("Andreaeaeales" = mossorderpal[1],
                              "Archidiales" = mossorderpal[2],
                              "Aulacomniales" = mossorderpal[3],
                              "Bryoxiphales" = mossorderpal[4],
                              "Buxbaumiales" = mossorderpal[5],
                              "Gigaspermales" = mossorderpal[6],
                              "Hypnodendrales" = mossorderpal[7],
                              "Ptychomniales" = mossorderpal[8],
                              "Rhizogoniales" = mossorderpal[9],
                              "Splachnales" = mossorderpal[10])) +
  theme(axis.title.y = element_text(size=16),
        axis.title.x = element_text(size=16),
        axis.text = element_text(size=15),
        plot.title = element_text(size=17, hjust=0.5),
        legend.title = element_text(size=13),
        legend.text = element_text(size = 10)) +
  labs(title = "Least Diverse (10 or fewer species)")
MossOrderLeastRich

grid.arrange(MossOrderMostRich, MossOrderMoreRich, MossOrderLessRich, MossOrderLeastRich, nrow = 2)


# 2.3 Percent Total Species Richness
# 2.3.1 Percent total species richness (in each cell)
MossOrderPercentRichScatter <- ggplot(MOLA_Percent, aes(Latitude, Percent, color=Order), show.legend=TRUE) +
  geom_point(shape=16, size=2.5, alpha=0.5) +
  xlab("Latitude") +
  ylab("Order Alpha Diversity") +
  theme_minimal() +
  theme(axis.title.y = element_text(size=32),
        axis.title.x = element_text(size=32),
        axis.text = element_text(size=20))
MossOrderPercentRichScatter

