#Jackie O'Malley and Julia Eckberg
#Summer 2019
###Separate tropical and temperate beta diversity values###
#Load in data for all zones 
LongLatBetaDF <- readRDS("Data/LongLatBetaDF.rds")

#Identify and plot cells in tropical zones, which are between 23.5 and -23.5 degrees latitude, save as a png 
LongLatBetaDF <- subset(LongLatBetaDF, select = -c(optional))
TropicalBeta <- LongLatBetaDF[(LongLatBetaDF$Latitude >= -23.5 & LongLatBetaDF$Latitude <= 23.5),]

TropicalScatterplot <- ggplot() + geom_point(data = TropicalBeta, aes(Latitude, Beta), shape = 16, size = 5, show.legend = FALSE, alpha=0.5, color = "cyan4") + 
  ylab("Tropical β diversity") + ylim(0, 0.6) + xlab("Latitude") + theme_minimal() + 
  theme(axis.title.y = element_text(size=32), axis.title.x = element_text(size=32),  axis.text = element_text(size=20))
TropicalScatterplot

png(file = "Documents/Bryophytes/Figures/tropicalscatterplot.png", width = 1000, height = 1000, pointsize = 30)
TropicalScatterplot
dev.off()

#Identify and make a scatterplot cells in temperate zones, so any cells outside of tropical range (-23.5 - 23.5), save as a png
TropicalLatitudes <- TropicalBeta$Latitude 

TropicalMatch <- LongLatBetaDF[LongLatBetaDF$Latitude %in% TropicalLatitudes, ]
TropicalMatchVec <- TropicalMatch[, "Latitude"]
TemperateBeta <- LongLatBetaDF[!LongLatBetaDF$Latitude %in% TropicalMatchVec, ]

TemperateScatterplot <- ggplot() + geom_point(data = TemperateBeta, aes(Latitude, Beta), shape = 16, size = 5, show.legend = FALSE, alpha=0.5, color = "cyan4") + 
  ylab("Temperate β diversity") + ylim(0, 0.6) + xlab("Latitude") + theme_minimal() + 
  theme(axis.title.y = element_text(size=32), axis.title.x = element_text(size=32),  axis.text = element_text(size=20))
TemperateScatterplot

png(file = Figures/"temperatescatterplot.png", width = 1000, height = 1000, pointsize = 30)
TemperateScatterplot
dev.off()

#Plot temperate and tropical boxplots together and save as a png, first convert longitude values to temperate and tropical labels and combine dataframes 
TropicalBeta$Longitude[TropicalBeta$Longitude <- "Tropical"]
TemperateBeta$Longitude[TemperateBeta$Longitude <- "Temperate"]

CombinedDF <- rbind(TropicalBeta, TemperateBeta)
CombinedBoxplot <- ggplot(CombinedDF, aes(x = Longitude, y = Beta)) + geom_boxplot() + theme_minimal() + ylab("All Areas β diversity") + ylim(0, 0.5) + xlab(" ") + 
  theme(axis.title.y = element_text(size=32), axis.text.y = element_text(size=20), axis.text.x = element_text(size=32))
CombinedBoxplot

png("Figures/tropicaltemperatebox.png", width = 1000, height = 1000, pointsize = 20)
CombinedBoxplot
dev.off()




#Looking at tropical v temperate values ONLY in mountainous regions 
#Load in the data subsetted to include only cells in mountainous regions 
BetaMount <- readRDS("Data/BetaMount.rds")

#First, subsetting tropical mountain beta diversity values and make scatterplot
TropicalMountBeta <- BetaMount[(BetaMount$Latitude >= -23.5 & BetaMount$Latitude <= 23.5),]

TropicalMountScatterplot <- ggplot() + geom_point(data = TropicalMountBeta, aes(Latitude, Beta), shape = 16, size = 5, show.legend = FALSE, alpha=0.5, color = "cyan4") + 
  ylab("Tropical Mountainous β diversity") + ylim(0, 0.6) + xlab("Latitude") + theme_minimal() + 
  theme(axis.title.y = element_text(size=32), axis.title.x = element_text(size=32),  axis.text = element_text(size=20))
TropicalMountScatterplot

#Then, subset and plot the temperate mountain beta diversity values and make a scatterplot
TropicalMountLatitudes <- TropicalMountBeta$Latitude 

TropicalMountMatch <- BetaMount[BetaMount$Latitude %in% TropicalMountLatitudes, ]
TropicalMountMatchVec <- TropicalMountMatch[, "Latitude"]
TemperateMountBeta <- BetaMount[!BetaMount$Latitude %in% TropicalMountMatchVec, ]

TemperateMountScatterplot <- ggplot() + geom_point(data = TemperateMountBeta, aes(Latitude, Beta), shape = 16, size = 5, show.legend = FALSE, alpha=0.5, color = "cyan4") + 
  ylab("Temperate Mountain β diversity") + ylim(0, 0.5) + xlab("Latitude") + theme_minimal() + 
  theme(axis.title.y = element_text(size=32), axis.title.x = element_text(size=32),  axis.text = element_text(size=20))
TemperateMountScatterplot

#Plot the mountainous temperate v tropical values together as a boxplot, first have to combine the dataframes and change longitude to tropical and temperate labels 
TropicalMountBeta$Longitude[TropicalMountBeta$Longitude <- "Tropical"]
TemperateMountBeta$Longitude[TemperateMountBeta$Longitude <- "Temperate"]

CombinedMountDF <- rbind(TropicalMountBeta, TemperateMountBeta)
CombinedMountBoxplot <- ggplot(CombinedMountDF, aes(x = Longitude, y = Beta)) + geom_boxplot() + theme_minimal() + ylab("Mountainous β diversity") + ylim(0, 0.5) + xlab(" ") + 
  theme(axis.title.y = element_text(size=32), axis.text.y = element_text(size=20), axis.text.x = element_text(size=32))
CombinedMountBoxplot

png(file = "Figures/combinedmountboxplot.png", width = 1000, height = 1000, pointsize = 30)
CombinedMountBoxplot
dev.off()

#Map box plot of tropical v temperate in all areas next to box plot of tropical v temperate of just the tropical zones 
png("Figures/combinedtropicaltemperatemaps.png", width = 1000, height = 1000, pointsize = 20)
grid.arrange(CombinedBoxplot, CombinedMountBoxplot, ncol=2)
dev.off()





#Make it so we're comparing mountainous and lowland tropical/temperate values, rather than comparing all areas to the mountainous beta values 
#Load in BetaLowland, which has the beta diversity, latitude, and cellIDs of lowland cells 
BetaLowland <- readRDS("Data/BetaLowland.rds")
#Now separate tropical values in lowland areas and plot those values, save as a png 
TropicalLowlandBeta <- BetaLowland[(BetaLowland$Latitude >= -23.5 & BetaLowland$Latitude <= 23.5),]

TropicalLowlandScatterplot <- ggplot() + geom_point(data = TropicalLowlandBeta, aes(Latitude, Beta), shape = 16, size = 5, show.legend = FALSE, alpha=0.5, color = "cyan4") + 
  ylab("Tropical Lowland β diversity") + ylim(0, 0.6) + xlab("Latitude") + theme_minimal() + 
  theme(axis.title.y = element_text(size=32), axis.title.x = element_text(size=32),  axis.text = element_text(size=20))
TropicalLowlandScatterplot

png(file = "Figures/tropicallowlandscatterplot.png", width = 1000, height = 1000, pointsize = 30)
TropicalLowlandScatterplot
dev.off()

#And do the same for temperate values in lowland areas, make a scatterplot
TropicalLowlandLatitudes <- TropicalLowlandBeta$Latitude 

TropicalLowlandMatch <- BetaLowland[BetaLowland$Latitude %in% TropicalLowlandLatitudes, ]
TropicalLowlandMatchVec <- TropicalLowlandMatch[, "Latitude"]
TemperateLowlandBeta <- BetaLowland[!BetaLowland$Latitude %in% TropicalLowlandMatchVec, ]

TemperateLowlandScatterplot <- ggplot() + geom_point(data = TemperateLowlandBeta, aes(Latitude, Beta), shape = 16, size = 5, show.legend = FALSE, alpha=0.5, color = "cyan4") + 
  ylab("Temperate Lowland β diversity") + ylim(0, 0.6) + xlab("Latitude") + theme_minimal() + 
  theme(axis.title.y = element_text(size=32), axis.title.x = element_text(size=32),  axis.text = element_text(size=20))
TemperateLowlandScatterplot

#Boxplots comparing temperate and tropical lowland values, save as a png
TropicalLowlandBeta$Longitude[TropicalLowlandBeta$Longitude <- "Tropical"]
TemperateLowlandBeta$Longitude[TemperateLowlandBeta$Longitude <- "Temperate"]

CombinedLowlandDF <- rbind(TropicalLowlandBeta, TemperateLowlandBeta)
CombinedLowlandBoxplot <- ggplot(CombinedLowlandDF, aes(x = Longitude, y = Beta)) + geom_boxplot() + theme_minimal() + ylab("Lowland β diversity") + ylim(0, 0.5) + xlab(" ") + 
  theme(axis.title.y = element_text(size=32), axis.text.y = element_text(size=20), axis.text.x = element_text(size=32)) + 
  scale_fill_manual(values = c("cyan4", "goldenrod2"))
CombinedLowlandBoxplot

png(file = "Figures/combinedlowlandboxplot.png", width = 1000, height = 1000, pointsize = 30)
CombinedLowlandBoxplot
dev.off()

#Boxplots showing temperate/tropical values comparing mountainous to lowland values 
png("Figures/combinedlowlandmountmaps.png", width = 1000, height = 1000, pointsize = 20)
grid.arrange(CombinedLowlandBoxplot, CombinedMountBoxplot, ncol=2)
dev.off()
