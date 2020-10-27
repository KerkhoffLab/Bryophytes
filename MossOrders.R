# MossOrders
# All of the figures and analysis for the moss orders presentation
# Hailey Napier
# October 2020

# 0.2 Load packages -------
library(dplyr)
library(ggplot2)

# 0.1 Load data -------
LongLatDF <- readRDS("Data/LongLatDF.rds")
MossRichness <- readRDS("Data/MossRichness.rds")

# 1.0 Moss alpha diversity scatterplot -------

# 1.1 Make dataframe with richness and long lat data for each cell
MossCellID <- MossRichness$CellID

MossLongLatDF <- LongLatDF %>%
  filter(LongLatDF$CellID %in% MossCellID)

MossAlphaLongLatDF <- full_join(MossLongLatDF, MossRichness, by = "CellID")


# 1.2 Make plot
MossAlphaScatterplot <- ggplot(MossAlphaLongLatDF, aes(Latitude, n)) + 
  geom_point(shape = 16, 
             size = 5, , 
             alpha=0.5, 
             color = "gray40") + 
  ylab("Species Richness") + 
  xlab("Latitude") + 
  theme_minimal() + 
  theme(axis.title.y = element_text(size=32), 
        axis.title.x = element_text(size=32),
        axis.text.y = element_text(size=20), 
        axis.text.x = element_text(size=20))
MossAlphaScatterplot

png("Figures/MossAlphaScatter.png", width = 1500, height = 1000, pointsize = 20)
dev.off()
  

