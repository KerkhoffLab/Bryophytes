# Biome Beta Diversity
# Hailey Napier,  July 2020
# Adapted from FamilyDiversity.R

# Load packages
library(reshape2)

# Load data
LongLatBetaDF <- readRDS("Data/LongLatBetaDF.rds")
LongLatDF <- readRDS("Data/LongLatDF.rds")
BiomeCellsDF <- readRDS("Data/BiomeCells.rds")
LongLatBetaCellDF <- full_join(LongLatDF, LongLatBetaDF, by = "Longitude")


# WRITE DATAFRAME
# Convert biome column from factor to character
BiomeCellsDF$Biome <- as.character(BiomeCellsDF$Biome)

#Loop through cells and assign biome and beta diversity
BiomeBetaDF <- data.frame("CellID" = 1:15038)
BiomeBetaDF$Beta <- NA
BiomeBetaDF$Biome <- NA

for(i in 1:15038){
  cell <- i
  biome <- BiomeCells$Type[which(BiomeCells$CellID == cell)]
  if(length(biome) == 0){
    biome = NA
  }
  beta <- LongLatBetaCellDF$Beta[which(LongLatBetaCellDF$CellID == cell)]
  BiomeBetaDF$Biome[i] <- biome
  BiomeBetaDF$Beta[i] <- beta
}

# SUBSET NAS OUT OF DATAFRAME FOR PLOTTING
NoNaBBDF <- BiomeBetaDF[which(!is.na(BiomeBetaDF$Biome)),]

# BOXPLOT 
# This plot includes includes cells multiple times if they're in more than one biome
BiomeBetaBV <- ggplot(NoNaBBDF, aes(x=Biome, y=Beta, fill=Biome, color=Biome)) + 
  geom_boxplot(show.legend = FALSE, fill=biome_cols_11, color="black") +
  guides(x = guide_axis(angle=30)) +
  theme_minimal() +        #un-comment whichever theme you want
  #theme_gray() +
  #theme_light() +
  #theme_bw() +
  geom_violin(scale="count", show.legend=FALSE, fill="gray", alpha=0.35,
              color="gray25") +
  xlab("Biome") +
  ylab("Beta Diversity") +  
  theme(axis.title.y = element_text(size=32), 
        axis.title.x = element_text(size=32),
        axis.text.y = element_text(size=20), 
        axis.text.x = element_text(angle = 30, hjust = 1, size = 12))
BiomeBetaBV
