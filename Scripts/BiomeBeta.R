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
  if(length(biome) == 1){
    biome = biome
  }else if(length(biome) == 2){
    x <- biome[1]
    y <- biome[2]
    xweight <- BiomeCells$Weight[which(BiomeCells$Type == x)]
    yweight <- BiomeCells$Weight[which(BiomeCells$Type == y)]
    if(xweight > yweight){
      biome = x
    }else{
      biome = y  
    }
  }else if(length(biome) == 3){
    x <- biome[1]
    y <- biome[2]
    z <- biome[3]
    xweight <- BiomeCells$Weight[which(BiomeCells$Type == x)]
    yweight <- BiomeCells$Weight[which(BiomeCells$Type == y)]
    zweight <- BiomeCells$Weight[which(BiomeCells$Type == z)]
    max <- max(xweight, yweight, zweight)
    if(max == xweight){
      biome = x
    }else if(max == yweight){
      biome = y  
    }else if(max = zweight){
      biome = z
    }
 }
  beta <- LongLatBetaCellDF$Beta[which(LongLatBetaCellDF$CellID == cell)]
  BiomeBetaDF$Biome[i] <- biome
  BiomeBetaDF$Beta[i] <- beta
}


length(biome)
biome

beta
cell
biome

cell  <-125
biome <- BiomeCells$Type[which(BiomeCells$CellID == cell)]
if(length(biome) == 1){
  biome = biome
}else if(length(biome) == 2){
  x <- biome[1]
  y <- biome[2]
  xweight <- BiomeCells$Weight[which(BiomeCells$Type == x)]
  yweight <- BiomeCells$Weight[which(BiomeCells$Type == y)]
  if(xweight > yweight){
    biome = x
  }else{
    biome = y  
  }
}else if(lenght(biome) == 3){
  x <- biome[1]
  y <- biome[2]
  z <- biome[3]
  xweight <- BiomeCells$Weight[which(BiomeCells$Type == x)]
  yweight <- BiomeCells$Weight[which(BiomeCells$Type == y)]
  zweight <- BiomeCells$Weight[which(BiomeCells$Type == z)]
  max <- max(xweight, yweight, zweight)
  if(max == xweight){
    biome = x
  }else if(max == yweight){
    biome = y  
  }else if(max = zweight){
    biome = z
  }
}


xweight <- 1
yweight <- 2
zweight <- 5

biome

beta <- LongLatBetaCellDF$Beta[which(LongLatBetaCellDF$CellID == cell)]
beta

cell <- 125
biome <- BiomeCells$Type[which(BiomeCells$CellID == cell)]


# SUBSET NAS OUT OF DATAFRAME FOR PLOTTING
NoNaBBDF <- BiomeBetaDF[which(!is.na(BiomeBetaDF$Biome)),]

# BOXPLOT 
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
