#Function to make boxplot of alpha diversity for a specified order in a specified biome/mountain range
#Input: str, name of order (ex. "Hypnales")
#Ouput: set of boxplots (one for each order)
#Hailey Napier
#July 16, 2020

OrdBiomeBP <- function(order,...){
  #load data
  BiomeNames <- readRDS("Data/BiomeNames.rds")
  NumberBiomes <- length(BiomeNames)
  
  #source functions
  source("Functions/ORange.R")
  
  #set up dataframe to add to 
  df <- data.frame(ORange(order, BiomeNames[1]))
  names(df)[1] <- "Alpha"
  df$CellID <- c(1:15038)
  df$Biome <- BiomeNames[1]
  
  #loop through biomes and add to the dataframe
  for(i in 2:NumberBiomes){
    temp <- data.frame(ORange(order, BiomeNames[i]))
    names(temp)[1] <- "Alpha"
    temp$CellID <- c(1:15038)
    temp$Biome <- BiomeNames[i] 
    df <- bind_rows(df, temp)
  }
  
  plot <- ggplot(df, aes(x = Biome, y = Alpha)) + 
    geom_violin(fill = "cyan4", alpha = 0.8, color = "cyan4") + theme_minimal() + 
    ylab("Alpha") + 
    xlab(" ") + 
    theme(axis.title.y = element_text(size=32), axis.text.y = element_text(size=20), axis.text.x = element_text(size=32))
 
   plot
}
