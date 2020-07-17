#Function to make boxplot of alpha diversity for a specified order in a specified biome/mountain range
#Input: str, name of order (ex. "Hypnales")
#Input: str, type of plot: "box" or "violin"
#Ouput: set of boxplots (one for each order)
#Hailey Napier
#July 16, 2020

OrdBiomeBP <- function(order, type,...){
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
  
  if(type == "violin"){
    plot <- ggplot(df, aes(x = Biome, y = Alpha)) + 
      geom_violin(fill = "cyan4", alpha = 0.8, color = "cyan4", scale = "count") + 
      theme_minimal() + 
      ggtitle(order) + 
      ylab("α diversity") + 
      xlab(" ") + 
      theme(axis.title.y = element_text(size=24), axis.text.y = element_text(size=20), axis.text.x = element_text(angle = 45, hjust = 1, size = 16), plot.title = element_text(size = 28, hjust = 0.5))
  }else if(type == "box"){
    plot <- ggplot(df, aes(x = Biome, y = Alpha)) + 
      geom_boxplot() + 
      ggtitle(order) +
      theme_minimal() + 
      geom_jitter(alpha = 0.5, width = 0.2, color = "cyan4") +
      ylab("α diversity") + 
      xlab(" ") + 
      theme(axis.title.y = element_text(size=24), axis.text.y = element_text(size=20), axis.text.x = element_text(angle = 45, hjust = 1, size = 20), plot.title = element_text(size = 28, hjust = 0.5))
  }
  
   plot
}
