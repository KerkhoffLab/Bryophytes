#Function to make boxplot of alpha diversity for a specified order in a specified biome/mountain range
#Input: str, name of order (ex. "Hypnales")
#Input: str, type of plot: "box" or "violin"
#Ouput: set of boxplots for alpha diversity in each biome for the specified order -- in either type violin or box
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
  
  #plot
  #violin
  if(type == "violin"){
    plot <- ggplot(df, aes(x = Biome, y = Alpha, fill = Biome)) + 
      geom_violin(scale = "count", show.legend = FALSE, fill = cols1) + 
      theme_minimal() + 
      ggtitle(order) + 
      ylab("Richness") + 
      xlab("Biome") + 
      theme(axis.title.y = element_text(size=32), 
            axis.title.x = element_text(size=32),
            axis.text.y = element_text(size=20), 
            axis.text.x = element_text(angle = 30, hjust = 1, size = 12), 
            plot.title = element_text(size = 28, hjust = 0.5))
  #box
  }else if(type == "box"){
    plot <- ggplot(df, aes(x = Biome, y = Alpha, fill = Biome)) + 
      geom_boxplot(show.legend=FALSE, fill=cols7) + 
      ggtitle(order) +
      theme_minimal() + 
      #geom_jitter(alpha = 0.5, width = 0.2, color = "cyan4") +
      #geom_violin(scale="count", show.legend=FALSE, fill="gray", alpha=0.5) +
      ylab("Richness") + 
      xlab("Biome") + 
      theme(axis.title.y = element_text(size=32), 
            axis.title.x = element_text(size=32),
            axis.text.y = element_text(size=20), 
            axis.text.x = element_text(angle = 30, hjust = 1, size = 12),
            plot.title = element_text(size = 28, hjust = 0.5))
  #box + transparent violin layered
  }else if(type == "boxyviolin"){
    plot <- ggplot(df, aes(x = Biome, y = Alpha, fill = Biome, color = Biome)) + 
      geom_boxplot(show.legend = FALSE, fill=cols7, color = "black") +
      ggtitle(order) +
      theme_minimal() +
      geom_violin(scale="count", show.legend=FALSE, fill="gray", alpha=0.35,
                  color = "gray25") +
      xlab("Biome") +
      ylab("Richness") +  
      theme(axis.title.y = element_text(size=32), 
            axis.title.x = element_text(size=32),
            axis.text.y = element_text(size=20), 
            axis.text.x = element_text(angle = 30, hjust = 1, size = 12),
            plot.title = element_text(size = 28, hjust = 0.5))
  }
  
   plot
}
