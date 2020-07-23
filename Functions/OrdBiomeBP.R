#Function to make boxplot of alpha diversity for a specified order in a specified biome/mountain range
#Input: order = str, name of order (ex. "Hypnales")
#Input: type = str, type of plot: "box", "violin", "boxyviolin"
#Input: hem = str, hemisphere: "both", "Northern", "Southern"; default = "both"
#Ouput: set of boxplots for alpha diversity in each biome for the specified order -- in either type violin or box
#Hailey Napier and Kathryn Dawdy
#July 16, 2020

OrdBiomeBP <- function(order, type, hem = "both"){
  
  #load data
  BiomeNames <- readRDS("Data/BiomeNames.rds")
  OrdRichAbove100 <- readRDS("Data/OrdRichAbove100.rds")
  OrdRich25to100 <- readRDS("Data/OrdRich25to100.rds")
  OrdRich10to25 <- readRDS("Data/OrdRich10to25.rds")
  OrdRichBelow10 <- readRDS("Data/OrdRichBelow10.rds")
  NumberBiomes <- length(BiomeNames)
  cols7 <- c("#D8B70A", "#972D15", "#A2A475", "#81A88D", "#02401B",
             "#446455", "#FDD262", "#D3DDDC", "#C7B19C",
             "#798E87", "#C27D38")
  
  #source functions
  source("Functions/ORange.R")
  
  #set up dataframe to add to
  df <- data.frame(ORange(order, BiomeNames[1], hem))
  names(df)[1] <- "Alpha"
  df$CellID <- c(1:15038)
  df$Biome <- BiomeNames[1]
  
  #make a vector of colors that match the biome map
  biomecols <- vector()
  if(sum(df$Alpha, na.rm = T) > 0){
    biomecols[1] <- cols7[1]
  }

  
  #loop through biomes and add to the dataframe
  for(i in 2:NumberBiomes){
    temp <- data.frame(ORange(order, BiomeNames[i], hem))
    names(temp)[1] <- "Alpha"
    temp$CellID <- c(1:15038)
    temp$Biome <- BiomeNames[i] 
    if(sum(temp$Alpha, na.rm = T) > 0){
      biomecols <- append(biomecols, cols7[i])
    }
    df <- bind_rows(df, temp)
  }
  
  #set scale
  if(order %in% OrdRichAbove100){
    plot_scale = 200
  }else if(order %in% OrdRich25to100){
    plot_scale = 100
  }else if(order %in% OrdRich10to25){
    plot_scale = 25
  }else if(order %in% OrdRichBelow10){
    plot_scale = 10
  }
  
  #set subtitle
  if(hem == "both"){
    sub <- "Both Hemispheres"
  }else if(hem == "Northern"){
    sub <- "Northern Hemisphere"
  }else if(hem == "Southern"){
    sub <- "Southern Hemisphere"
  }
  
  
  #plot
  #violin
  if(type == "violin"){
    plot <- ggplot(df, aes(x = Biome, y = Alpha, fill = Biome)) + 
      geom_violin(scale = "count", show.legend = FALSE, fill = cols1) + 
      theme_minimal() + 
      ylim(0,plot_scale) +
      ggtitle(order, subtitle = sub) + 
      ylab("Richness") + 
      xlab("Biome") + 
      theme(axis.title.y = element_text(size=32), 
            axis.title.x = element_text(size=32),
            axis.text.y = element_text(size=20), 
            axis.text.x = element_text(angle = 30, hjust = 1, size = 12), 
            plot.title = element_text(size = 28, hjust = 0.5),  
            plot.subtitle = element_text(size = 20, hjust = 0.5))
  #box
  }else if(type == "box"){
    plot <- ggplot(df, aes(x = Biome, y = Alpha, fill = Biome)) + 
      geom_boxplot(show.legend=FALSE, fill = biomecols) + 
      ggtitle(order, subtitle = sub) +
      theme_minimal() + 
      ylim(0, plot_scale) + 
      geom_jitter(alpha = 0.5, width = 0.2, color = "gray") +
      ylab("Richness") + 
      xlab("Biome") + 
      theme(axis.title.y = element_text(size=32), 
            axis.title.x = element_text(size=32),
            axis.text.y = element_text(size=20), 
            axis.text.x = element_text(angle = 30, hjust = 1, size = 12),
            plot.title = element_text(size = 28, hjust = 0.5), 
            plot.subtitle = element_text(size = 20, hjust = 0.5)) + 
      theme(legend.position = "none")
  #box + transparent violin layered
  }else if(type == "boxyviolin"){
    plot <- ggplot(df, aes(x = Biome, y = Alpha, fill = Biome, color = Biome)) + 
      geom_boxplot(show.legend = FALSE, fill = biomecols, color = "black") +
      ggtitle(order, subtitle = sub) +
      theme_minimal() +
      ylim(0,plot_scale) + 
      geom_violin(scale="count", show.legend=FALSE, fill="gray", alpha=0.35,
                  color = "gray25") +
      xlab("Biome") +
      ylab("Richness") +  
      theme(axis.title.y = element_text(size=32), 
            axis.title.x = element_text(size=32),
            axis.text.y = element_text(size=20), 
            axis.text.x = element_text(angle = 30, hjust = 1, size = 12),
            plot.title = element_text(size = 28, hjust = 0.5),
            plot.subtitle = element_text(size = 20, hjust = 0.5)) +
      theme(legend.position = "none")
  }
  
   return(plot)
}
