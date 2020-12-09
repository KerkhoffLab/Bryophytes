# Figures for Napier BIOL 385 Paper (Fall 2020)
# Hailey Napier
# December 2020

# ****Run MossOrderLM.R first for data****
OrderCoefPlotDF <- readRDS("Data/OrderCoefPlotDF.rds")
OrderLMAdjBiomeCoefDF <- readRDS("Data/OrdAdjBiomeLMCoefDF.rds")


# 1.0 Plot MAT and MAP pointrange plots -------
MATdf <-OrderCoefPlotDF %>%
  filter(OrderCoefPlotDF$Parameter == "log1p(MAT_Kelvin)")

OrdCoefPlot_MAT <- ggplot() + 
  geom_pointrange(data = MATdf, 
                  aes(x = Order, 
                      y = Coefficient, 
                      ymin = LowLim, 
                      ymax = UpperLim), 
                  col = "black") +
  geom_errorbar(data = MATdf, 
                aes(x = Order, 
                    y = Coefficient, 
                    ymin = LowLim, 
                    ymax = UpperLim), 
                width = 0.5, 
                col = "black")  +
  geom_hline(aes(yintercept = 0), linetype = "dashed", col = "blue") +
  ylab("β log(MAT)") +
  ggtitle("Temperature Coefficients") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) 

OrdCoefPlot_MAT


MAPdf <- OrderCoefPlotDF %>%
  filter(OrderCoefPlotDF$Parameter == "log1p(MAP)")

OrdCoefPlot_MAP <- ggplot() + 
  geom_pointrange(data = MAPdf, 
                  aes(x = Order, 
                      y = Coefficient, 
                      ymin = LowLim, 
                      ymax = UpperLim), 
                  col = "black") +
  geom_errorbar(data = MAPdf, 
                aes(x = Order, 
                    y = Coefficient, 
                    ymin = LowLim, 
                    ymax = UpperLim), 
                width = 0.5, 
                col = "black")  +
  geom_hline(aes(yintercept = 0), linetype = "dashed", col = "blue") +
  ylab("β log(MAP)") +
  ggtitle("Precipitation Coefficients") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  
        plot.title = element_text(hjust = 0.5))

OrdCoefPlot_MAP


# 2.0 Plots each for biome -------

# Function to make plots
#Input: str; biome name
#Output: pointrange plot with coefficient values and error bars for each order for the selected biome

biome_coef_pointrange_plot <- function(biome = "Coniferous_Forests"){
  biome_cols_11 <- c("#D8B70A", "#972D15", "#A2A475", "#81A88D", "#02401B",
                     "#446455", "#FDD262", "#D3DDDC", "#C7B19C", "#798E87", 
                     "#C27D38")
  
  if(biome %in% BiomeNames){
    plotdf <- OrderLMAdjBiomeCoefDF %>%
      filter(OrderLMAdjBiomeCoefDF$Biome == biome)
    
    plot_title <- paste(biome, "Coefficients", sep = " ")
    y_axis_title <- paste("β", biome, sep = "")
    
    biome_index <- which(biome == BiomeNames)[[1]]
    biome_col <- biome_cols_11[biome_index]
    
    plot <- ggplot() + 
      geom_pointrange(data = plotdf, 
                      aes(x = Order, 
                          y = Coefficient, 
                          ymin = LowLim, 
                          ymax = UpLim), 
                      col = biome_col) +
      geom_errorbar(data = plotdf, 
                    aes(x = Order, 
                        y = Coefficient, 
                        ymin = LowLim, 
                        ymax = UpLim), 
                    width = 0.5, 
                    col = biome_col)  +
      geom_hline(aes(yintercept = 0), linetype = "dashed", col = "black") +
      ylab(y_axis_title) +
      ggtitle(plot_title) +
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1),  
            plot.title = element_text(hjust = 0.5))
    
  }else{
    plot <- "Please make sure the biome you entered is in BiomeNames"
  }
  return(plot)
}

# Loop through and make a folder of biome coefficient plots
dir.create("Figures/BiomeCoefPlots")
for(i in 1:length(BiomeNames)){
  biome <- BiomeNames[i]
  plot_name <- paste("Figures/BiomeCoefPlots/", biome,"_CoefPlot.png", sep = "")
  plot <- biome_coef_pointrange_plot(biome)

  png(plot_name, width = 1500, height = 1000, pointsize = 20)
  print({plot})
  dev.off()
}


# Plot all biomes on one plot
biome_cols_11 <- c("#D8B70A", "#972D15", "#A2A475", "#81A88D", "#02401B",
                   "#446455", "#FDD262", "#D3DDDC", "#C7B19C", "#798E87", 
                   "#C27D38")
plot <- ggplot() + 
  geom_pointrange(data = OrderLMAdjBiomeCoefDF, 
                  aes(x = Order, 
                      y = Coefficient, 
                      ymin = LowLim, 
                      ymax = UpLim, 
                      color = Biome)) +
  geom_errorbar(data = OrderLMAdjBiomeCoefDF, 
                aes(x = Order, 
                    y = Coefficient, 
                    ymin = LowLim, 
                    ymax = UpLim,
                    color = Biome), 
                width = 0.5)  +
  geom_hline(aes(yintercept = 0), linetype = "dashed", col = "black") +
  ylab("β") +
  ggtitle("Biome Coefficients") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  
        plot.title = element_text(hjust = 0.5)) 
plot
