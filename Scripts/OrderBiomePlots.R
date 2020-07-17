#Order Biome Plots
#Make and save boxplots for every biome for each order
#Hailey Napier
#July 17, 2020

#Make a new folder for plots
dir.create("Figures/OrderBiomePlots")

#Loop through orders and put plots in folder
for(i in 1:NumberOrders){
  order <- OrderNames[i]
  filename <- paste("Figures/OrderBiomePlots/", order, "_BiomePlot.png", sep = "")
  p <- OrdBiomeBP(order, "box")
  png(filename, width= 2000, height = 1000, pointsize = 30)
  print({p})
  dev.off()
}
