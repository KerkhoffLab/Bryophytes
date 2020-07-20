#Order Biome Plots
#Make and save boxplots for every biome for each order in a folder
#Hailey Napier
#July 17, 2020

#Load packages ------------------------------------------------------------------------
library(gridExtra)
library(grid)

#Source functions ---------------------------------------------------------------------
source("Functions/ORange.R")
source("OrdBiomeBP.R")

#Load data ----------------------------------------------------------------------------
OrderNodes20 <- read.csv("./Data/OrderNodesS20.csv")


#Make a new folder for plots
dir.create("Figures/OrderBiomePlots")

#Arrange plots in order of appearance on phylogeny 
tree20index <- match(OrderNodes20$Order, OrderNames)
tree20index <- tree20index[complete.cases(tree20index)]
tree20index <- data.frame(tree20index)
names(tree20index)[1] <- "index"
Order <- OrderNodes20$Order
Order <- Order[complete.cases(Order)]
tree20index$name <- Order
#Index numbers of orders in tree in order of appearance
tree20index$number <- c(1,2,3,4,5,6,7,8,9,10,11,21,20,18,13,14,12,15,16,17,19,22)


#Loop through orders and put plots in folder
for(i in 1:nrow(tree20index)){
  order <- tree20index$name[i]
  filename <- paste("Figures/OrderBiomePlots/plot", i, ".png", sep = "")
  p <- OrdBiomeBP(order, "box")
  png(filename, width= 2000, height = 1000, pointsize = 30)
  print({p})
  dev.off()
}

#Arrange plots
rl = lapply(sprintf("Figures/OrderBiomePlots/plot%i.png", 1:nrow(tree20index)), png::readPNG)
gl = lapply(rl, grid::rasterGrob)
gridExtra::grid.arrange(grobs=gl, ncol = 2)



