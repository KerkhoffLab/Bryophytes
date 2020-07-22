#Order Richness
#Grouping orders based on alpha diversity in order to determine y axis in plots
#Hailey Napier and Kathryn Dawdy
#July 22, 2020

# 1.0 Make a new dataframe for faceted plots --------------------------------
orderrichdf <- data.frame(OrderNames)
orderrichdf$totalalpha <- NA
for(i in 1:length(OrderNames)){
  o <- OrderNames[i]
  orderrichdf$totalalpha[i] <- TotalAlpha(o)
}


# 2.0 Make a bar chart to look at max alpha diversity -----------------------
theme_set(theme_gray())
ggplot(orderrichdf, aes(x = OrderNames, y = totalalpha)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# 2.1 Make a dataframe to look at numbers for max alpha diversity 
OrderMaxAlpha <- data.frame(tapply(OrderBiomeDF$Alpha, OrderBiomeDF$Order, max, na.rm = T))
names(OrderMaxAlpha)[1]  <- "MaxAlpha"
Names <- sort(unique(OrderBiomeDF$Order))
OrderMaxAlpha$Names <- Names

# 2.2 Put order names into vectors based on max alpha diversity 
OrdRichAbove100 <- vector()
OrdRich25to100 <- vector()
OrdRich10to25 <- vector()
OrdRichBelow10 <- vector()
for(i in 1:length(Names)){
  name <- Names[i]
  if(OrderMaxAlpha$MaxAlpha[i] > 100){
    OrdRichAbove100 <- c(OrdRichAbove100, name)
  }else if(OrderMaxAlpha$MaxAlpha[i] > 25){
    OrdRich25to100 <- c(OrdRich25to100, name)
  }else if(OrderMaxAlpha$MaxAlpha[i] > 10){
    OrdRich10to25 <- c(OrdRich10to25, name)
  }else if(OrderMaxAlpha$MaxAlpha[i] < 10){
    OrdRichBelow10 <- c(OrdRichBelow10, name)
  }
}

saveRDS(OrdRichAbove100, "Data/OrdRichAbove100.rds")
saveRDS(OrdRich25to100, "Data/OrdRich25to100.rds")
saveRDS(OrdRich10to25, "Data/OrdRich10to25.rds")
saveRDS(OrdRichBelow25, "Data/OrdRichBelow10.rds")
