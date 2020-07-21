orderrichdf <- data.frame(OrderNames)
orderrichdf$totalalpha <- NA
for(i in 1:length(OrderNames)){
  o <- OrderNames[i]
  orderrichdf$totalalpha[i] <- TotalAlpha(o)
}

ggplot(orderrichdf, aes(x = OrderNames, y = totalalpha)) + 
  geom_bar(stat = "identity")

