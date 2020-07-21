orderrichdf <- data.frame(OrderNames)
orderrichdf$totalalpha <- NA
for(i in 1:length(OrderNames)){
  o <- OrderNames[i]
  orderrichdf$totalalpha[i] <- TotalAlpha(o)
}

theme_set(theme_gray())

ggplot(orderrichdf, aes(x = OrderNames, y = totalalpha)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

OrdRichAbove100k <- c("Hypnales","Dicranales","Bryales")
OrdRichAbove100k
saveRDS(OrdRichAbove100k, "Data/OrdRichAbove100k.rds")

OrderNames <- readRDS("Data/OrderNames.rds")
NoHyp <- OrderNames[OrderNames!="Hypnales"]
NoHypNoDic <- NoHyp[NoHyp!="Dicranales"]
OrdRichBelow100k <- NoHypNoDic[NoHypNoDic!="Bryales"]
OrdRichBelow100k
saveRDS(OrdRichBelow100k, "Data/OrdRichBelow100k.rds")
