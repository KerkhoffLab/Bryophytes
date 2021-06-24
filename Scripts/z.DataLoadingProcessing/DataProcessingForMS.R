# Data Processing for the Moss MS
# Compiled by Hailey Napier
# June 2021

# 0.0 Load Packages

# ?.0 Find order names -------
OrderNames <- unique(BryophytePresence$Order)
OrderNames <- OrderNames[!is.na(OrderNames)]
NumberOrders <- length(OrderNames)

# Tally richness by cell and create richness vector
CellRichness <- tally(group_by(BryophytePresence, CellID))
colnames(CellRichness)[2] <- "Richness"
RichnessVec <- numeric(15038)
RichnessVec[CellRichness$CellID] <- CellRichness$Richness

# ?.0 Make MossPresence and find moss order names
MossPresence <- BryophytePresence %>%
  filter(Group == "Moss" | Group == "Mosses")
MossOrderNames <- unique(MossPresence$Order)

#Save richness and presence data
saveRDS(OrderNames, file = "Data/OrderNames.rds")
saveRDS(CellRichness, file = "Data/CellRichness.rds")
saveRDS(RichnessVec, file = "Data/RichnessVec.rds")
saveRDS(BryophytePresence, file = "Data/BryophytePresence.rds")
saveRDS(BryophytePresence, file = "Data/MossPresence.rds")
saveRDS(OrderNames, file = "Data/MossOrderNames.rds")
