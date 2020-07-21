#Function to find total alpha diversity of an order
#Input: str, order name (ex. = "Hypnales")
#Output: int, sum of richness
#Hailey Napier
#July 21, 2020

TotalAlpha <- function(order){
  OrderNames <- readRDS("Data/OrderNames.rds")
  OrderRichList <- readRDS("Data/OrderRichList.rds")
  
  orderindex <- which(OrderNames == order)
  
  ordersum <- sum(OrderRichList[[orderindex]], na.rm = T)
  
  return(ordersum)
}
