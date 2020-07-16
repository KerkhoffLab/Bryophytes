# Quantitative Alpha Diversity Comparison for Orders
  ##Input: order1 = first order name; order2 = second order name
  ##Output: 1) ratio of (smaller order alpha div/larger order alpha div)
  ##Output: 2) data.frame displaying order name and total alpha diversity for each order
# Hailey Napier
# July 15, 2020

order.alpha.comp <- function(order1,order2,...){
  OrderNames <- readRDS("Data/OrderNames.rds")
  OrderRichList <- readRDS("Data/OrderRichList.rds")
  
  order1index <- which(OrderNames == order1)
  order2index <- which(OrderNames == order2)

  order1sum <- sum(OrderRichList[[order1index]], na.rm = T)
  order2sum <- sum(OrderRichList[[order2index]], na.rm = T)
  
  if(order1sum != order2sum){
    larger <- max(order1sum,  order2sum)
    smaller <- min(order1sum, order2sum)
    
    dif <- smaller/larger
  }else{
    dif <- ("The orders have the same total alpha diversity.")
  }
  
  df <- data.frame(Order = c(order1, order2), TotalAlpha = c(order1sum, order2sum))
  
  
  print(dif)
  df
}



    