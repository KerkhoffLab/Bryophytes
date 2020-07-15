# Qunatitative Alpha Diversity Comparison for Orders
  ##Input: order1 = first order name; order2 = second order name
  ##Output: 1) ratio of smaller order alpha div/larger order alpha div
  ##Output: 2) order1: order1sum; order2: order2sum 
# Hailey Napier
# July 14, 2020

order.div.comp <- function(order1,order2,...){
  order1index <- which(OrderNames == order1)
  order2index <- which(OrderNames == order2)

  order1sum <- sum(OrderRichList[[order1index]], na.rm = T)
  order2sum <- sum(OrderRichList[[order2index]], na.rm = T)
  
  larger <- max(order1sum,  order2sum)
  smaller <- min(order1sum, order2sum)
  
  dif <- smaller/larger
  
  line2 <- paste(order1, ":  ", order1sum, sep = "")
  line3 <- paste(order2, ": ", order2sum, sep = "")
  writeLines(c(dif,line2,line3))
}



    