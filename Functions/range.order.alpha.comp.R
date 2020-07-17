# Quantitative Alpha Diversity Comparison for Orders in Specific Mountain Ranges
##Input: order1 = first order name, str; order2 = second order name, str; range = name of mountain range (Appalachian, Rocky, Andes), str
##Output: 1) ratio of (smaller order alpha div/larger order alpha div)
##Output: 2) data.frame displaying order name and total alpha diversity for each order
# Hailey Napier
# July 15, 2020

##MUST RUN MountainRanges.R FIRST##

range.order.alpha.comp <- function(order1,order2,range,...){
  o1range <- ORange(order1, range)
  o2range <- ORange(order2, range)
  
  order1sum <- sum(o1range, na.rm = T)
  order2sum <- sum(o2range, na.rm = T)
  
  if(order1sum != order2sum){
    larger <- max(order1sum,  order2sum)
    smaller <- min(order1sum, order2sum)
    
    dif <- smaller/larger
  }else{
    dif <- ("The orders have the same total alpha diversity.")
  }
  
  df <- data.frame(Order = c(order1, order2), TotalAlpha = c(order1sum, order2sum))
  
  print(range)
  print(dif)
  df
}

