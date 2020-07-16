#Quantitative alpha diversity comparison for families
  ##Input: order1 = first order name; order2 = second order name
  ##Output: 1) ratio of smaller order alpha div/larger order alpha div
  ##Output: 2) order1: order1sum; order2: order2sum 
#Hailey Napier
#July 16, 2020

fam.alpha.comp <- function(fam1,fam2,...){
  FamilyNames <- readRDS("Data/FamilyNames.rds")
  FamRichList <- readRDS("Data/FamRichList.rds")
  
  fam1index <- which(FamilyNames == fam1)
  fam2index <- which(FamilyNames == fam2)
  
  fam1sum <- sum(FamRichList[[fam1index]], na.rm = T)
  fam2sum <- sum(FamRichList[[fam2index]], na.rm = T)
  
  if(fam1sum != fam2sum){
    larger <- max(fam1sum,  fam2sum)
    smaller <- min(fam1sum, fam2sum)
    
    dif <- smaller/larger
  }else{
    dif <- ("The families have the same total alpha diversity.")
  }
  
  line2 <- paste(fam1, ":  ", fam1sum, sep = "")
  line3 <- paste(fam2, ": ", fam2sum, sep = "")
  writeLines(c(dif,line2,line3))
}
