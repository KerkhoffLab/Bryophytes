#Quantitative alpha diversity comparison for families
  ##Input: fam1 = first family name; fam2 = second family name
  ##Output: 1) ratio of (smaller family alpha div/larger family alpha div)
  ##Output: 2) data.frame displaying family name and total alpha diversity for each family
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
  
  df <- data.frame(Family = c(fam1, fam2), TotalAlpha = c(fam1sum, fam2sum))
  
  print(dif)
  df
}
