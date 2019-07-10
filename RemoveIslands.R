#Remove islands

#based on a posting to r-sig-geo by Hans-Jorg Bibiko on 29.01.2009 
#see http://www.mail-archive.com/r-sig-geo@stat.math.ethz.ch/msg04341.html
data(wrld_simpl)
minIslandSizeForInclusionInMap <- 3
#prob in degrees #for each country in the map 
for (i in (1:length(wrld_simpl@polygons))) {
  polyCnt <- length(wrld_simpl@polygons[[i]]@Polygons) 
  if (polyCnt > 2) {
    pO <- sapply(wrld_simpl@polygons[[i]]@Polygons, 
                 function(x) {slot(x,'area') > minIslandSizeForInclusionInMap} ) 
        new_pO <- pO[ wrld_simpl@polygons[[i]]@plotOrder ] 
    wrld_simpl@polygons[[i]]@plotOrder <- wrld_simpl@polygons[[i]]@plotOrder[ new_pO ] } } 

plot(wrld_simpl) 
wrld_simpl_lessIslands <- wrld_simpl
plot(wrld_simpl_lessIslands)
  
GetRidOf(islands) (((please)))



