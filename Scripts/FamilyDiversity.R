#Family Diversity
#Hailey Napier
#June 2020

# 1. Plot all family richness (how many families are in each cell)


# 2.0 Plot species richness for each family (separate plot for each family)

# 2.1 Make family name vector and variable for vector length
FamilyNames <- unique(BryophytePresence$Family)
NumberFamilies <- length(FamilyNames)

# 2. Loop through family names and subset BryophtePresence for each family
i <- 1
for(i in NumberFamilies){
  fam <- FamilyNames[i]
  subset(BryophytePresence, Family == fam)
}
