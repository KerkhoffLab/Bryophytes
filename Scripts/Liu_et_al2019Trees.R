#Reading Liu et al.(2019) tree & Processing data
#Hailey Napier
#July 6, 2020


# 0.0 Load Packages ---------------------
library(ape)

library(phytools)

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("ggtree")
BiocManager::install("treeio")
BiocManager::install("rphast")
library(ggtree)
library(treeio)
library(ggplot2)
library(ggimage)

library(tidyr)
library(dplyr)
library(tools)

library(grid)
library(gridExtra)

library(png)

# 1.0 try the read.tree function --------
read.tree("Data/trees/nt-pt-MrBayes-FigS8.tre")

# 2.0 Plot -----------------------------
#ggtree
ggtree(testtree) + geom_tiplab(align = T, size = 1) + ggplot2::xlim(0,50)

#add images to ggtree tips 
#make a data.frame that has lavels and images
#testimage.df <- data.frame(c("t1", "t2", "t3"))
#names(testimage.df)[1] <- "tip"
#testimage.df$image <- c("Figures/FamRichnessMap.png", "Figures/HemispheresBoxPlot.png", "Figures/LiverwortBetaMap.png")

#too many tips, you can't see anything
ggtree(testtree) + 
  geom_tiplab(image = "Figures/FamRichnessMap.png", geom = "image", size = .005, image_fun = function(.) magick::image_transparent(., "white")) + 
  ggplot2::xlim(0,50) + 
  geom_tiplab(offset = 1, size = 1.2)

#make a random tree
set.seed(1982)
randomtree <- rtree(5)
ggtree(randomtree) + geom_tiplab(image = "Figures/FamRichnessMap.png", geom = "image", size = .12) + ggplot2::xlim(0,2)

#make background transparent?
ggtree(randomtree) + 
  geom_tiplab(image = "Figures/FamRichnessMap.png", geom = "image", size = .12, image_fun = function(.) magick::image_transparent(., "white")) + 
  ggplot2::xlim(0,2.5) +
  geom_tiplab(offset = 0.4)

#try reading in .tree files
testtreetwo <- read.nexus(file = "Data/trees/aa-mt-phylobayes-FigS14.tree")
ggtree(testtreetwo, branch.length = "none") +
  geom_tiplab(size = 1.5) +
  ggplot2::xlim(0,40)


#3.0 Clean up tree data -------------------
#look at data
testtreetwo.df <- ggtree(testtreetwo)$data

#subset to add family, order, and group
testtreetwo.df$keep <- ifelse(testtreetwo.df$isTip == "TRUE", 1, 0)

TTnames <- data.frame(testtreetwo.df$label <- gsub("_", " ", testtreetwo.df$label))
names(TTnames)[1] <- "name"

#Make list of genus names in our data
unique(BryophytePresence$Family[which(BryophytePresence$Genus == TTnames$genus[i])])
GenusNames <- unique(BryophytePresence$Genus)
GenusNames <- GenusNames[complete.cases(GenusNames)]

#separate genus from species/other stuff
TTnames <- separate(TTnames, name, into = c("genus","other"))
TTnames$other <- NULL
TTnames$family <- NA
TTnames$order <- NA
TTnames$group <- NA


#loop through to add family order and group for each genus
for(i in 1:nrow(TTnames)){
  TTnames$genus[i] <- gsub('[[:digit:]]+', '', TTnames$genus[i])
  TTnames$genus[i] <- toTitleCase(TTnames$genus[i])
  test.genus <- TTnames$genus[i]
  for(j in 1:length(GenusNames)){
    ref.genus <- GenusNames[j]
    if(test.genus == ref.genus){
      TTnames$family[i] <- unique(BryophytePresence$Family[which(BryophytePresence$Genus == test.genus)])
      TTnames$order[i] <- unique(BryophytePresence$Order[which(BryophytePresence$Genus == test.genus)])
      TTnames$group[i] <- unique(BryophytePresence$Group[which(BryophytePresence$Genus == test.genus)])
      break
    }
  }
}

#Add the new columns to the data and save as .csv
FigS14_FOG <- bind_cols(testtreetwo.df, TTnames)
#dir.create("FamilyTrees")
write.csv(FigS14_FOG, "Data/FamilyTrees/FigS14_FOG.csv")


#Add family, order, and group info to other trees

#FigS13
FigS13 <- read.tree("Data/trees/aa-mt-RAxML-FigS13.tre")
FigS13_FOG <- ggtree(FigS13)$data
FigS13_FOG <- ifelse(FigS13_FOG$isTip == "TRUE", 1, 0)

FigS13_names <- data.frame(FigS13_FOG$label <- gsub("_", " ", FigS13_FOG$label))
names(FigS13_names)[1] <- "name"
FigS13_names <- separate(FigS13_names, name, into = c("genus","other"))
FigS13_names$other <- NULL
FigS13_names$family <- NA
FigS13_names$order <- NA
FigS13_names$group <- NA

for(i in 1:nrow(FigS13_names)){
  FigS13_names$genus[i] <- gsub('[[:digit:]]+', '', FigS13_names$genus[i])
  FigS13_names$genus[i] <- toTitleCase(FigS13_names$genus[i])
  test.genus <- FigS13_names$genus[i]
  for(j in 1:length(GenusNames)){
    ref.genus <- GenusNames[j]
    if(test.genus == ref.genus){
      FigS13_names$family[i] <- unique(BryophytePresence$Family[which(BryophytePresence$Genus == test.genus)])
      FigS13_names$order[i] <- unique(BryophytePresence$Order[which(BryophytePresence$Genus == test.genus)])
      FigS13_names$group[i] <- unique(BryophytePresence$Group[which(BryophytePresence$Genus == test.genus)])
      break
    }
  }
}

FigS13_FOG <- bind_cols(FigS13_FOG, FigS13_names)
write.csv(FigS13_FOG, "Data/FamilyTrees/FigS13_FOG.csv")

#FigS24
FigS24 <- read.tree("Data/trees/aa-nu-astral-lpp-FigS24.tre")
FigS24_FOG <- ggtree(FigS24)$data

FigS24_names <- data.frame(FigS24_FOG$label <- gsub("_", " ", FigS24_FOG$label))
names(FigS24_names)[1] <- "name"
FigS24_names <- separate(FigS24_names, name, into = c("genus","other"))
FigS24_names$other <- NULL
FigS24_names$family <- NA
FigS24_names$order <- NA
FigS24_names$group <- NA

for(i in 1:nrow(FigS24_names)){
  FigS24_names$genus[i] <- gsub('[[:digit:]]+', '', FigS24_names$genus[i])
  FigS24_names$genus[i] <- toTitleCase(FigS24_names$genus[i])
  test.genus <- FigS24_names$genus[i]
  for(j in 1:length(GenusNames)){
    ref.genus <- GenusNames[j]
    if(test.genus == ref.genus){
      FigS24_names$family[i] <- unique(BryophytePresence$Family[which(BryophytePresence$Genus == test.genus)])
      FigS24_names$order[i] <- unique(BryophytePresence$Order[which(BryophytePresence$Genus == test.genus)])
      FigS24_names$group[i] <- unique(BryophytePresence$Group[which(BryophytePresence$Genus == test.genus)])
      break
    }
  }
}

FigS24_FOG <- bind_cols(FigS24_FOG, FigS24_names)
write.csv(FigS24_FOG, "Data/FamilyTrees/FigS24_FOG.csv")

#FigS21
FigS21 <- read.nexus("Data/trees/aa-nu-phylobayes-FigS21.tree")
FigS21_FOG <- ggtree(FigS21)$data

FigS21_names <- data.frame(FigS21_FOG$label <- gsub("_", " ", FigS21_FOG$label))
names(FigS21_names)[1] <- "name"
FigS21_names <- separate(FigS21_names, name, into = c("genus","other"))
FigS21_names$other <- NULL
FigS21_names$family <- NA
FigS21_names$order <- NA
FigS21_names$group <- NA

for(i in 1:nrow(FigS21_names)){
  test.genus <- FigS21_names$genus[i]
  for(j in 1:length(GenusNames)){
    ref.genus <- GenusNames[j]
    if(test.genus == ref.genus){
      FigS21_names$family[i] <- unique(BryophytePresence$Family[which(BryophytePresence$Genus == test.genus)])
      FigS21_names$order[i] <- unique(BryophytePresence$Order[which(BryophytePresence$Genus == test.genus)])
      FigS21_names$group[i] <- unique(BryophytePresence$Group[which(BryophytePresence$Genus == test.genus)])
      break
    }
  }
}

FigS21_FOG <- bind_cols(FigS21_FOG, FigS21_names)
write.csv(FigS21_FOG, "Data/FamilyTrees/FigS21_FOG.csv")

#FigS25
FigS25 <- read.tree("Data/trees/aa-nu-astral-mlbs-FigS25.tre")
FigS25_FOG <- ggtree(FigS25)$data

FigS25_names <- data.frame(FigS25_FOG$label <- gsub("_", " ", FigS25_FOG$label))
names(FigS25_names)[1] <- "name"
FigS25_names <- separate(FigS25_names, name, into = c("genus","other"))
FigS25_names$other <- NULL
FigS25_names$family <- NA
FigS25_names$order <- NA
FigS25_names$group <- NA

#fix some weird sphagnum stuff
FigS25_names$genus[100] <- "Sphagnum"
FigS25_names$genus[101] <- "Sphagnum"
FigS25_names$genus[104] <- "Sphagnum"

for(i in 1:nrow(FigS25_names)){
  FigS25_names$genus[i] <- gsub('[[:digit:]]+', '', FigS25_names$genus[i])
  FigS25_names$genus[i] <- toTitleCase(FigS25_names$genus[i])
  test.genus <- FigS25_names$genus[i]
  for(j in 1:length(GenusNames)){
    ref.genus <- GenusNames[j]
    if(test.genus == ref.genus){
      FigS25_names$family[i] <- unique(BryophytePresence$Family[which(BryophytePresence$Genus == test.genus)])
      FigS25_names$order[i] <- unique(BryophytePresence$Order[which(BryophytePresence$Genus == test.genus)])
      FigS25_names$group[i] <- unique(BryophytePresence$Group[which(BryophytePresence$Genus == test.genus)])
      break
    }
  }
}

FigS25_FOG <- bind_cols(FigS25_FOG, FigS25_names)
write.csv(FigS25_FOG, "Data/FamilyTrees/FigS25_FOG.csv")

#FigS17
FigS17 <- read.tree("Data/trees/aa-org-RAxML-FigS17.tre")
FigS17_FOG <- ggtree(FigS17)$data

FigS17_names <- data.frame(FigS17_FOG$label <- gsub("_", " ", FigS17_FOG$label))
names(FigS17_names)[1] <- "name"
FigS17_names <- separate(FigS17_names, name, into = c("genus","other"))
FigS17_names$other <- NULL
FigS17_names$family <- NA
FigS17_names$order <- NA
FigS17_names$group <- NA

for(i in 1:nrow(FigS17_names)){
  n <- FigS17_names$genus[i]
  FigS17_names$genus[i] <- ifelse(n == "sphagnumL" | n == "sphagnumP" | n == "sphagnumR", "Sphagnum", n)
  FigS17_names$genus[i] <- gsub('[[:digit:]]+', '', FigS17_names$genus[i])
  FigS17_names$genus[i] <- toTitleCase(FigS17_names$genus[i])
  test.genus <- FigS17_names$genus[i]
  for(j in 1:length(GenusNames)){
    ref.genus <- GenusNames[j]
    if(test.genus == ref.genus){
      FigS17_names$family[i] <- unique(BryophytePresence$Family[which(BryophytePresence$Genus == test.genus)])
      FigS17_names$order[i] <- unique(BryophytePresence$Order[which(BryophytePresence$Genus == test.genus)])
      FigS17_names$group[i] <- unique(BryophytePresence$Group[which(BryophytePresence$Genus == test.genus)])
      break
    }
  }
}

FigS17_FOG <- bind_cols(FigS17_FOG, FigS17_names)
write.csv(FigS17_FOG, "Data/FamilyTrees/FigS17_FOG.csv")

#FigS20
FigS20 <- read.tree("Data/trees/aa-nu-RAxML-FigS20.tre")
FigS20_FOG <- ggtree(FigS20)$data

FigS20_names <- data.frame(FigS20_FOG$label <- gsub("_", " ", FigS20_FOG$label))
names(FigS20_names)[1] <- "name"
FigS20_names <- separate(FigS20_names, name, into = c("genus","other"))
FigS20_names$other <- NULL
FigS20_names$family <- NA
FigS20_names$order <- NA
FigS20_names$group <- NA

for(i in 1:nrow(FigS20_names)){
  n <- FigS20_names$genus[i]
  FigS20_names$genus[i] <- ifelse(n == "sphagnumL" | n == "sphagnumP" | n == "sphagnumR", "Sphagnum", n)
  FigS20_names$genus[i] <- gsub('[[:digit:]]+', '', FigS20_names$genus[i])
  FigS20_names$genus[i] <- toTitleCase(FigS20_names$genus[i])
  test.genus <- FigS20_names$genus[i]
  for(j in 1:length(GenusNames)){
    ref.genus <- GenusNames[j]
    if(test.genus == ref.genus){
      FigS20_names$family[i] <- unique(BryophytePresence$Family[which(BryophytePresence$Genus == test.genus)])
      FigS20_names$order[i] <- unique(BryophytePresence$Order[which(BryophytePresence$Genus == test.genus)])
      FigS20_names$group[i] <- unique(BryophytePresence$Group[which(BryophytePresence$Genus == test.genus)])
      break
    }
  }
}

FigS20_FOG <- bind_cols(FigS20_FOG, FigS20_names)
write.csv(FigS20_FOG, "Data/FamilyTrees/FigS20_FOG.csv")

#FigS10
FigS10 <- read.nexus("Data/trees/aa-pt-phylobayes-FigS10.tree")
FigS10_FOG <- ggtree(FigS10)$data

FigS10_names <- data.frame(FigS10_FOG$label <- gsub("_", " ", FigS10_FOG$label))
names(FigS10_names)[1] <- "name"
FigS10_names <- separate(FigS10_names, name, into = c("genus","other"))
FigS10_names$other <- NULL
FigS10_names$family <- NA
FigS10_names$order <- NA
FigS10_names$group <- NA

for(i in 1:nrow(FigS10_names)){
  n <- FigS10_names$genus[i]
  FigS10_names$genus[i] <- ifelse(n == "sphagnumL" | n == "sphagnumP" | n == "sphagnumR", "Sphagnum", n)
  FigS10_names$genus[i] <- gsub('[[:digit:]]+', '', FigS10_names$genus[i])
  FigS10_names$genus[i] <- toTitleCase(FigS10_names$genus[i])
  test.genus <- FigS10_names$genus[i]
  for(j in 1:length(GenusNames)){
    ref.genus <- GenusNames[j]
    if(test.genus == ref.genus){
      FigS10_names$family[i] <- unique(BryophytePresence$Family[which(BryophytePresence$Genus == test.genus)])
      FigS10_names$order[i] <- unique(BryophytePresence$Order[which(BryophytePresence$Genus == test.genus)])
      FigS10_names$group[i] <- unique(BryophytePresence$Group[which(BryophytePresence$Genus == test.genus)])
      break
    }
  }
}

FigS10_FOG <- bind_cols(FigS10_FOG, FigS10_names)
write.csv(FigS10_FOG, "Data/FamilyTrees/FigS10_FOG.csv")

#FigS9
FigS9 <- read.tree("Data/trees/aa-pt-RAxML-FigS9.tre")
FigS9_FOG <- ggtree(FigS9)$data

FigS9_names <- data.frame(FigS9_FOG$label <- gsub("_", " ", FigS9_FOG$label))
names(FigS9_names)[1] <- "name"
FigS9_names <- separate(FigS9_names, name, into = c("genus","other"))
FigS9_names$other <- NULL
FigS9_names$family <- NA
FigS9_names$order <- NA
FigS9_names$group <- NA

for(i in 1:nrow(FigS9_names)){
  n <- FigS9_names$genus[i]
  FigS9_names$genus[i] <- ifelse(n == "sphagnumL" | n == "sphagnumP" | n == "sphagnumR", "Sphagnum", n)
  FigS9_names$genus[i] <- gsub('[[:digit:]]+', '', FigS9_names$genus[i])
  FigS9_names$genus[i] <- toTitleCase(FigS9_names$genus[i])
  test.genus <- FigS9_names$genus[i]
  for(j in 1:length(GenusNames)){
    ref.genus <- GenusNames[j]
    if(test.genus == ref.genus){
      FigS9_names$family[i] <- unique(BryophytePresence$Family[which(BryophytePresence$Genus == test.genus)])
      FigS9_names$order[i] <- unique(BryophytePresence$Order[which(BryophytePresence$Genus == test.genus)])
      FigS9_names$group[i] <- unique(BryophytePresence$Group[which(BryophytePresence$Genus == test.genus)])
      break
    }
  }
}

FigS9_FOG <- bind_cols(FigS9_FOG, FigS9_names)
write.csv(FigS9_FOG, "Data/FamilyTrees/FigS9_FOG.csv")

#FigS12
FigS12 <- read.nexus("Data/trees/nt-mt-MrBayes-FigS12.tre")
FigS12_names <- data.frame(FigS12_names)
names(FigS12_names)[1] <- "name"
FigS12_names <- separate(FigS12_names, name, into = c("genus","other"))
FigS12_names$other <- NULL
FigS12_names$family <- NA
FigS12_names$order <- NA
FigS12_names$group <- NA

for(i in 1:nrow(FigS12_names)){
  n <- FigS12_names$genus[i]
  FigS12_names$genus[i] <- ifelse(n == "sphagnumL" | n == "sphagnumP" | n == "sphagnumR", "Sphagnum", n)
  FigS12_names$genus[i] <- gsub('[[:digit:]]+', '', FigS12_names$genus[i])
  FigS12_names$genus[i] <- toTitleCase(FigS12_names$genus[i])
  test.genus <- FigS12_names$genus[i]
  for(j in 1:length(GenusNames)){
    ref.genus <- GenusNames[j]
    if(test.genus == ref.genus){
      FigS12_names$family[i] <- unique(BryophytePresence$Family[which(BryophytePresence$Genus == test.genus)])
      FigS12_names$order[i] <- unique(BryophytePresence$Order[which(BryophytePresence$Genus == test.genus)])
      FigS12_names$group[i] <- unique(BryophytePresence$Group[which(BryophytePresence$Genus == test.genus)])
      break
    }
  }
}

write.csv(FigS12_names, "Data/FamilyTrees/FigS12_names.csv")
# maybe we can replace the taxlabel column with the genus/family column in this data?

#FigS11
FigS11 <- read.tree("Data/trees/nt-mt-RAxML-FigS11.tre")
FigS11_FOG <- ggtree(FigS11)$data

FigS11_names <- data.frame(FigS11_FOG$label <- gsub("_", " ", FigS11_FOG$label))
names(FigS11_names)[1] <- "name"
FigS11_names <- separate(FigS11_names, name, into = c("genus","other"))
FigS11_names$other <- NULL
FigS11_names$family <- NA
FigS11_names$order <- NA
FigS11_names$group <- NA

for(i in 1:nrow(FigS11_names)){
  n <- FigS11_names$genus[i]
  FigS11_names$genus[i] <- ifelse(n == "sphagnumL" | n == "sphagnumP" | n == "sphagnumR", "Sphagnum", n)
  FigS11_names$genus[i] <- gsub('[[:digit:]]+', '', FigS11_names$genus[i])
  FigS11_names$genus[i] <- toTitleCase(FigS11_names$genus[i])
  test.genus <- FigS11_names$genus[i]
  for(j in 1:length(GenusNames)){
    ref.genus <- GenusNames[j]
    if(test.genus == ref.genus){
      FigS11_names$family[i] <- unique(BryophytePresence$Family[which(BryophytePresence$Genus == test.genus)])
      FigS11_names$order[i] <- unique(BryophytePresence$Order[which(BryophytePresence$Genus == test.genus)])
      FigS11_names$group[i] <- unique(BryophytePresence$Group[which(BryophytePresence$Genus == test.genus)])
      break
    }
  }
}

FigS11_FOG <- bind_cols(FigS11_FOG, FigS11_names)
write.csv(FigS11_FOG, "Data/FamilyTrees/FigS11_FOG.csv")

#FigS22
FigS22 <- read.tree("Data/trees/nt-nu-astral-lpp-FigS22.tre")
FigS22_FOG <- ggtree(FigS22)$data

FigS22_names <- data.frame(FigS22_FOG$label <- gsub("_", " ", FigS22_FOG$label))
names(FigS22_names)[1] <- "name"
FigS22_names <- separate(FigS22_names, name, into = c("genus","other"))
FigS22_names$other <- NULL
FigS22_names$family <- NA
FigS22_names$order <- NA
FigS22_names$group <- NA

for(i in 1:nrow(FigS22_names)){
  n <- FigS22_names$genus[i]
  FigS22_names$genus[i] <- ifelse(n == "sphagnumL" | n == "sphagnumP" | n == "sphagnumR", "Sphagnum", n)
  FigS22_names$genus[i] <- gsub('[[:digit:]]+', '', FigS22_names$genus[i])
  FigS22_names$genus[i] <- toTitleCase(FigS22_names$genus[i])
  test.genus <- FigS22_names$genus[i]
  for(j in 1:length(GenusNames)){
    ref.genus <- GenusNames[j]
    if(test.genus == ref.genus){
      FigS22_names$family[i] <- unique(BryophytePresence$Family[which(BryophytePresence$Genus == test.genus)])
      FigS22_names$order[i] <- unique(BryophytePresence$Order[which(BryophytePresence$Genus == test.genus)])
      FigS22_names$group[i] <- unique(BryophytePresence$Group[which(BryophytePresence$Genus == test.genus)])
      break
    }
  }
}

FigS22_FOG <- bind_cols(FigS22_FOG, FigS22_names)
write.csv(FigS22_FOG, "Data/FamilyTrees/FigS22_FOG.csv")

#FigS23
FigS23 <- read.tree("Data/trees/nt-nu-astral-mlbs-FigS23.tre")
FigS23_FOG <- ggtree(FigS23)$data

FigS23_names <- data.frame(FigS23_FOG$label <- gsub("_", " ", FigS23_FOG$label))
names(FigS23_names)[1] <- "name"
FigS23_names <- separate(FigS23_names, name, into = c("genus","other"))
FigS23_names$other <- NULL
FigS23_names$family <- NA
FigS23_names$order <- NA
FigS23_names$group <- NA

for(i in 1:nrow(FigS23_names)){
  n <- FigS23_names$genus[i]
  FigS23_names$genus[i] <- ifelse(n == "sphagnumL" | n == "sphagnumP" | n == "sphagnumR", "Sphagnum", n)
  FigS23_names$genus[i] <- gsub('[[:digit:]]+', '', FigS23_names$genus[i])
  FigS23_names$genus[i] <- toTitleCase(FigS23_names$genus[i])
  test.genus <- FigS23_names$genus[i]
  for(j in 1:length(GenusNames)){
    ref.genus <- GenusNames[j]
    if(test.genus == ref.genus){
      FigS23_names$family[i] <- unique(BryophytePresence$Family[which(BryophytePresence$Genus == test.genus)])
      FigS23_names$order[i] <- unique(BryophytePresence$Order[which(BryophytePresence$Genus == test.genus)])
      FigS23_names$group[i] <- unique(BryophytePresence$Group[which(BryophytePresence$Genus == test.genus)])
      break
    }
  }
}

FigS23_FOG <- bind_cols(FigS23_FOG, FigS23_names)
write.csv(FigS23_FOG, "Data/FamilyTrees/FigS23_FOG.csv")

#FigS19
FigS19 <- read.nexus("Data/trees/nt-nu-MrBayesFigS19.tree")
FigS19_FOG <- ggtree(FigS19)$data

FigS19_names <- data.frame(FigS19_FOG$label <- gsub("_", " ", FigS19_FOG$label))
names(FigS19_names)[1] <- "name"
FigS19_names <- separate(FigS19_names, name, into = c("genus","other"))
FigS19_names$other <- NULL
FigS19_names$family <- NA
FigS19_names$order <- NA
FigS19_names$group <- NA

for(i in 1:nrow(FigS19_names)){
  n <- FigS19_names$genus[i]
  FigS19_names$genus[i] <- ifelse(n == "sphagnumL" | n == "sphagnumP" | n == "sphagnumR", "Sphagnum", n)
  FigS19_names$genus[i] <- gsub('[[:digit:]]+', '', FigS19_names$genus[i])
  FigS19_names$genus[i] <- toTitleCase(FigS19_names$genus[i])
  test.genus <- FigS19_names$genus[i]
  for(j in 1:length(GenusNames)){
    ref.genus <- GenusNames[j]
    if(test.genus == ref.genus){
      FigS19_names$family[i] <- unique(BryophytePresence$Family[which(BryophytePresence$Genus == test.genus)])
      FigS19_names$order[i] <- unique(BryophytePresence$Order[which(BryophytePresence$Genus == test.genus)])
      FigS19_names$group[i] <- unique(BryophytePresence$Group[which(BryophytePresence$Genus == test.genus)])
      break
    }
  }
}

FigS19_FOG <- bind_cols(FigS19_FOG, FigS19_names)
write.csv(FigS19_FOG, "Data/FamilyTrees/FigS19_FOG.csv")

#FigS18
FigS18 <- read.tree("Data/trees/nt-nu-RAxML-FigS18.tre")
FigS18_FOG <- ggtree(FigS18)$data

FigS18_names <- data.frame(FigS18_FOG$label <- gsub("_", " ", FigS18_FOG$label))
names(FigS18_names)[1] <- "name"
FigS18_names <- separate(FigS18_names, name, into = c("genus","other"))
FigS18_names$other <- NULL
FigS18_names$family <- NA
FigS18_names$order <- NA
FigS18_names$group <- NA

for(i in 1:nrow(FigS18_names)){
  n <- FigS18_names$genus[i]
  FigS18_names$genus[i] <- ifelse(n == "sphagnumL" | n == "sphagnumP" | n == "sphagnumR", "Sphagnum", n)
  FigS18_names$genus[i] <- gsub('[[:digit:]]+', '', FigS18_names$genus[i])
  FigS18_names$genus[i] <- toTitleCase(FigS18_names$genus[i])
  test.genus <- FigS18_names$genus[i]
  for(j in 1:length(GenusNames)){
    ref.genus <- GenusNames[j]
    if(test.genus == ref.genus){
      FigS18_names$family[i] <- unique(BryophytePresence$Family[which(BryophytePresence$Genus == test.genus)])
      FigS18_names$order[i] <- unique(BryophytePresence$Order[which(BryophytePresence$Genus == test.genus)])
      FigS18_names$group[i] <- unique(BryophytePresence$Group[which(BryophytePresence$Genus == test.genus)])
      break
    }
  }
}

FigS18_FOG <- bind_cols(FigS18_FOG, FigS18_names)
write.csv(FigS18_FOG, "Data/FamilyTrees/FigS18_FOG.csv")

#FigS16
FigS16 <- read.nexus("Data/trees/nt-org-MrBayes-FigS16.tre")
FigS16_names <- data.frame(FigS16$taxlabels)
names(FigS16_names)[1] <- "name"
FigS16_names <- separate(FigS16_names, name, into = c("genus","other"))
FigS16_names$other <- NULL
FigS16_names$family <- NA
FigS16_names$order <- NA
FigS16_names$group <- NA

for(i in 1:nrow(FigS16_names)){
  n <- FigS16_names$genus[i]
  FigS16_names$genus[i] <- ifelse(n == "sphagnumL" | n == "sphagnumP" | n == "sphagnumR", "Sphagnum", n)
  FigS16_names$genus[i] <- gsub('[[:digit:]]+', '', FigS16_names$genus[i])
  FigS16_names$genus[i] <- toTitleCase(FigS16_names$genus[i])
  test.genus <- FigS16_names$genus[i]
  for(j in 1:length(GenusNames)){
    ref.genus <- GenusNames[j]
    if(test.genus == ref.genus){
      FigS16_names$family[i] <- unique(BryophytePresence$Family[which(BryophytePresence$Genus == test.genus)])
      FigS16_names$order[i] <- unique(BryophytePresence$Order[which(BryophytePresence$Genus == test.genus)])
      FigS16_names$group[i] <- unique(BryophytePresence$Group[which(BryophytePresence$Genus == test.genus)])
      break
    }
  }
}

write.csv(FigS16_names, "Data/FamilyTrees/FigS16_names.csv")

#FigS15
FigS15 <- read.tree("Data/trees/nt-org-RAxML-FigS15.tre")
FigS15_FOG <- ggtree(FigS15)$data

FigS15_names <- data.frame(FigS15_FOG$label <- gsub("_", " ", FigS15_FOG$label))
names(FigS15_names)[1] <- "name"
FigS15_names <- separate(FigS15_names, name, into = c("genus","other"))
FigS15_names$other <- NULL
FigS15_names$family <- NA
FigS15_names$order <- NA
FigS15_names$group <- NA

for(i in 1:nrow(FigS15_names)){
  n <- FigS15_names$genus[i]
  FigS15_names$genus[i] <- ifelse(n == "sphagnumL" | n == "sphagnumP" | n == "sphagnumR", "Sphagnum", n)
  FigS15_names$genus[i] <- gsub('[[:digit:]]+', '', FigS15_names$genus[i])
  FigS15_names$genus[i] <- toTitleCase(FigS15_names$genus[i])
  test.genus <- FigS15_names$genus[i]
  for(j in 1:length(GenusNames)){
    ref.genus <- GenusNames[j]
    if(test.genus == ref.genus){
      FigS15_names$family[i] <- unique(BryophytePresence$Family[which(BryophytePresence$Genus == test.genus)])
      FigS15_names$order[i] <- unique(BryophytePresence$Order[which(BryophytePresence$Genus == test.genus)])
      FigS15_names$group[i] <- unique(BryophytePresence$Group[which(BryophytePresence$Genus == test.genus)])
      break
    }
  }
}

FigS15_FOG <- bind_cols(FigS15_FOG, FigS15_names)
write.csv(FigS15_FOG, "Data/FamilyTrees/FigS15_FOG.csv")

#FigS8
FigS8 <- read.nexus("Data/trees/nt-pt-MrBayes-FigS8.tre")
FigS8_names <- data.frame(FigS8$taxlabels)
names(FigS8_names)[1] <- "name"
FigS8_names <- separate(FigS8_names, name, into = c("genus","other"))
FigS8_names$other <- NULL
FigS8_names$family <- NA
FigS8_names$order <- NA
FigS8_names$group <- NA

for(i in 1:nrow(FigS8_names)){
  n <- FigS8_names$genus[i]
  FigS8_names$genus[i] <- ifelse(n == "sphagnumL" | n == "sphagnumP" | n == "sphagnumR", "Sphagnum", n)
  FigS8_names$genus[i] <- gsub('[[:digit:]]+', '', FigS8_names$genus[i])
  FigS8_names$genus[i] <- toTitleCase(FigS8_names$genus[i])
  test.genus <- FigS8_names$genus[i]
  for(j in 1:length(GenusNames)){
    ref.genus <- GenusNames[j]
    if(test.genus == ref.genus){
      FigS8_names$family[i] <- unique(BryophytePresence$Family[which(BryophytePresence$Genus == test.genus)])
      FigS8_names$order[i] <- unique(BryophytePresence$Order[which(BryophytePresence$Genus == test.genus)])
      FigS8_names$group[i] <- unique(BryophytePresence$Group[which(BryophytePresence$Genus == test.genus)])
      break
    }
  }
}

write.csv(FigS8_names, "Data/FamilyTrees/FigS8_names.csv")

#FigS7
FigS7 <- read.tree("Data/trees/nt-pt-RAxML-FigS7.tre")
FigS7_FOG <- ggtree(FigS7)$data

FigS7_names <- data.frame(FigS7_FOG$label <- gsub("_", " ", FigS7_FOG$label))
names(FigS7_names)[1] <- "name"
FigS7_names <- separate(FigS7_names, name, into = c("genus","other"))
FigS7_names$other <- NULL
FigS7_names$family <- NA
FigS7_names$order <- NA
FigS7_names$group <- NA

for(i in 1:nrow(FigS7_names)){
  n <- FigS7_names$genus[i]
  FigS7_names$genus[i] <- ifelse(n == "sphagnumL" | n == "sphagnumP" | n == "sphagnumR", "Sphagnum", n)
  FigS7_names$genus[i] <- gsub('[[:digit:]]+', '', FigS7_names$genus[i])
  FigS7_names$genus[i] <- toTitleCase(FigS7_names$genus[i])
  test.genus <- FigS7_names$genus[i]
  for(j in 1:length(GenusNames)){
    ref.genus <- GenusNames[j]
    if(test.genus == ref.genus){
      FigS7_names$family[i] <- unique(BryophytePresence$Family[which(BryophytePresence$Genus == test.genus)])
      FigS7_names$order[i] <- unique(BryophytePresence$Order[which(BryophytePresence$Genus == test.genus)])
      FigS7_names$group[i] <- unique(BryophytePresence$Group[which(BryophytePresence$Genus == test.genus)])
      break
    }
  }
}

FigS7_FOG <- bind_cols(FigS7_FOG, FigS7_names)
write.csv(FigS7_FOG, "Data/FamilyTrees/FigS7_FOG.csv")


# 4.0 Compare families in each tree --------------
F8Fam <- data.frame(unique(FigS8_names$family))
names(F8Fam)[1] <- "Family"
F8Fam$Tree <- "F8"
LiuTreeFam <- F8Fam
F7Fam <- data.frame(unique(FigS7_names$family))
names(F7Fam)[1] <- "Family"
F7Fam$Tree <- "F7"
LiuTreeFam <- bind_rows(LiuTreeFam, F7Fam)
F25Fam <- data.frame(unique(FigS25_names$family))
names(F25Fam)[1] <- "Family"
F25Fam$Tree <- "F25"
LiuTreeFam <- bind_rows(LiuTreeFam, F25Fam)
F24Fam <- data.frame(unique(FigS24_names$family))
names(F24Fam)[1] <- "Family"
F24Fam$Tree <- "F24"
LiuTreeFam <- bind_rows(LiuTreeFam, F24Fam)
F22Fam <- data.frame(unique(FigS22_names$family))
names(F22Fam)[1] <- "Family"
F22Fam$Tree <- "F22"
LiuTreeFam <- bind_rows(LiuTreeFam, F22Fam)
F21Fam <- data.frame(unique(FigS21_names$family))
names(F21Fam)[1] <- "Family"
F21Fam$Tree <- "F21"
LiuTreeFam <- bind_rows(LiuTreeFam, F21Fam)
F20Fam <- data.frame(unique(FigS20_names$family))
names(F20Fam)[1] <- "Family"
F20Fam$Tree <- "F20"
LiuTreeFam <- bind_rows(LiuTreeFam, F20Fam)
F19Fam <- data.frame(unique(FigS19_names$family))
names(F19Fam)[1] <- "Family"
F19Fam$Tree <- "F19"
LiuTreeFam <- bind_rows(LiuTreeFam, F19Fam)
F18Fam <- data.frame(unique(FigS18_names$family))
names(F18Fam)[1] <- "Family"
F18Fam$Tree <- "F18"
LiuTreeFam <- bind_rows(LiuTreeFam, F18Fam)
F16Fam <- data.frame(unique(FigS16_names$family))
names(F16Fam)[1] <- "Family"
F16Fam$Tree <- "F16"
LiuTreeFam <- bind_rows(LiuTreeFam, F16Fam)
F15Fam <- data.frame(unique(FigS15_names$family))
names(F15Fam)[1] <- "Family"
F15Fam$Tree <- "F15"
LiuTreeFam <- bind_rows(LiuTreeFam, F15Fam)
F14Fam <- data.frame(unique(FigS14_FOG$family))
names(F14Fam)[1] <- "Family"
F14Fam$Tree <- "F14"
LiuTreeFam <- bind_rows(LiuTreeFam, F14Fam)
F13Fam <- data.frame(unique(FigS13_names$family))
names(F13Fam)[1] <- "Family"
F13Fam$Tree <- "F13"
LiuTreeFam <- bind_rows(LiuTreeFam, F13Fam)
F12Fam <- data.frame(unique(FigS12_names$family))
names(F12Fam)[1] <- "Family"
F12Fam$Tree <- "F12"
LiuTreeFam <- bind_rows(LiuTreeFam, F12Fam)
F11Fam <- data.frame(unique(FigS11_names$family))
names(F11Fam)[1] <- "Family"
F11Fam$Tree <- "F11"
LiuTreeFam <- bind_rows(LiuTreeFam, F11Fam)
F10Fam <- data.frame(unique(FigS10_names$family))
names(F10Fam)[1] <- "Family"
F10Fam$Tree <- "F10"
LiuTreeFam <- bind_rows(LiuTreeFam, F10Fam)

length(unique(LiuTreeFam$Family))
unique(LiuTreeFam$Family)

length(FamilyNames)

write.csv(LiuTreeFam, "Data/FamilyTrees/LiuTreeFam.csv")


# 5.0 Compare orders in each tree ----------------
F8Order <- data.frame(unique(FigS8_names$order))
names(F8Order)[1] <- "order"
F8Order$Tree <- "F8"
LiuTreeOrder <- F8Order
F7Order <- data.frame(unique(FigS7_names$order))
names(F7Order)[1] <- "order"
F7Order$Tree <- "F7"
LiuTreeOrder <- bind_rows(LiuTreeOrder, F7Order)
F25Order <- data.frame(unique(FigS25_names$order))
names(F25Order)[1] <- "order"
F25Order$Tree <- "F25"
LiuTreeOrder <- bind_rows(LiuTreeOrder, F25Order)
F24Order <- data.frame(unique(FigS24_names$order))
names(F24Order)[1] <- "order"
F24Order$Tree <- "F24"
LiuTreeOrder <- bind_rows(LiuTreeOrder, F24Order)
F22Order <- data.frame(unique(FigS22_names$order))
names(F22Order)[1] <- "order"
F22Order$Tree <- "F22"
LiuTreeOrder <- bind_rows(LiuTreeOrder, F22Order)
F21Order <- data.frame(unique(FigS21_names$order))
names(F21Order)[1] <- "order"
F21Order$Tree <- "F21"
LiuTreeOrder <- bind_rows(LiuTreeOrder, F21Order)
F20Order <- data.frame(unique(FigS20_names$order))
names(F20Order)[1] <- "order"
F20Order$Tree <- "F20"
LiuTreeOrder <- bind_rows(LiuTreeOrder, F20Order)
F19Order <- data.frame(unique(FigS19_names$order))
names(F19Order)[1] <- "order"
F19Order$Tree <- "F19"
LiuTreeOrder <- bind_rows(LiuTreeOrder, F19Order)
F18Order <- data.frame(unique(FigS18_names$order))
names(F18Order)[1] <- "order"
F18Order$Tree <- "F18"
LiuTreeOrder <- bind_rows(LiuTreeOrder, F18Order)
F16Order <- data.frame(unique(FigS16_names$order))
names(F16Order)[1] <- "order"
F16Order$Tree <- "F16"
LiuTreeOrder <- bind_rows(LiuTreeOrder, F16Order)
F15Order <- data.frame(unique(FigS15_names$order))
names(F15Order)[1] <- "order"
F15Order$Tree <- "F15"
LiuTreeOrder <- bind_rows(LiuTreeOrder, F15Order)
F14Order <- data.frame(unique(FigS14_FOG$order))
names(F14Order)[1] <- "order"
F14Order$Tree <- "F14"
LiuTreeOrder <- bind_rows(LiuTreeOrder, F14Order)
F13Order <- data.frame(unique(FigS13_names$order))
names(F13Order)[1] <- "order"
F13Order$Tree <- "F13"
LiuTreeOrder <- bind_rows(LiuTreeOrder, F13Order)
F12Order <- data.frame(unique(FigS12_names$order))
names(F12Order)[1] <- "order"
F12Order$Tree <- "F12"
LiuTreeOrder <- bind_rows(LiuTreeOrder, F12Order)
F11Order <- data.frame(unique(FigS11_names$order))
names(F11Order)[1] <- "order"
F11Order$Tree <- "F11"
LiuTreeOrder <- bind_rows(LiuTreeOrder, F11Order)
F10Order <- data.frame(unique(FigS10_names$order))
names(F10Order)[1] <- "order"
F10Order$Tree <- "F10"
LiuTreeOrder <- bind_rows(LiuTreeOrder, F10Order)

length(unique(LiuTreeOrder$order))
length(OrderNames)

write.csv(LiuTreeOrder, "Data/FamilyTrees/LiuTreeOrder.csv")
