#Plot Liu et al. 2019 Trees
#Hailey  Napier
#July 2020

# THIS SCRIPT IS CONTINUED FROM Liu_et_al2019Trees.R


#Load Packages
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

library(RColorBrewer)
library(wesanderson)

# 6.0 Plot trees with new information --------------------
test_F11Tree <- FigS11_FOG
test_F11Tree$label <- test_F11Tree$family
test_F11Tree$genus <- NULL
test_F11Tree$group <- NULL
test_F11Tree$order <- NULL
test_F11Tree$family <- NULL

test_F11Tree_node <- test_F11Tree %>%
  filter(isTip == "FALSE")
test_F11Tree_tip <- test_F11Tree %>%
  filter(isTip == "TRUE") %>%
  filter(label != "NA")
test_F11Tree_pruned <- bind_rows(test_F11Tree_node, test_F11Tree_tip)

ggtree(test_F11Tree_pruned) + 
  geom_tiplab(size = 2.0)

ggtree(test_F11Tree) +
  geom_tiplab(size = 2.0) 

to_drop <- "NA"
test_F11Tree_drop <- drop.tip(test_F11Tree, to_drop)

#try to make data.frame into tree object
phylo4d(test_F11Tree)

get.tree(test_F11Tree)

as.phylo.data.frame(test_F11Tree)


#make tree from $data
test <- read.tree("Data/trees/nt-mt-RAxML-FigS11.tre")
ggtree(test)$data
test$tip.label

#this works!
test_tree <- get.tree(ggtree(test)$data)
ggtree(test_tree) +
  geom_tiplab(size = 2.0)
#this works too, but it's basically the same thing
test_tree <- ggtree(test)$data
get.tree(test_tree)
ggtree(test_tree) +
  geom_tiplab(size = 2.0)

#says it's not a tree, so can't make it a data.frame
test_tree.df <- data.frame(ggtree(test)$data)
test_get <- get.tree(test_tree.df)

#doesn't work
#calling ggtree here seems to shut down R, so that's not good
##it looks like instead of replacing just the tip labels it ends up replacing all the data --- 
##don't know why that shuts down R, seems like it could just say it doesn't work, but I guess it might be something
##about it being in the same format as a tree that should work.
clean11tiplabel <- test_F11Tree %>%
  filter(isTip == "TRUE") %>%
  select(label)
clean11label <- test_F11Tree %>%
  select(label)
clean11label
test$tip.label
#test$tip.label <- clean11label
#ggtree(test) +
#geom_tiplab(size = 2.0)

#use treeio to rename taxa?
lab <- data.frame(test$tip.label)
names(lab)[1] <- "tip.label"
lab$lab2 <- clean11tiplabel
#this also crashed R
##for some reason the rename_taxa sets the tip labels to nothing
test_rename <- rename_taxa(test, lab, lab1, lab2.label)
#ggtree(test_rename)

#try a different method --> store new tip labels as a tip annotation 
newlabs <- data.frame(ggtree(test)$data[4])
newlabs$lab2 <- clean11label
test2 <- full_join(test, newlabs, by = "label")
test2

#this crashed R
#ggtree(test2) + 
#geom_tiplab

#Try another way 
#hmmm, this isn't working either
test <- read.tree("Data/trees/nt-mt-RAxML-FigS11.tre")
newlabs <- data.frame(label = ggtree(test)$data[4])
newlabs$lab2 <- clean11label

#crazy tree taken over by the black death
ggtree(test) %<+% newlabs + 
  geom_tiplab(aes(label = lab2))

###########################################
#WORKING WITH KATHRYN'S CODE ------
#went through LiuTreeFam and FigS11_FOG manually to identify which species are in what family
#then type that into the top line of code
#TO DO
#1 make a program that assigns a family to each species in the data --- DONE
#2 add maps to tree -- DONE
#3 make tree work for multiple families --- DONE

Pterobryaceae <- c("Jaegerina_solitaria", "Calyptothecium_pinnatum_3742")
Pterobryaceae11 <- ggtree(F11tree, branch.length="none") + 
  geom_tiplab(aes(color = label %in% Pterobryaceae), size = 0.7) +
  geom_text2(aes(subset=!isTip, label=node), hjust=-.3, size=2)
Pterobryaceae11

###########################################




# 1. Program that assigns a family to each species in the data ##########
#get family names
tree20 <- read.tree("Data/trees/aa-nu-RAxML-FigS20.tre")
clean20famlabels <- FigS20_FOG %>%
  filter(isTip == "TRUE") %>%
  select(family)
Ref20DF <- data.frame(tree20$tip.label)
Ref20DF$Family <- clean20famlabels
names(Ref20DF)[1] <- "Species"

#find species in Pterobryaceae
Pterobryaceae <- Ref20DF %>%
  filter(Family == "Pterobryaceae")
Pterobryaceae <- Pterobryaceae$Species
Pterobryaceae

#loop to find species in each family -- creates a vector of species names for each family name (does first line of Kathryn's code) 
tree20fam <- F20Fam$Family
tree20fam
for(i in 1:length(tree20fam)){
  fam <- tree20fam[i]
  tempdf <- Ref20DF %>%
    filter(Family == fam)
  tempvec <- tempdf$Species
  assign(fam, tempvec)
}


# 2. Make the tree work with multiple families ###############
#same color for multiple families
PterobryaceaeBryaceae11 <- ggtree(tree11, branch.length="none") + 
  geom_tiplab(aes(color = label %in% c(Pterobryaceae, Bryaceae)), size = 0.7) +
  geom_text2(aes(subset=!isTip, label=node), hjust=-.3, size=2)
PterobryaceaeBryaceae11

PterobryaceaeBryaceae11diffcol <- ggtree(tree11, branch.length="none") + 
  geom_tiplab(aes(color = label %in% c(Pterobryaceae, Bryaceae)) , size = 0.7) +
  geom_text2(aes(subset=!isTip, label=node), hjust=-.3, size=2) +
  scale_color_manual(values = c("gray63", "cyan4"))
PterobryaceaeBryaceae11diffcol


# 3. Do it with orders!#######################################
#Program that assigns an order to each species in the data
#get order names
tree20 <- read.tree("Data/trees/aa-nu-RAxML-FigS20.tre")
ggtree20 <- ggtree(tree20)
clean20orderlabels <- FigS20_FOG %>%
  filter(FigS20_FOG$isTip == "TRUE") %>%
  select(order)
Ref20DF <- data.frame(tree20$tip.label)
Ref20DF$Family <- clean20famlabels
Ref20DF$Order <- clean20orderlabels
names(Ref20DF)[1] <- "Species"

#loop to find species in each order -- creates a vector of species names for each order name (does first line of Kathryn's code, for orders) 
tree20order <- unique(FigS20_FOG$order)
tree20order <- tree20order[complete.cases(tree20order)]
tree20order
tree20orderlist <- list()
for(i in 1:length(tree20order)){
  order <- tree20order[i]
  tempdf <- Ref20DF %>%
    filter(Order == order)
  tempvec <- tempdf$Species
  tree20orderlist[[i]] <- tempvec
}

# 4. Use function to find MRCA##############################
# 4.1 ORDERS  #
#Make a loop that goes through each  order and finds the most recent common ancestor then puts it into a data.frame
Tree20MRCA <- data.frame(Order = NA, Node = NA)
for(i in 1:length(tree20order)){
  order <- tree20order[i]
  tiplabs <- tree20orderlist[[i]]
  node <- MRCA(tree20, tiplabs)
  tempdf <- data.frame(Order = order, Node = node)
  Tree20MRCA <- bind_rows(Tree20MRCA, tempdf)
}
#get rid of NAs
Tree20MRCA <- Tree20MRCA[complete.cases(Tree20MRCA),]

#  4.2 FAMILIES #
#make a list of vectors of species in each family
tree20fam <- unique(FigS20_FOG$family)
tree20fam <- tree20fam[complete.cases(tree20fam)]
tree20fam
tree20famlist <- list()
for(i in 1:length(tree20fam)){
  fam <- tree20fam[i]
  tempdf <- Ref20DF %>%
    filter(Family == fam)
  tempvec <- tempdf$Species
  tree20famlist[[i]] <- tempvec
}

Tree20FamMRCA <- data.frame(Family = NA, Node = NA)
for(i in 1:length(tree20fam)){
  fam <- tree20fam[i]
  tiplabs <- tree20famlist[[i]]
  node <- MRCA(tree20, tiplabs)
  tempdf <- data.frame(Family = fam, Node = node)
  Tree20FamMRCA <- bind_rows(Tree20FamMRCA, tempdf)
}
#get rid of NAs
Tree20FamMRCA <- Tree20FamMRCA[complete.cases(Tree20FamMRCA),]


# 4.3 Plot family MRCAs #
df <- data.frame(Tree20FamMRCA$Node)
names(df)[1] <- "node"
mrcanodes <- Tree20FamMRCA$Node

ggtree(tree20, branch.length="none") + 
  geom_tiplab(size = 0.7) +
  geom_point2(aes(subset=node==mrcanodes[1]), color = "orangered", size=0.5) +
  geom_point2(aes(subset=node==mrcanodes[2]), color = "orangered", size=0.5) +
  geom_point2(aes(subset=node==mrcanodes[3]), color = "orangered", size=0.5) +
  geom_point2(aes(subset=node==mrcanodes[4]), color = "orangered", size=0.5) +
  geom_point2(aes(subset=node==mrcanodes[5]), color = "orangered", size=0.5) +
  geom_point2(aes(subset=node==mrcanodes[6]), color = "orangered", size=0.5) +
  geom_point2(aes(subset=node==mrcanodes[7]), color = "orangered", size=0.5) +
  geom_point2(aes(subset=node==mrcanodes[8]), color = "orangered", size=0.5) +
  geom_point2(aes(subset=node==mrcanodes[9]), color = "orangered", size=0.5) +
  geom_point2(aes(subset=node==mrcanodes[10]), color = "orangered", size=0.5) +
  geom_point2(aes(subset=node==mrcanodes[11]), color = "orangered", size=0.5) +
  geom_point2(aes(subset=node==mrcanodes[12]), color = "orangered", size=0.5) +
  geom_point2(aes(subset=node==mrcanodes[13]), color = "orangered", size=0.5) +
  geom_point2(aes(subset=node==mrcanodes[14]), color = "orangered", size=0.5) +
  geom_point2(aes(subset=node==mrcanodes[15]), color = "orangered", size=0.5) +
  geom_point2(aes(subset=node==mrcanodes[16]), color = "orangered", size=0.5) +
  geom_point2(aes(subset=node==mrcanodes[17]), color = "orangered", size=0.5) +
  geom_point2(aes(subset=node==mrcanodes[18]), color = "orangered", size=0.5) +
  geom_point2(aes(subset=node==mrcanodes[19]), color = "orangered", size=0.5) +
  geom_point2(aes(subset=node==mrcanodes[20]), color = "orangered", size=0.5) +
  geom_point2(aes(subset=node==mrcanodes[21]), color = "orangered", size=0.5) +
  geom_point2(aes(subset=node==mrcanodes[22]), color = "orangered", size=0.5) +
  geom_point2(aes(subset=node==mrcanodes[23]), color = "orangered", size=0.5) +
  geom_point2(aes(subset=node==mrcanodes[24]), color = "orangered", size=0.5) +
  geom_point2(aes(subset=node==mrcanodes[25]), color = "orangered", size=0.5) +
  geom_point2(aes(subset=node==mrcanodes[26]), color = "orangered", size=0.5) +
  geom_point2(aes(subset=node==mrcanodes[27]), color = "orangered", size=0.5) +
  geom_point2(aes(subset=node==mrcanodes[28]), color = "orangered", size=0.5) +
  geom_point2(aes(subset=node==mrcanodes[29]), color = "orangered", size=0.5) +
  geom_point2(aes(subset=node==mrcanodes[30]), color = "orangered", size=0.5) +
  geom_point2(aes(subset=node==mrcanodes[31]), color = "orangered", size=0.5) +
  geom_point2(aes(subset=node==mrcanodes[32]), color = "orangered", size=0.5) +
  geom_point2(aes(subset=node==mrcanodes[33]), color = "orangered", size=0.5) +
  geom_point2(aes(subset=node==mrcanodes[34]), color = "orangered", size=0.5) +
  geom_point2(aes(subset=node==mrcanodes[35]), color = "orangered", size=0.5) +
  geom_point2(aes(subset=node==mrcanodes[36]), color = "orangered", size=0.5) +
  geom_point2(aes(subset=node==mrcanodes[37]), color = "orangered", size=0.5) +
  geom_point2(aes(subset=node==mrcanodes[38]), color = "orangered", size=0.5) +
  geom_point2(aes(subset=node==mrcanodes[39]), color = "orangered", size=0.5) 

# this kind of works, but I can't seem to change the size/color of the points
p <- ggtree(tree20, branch.length = "none") + 
  geom_tiplab(size = 0.7)


# 5. Making family and order trees##########################
#Make order tree with different colored branches by order
#using manually gathered nodes
df <- data.frame(node = c(272,197,262,231,218,249,239,261,268,269,200),
                 images = c("Figures/TreeMaps/Splachnales.png",
                            "Figures/TreeMaps/Rhizogoniales.png",
                            "Figures/TreeMaps/Bryales.png",
                            "Figures/TreeMaps/Hookeriales.png",
                            "Figures/TreeMaps/Hypnales.png",
                            "Figures/TreeMaps/Hypnodendrales.png",
                            "Figures/TreeMaps/Ptychomniales.png",
                            "Figures/TreeMaps/Aulacomniales.png",
                            "Figures/TreeMaps/Bartramiales.png",
                            "Figures/TreeMaps/Hedwigiales.png",
                            "Figures/TreeMaps/Orthotrichales.png"))

tree2 <- groupClade(tree20, c(272, 197, 262, 231, 218, 249, 239, 261, 268, 269, 200))

ggtree(tree2, branch.length = "none", aes(color=group)) %<+% df +
  geom_cladelabel(node=272, label="Splachnales", fontsize=2) +
  geom_cladelabel(node=197, label="Rhizogoniales", fontsize=2) +
  geom_cladelabel(node=262, label="Bryales", fontsize=2) +
  geom_cladelabel(node=231, label="Hookeriales", fontsize=2) +
  geom_cladelabel(node=218, label="Hypnales", fontsize=2) +
  geom_cladelabel(node=249, label="Hypnodendrales", fontsize=2) +
  geom_cladelabel(node=239, label="Ptychomniales", fontsize=2) +
  geom_cladelabel(node=261, label="Aulacomniales", fontsize=2) +
  geom_cladelabel(node=268, label="Bartramiales", fontsize=2) +
  geom_cladelabel(node=269, label="Hedwigiales", fontsize=2) +
  geom_cladelabel(node=200, label="Orthotrichales", fontsize=2) +
  theme(legend.position = "none") + 
  geom_nodelab(aes(image=images), geom="image", image_fun = function(.) magick::image_transparent(., "white")) 

#Highlight clades
tree <- ggtree(tree2, branch.length = "none") %<+% df + xlim(NA,38) +
  geom_cladelabel(node=272, label="Splachnales", fontsize=2) +
  geom_cladelabel(node=197, label="Rhizogoniales", fontsize=2) +
  geom_cladelabel(node=262, label="Bryales", fontsize=2) +
  geom_cladelabel(node=231, label="Hookeriales", fontsize=2) +
  geom_cladelabel(node=218, label="Hypnales", fontsize=2) +
  geom_cladelabel(node=249, label="Hypnodendrales", fontsize=2, col = "steelblue") +
  geom_cladelabel(node=239, label="Ptychomniales", fontsize=2) +
  geom_cladelabel(node=261, label="Aulacomniales", fontsize=2) +
  geom_cladelabel(node=268, label="Bartramiales", fontsize=2) +
  geom_cladelabel(node=269, label="Hedwigiales", fontsize=2, col = "darkgreen") +
  geom_cladelabel(node=200, label="Orthotrichales", fontsize=2) +
  geom_nodelab(aes(image=images), geom="image", image_fun = function(.) magick::image_transparent(., "white")) 

tree +
  geom_hilight(249,fill = "steelblue") +
  geom_hilight(269, fill = "darkgreen") 

tree +
  geom_balance(node=249, fill='steelblue', alpha=0.6, extend=1) +
  geom_balance(node=269, fill='darkgreen', alpha=0.6, extend=1) 


##FINAL ORDER TREE & MAP CODE##
#manually assign order nodes
nodes <- subset(FigS20_FOG, order == "Hypopterygiales")
nodes <- nodes$node
nodes

p <- ggtree(tree20, branch.length = "none") + 
  geom_tiplab(size = 0.7) + geom_text2(aes(subset=!isTip, label=node), hjust=-.3, size=2)
for(i in 1:length(nodes)){
  n <- nodes[i]
  loop_input = paste("geom_point2(aes(subset=node==",n," ,size = 1))", sep="")
  p <- p + eval(parse(text=loop_input))
}
p

#Make highlighted and clade labeled tree
OrderNodes20 <- read.csv("./Data/OrderNodesS20.csv")
getPalette <- colorRampPalette(brewer.pal(8, "Dark2"))
OrderNodes20$hexcols <- getPalette(nrow(OrderNodes20))
tree <- ggtree(tree2, branch.length = "none") + xlim(NA,38) 
for(i in 1:nrow(OrderNodes20)){
  n <- OrderNodes20$Node[i]
  o <- OrderNodes20$Order[i]
  c <- OrderNodes20$hexcols[i]
  tree <- tree + geom_cladelabel(node= n, label=o, fontsize=2, col = c) + geom_hilight(n, fill=c)
}
tree

#Make new maps
dir.create("Figures/TreeMaps/Tree20OrderMaps")
#Find the index numbers for the orders in tree20
tree20index <- match(OrderNodes20$Order, OrderNames)
tree20index <- tree20index[complete.cases(tree20index)]
tree20index <- data.frame(tree20index)
names(tree20index)[1] <- "index"
Order <- OrderNodes20$Order
Order <- Order[complete.cases(Order)]
tree20index$name <- Order
tree20index$number <- c(1,2,3,4,5,6,7,8,9,10,11,21,20,18,13,14,12,15,16,17,19,22)

for(i in 1:nrow(tree20index)){
  index <- tree20index$index[which(tree20index$number == i)]
  
  TempOrderRichnessRaster <- setValues(BlankRas, OrderRichList[[index]])
  TempOrderDF <- rasterToPoints(TempOrderRichnessRaster)
  TempOrderDF <- data.frame(TempOrderDF)
  colnames(TempOrderDF) <- c("Longitude", "Latitude", "Alpha")
  
  
  Map <- ggplot() + geom_tile(data=TempOrderDF, aes(x=Longitude, y=Latitude, fill=Alpha)) +   
    scale_fill_gradientn(name="α diversity", colours=cols, na.value="transparent", limits = c(0, 200)) +
    coord_equal() +
    geom_sf(data = nw_bound_sf, size = 1.0, fill=NA) + 
    geom_sf(data = nw_mount_sf, size = 0.5, fill=NA) + theme_void() + 
    labs(title = OrderNames[index]) + 
    theme(legend.text=element_text(size=20), legend.title=element_text(size=32), plot.title = element_text(size = 40, hjust = 0.5))
  filename <- paste("./Figures/TreeMaps/Tree20OrderMaps/treemap", i, ".png", sep="")
  png(filename, width= 1000, height = 1000, pointsize = 30)
  print({Map})
  dev.off()
}

rl = lapply(sprintf("Figures/TreeMaps/Tree20OrderMaps/treemap%i.png", 1:22), png::readPNG)
gl = lapply(rl, grid::rasterGrob)
gridExtra::grid.arrange(grobs=gl)


#FINAL FAMILY TREE & MAP CODE##
#manually assign family nodes
nodes <- subset(FigS20_FOG, family == "Hookeriaceae")
nodes <- nodes$node
nodes

p <- ggtree(tree20, branch.length = "none") + 
  geom_tiplab(size = 0.7) + geom_text2(aes(subset=!isTip, label=node), hjust=-.3, size=2)
for(i in 1:length(nodes)){
  n <- nodes[i]
  loop_input = paste("geom_point2(aes(subset=node==",n," ,size = 1))", sep="")
  p <- p + eval(parse(text=loop_input))
}
p

#Make highlighted and clade labeled tree
famnode <- FamilyNodes20$Node
famnode

FamilyNodes20 <- read.csv("./Data/FamilyNodesS20.csv")

getPalette <- colorRampPalette(brewer.pal(8, "Dark2"))
FamilyNodes20$hexcols <- getPalette(nrow(FamilyNodes20))
tree <- ggtree(tree20, branch.length = "none") + xlim(NA,38) 
for(i in 1:nrow(FamilyNodes20)){
  n <- FamilyNodes20$Node[i]
  o <- FamilyNodes20$Family[i]
  c <- FamilyNodes20$hexcols[i]
  tree <- tree + geom_cladelabel(node= n, label=o, fontsize=1.5, col = c) + geom_hilight(n, fill=c)
}
tree

#Make new maps
dir.create("Figures/TreeMaps/Tree20FamMaps")
#Find the index numbers for the families in tree20
tree20index <- match(FamilyNodes20$Family, FamilyNames)
tree20index <- tree20index[complete.cases(tree20index)]
tree20index <- data.frame(tree20index)
names(tree20index)[1] <- "index"
Family <- FamilyNodes20$Family
Family <- Family[complete.cases(Family)]
tree20index$name <- Family

tree20index$number <- c(3,2,1,9,8,13,14,12,23,25,20,21,19,17,18,24,22,30,31,32,33,37,36,35,34,26,27,29,28,16,15,11,10,7,5,6,4)

for(i in 1:nrow(tree20index)){
  index <- tree20index$index[which(tree20index$number == i)]
  
  TempFamRichnessRaster <- setValues(BlankRas, FamRichList[[index]])
  TempFamDF <- rasterToPoints(TempFamRichnessRaster)
  TempFamDF <- data.frame(TempFamDF)
  colnames(TempFamDF) <- c("Longitude", "Latitude", "Alpha")
  
  
  Map <- ggplot() + geom_tile(data=TempFamDF, aes(x=Longitude, y=Latitude, fill=Alpha)) +   
    scale_fill_gradientn(name="α diversity", colours=cols, na.value="transparent", limits = c(0, 100)) +
    coord_equal() +
    geom_sf(data = nw_bound_sf, size = 1.0, fill=NA) + 
    geom_sf(data = nw_mount_sf, size = 0.5, fill=NA) + theme_void() + 
    labs(title = FamilyNames[index]) + 
    theme(legend.text=element_text(size=20), legend.title=element_text(size=32), plot.title = element_text(size = 40, hjust = 0.5))
  filename <- paste("./Figures/TreeMaps/Tree20FamMaps/treemap", i, ".png", sep="")
  png(filename, width= 1000, height = 1000, pointsize = 30)
  print({Map})
  dev.off()
}


rl = lapply(sprintf("Figures/TreeMaps/Tree20FamMaps/treemap%i.png", 1:nrow(tree20index)), png::readPNG)
gl = lapply(rl, grid::rasterGrob)
gridExtra::grid.arrange(grobs=gl)






##### 10/3/20 addition for presentation
#Make moss only tree with highlighted order clade labels
OrderNodes20 <- read.csv("./Data/OrderNodesS20.csv")
MossOrderNames <- readRDS("./Data/MossOrderNames.rds")

MossOrderNodes20 <- OrderNodes20 %>%
  filter(Order %in% MossOrderNames)
getPalette <- (wes_palette("Darjeeling1", 20, type = "continuous"))
for(i in 1:nrow(MossOrderNodes20)){
  MossOrderNodes20$hexcols[i] <- getPalette[i]
}

tree20 <- read.tree("Data/trees/aa-nu-RAxML-FigS20.tre")
tree2 <- ggtree::groupClade(tree20, c(272, 197, 262, 231, 218, 249, 239, 261, 268, 269, 200))
tree <- ggtree(tree2, branch.length = "none") + xlim(NA,38) 

for(i in 1:nrow(MossOrderNodes20)){
  n <- MossOrderNodes20$Node[i]
  o <- MossOrderNodes20$Order[i]
  c <- MossOrderNodes20$hexcols[i]
  tree <- tree + geom_cladelabel(node=n, label=o, fontsize=3, col=c) + geom_hilight(n, fill=c)
}
tree


