#Figuring out how to make phylogenetic trees with our maps
#Using Liu et al. 2019 tree data
#Kathryn Dawdy and Hailey Napier, July 2020

#Install ggtree
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("ggtree")

#install treeio (I don't think we need this but just in case)
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

#BiocManager::install("treeio")

#Load packages
require(ape)
require(phytools)
require(ggtree)
require(ggplot2)
require(ggimage)
require(treeio)
require(tidyr)
require(dplyr)


# 1.0 test trees -------------------------------------------------------
# 1.1 loading test trees -----------------------------------------------
testtree <- read.tree(file= "Data/trees/aa-nu-astral-mlbs-FigS25.tre")

testtreetwo <- read.nexus(file = "Data/trees/aa-mt-phylobayes-FigS14.tree")

# 1.2 plotting test trees ----------------------------------------------
## initial plot
plot(testtree, no.margin=TRUE, edge.width = 2)
## unrooted tree
plot(unroot(testtree),type="unrooted",no.margin=TRUE,lab4ut="axial",
     edge.width=2)

## using plotTree
plotTree(testtree, fsize = 0.3)

## using ggtree
ggtree(testtree)


# 1.3 Hailey's code ----------------------------------------------------
## testtreetwo
testtreetwo <- read.nexus(file = "Data/trees/aa-mt-phylobayes-FigS14.tree")
ggtree(testtreetwo, branch.length = "none") +
  geom_tiplab(size = 1.5) +
  ggplot2::xlim(0,40) 

## random tree to paste maps onto tree
set.seed(1982)
randomtree <- rtree(5)
ggtree(randomtree) + 
  geom_tiplab(image = "Figures/FamRichnessMap.png", geom = "image", size = .12) + 
  ggplot2::xlim(0,2)

## family node tree
FigS11_FOG <- read.csv("Data/FamilyTrees/FigS11_FOG.csv")
test_F11Tree <- FigS11_FOG
test_F11Tree$label <- test_F11Tree$family
test_F11Tree$genus <- NULL
test_F11Tree$group <- NULL
test_F11Tree$order <- NULL
test_F11Tree$family <- NULL
ggtree(test_F11Tree) +
  geom_tiplab(size = 2.0) + 
  geom_cladelabel(node=35, image = "/Figures/TreeMaps/Dicranales", label="image", angle=0, 
                  fontsize=8, offset=.5, vjust=.5)
 
# 1.4 Trying to get text and maps on tree ------------------------------
testtreetwo <- read.nexus(file = "Data/trees/aa-mt-phylobayes-FigS14.tree")
ggtree(testtreetwo, branch.length = "none") +
  geom_tiplab(size = 0.7) +
  ggplot2::xlim(0,40) +
  geom_tiplab(image = "Figures/FamRichnessMap.png", geom = "image", size = .01) 

# 1.5 Annotate tree with images (test with 2 families) -----------------
nwk <- "(OneScaleRichMap_Allisoniaceae, OneScaleRichMap_Fossombroniaceae);"
x = read.tree(text = nwk)
ggtree(x) + xlim(NA, 7) +
  geom_tiplab(aes(image=paste0("Figures/OneScale_RichByFamMaps/Liverworts/Fossombroniales/", label, '.png')),
              geom="image", offset=4, align=5, size=.3) +
  geom_tiplab(geom='label', offset=1, hjust=.1, size=3)

# 1.6 Annotate with clade labels
#trying to annotate clades
set.seed(2015-12-21)
tree <- rtree(30)
p <- ggtree(tree) + xlim(NA, 6)

p + geom_cladelabel(node=45, label="test label") +
  geom_cladelabel(node=34, label="another clade")

# Look at tree info in table -------------------------------------------
ggtree(testtreetwo)$data

# 2.0 Testing stuff ----------------------------------------------------

#adds horizontal axis - scaled to genetic change/evolutionary distance
ggtree(testtree) + theme_tree2()

#disables scaling to produce cladogram
ggtree(testtree, branch.length="none")

?ggtree


# try using facet_plot() to display data? could show multiple plots at one time
#geom_cladelabel() might be useful to group the species in our trees into families. then we can add images




#working on fake tree
nwk <- "(OneScaleRichMap_Allisoniaceae, OneScaleRichMap_Fossombroniaceae);"
x = read.tree(text = nwk)
ggtree(x) + xlim(NA, 7)  +
  geom_cladelabel(node=3, label="test label", offset=.4, offset.text=.4, hjust='left')


ggtree(x) + xlim(NA, 7) + ylim(NA, 3) +
  geom_tiplab(aes(image=paste0("Figures/OneScale_RichByFamMaps/Liverworts/Fossombroniales/", label, '.png')),
              geom="image", offset=4, align=5, size=.3) +
  geom_tiplab(geom='label', offset=1, hjust=.1, size=3)


#trying to use their tree...
F11tree <- read.tree(file = "Data/trees/nt-mt-RAxML-FigS11.tre")
ggtree(F11tree, branch.length = "none") +
  geom_tiplab(size = 0.7)

#...find one species per family, remove the rest of the species tips (so one species per family)
#...rename the species as their corresponding family


# 3.0 Annotating Nodes -------------------------------------------------
# 3.0.1 Make base tree - use F20 or F24 - used in Liu et al. 2019 Fig. 1
F11tree <- read.tree(file = "Data/trees/nt-mt-RAxML-FigS11.tre")
ggtree(F11tree, branch.length = "none") +
  geom_tiplab(size = 0.7)

F20tree <- read.tree(file = "Data/trees/aa-nu-RAxML-FigS20.tre")
ggtree(F20tree, branch.length = "none") +
  geom_tiplab(size = 0.7)

# 3.1 Figure out which nodes we want to work with ----------------------

# 3.1.1 Remove family NA values to see all species in each family
require(tidyr)
FamiliesS11 <- FigS11_FOG%>%drop_na(family)
##make it easier to read
FamiliesS11$isTip <- NULL
FamiliesS11$x <- NULL
FamiliesS11$y <- NULL
FamiliesS11$branch <- NULL
FamiliesS11$angle <- NULL

View(FamiliesS11)

# 3.1.2 Add node numbers
ggtree(F11tree, branch.length = "none") + 
  geom_text2(aes(subset=!isTip, label=node), hjust=-.3, size=2) + 
  geom_tiplab(size = 0.7)


# 3.1.3 Color each family to identify node number
Pterobryaceae <- c("Jaegerina_solitaria", "Calyptothecium_pinnatum_3742")
PterobryaceaeTree <- ggtree(F11tree, branch.length="none") + 
  geom_tiplab(aes(color = label %in% Pterobryaceae), size = 0.7) +
  geom_text2(aes(subset=!isTip, label=node), hjust=-.3, size=2)

PterobryaceaeTree

## 3.1.4 Hailey's added stuff - we didn't end up doing this
# Program that assigns a family to each species in the data
#get family names
tree20 <- read.tree("Data/trees/aa-nu-RAxML-FigS20.tre")
clean20famlabels <- FigS20_FOG %>%
  filter(FigS20_FOG$isTip == "TRUE") %>%
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
tree20fam <- unique(FigS20_FOG$family)
tree20fam
for(i in 1:length(tree20fam)){
  fam <- tree20fam[i]
  tempdf <- Ref20DF %>%
    filter(Family == fam)
  tempvec <- tempdf$Species
  assign(fam, tempvec)
}


# 3.1.5 Make Trees to locate nodes
DaltoniaceaeTree <- ggtree(F11tree, branch.length="none") + 
  geom_tiplab(aes(color = label %in% Daltoniaceae), size = 0.7) +
  geom_text2(aes(subset=!isTip, label=node), hjust=-.3, size=2)
DaltoniaceaeTree

## Using S20 Tree
F20nodes <- ggtree(F20tree, branch.length = "none") +
  geom_text2(aes(subset=!isTip, label=node), hjust=-.3, size=2) + 
  scale_y_reverse() +
  geom_tiplab(size = 1.5)
F20nodes

# 3.1.6 - Most Recent Common Ancestor (MRCA) (Hailey's code)
#Do it with orders!
#Program that assigns a order to each species in the data
#get order names
tree20 <- read.tree("Data/trees/aa-nu-RAxML-FigS20.tre")
ggtree20 <- ggtree(tree20)
clean20orderlabels <- FigS20_FOG %>%
  filter(FigS20_FOG$isTip == "TRUE") %>%
  select(order)
Ref20DF <- data.frame(tree20$tip.label)
Ref20DF$Order <- clean20orderlabels
names(Ref20DF)[1] <- "Species"
#loop to find species in each order -- creates a vector of species names for each order name (does first line of Kathryn's code, for orders) 
tree20order <- unique(FigS20_FOG$order)
tree20order <- tree20order[complete.cases(tree20order)]
tree20orderlist <- list()
for(i in 1:length(tree20order)){
  order <- tree20order[i]
  tempdf <- Ref20DF %>%
    filter(Order == order)
  tempvec <- tempdf$Species
  tree20orderlist[[i]] <- tempvec
}
#Make a loop that goes through each  order and finds the most recent common ancestor then puts it into a data.frame
Tree20MRCA <- data.frame(Order = NA, Node = NA)
for(i in 1:length(tree20order)){
  order <- tree20order[i]
  tiplabs <- tree20orderlist[[i]]
  node <- MRCA(tree20, tiplabs)
  tempdf <- data.frame(Order = order, Node = node)
  Tree20MRCA <- bind_rows(Tree20MRCA, tempdf)
}
Tree20MRCA <- Tree20MRCA[complete.cases(Tree20MRCA),]



#Use function to find MRCA for families
#make a list of vectors of species in each family
tree20fam <- unique(FigS20_FOG$family)
tree20fam <- tree20fam[complete.cases(tree20fam)]
tree20fam
tree20famlist <- list()
Ref20DF$Family <- clean20famlabels
for(i in 1:length(tree20fam)){
  fam <- tree20fam[i]
  tempdf <- Ref20DF %>%
    filter(Ref20DF$Family == fam)
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


# 3.2 Assign maps to nodes ---------------------------------------------

# 3.2.1 ORDERS ---------------------------------------------------------
#using Tree20MRCA nodes
d <- data.frame(node = c(151,157,163,175,173,20,181,196,192,210,218,208,
                         83,261,268,270,275,280,168,291,148),
                images = c("Figures/TreeMaps/Jungermanniales.png",
                           "Figures/TreeMaps/Sphagnales.png",
                           "Figures/TreeMaps/Polytrichales.png",
                           "Figures/TreeMaps/Grimmiales.png",
                           "Figures/TreeMaps/Orthotrichales.png",
                           "Figures/TreeMaps/Bryoxiphiales.png",
                           "Figures/TreeMaps/Dicranales.png",
                           "Figures/TreeMaps/Rhizogoniales.png",
                           "Figures/TreeMaps/Bryales.png",
                           "Figures/TreeMaps/Hookeriales.png",
                           "Figures/TreeMaps/Hypnales.png",
                           "Figures/TreeMaps/Hypnodendrales.png",
                           "Figures/TreeMaps/Ptychomniales.png",
                           "Figures/TreeMaps/Aulacomniales.png",
                           "Figures/TreeMaps/Bartramiales.png",
                           "Figures/TreeMaps/Hedwigiales.png",
                           "Figures/TreeMaps/Funariales.png",
                           "Figures/TreeMaps/Gigaspermales.png",
                           "Figures/TreeMaps/Buxbaumiales.png",
                           "Figures/TreeMaps/Andreaeaeales.png",
                           "Figures/TreeMaps/Marchantiales.png"))
ggtree(F20tree, branch.length = "none") %<+% d +
  geom_nodelab(aes(image=images), geom="image", image_fun = function(.) magick::image_transparent(., "white"))
  
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
ggtree(F20tree, branch.length = "none") %<+% df + xlim(NA,38) +
  geom_nodelab(aes(image=images), geom="image", image_fun = function(.) magick::image_transparent(., "white")) +
  geom_cladelabel(node=272, label="Splachnales", fontsize=3) +
  geom_cladelabel(node=197, label="Rhizogoniales", fontsize=3) +
  geom_cladelabel(node=262, label="Bryales", fontsize=3) +
  geom_cladelabel(node=231, label="Hookeriales", fontsize=3) +
  geom_cladelabel(node=218, label="Hypnales", fontsize=3) +
  geom_cladelabel(node=249, label="Hypnodendrales", fontsize=3) +
  geom_cladelabel(node=239, label="Ptychomniales", fontsize=3) +
  geom_cladelabel(node=261, label="Aulacomniales", fontsize=3) +
  geom_cladelabel(node=268, label="Bartramiales", fontsize=3) +
  geom_cladelabel(node=269, label="Hedwigiales", fontsize=3) +
  geom_cladelabel(node=200, label="Orthotrichales", fontsize=3) +
  geom_hilight(node=272, fill="mediumseagreen", alpha=.6) +
  geom_hilight(node=197, fill="slategray", alpha=.6) +
  geom_hilight(node=262, fill="darkgoldenrod1", alpha=.6) +
  geom_hilight(node=231, fill="tomato1", alpha=.6) +
  geom_hilight(node=218, fill="darkslategray3", alpha=.6) +
  geom_hilight(node=249, fill="darkgreen", alpha=.6) +
  geom_hilight(node=239, fill="cornflowerblue", alpha=.6) +
  geom_hilight(node=261, fill="darkorange", alpha=.6) +
  geom_hilight(node=268, fill="darkgoldenrod3", alpha=.6) +
  geom_hilight(node=269, fill="lightslateblue", alpha=.6) +
  geom_hilight(node=200, fill="plum1", alpha=.6)



#test with one order
dafr <- data.frame(node = 218, images = c(file="Figures/TreeMaps/Hypnales.png"))
ggtree(F20tree, branch.length = "none") %<+% dafr +
  geom_nodelab(aes(image=images), geom="image")

# 3.2.2 FAMILIES -------------------------------------------------------
#using Tree20FamMRCA nodes
b <- data.frame(node = c(1,2,157,13,175,173,20,21,23,181,25,26,30,190,
                         196,192,210,48,55,59,208,62,229,231,76,83,99,
                         261,267,268,270,276,125,126,280,168,287,291,
                         148),
                images = c("Figures/TreeMaps/Scapaniaceae.png",
                           "Figures/TreeMaps/Lepidoziaceae.png",
                           "Figures/TreeMaps/Sphagnaceae.png",
                           "Figures/TreeMaps/Oedipodiaceae.png",
                           "Figures/TreeMaps/Grimmiaceae.png",
                           "Figures/TreeMaps/Orthotrichaceae.png",
                           "Figures/TreeMaps/Bryoxiphiaceae.png",
                           "Figures/TreeMaps/Seligeriaceae.png",
                           "Figures/TreeMaps/Ptychomitriaceae.png",
                           "Figures/TreeMaps/Dicranaceae.png",
                           "Figures/TreeMaps/Rhabdoweisiaceae.png",
                           "Figures/TreeMaps/Ditrichaceae.png",
                           "Figures/TreeMaps/Bruchiaceae.png",
                           "Figures/TreeMaps/Fissidentaceae.png",
                           "Figures/TreeMaps/Rhizogoniaceae.png",
                           "Figures/TreeMaps/Bryaceae.png",
                           "Figures/TreeMaps/Hookeriaceae.png",
                           "Figures/TreeMaps/Hypopterygiaceae.png",
                           "Figures/TreeMaps/Plagiotheciaceae.png",
                           "Figures/TreeMaps/Neckeraceae.png",
                           "Figures/TreeMaps/Hypnodendraceae.png",
                           "Figures/TreeMaps/Thuidiaceae.png",
                           "Figures/TreeMaps/Pterobryaceae.png",
                           "Figures/TreeMaps/Daltoniaceae.png",
                           "Figures/TreeMaps/Pilotrichaceae.png",
                           "Figures/TreeMaps/Ptychomniaceae.png",
                           "Figures/TreeMaps/Racopilaceae.png",
                           "Figures/TreeMaps/Aulacomniaceae.png",
                           "Figures/TreeMaps/Mniaceae.png",
                           "Figures/TreeMaps/Bartramiaceae.png",
                           "Figures/TreeMaps/Hedwigiaceae.png",
                           "Figures/TreeMaps/Funariaceae.png",
                           "Figures/TreeMaps/Encalyptaceae.png",
                           "Figures/TreeMaps/Disceliaceae.png",
                           "Figures/TreeMaps/Gigaspermaceae.png",
                           "Figures/TreeMaps/Buxbaumiaceae.png",
                           "Figures/TreeMaps/Polytrichaceae.png",
                           "Figures/TreeMaps/Andreaeaceae.png",
                           "Figures/TreeMaps/Marchantiaceae.png"))
ggtree(F20tree, branch.length = "none") %<+% b + xlim(NA,38) +
  geom_nodelab(aes(image=images), geom="image", image_fun = function(.) magick::image_transparent(., "white")) +
  geom_cladelabel(node=1, label="Scapaniaceae", fontsize=1.5) +
  geom_cladelabel(node=2, label="Lepidoziaceae", fontsize=1.5) +
  geom_cladelabel(node=157, label="Sphagnaceae", fontsize=1.5) +
  geom_cladelabel(node=13, label="Oedipodiaceae", fontsize=1.5) +
  geom_cladelabel(node=175, label="Grimmiaceae", fontsize=1.5) +
  geom_cladelabel(node=173, label="Orthotrichaceae", fontsize=1.5) +
  geom_cladelabel(node=20, label="Bryoxiphiaceae", fontsize=1.5) +
  geom_cladelabel(node=21, label="Seligeriaceae", fontsize=1.5) +
  geom_cladelabel(node=23, label="Ptychomitriaceae", fontsize=1.5) +
  geom_cladelabel(node=181, label="Dicranaceae", fontsize=1.5) +
  geom_cladelabel(node=25, label="Rhabdoweisiaceae", fontsize=1.5) +
  geom_cladelabel(node=26, label="Ditrichaceae", fontsize=1.5) +
  geom_cladelabel(node=30, label="Bruchiaceae", fontsize=1.5) +
  geom_cladelabel(node=190, label="Fissidentaceae", fontsize=1.5) +
  geom_cladelabel(node=196, label="Rhizogoniaceae", fontsize=1.5) +
  geom_cladelabel(node=192, label="Bryaceae", fontsize=1.5) +
  geom_cladelabel(node=210, label="Hookeriaceae", fontsize=1.5) +
  geom_cladelabel(node=48, label="Hypopterygiaceae", fontsize=1.5) +
  geom_cladelabel(node=55, label="Plagiotheciaceae", fontsize=1.5) +
  geom_cladelabel(node=59, label="Neckeraceae", fontsize=1.5) +
  geom_cladelabel(node=208, label="Hypnodendraceae", fontsize=1.5) +
  geom_cladelabel(node=62, label="Thuidiaceae", fontsize=1.5) +
  geom_cladelabel(node=229, label="Pterobryaceae", fontsize=1.5) +
  geom_cladelabel(node=231, label="Daltoniaceae", fontsize=1.5) +
  geom_cladelabel(node=76, label="Pilotrichaceae", fontsize=1.5) +
  geom_cladelabel(node=83, label="Ptychomniaceae", fontsize=1.5) +
  geom_cladelabel(node=99, label="Racopilaceae", fontsize=1.5) +
  geom_cladelabel(node=261, label="Aulacomniaceae", fontsize=1.5) +
  geom_cladelabel(node=267, label="Mniaceae", fontsize=1.5) +
  geom_cladelabel(node=268, label="Bartramiaceae", fontsize=1.5) +
  geom_cladelabel(node=270, label="Hedwigiaceae", fontsize=1.5) +
  geom_cladelabel(node=276, label="Funariaceae", fontsize=1.5) +
  geom_cladelabel(node=125, label="Encalyptaceae", fontsize=1.5) +
  geom_cladelabel(node=126, label="Disceliaceae", fontsize=1.5) +
  geom_cladelabel(node=280, label="Gigaspermaceae", fontsize=1.5) +
  geom_cladelabel(node=168, label="Buxbaumiaceae", fontsize=1.5) +
  geom_cladelabel(node=287, label="Polytrichaceae", fontsize=1.5) +
  geom_cladelabel(node=291, label="Andreaeaceae", fontsize=1.5) +
  geom_cladelabel(node=148, label="Marchantiaceae", fontsize=1.5)

#removing families that are either on the backbone or not being pasted
goodfamilies <- data.frame(node = c(157,175,181,190,229,231,261,267,268,
                                    270,276,280,287,291),
                images = c("Figures/TreeMaps/Sphagnaceae.png",
                           "Figures/TreeMaps/Grimmiaceae.png",
                           "Figures/TreeMaps/Dicranaceae.png",
                           "Figures/TreeMaps/Fissidentaceae.png",
                           "Figures/TreeMaps/Pterobryaceae.png",
                           "Figures/TreeMaps/Daltoniaceae.png",
                           "Figures/TreeMaps/Aulacomniaceae.png",
                           "Figures/TreeMaps/Mniaceae.png",
                           "Figures/TreeMaps/Bartramiaceae.png",
                           "Figures/TreeMaps/Hedwigiaceae.png",
                           "Figures/TreeMaps/Funariaceae.png",
                           "Figures/TreeMaps/Gigaspermaceae.png",
                           "Figures/TreeMaps/Polytrichaceae.png",
                           "Figures/TreeMaps/Andreaeaceae.png"))
ggtree(F20tree, branch.length = "none") %<+% goodfamilies + xlim(NA,38) +
  geom_nodelab(aes(image=images), geom="image", image_fun = function(.) magick::image_transparent(., "white")) +
  geom_cladelabel(node=157, label="Sphagnaceae", fontsize=2) +
  geom_cladelabel(node=175, label="Grimmiaceae", fontsize=2) +
  geom_cladelabel(node=181, label="Dicranaceae", fontsize=2) +
  geom_cladelabel(node=26, label="Ditrichaceae", fontsize=2) +
  geom_cladelabel(node=190, label="Fissidentaceae", fontsize=2) +
  geom_cladelabel(node=229, label="Pterobryaceae", fontsize=2) +
  geom_cladelabel(node=231, label="Daltoniaceae", fontsize=2) +
  geom_cladelabel(node=261, label="Aulacomniaceae", fontsize=2) +
  geom_cladelabel(node=267, label="Mniaceae", fontsize=2) +
  geom_cladelabel(node=268, label="Bartramiaceae", fontsize=2) +
  geom_cladelabel(node=270, label="Hedwigiaceae", fontsize=2) +
  geom_cladelabel(node=276, label="Funariaceae", fontsize=2) +
  geom_cladelabel(node=280, label="Gigaspermaceae", fontsize=2) +
  geom_cladelabel(node=287, label="Polytrichaceae", fontsize=2) +
  geom_cladelabel(node=291, label="Andreaeaceae", fontsize=2)
  
#identify which families are on the phylogeny backbone
changes <- data.frame(node = 192, 
                             images="Figures/TreeMaps/Bryaceae.png")
ggtree(F20tree, branch.length = "none") %<+% changes +
  geom_nodelab(aes(image=images), geom="image", image_fun = function(.) magick::image_transparent(., "white"))


# 3.3 Adding points on nodes of interest -------------------------------
# 3.3.1 with a loop
mrcanodes <- Tree20FamMRCA$Node
p <- ggtree(tree20, branch.length = "none") +
  geom_tiplab(size = 0.7)
for(i in 1:length(mrcanodes)){
  p <- p + geom_point2(aes(subset=node==mrcanodes[i]), color = "orangered", size=0.5)
}
p

# 3.3.2 manually
ggtree(tree20, branch.length="none") + xlim(NA,38) +
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


# 3.4 Hailey: Make tree with different colored branches by order -------
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
tree2 <- groupClade(F20tree, c(272, 197, 262, 231, 218, 249, 239, 261, 268, 269, 200))
ggtree(tree2, branch.length = "none", aes(color=group)) %<+% df + xlim(NA,38) +
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


# 4.0 Testing some aesthetic stuff -------------------------------------
ggtree(F20tree, branch.length="none") + xlim(NA,38) + 
  geom_hilight(node=272, fill="mediumseagreen", alpha=.6) +
  geom_hilight(node=197, fill="slategray", alpha=.6) +
  geom_hilight(node=262, fill="darkgoldenrod1", alpha=.6) +
  geom_hilight(node=231, fill="tomato1", alpha=.6) +
  geom_hilight(node=218, fill="darkslategray3", alpha=.6) +
  geom_hilight(node=249, fill="darkgreen", alpha=.6) +
  geom_hilight(node=239, fill="cornflowerblue", alpha=.6) +
  geom_hilight(node=261, fill="darkorange", alpha=.6) +
  geom_hilight(node=268, fill="darkgoldenrod3", alpha=.6) +
  geom_hilight(node=269, fill="lightslateblue", alpha=.6) +
  geom_hilight(node=200, fill="plum1", alpha=.6)
colors()
require(yarrr)
yarrr::piratepal()


#Trying to put maps on clade labels
ggtree(F20tree, branch.length = "none") +
  geom_cladelabel(node=272, image="Figures/TreeMaps/Splachnales.png", label="none")


#make background transparent
ggtree(randomtree) + 
  geom_tiplab(image = "Figures/FamRichnessMap.png", geom = "image", size = .12, image_fun = function(.) magick::image_transparent(., "white")) + 
  ggplot2::xlim(0,2.5) +
  geom_tiplab(offset = 0.4)

#this produces bootstrap frequencies, not node numbers (might be useful later)
ggtree(F11tree, branch.length = "none") +
geom_tiplab(size = 0.7) + 
geom_label2(aes(subset=!isTip, label=node), size=.5)


# 5.0 Make grids of maps -----------------------------------------------

# 5.1 With grid.arrange ------------------------------------------------
library(png)
library(grid)
library(gridExtra)

plot1 <- readPNG('Figures/TreeMaps/Hypnales.png')
plot2 <- readPNG('Figures/TreeMaps/Splachnales.png')

grid.arrange(rasterGrob(plot1),rasterGrob(plot2),ncol=1)

# 5.2 With ggplot? -----------------------------------------------------
OrderGrid <- data.frame(OrderNodes20$Order)
library(cowplot)
library(raster)
p <- ggplot(data=OrderNodes20) + facet_wrap(vars(Order)) + theme_cowplot(8)

ggdraw(p) +
  draw_image("Figures/TreeMaps/Hypnales.png",  x = .565, y = .572, hjust = 1, vjust = 1, width = 0.13, height = 0.2)

# 6.0 Hailey's code ----------------------------------------------------
Tree20ManualMRCA <- data.frame(df$node)
Tree20ManualMRCA$node <- df$node
Tree20ManualMRCA$df.node <- NULL
Tree20ManualMRCA$order <- c("Splachnales", "Rhizogoniales", "Bryales", "Hookeriales", "Hypnales", "Hypnodendrales", "Ptychomniales", "Aulacomniales", "Bartramiales", "Hedwigiales", "Orthotrichales")
tree <- ggtree(tree2, branch.length = "none") %<+% df + xlim(NA,38) + 
  geom_nodelab(aes(image=images), geom="image", image_fun = function(.) magick::image_transparent(., "white"))
for(i in 1:nrow(Tree20ManualMRCA)){
  tree <- tree + geom_cladelabel(node= Tree20ManualMRCA$node[i], label=Tree20ManualMRCA$order[i], fontsize=2)
}
tree

#make it with colors
Tree20ManualMRCA <- data.frame(df$node)
Tree20ManualMRCA$node <- df$node
Tree20ManualMRCA$df.node <- NULL
Tree20ManualMRCA$order <- c("Splachnales", "Rhizogoniales", "Bryales", "Hookeriales", "Hypnales", "Hypnodendrales", "Ptychomniales", "Aulacomniales", "Bartramiales", "Hedwigiales", "Orthotrichales")
Tree20ManualMRCA$hexcols <- brewer.pal(n=11, name = "Spectral")
tree <- ggtree(tree2, branch.length = "none") %<+% df + xlim(NA,38) 
for(i in 1:nrow(Tree20ManualMRCA)){
  tree <- tree + geom_cladelabel(node= Tree20ManualMRCA$node[i], label=Tree20ManualMRCA$order[i], fontsize=2, col = Tree20ManualMRCA$hexcols[i]) + geom_hilight(Tree20ManualMRCA$node[i], fill=Tree20ManualMRCA$hexcols[i])
}
tree + geom_nodelab(aes(image=images), geom="image", image_fun = function(.) magick::image_transparent(., "white"))


Tree20ManualMRCA <- data.frame(df$node)
Tree20ManualMRCA$node <- df$node
Tree20ManualMRCA$df.node <- NULL
Tree20ManualMRCA$order <- c("Splachnales", "Rhizogoniales", "Bryales", "Hookeriales", "Hypnales", "Hypnodendrales", "Ptychomniales", "Aulacomniales", "Bartramiales", "Hedwigiales", "Orthotrichales")
Tree20ManualMRCA$hexcols <- brewer.pal(n=11, name = "Spectral")
tree <- ggtree(tree2) %<+% df 
for(i in 1:nrow(Tree20ManualMRCA)){
  tree <- tree + geom_cladelabel(node= Tree20ManualMRCA$node[i], label=Tree20ManualMRCA$order[i], fontsize=2, col = Tree20ManualMRCA$hexcols[i]) + geom_hilight(Tree20ManualMRCA$node[i], fill=Tree20ManualMRCA$hexcols[i])
}
tree + geom_nodelab(aes(image=images), geom="image", image_fun = function(.) magick::image_transparent(., "white"))


#Make highlighted and clade labeled tree
OrderNodes20 <- read.csv("./Data/OrderNodesS20.csv")
getPalette <- colorRampPalette(brewer.pal(9, "Set1"))
OrderNodes20$hexcols <- getPalette(nrow(OrderNodes20))
tree <- ggtree(tree2, branch.length = "none") + xlim(NA,38) 
for(i in 1:nrow(OrderNodes20)){
  n <- OrderNodes20$Node[i]
  o <- OrderNodes20$Order[i]
  c <- OrderNodes20$hexcols[i]
  tree <- tree + geom_cladelabel(node= n, label=o, fontsize=3, col = c) + geom_hilight(n, fill=c)
}
tree
