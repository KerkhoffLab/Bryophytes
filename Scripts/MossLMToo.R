# Moss Linear Regression Models
# Kathryn Dawdy and Hailey Napier
# November 2020

# 0.0 Load packages ------------------------------------------------------------
devtools::install_github("AckerDWM/gg3D")
library(gg3D)
library(raster)
library(ggplot2)
require(ggiraph)
require(ggiraphExtra)
require(plyr)
library(dplyr)
require(plotly)

# 0.1 Load data ----------------------------------------------------------------
LMDF <- readRDS("Data/LMDF.rds")
LMDF2 <- readRDS("Data/LMDF2.rds")
LMDF3 <- readRDS("Data/LMDF3.rds")
MossRichnessRaster <- readRDS("Data/MossRichnessRaster.rds")
nw_mount <- shapefile("Data/MapOutlines/Mountains/Koeppen-Geiger_biomes.shp")
cols <- c("#D8B70A", "#972D15", "#A2A475", "#81A88D", "#02401B",
           "#446455", "#FDD262", "#D3DDDC", "#C7B19C",
           "#798E87", "#C27D38")
OrderLM_AIC_DF <-readRDS("Data/OrderLM_AIC_DF.rds")

# 1.0 Make LMDF with montane/lowland column ------------------------------------
# 1.1 First run MossLM.R (as it is on 11/6/2020 !)
saveRDS(LMDF, "Data/LMDF.rds")
LMDF <- readRDS("Data/LMDF.rds")
  
# 1.3 Extract cells in montane regions
AlphaMountLM <- raster::extract(MossRichnessRaster, nw_mount, df = TRUE, cellnumbers = TRUE)
colnames(AlphaMountLM) <- c("Topo", "CellID", "Alpha")
AlphaMountLM$Topo <-"Montane"
AlphaMountLM$Alpha <- NULL

# 1.4 Remove duplicated entries in AlphaMountLM
dupes <- AlphaMountLM$CellID[which(duplicated(AlphaMountLM) == T)]
dupes <- unique(dupes)

for(i in dupes){
  dupedcells <- which(AlphaMountLM$CellID == i)
  if(length(dupedcells) == 2){
    AlphaMountLM$CellID[dupedcells[1]] <- NA
  }else if(length(dupedcells) == 3)
    AlphaMountLM$CellID[dupedcells[2]] <- NA
  AlphaMountLM$CellID[dupedcells[1]] <- NA
}

AlphaMountLM <- AlphaMountLM %>%
  filter(!is.na(AlphaMountLM$CellID))

# 1.5 Join LMDF and AlphaMountLM by CellID
LMDF2 <- full_join(LMDF, AlphaMountLM, by="CellID")

# 1.6 Make non-montane cells lowland
LMDF2$Topo[is.na(LMDF2$Topo)] <- "Lowland"
saveRDS(LMDF2, "Data/LMDF2.rds")

# 1.7 Make richness values of 0 into NAs
LMDF3 <- LMDF2
LMDF3$TotalRichness[which(LMDF3$TotalRichness==0)] <- NA
saveRDS(LMDF3, "Data/LMDF3.rds")

# 1.8 Convert MAT from celsius to Kelvin
LMDF4 <- LMDF3
LMDF4$MAT_Kelvin <- (LMDF4$MAT) + 273.15

LMDF4$LogMAT_Kelvin <- log1p(LMDF4$MAT_Kelvin)
LMDF4$LogMAP <- log1p(LMDF4$MAP)
LMDF4$LogTotalRich <- log1p(LMDF4$TotalRichness)


# 2.0 Playing around to get comfortable + comparing LMDF2 and LMDF3 ------------
# 2.1 Simple plots of richness as a function of predictor variables ----
Rich_by_MAT_2 <- plot(TotalRichness~MAT, data=LMDF2)
Rich_by_MAT <- plot(TotalRichness~MAT, data=LMDF3) #Yeah, I'm gonna use LMDF3 since it removes richness values of 0
Rich_by_MAP <- plot(TotalRichness~MAP, data=LMDF3)

# 2.2 Log transform axes ----
logRich_by_MAT <- plot(log1p(TotalRichness)~MAT, data=LMDF3)
logRich_by_MAP <- plot(log1p(TotalRichness)~MAP, data=LMDF3)

Rich_by_logMAT <- plot(TotalRichness~log1p(MAT), data=LMDF3)
Rich_by_logMAP <- plot(TotalRichness~log1p(MAP), data=LMDF3)

logRich_by_logMAT <- plot(log1p(TotalRichness)~log1p(MAT), data=LMDF3)
logRich_by_logMAP <- plot(log1p(TotalRichness)~log1p(MAP), data=LMDF3)

# 2.3 Generate linear regressions for each variable ----
lm_Rich_by_MAT <- lm(TotalRichness~MAT, data=LMDF3)
summary(lm_Rich_by_MAT)
plot(TotalRichness~MAT, data=LMDF3)
abline(lm_Rich_by_MAT, col="blue")

lm_Rich_by_MAP <- lm(TotalRichness~MAP, data=LMDF3)
summary(lm_Rich_by_MAP)
plot(TotalRichness~MAP, data=LMDF3)
abline(lm_Rich_by_MAP, col="blue")

lm_Rich_by_MAT_MAP <- lm(TotalRichness~MAT + MAP, data=LMDF3)
summary(lm_Rich_by_MAT_MAP) #Adjusted R-squared:  0.07607 

lm_log_Rich_MAT_MAP <- lm(log1p(TotalRichness) ~ log1p(MAT) + log1p(MAP), 
                          data=LMDF3)
summary(lm_log_Rich_MAT_MAP) #Adjusted R-squared:  0.1128 

lm_Biome <- lm(log1p(TotalRichness) ~ log1p(MAT) + log1p(MAP) + Biome,
                   data=LMDF3)
summary(lm_Biome) #Adjusted R-squared:  0.2936

lm_Biome_Topo <- lm(log1p(TotalRichness) ~ log1p(MAT) + log1p(MAP) + Biome + Topo,
                    data=LMDF3)
summary(lm_Biome_Topo) #Adjusted R-squared:  0.3767 

lm_Biome_Topo_multiplied <- lm(log1p(TotalRichness) ~ log1p(MAT) + log1p(MAP) + 
                                 Biome*log1p(MAT) + Biome*log1p(MAP) +
                                 Topo*log1p(MAT) + Topo*log1p(MAP), 
                               data = LMDF3)
summary(lm_Biome_Topo_multiplied) #Adjusted R-squared:  0.5012 

# 2.4 Now try order richness ----
lm_OrdRich_by_MAT_MAP <- lm(OrderRichness~MAT + MAP, data=LMDF3)
summary(lm_OrdRich_by_MAT_MAP) #Adjusted R-squared:  0.005792



# 3.0 Plotting lms with ggplot2 ------------------------------------------------
ggplot(LMDF3,aes(y=TotalRichness,x=MAT))+geom_point()+geom_smooth(method="lm")
#ggPredict(lm_Rich_by_MAT,se=TRUE,interactive=TRUE)   #make it interactive!

ggplot(LMDF3,aes(y=TotalRichness,x=MAP))+geom_point()+geom_smooth(method="lm")

# 3.1 use gg3D to make 3D scatter plots! ..... not working :(
ggplot(LMDF3, aes(x=(MAT), y=(MAP), z=(TotalRichness)
                  #, color=Biome
                  )) + 
  theme_void() +
  axes_3D() +
  stat_3D()



# 4.0 3D Plots -----------------------------------------------------------------
# 4.1 Richness by MAT by MAP colored by biome scatterplot ----------------------
fig <- plot_ly(LMDF3, x=~MAP, y=~MAT, z=~TotalRichness, 
        type="scatter3d", mode="markers", color=~Biome, colors=cols,
        marker=list(size=5))
axx <- list(
  #backgroundcolor="rgb(200, 200, 230",
  #gridcolor="rgb(255,255,255)",
  #showbackground=TRUE,
  #zerolinecolor="rgb(255,255,255",
  title = "MAP"
)

axy <- list(
  #backgroundcolor="rgb(230, 200,230)",
  #gridcolor="rgb(255,255,255)",
  #showbackground=TRUE,
  #zerolinecolor="rgb(255,255,255",
  title = "MAT"
)

axz <- list(
  #backgroundcolor="rgb(230, 230,200)",
  #gridcolor="rgb(255,255,255)",
  #showbackground=TRUE,
  #zerolinecolor="rgb(255,255,255",
  title = "Total Richness"
)
#fig <- fig %>% add_trace(x=~LMDF3$MAP, z=lm(LMDF3$TotalRichness~LMDF3$MAT), mode="lines")
fig <- fig %>% layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz))
fig

# 4.2 Richness by MAT by MAP colored by topography -----------------------------
fig1 <- plot_ly(x=LMDF3$MAP, y=LMDF3$MAT, z=LMDF3$TotalRichness, 
               type="scatter3d", mode="markers", color=LMDF3$Topo, 
               marker=list(size=5))
axx <- list(
  title = "MAP"
)

axy <- list(
  title = "MAT"
)

axz <- list(
  title = "Total Richness"
)
fig1 <- fig1 %>% layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz))
fig1


# 4.3 Richness by MAT and Biome ------------------------------------------------
fig2 <- plot_ly(x=LMDF3$Biome, y=LMDF3$MAT, z=LMDF3$TotalRichness, 
                type="scatter3d", mode="markers", color=LMDF3$Topo, 
                marker=list(size=5))
axx <- list(
  title = "Biome",
  nticks=11
)

axy <- list(
  title = "MAT"
)

axz <- list(
  title = "Total Richness"
)
fig2 <- fig2 %>% layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz))
fig2


#4.4 Log(TotalRichness) by log(MAP) by MAT colored by biome --------------------
fig3 <- plot_ly(LMDF3, x=~log1p(MAP), y=~MAT, z=~log1p(TotalRichness), 
               type="scatter3d", mode="markers", color=~Biome, colors=cols,
               marker=list(size=5))
axx <- list(
  #backgroundcolor="rgb(200, 200, 230)",
  #gridcolor="rgb(255,255,255)",
  #showbackground=TRUE,
  #zerolinecolor="rgb(255,255,255)",
  #title = "log(MAP)"
)

axy <- list(
  #backgroundcolor="rgb(230, 200,230)",
  #gridcolor="rgb(255,255,255)",
  #showbackground=TRUE,
  #zerolinecolor="rgb(255,255,255)",
  #title = "MAT"
)

axz <- list(
  #backgroundcolor="rgb(230, 230,200)",
  #gridcolor="rgb(255,255,255)",
  #showbackground=TRUE,
  #zerolinecolor="rgb(255,255,255)",
  #title = "log(Total Richness)"
)
#fig <- fig %>% add_trace(x=~LMDF3$MAP, z=lm(LMDF3$TotalRichness~LMDF3$MAT), mode="lines")
fig3 <- fig3 %>% layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz))
fig3

# Star map :)
funcols <- c()
funfig <- plot_ly(LMDF3, x=~log1p(MAP), y=~MAT, z=~log1p(TotalRichness), 
                type="scatter3d", mode="markers", color=~Topo, colors=c("#F0CF4C","#F0CF4C"),
                marker=list(size=5))
axx <- list(
  backgroundcolor="rgb(17, 23, 56)",
  gridcolor="rgb(255,255,255)",
  showbackground=TRUE,
  zerolinecolor="rgb(255,255,255)",
  title = "log(MAP)"
)

axy <- list(
  backgroundcolor="rgb(17, 23, 56)",
  gridcolor="rgb(255,255,255)",
  showbackground=TRUE,
  zerolinecolor="rgb(255,255,255)",
  title = "MAT"
)

axz <- list(
  backgroundcolor="rgb(17, 23, 56)",
  gridcolor="rgb(255,255,255)",
  showbackground=TRUE,
  zerolinecolor="rgb(255,255,255)",
  title = "log(Total Richness)"
)
funfig <- funfig %>% layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz))
funfig


# 5.0 AIC ----------------------------------------------------------------------
testlm <- lm(TotalRichness~MAT, data=LMDF3)
testlm2 <- lm(TotalRichness~MAT + MAP, data=LMDF3)
testlm3 <- lm(TotalRichness~log1p(MAT), data=LMDF3)
testlm4 <- lm(TotalRichness~log1p(MAT) + log1p(MAP), data=LMDF3)
testlm5 <- lm(TotalRichness~MAP, data=LMDF3)
testlm6 <- lm(TotalRichness~log1p(MAP), data=LMDF3)
testlm7 <- lm(log1p(TotalRichness)~log1p(MAP), data=LMDF3)
testlm8 <- lm(log1p(TotalRichness) ~ MAT, data=LMDF3)
testlm9 <- lm(log1p(TotalRichness) ~ log1p(MAP), data=LMDF3)
testlm10 <- lm(log1p(TotalRichness) ~ MAT + log1p(MAP), data=LMDF3)

AIC(testlm, testlm2)
AIC(testlm3)
AIC(testlm4)
AIC(testlm3, testlm4)
AIC(testlm, testlm2, testlm3, testlm4)  #"models are not all fitted to the same number of observations"- probably because NaNs produced when you take the log of negative MATs
AIC(testlm, testlm3)
AIC(testlm5, testlm6)
AIC(testlm5, testlm7)
AIC(testlm8, testlm9, testlm10)

testDF <- LMDF3
testDF$LogMAT <- log1p(testDF$MAT)
testDF$LogMAP <- log1p(testDF$MAP)
testDF$LogTotalRich <- log1p(testDF$TotalRichness)

lm_fx_test




# ?.? Truly not sure if anything below works ----
#LM with biomes + topo (not interaction terms)
mosslm_1 <- lm(log1p(TotalRichness) ~ log1p(MAT) + log1p(MAP) + Biome + 
                         Topo, data = LMDF2)
summary(mosslm_1)
plot(mosslm_1)

#LM with biomes + topo (interaction terms?)
mosslm_2 <- lm(log1p(TotalRichness) ~ log1p(MAT) + log1p(MAP) + 
                  Biome*log1p(MAT) + Biome*log1p(MAP) +
                  Topo*log1p(MAT) + Topo*log1p(MAP), data = LMDF2)
summary(mosslm_2)
plot(mosslm_2)


