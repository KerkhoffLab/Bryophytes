# MossOrderLM
# Running the same linear model with each order with more than 10 species
# Hailey Napier
# November 2020

# 0.0 FIRST ---------------------------------
# 0.1 Load packages
library(dplyr)
library(raster)

# 0.2 Load data 
LMDF2 <- readRDS("Data/LMDF2.rds")
  # find in MossLM.R
RichnessVec <- readRDS("Data/RichnessVec.rds")
  # find in DataProcessing.R
MossOrdRichAbove100 <- readRDS("Data/MossOrdRichAbove100.rds")
MossOrdRich25to100 <- readRDS("Data/MossOrdRich25to100.rds")
MossOrdRich10to25 <- readRDS("Data/MossOrdRich10to25.rds")


# 1.0 Add NAs for 0s in TotalRichness ------
# 1.1 Replace 0s in RichnessVec with NAs
RichnessVecNA <- RichnessVec
RichnessVecNA[RichnessVecNA == 0] <- NA
RichnessVecNA

# 1.2 Make a DF with no NAs, just for fun
RichnessDFNoNA <- data.frame(Alpha = RichnessVecNA, CellID = 1:15038)
RichnessDFNoNA <- RichnessDFNoNA %>%
  filter(!is.na(RichnessDFNoNA$Alpha))
nrow(RichnessDFNoNA)

RichnessTopoDFNoNA <- left_join(RichnessDFNoNA, AlphaMountLM, by = "CellID")

# 1.2 Replace TotalRichness column in LMDF2 with RichnessVecNA (repeated 22 times)
LMDF2$TotalRichness <- rep(RichnessVecNA, 22)


# 2.0 Make lm for total richness ---------------
mossrichnesslm <- lm(log1p(TotalRichness) ~ log1p(MAT) + log1p(MAP) + 
                                 Biome*log1p(MAT) + Biome*log1p(MAP) +
                                 Topo*log1p(MAT) + Topo*log1p(MAP), 
                               data = LMDF2)


# 3.0 Loop to test lm for orders ---------------
# 3.1 Make vector of moss order names that doesn't include the least diverse orders
MossOrdRich10to100 <- c(MossOrdRichAbove100, MossOrdRich25to100, MossOrdRich10to25)

# 3.4 Linear model function for orders
# input: str, name of order, default = total richness
# output: linear model using input order's data
order_lm <- function(order = "all"){
  if(order == "all"){
    lm <- lm(log1p(TotalRichness) ~ log1p(MAT) + log1p(MAP) + 
               Biome*log1p(MAT) + Biome*log1p(MAP) +
               Topo*log1p(MAT) + Topo*log1p(MAP), 
             data = LMDF2)
  }else if(order %in% LMDF2$OrderName){
    tempdf <- LMDF2 %>%
      filter(LMDF2$OrderName == order)
    
    lm <- lm(log1p(OrderRichness) ~ log1p(MAT) + log1p(MAP) + 
                     Biome*log1p(MAT) + Biome*log1p(MAP) +
                     Topo*log1p(MAT) + Topo*log1p(MAP), 
                   data = tempdf)
  }else{
    lm <- "Invalid order name, please try again"
  }
  return(lm)
}

# 3.3 Make a vector of coefficint names
test <- order_lm("Hypnales")
coef_names <- c(names(test$coefficients),"AdjRSquared")

# 3.4 Make a dataframe for loop output
MossOrdRich10to100
OrderLMCoefDF <- NULL
OrderLMCoefDF <- data.frame(
  "Coefficient" = coef_names,
  "Dicranales" = rep(NA, length(coef_names)),    
  "Hypnales" = rep(NA, length(coef_names)),
  "Bartramiales" = rep(NA, length(coef_names)),
  "Bryales" = rep(NA, length(coef_names)),
  "Grimmiales" = rep(NA, length(coef_names)),
  "Hookeriales" = rep(NA, length(coef_names)),
  "Orthotrichales" = rep(NA, length(coef_names)),
  "Pottiales" = rep(NA, length(coef_names)),
  "Funariales" = rep(NA, length(coef_names)),
  "Hedwigiales" = rep(NA, length(coef_names)),
  "Polytrichales" = rep(NA, length(coef_names)),
  "Sphagnales" = rep(NA, length(coef_names))) 

# 3.5 Loop
# Loop output is a dataframe with coefficients and adjusted r squared for lm for each order
for(i in 1:length(MossOrdRich10to100)){
  order <- MossOrdRich10to100[i]
  lm <- order_lm(order)
  for(j in 1:(length(coef_names) - 1)){
    coef <- summary(lm)$coefficients[j]
    OrderLMCoefDF[j, order] <- coef
  }
  adjrsquared <- summary(lm)$adj.r.squared
  lastindex <- length(coef_names)
  OrderLMCoefDF[lastindex, order] <- adjrsquared
}

saveRDS(OrderLMCoefDF, "Data/OrderLMCoefDF.rds")

# download csv
# write.csv(OrderLMCoefDF, "/Users/haileynapier/Desktop/OrderLMCoefDF.csv")


# TEST AICS FOR EACH ORDER FOR LMS WITH DIFFERENT NUMBERS OF PARAMETERS
# 4.0 Write a new function that that alters the lm and the order -------------
# 4.1 Make a new dataframe with log transformed variables
LogTransLMDF <- LMDF2
LogTransLMDF$LogMAP <- log1p(LogTransLMDF$MAP)
LogTransLMDF$LogMAT <- log1p(LogTransLMDF$MAT)
LogTransLMDF$LogOrdRich <- log1p(LogTransLMDF$OrderRichness)
  
# 4.2 Make a vector of all lm parameters
lm_parameters <- c("LogOrdRich", "LogMAT", "LogMAP", "Biome*LogMAT + Biome*LogMAP", "Topo*LogMAT + Topo*LogMAP")

# 4.3 Write the function
# input: str, name of order, default = total richness
      #  int, vector of indexes for parameters to include in null model
      #       1: LogOrdRich
      #       2: LogMAT
      #       3: LogMAP
      #       4: Biome*LogMAT + Biome*LogMAP
      #       5: Topo*LogMAT + Topo*LogMAP
# output: linear model using input order's data and parameters specified in input

order_any_lm <- function(order = "Hypnales", parameter_index_vector = 2:5){
  if(order %in% LMDF2$OrderName){
    tempdf <- LogTransLMDF %>%
      filter(LogTransLMDF$OrderName == order)
    lm <- lm(as.formula(paste(lm_parameters[1], "~", paste(lm_parameters[parameter_index_vector], collapse="+"))), data=tempdf)
  }else{
    lm <- "Invalid order name, please try again"
  }
  return(lm)
}

# 4.4 Make a list of vectors of all of the possible parameter index combinations of any length
parameter_index <- c(2,3,4,5)
parameter_index
parameter_comb_list <- list()
index <- 0
for(i in 1:5){
  combs <- combn(parameter_index, i)
  ncombs <- ncol(combs)
  for(j in 1:ncombs){
    index <- index + 1
    parameter_comb_list[[index]] <- combs[,j]
  }
}

# 4.5 Make a dataframe for loop output





   