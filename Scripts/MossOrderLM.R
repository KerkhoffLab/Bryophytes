# MossOrderLM
# Running the same linear model with each order with more than 10 species
# Hailey Napier
# November 2020

# 0.0 FIRST ---------------------------------
# 0.1 Load packages
library(dplyr)
library(raster)
library(ggplot2)

# 0.2 Load data 
LMDF2 <- readRDS("Data/LMDF2.rds")
AlphaMountLM <- readRDS("Data/AlphaMountLM.rds")
  # find in MossLM.R
RichnessVec <- readRDS("Data/RichnessVec.rds")
  # find in DataProcessing.R
MossOrdRichAbove100 <- readRDS("Data/MossOrdRichAbove100.rds")
MossOrdRich25to100 <- readRDS("Data/MossOrdRich25to100.rds")
MossOrdRich10to25 <- readRDS("Data/MossOrdRich10to25.rds")

BiomeNames <- readRDS("Data/BiomeNames.rds")



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

# 1.3 Fix up MAT
LMDF2$MAT_Kelvin <- (LMDF2$MAT) + 273.15


# 2.0 Make lm for total richness ---------------
mossrichnesslm <- lm(log1p(TotalRichness) ~ log1p(MAT_Kelvin) + log1p(MAP) + 
                                 Biome*log1p(MAT_Kelvin) + Biome*log1p(MAP) +
                                 Topo*log1p(MAT_Kelvin) + Topo*log1p(MAP), 
                               data = LMDF2)


# 3.0 Loop to test lm for orders ---------------
# 3.1 Make vector of moss order names that doesn't include the least diverse orders
MossOrdRich10to100 <- c(MossOrdRichAbove100, MossOrdRich25to100, MossOrdRich10to25)
saveRDS(MossOrdRich10to100, "Data/MossOrdRich10to100.rds")

# 3.4 Linear model function for orders
# input: str, name of order, default = total richness
# output: linear model using input order's data
order_lm <- function(order = "all"){
  if(order == "all"){
    lm <- lm(log1p(TotalRichness) ~ log1p(MAT_Kelvin) + log1p(MAP) + 
               Biome*log1p(MAT_Kelvin) + Biome*log1p(MAP) +
               Topo*log1p(MAT_Kelvin) + Topo*log1p(MAP), 
             data = LMDF2)
  }else if(order %in% LMDF2$OrderName){
    tempdf <- LMDF2 %>%
      filter(LMDF2$OrderName == order)
    
    lm <- lm(log1p(OrderRichness) ~ log1p(MAT_Kelvin) + log1p(MAP) + 
                     Biome*log1p(MAT_Kelvin) + Biome*log1p(MAP) +
                     Topo*log1p(MAT_Kelvin) + Topo*log1p(MAP), 
                   data = tempdf)
  }else{
    lm <- "Invalid order name, please try again"
  }
  return(lm)
}

# 3.3 Make a vector of coefficient names
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
  confints <- confint(lm)
  for(j in 1:(length(coef_names) - 1)){
    #coefficient
    coef <- summary(lm)$coefficients[j]
    OrderLMCoefDF[j, order] <- coef
  }
  adjrsquared <- summary(lm)$adj.r.squared
  lastindex <- length(coef_names)
  OrderLMCoefDF[lastindex, order] <- adjrsquared
}

saveRDS(OrderLMCoefDF, "Data/OrderLMCoefDF.rds")

# download csv
 write.csv(OrderLMCoefDF, "/Users/haileynapier/Desktop/OrderLMCoefDF.csv")


# TEST AICS FOR EACH ORDER FOR LMS WITH DIFFERENT NUMBERS OF PARAMETERS
# 4.0 Write a new function that that alters the lm and the order -------------
# 4.1 Make a new dataframe with log transformed variables
LogTransLMDF <- LMDF2
LogTransLMDF$LogMAP <- log1p(LogTransLMDF$MAP)
LogTransLMDF$LogMAT <- log1p(LogTransLMDF$MAT_Kelvin)
LogTransLMDF$LogOrdRich <- log1p(LogTransLMDF$OrderRichness)
LogTransLMDF$LogTotRich <- log1p(LogTransLMDF$TotalRichness)
  
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
  }else if(order == "AllOrders"){
    lm <- lm(as.formula(paste("LogTotRich", "~", paste(lm_parameters[parameter_index_vector], collapse="+"))), data=LogTransLMDF)
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
for(i in 1:4){
  combs <- combn(parameter_index, i)
  ncombs <- ncol(combs)
  for(j in 1:ncombs){
    index <- index + 1
    parameter_comb_list[[index]] <- combs[,j]
  }
}

# 4.5 Make a dataframe for loop output
parameters <- c("MAT", 
                "MAP", 
                "Biome_int", 
                "Topo_int", 
                "MAT + MAP", 
                "MAT + Biome_int", 
                "MAT + Topo_int",
                "MAP + Biome_int",
                "MAP + Topo_int", 
                "Biome_int + Topo_int", 
                "MAT + MAP + Biome_int", 
                "MAT + MAP + Topo_int", 
                "MAT + Biome_int + Topo_int", 
                "MAP + Biome_int + Topo_int",
                "MAT + MAP + Biome_int + Topo_int")

OrderLM_AIC_DF <- NULL
OrderLM_AIC_DF<- data.frame(
  "Parameters" = parameters,
  "Dicranales_AICs" = rep(NA, length(parameters)), 
  "Dicranales_adjR2" = rep(NA, length(parameters)), 
  "Hypnales_AICs" = rep(NA, length(parameters)),
  "Hypnales_adjR2" = rep(NA, length(parameters)),
  "Bartramiales_AICs" = rep(NA, length(parameters)),
  "Bartramiales_adjR2" = rep(NA, length(parameters)),
  "Bryales_AICs" = rep(NA, length(parameters)),
  "Bryales_adjR2" = rep(NA, length(parameters)),
  "Grimmiales_AICs" = rep(NA, length(parameters)),
  "Grimmiales_adjR2" = rep(NA, length(parameters)),
  "Hookeriales_AICs" = rep(NA, length(parameters)),
  "Hookeriales_adjR2" = rep(NA, length(parameters)),
  "Orthotrichales_AICs" = rep(NA, length(parameters)),
  "Orthotrichales_adjR2" = rep(NA, length(parameters)),
  "Pottiales_AICs" = rep(NA, length(parameters)),
  "Pottiales_adjR2" = rep(NA, length(parameters)),
  "Funariales_AICs" = rep(NA, length(parameters)),
  "Funariales_adjR2" = rep(NA, length(parameters)),
  "Hedwigiales_AICs" = rep(NA, length(parameters)),
  "Hedwigiales_adjR2" = rep(NA, length(parameters)),
  "Polytrichales_AICs" = rep(NA, length(parameters)),
  "Polytrichales_adjR2" = rep(NA, length(parameters)),
  "Sphagnales_AICs" = rep(NA, length(parameters)), 
  "Sphagnales_adjR2" = rep(NA, length(parameters))) 

# 4.6 Loop
# Loop through each order
  # for each order, loop through each parameter combination and make lm
    # store AIC and R2 values for each lm in dataframe

for(i in 1:length(MossOrdRich10to100)){
  order <- MossOrdRich10to100[i]
  AIC_colname <- paste(order, "_AICs", sep = "")  
  adjR2_colname <- paste(order, "_adjR2", sep = "")
  for(j in 1: length(parameter_comb_list)){
    par_vec <- parameter_comb_list[[j]]
    lm <- order_any_lm(order, par_vec)
    AIC <- AIC(lm)
    adjR2 <- summary(lm)$adj.r.squared
    OrderLM_AIC_DF[j, AIC_colname] <- AIC
    OrderLM_AIC_DF[j, adjR2_colname] <- adjR2
  }
}

saveRDS(OrderLM_AIC_DF, "Data/OrderLM_AIC_DF.rds")

#download csv
write.csv(OrderLM_AIC_DF, "/Users/haileynapier/Desktop/OrderLM_AIC_DF.csv")


# COEFFICIENT STATISTICS
# 5.0 Make a dataframe with coefficient statistics --------------------------------
#left Hookeriales out of this one because it's weird, not sure what's going on there

# 5.1 Make a vector of moss names minus Hookeriales
MossOrdRich10to100NoHook <- MossOrdRich10to100[which(MossOrdRich10to100 != "Hookeriales")]
MossOrdRich10to100NoHook

# 5.2 Make a dataframe for loop output
OrderLMCoefStatsDF <- NULL
OrderLMCoefStatsDF <- data.frame(
  "Coefficient" = coef_names,
  "Dicranales" = rep(NA, length(coef_names)),
  "Dicranales_pval" = rep(NA, length(coef_names)),
  "Dicranales_confint_lower" = rep(NA, length(coef_names)),
  "Dicranales_confint_upper" = rep(NA, length(coef_names)),
  "Hypnales" = rep(NA, length(coef_names)),
  "Hypnales_pval" = rep(NA, length(coef_names)), 
  "Hypnales_confint_lower" = rep(NA, length(coef_names)),
  "Hypnales_confint_upper" = rep(NA, length(coef_names)),
  "Bartramiales" = rep(NA, length(coef_names)),
  "Bartramiales_pval" = rep(NA, length(coef_names)),
  "Bartramiales_confint_lower" = rep(NA, length(coef_names)),
  "Bartramiales_confint_upper" = rep(NA, length(coef_names)),
  "Bryales" = rep(NA, length(coef_names)),
  "Bryales_pval" = rep(NA, length(coef_names)),
  "Bryales_confint_lower" = rep(NA, length(coef_names)),
  "Bryales_confint_upper" = rep(NA, length(coef_names)),
  "Grimmiales" = rep(NA, length(coef_names)),
  "Grimmiales_pval" = rep(NA, length(coef_names)),
  "Grimmiales_confint_lower" = rep(NA, length(coef_names)),
  "Grimmiales_confint_upper" = rep(NA, length(coef_names)),
  "Orthotrichales" = rep(NA, length(coef_names)),
  "Orthotrichales_pval" = rep(NA, length(coef_names)),
  "Orthotrichales_confint_lower" = rep(NA, length(coef_names)),
  "Orthotrichales_confint_upper" = rep(NA, length(coef_names)),
  "Pottiales" = rep(NA, length(coef_names)),
  "Pottiales_pval" = rep(NA, length(coef_names)),
  "Pottiales_confint_lower" = rep(NA, length(coef_names)),
  "Pottiales_confint_upper" = rep(NA, length(coef_names)),
  "Funariales" = rep(NA, length(coef_names)),
  "Funariales_pval" = rep(NA, length(coef_names)),
  "Funariales_confint_lower" = rep(NA, length(coef_names)),
  "Funariales_confint_upper" = rep(NA, length(coef_names)),
  "Hedwigiales" = rep(NA, length(coef_names)),
  "Hedwigiales_pval" = rep(NA, length(coef_names)),
  "Hedwigiales_confint_lower" = rep(NA, length(coef_names)),
  "Hedwigiales_confint_upper" = rep(NA, length(coef_names)),
  "Polytrichales" = rep(NA, length(coef_names)),
  "Polytrichales_pval" = rep(NA, length(coef_names)),
  "Polytrichales_confint_lower" = rep(NA, length(coef_names)),
  "Polytrichales_confint_upper" = rep(NA, length(coef_names)),
  "Sphagnales" = rep(NA, length(coef_names)),
  "Sphagnales_pval" = rep(NA, length(coef_names)),
  "Sphagnales_confint_lower" = rep(NA, length(coef_names)),
  "Sphagnales_confint_upper" = rep(NA, length(coef_names)))

# 5.3 Loop
# Loop output is a dataframe with coefficients and adjusted r squared for lm for each order
# Also includes p values and 95% confidence intervals for each coefficient
for(i in 1:length(MossOrdRich10to100NoHook)){
  order <- MossOrdRich10to100NoHook[i]
  lm <- order_lm(order)
  confints <- confint(lm)
  for(j in 1:(length(coef_names) - 1)){
    #coefficient
    coef <- summary(lm)$coefficients[j]
    OrderLMCoefStatsDF[j, order] <- coef
    
    #confidence interval for each coefficient
    lower_lim_name <- paste(order, "confint_lower", sep = "_")
    lower_lim <- confints[j,1]
    OrderLMCoefStatsDF[j,lower_lim_name] <- lower_lim
    upper_lim_name <- paste(order, "confint_upper", sep = "_")
    upper_lim <- confints[j,2]
    OrderLMCoefStatsDF[j, upper_lim_name] <- upper_lim
    
    #p value for each coefficient
    pval_name <- paste(order, "pval", sep = "_")
    pval <- summary(lm)$coefficients[j,4]
    OrderLMCoefStatsDF[j, pval_name] <- pval
  }
  adjrsquared <- summary(lm)$adj.r.squared
  lastindex <- length(coef_names)
  OrderLMCoefStatsDF[lastindex, order] <- adjrsquared
}

saveRDS(OrderLMCoefStatsDF, "Data/OrderLMCoefDF.rds")

# 5.4 Adjust coefficients for biomes (intercept is coniferous forest)
# Make dataframe for loop output 
nrows <- length(BiomeNames)*length(MossOrdRich10to100NoHook)
OrderLMAdjBiomeCoefDF <- data.frame("Biome" = rep(BiomeNames, length(MossOrdRich10to100NoHook)), 
                                    "Order" = rep(NA, nrows), 
                                    "Coefficient"  = rep(NA, nrows), 
                                    "LowLim" = rep(NA, nrows), 
                                    "UpLim" = rep(NA, nrows)) 

# Loop that adjusts biome coefficients and puts data into correct format for plotting
end <- 0
for(i in 1:length(MossOrdRich10to100NoHook)){
  start <- end + 1
  end <- start + length(BiomeNames) - 1
  
  order <- MossOrdRich10to100NoHook[i]
  lowlim_name <- paste(order, "_confint_lower", sep = "")
  uplim_name <- paste(order, "_confint_upper", sep = "")
  
  intercept <- OrderLMCoefStatsDF[1,order]
  lowlim_intercept <- OrderLMCoefStatsDF[1,lowlim_name]
  uplim_intercept <- OrderLMCoefStatsDF[1,uplim_name]
  
  coefvec <- c(intercept, OrderLMCoefStatsDF[4:13, order])
  lowlim_vec <- c(lowlim_intercept, OrderLMCoefStatsDF[4:13, lowlim_name])
  uplim_vec <- c(uplim_intercept, OrderLMCoefStatsDF[4:13, uplim_name])
  
  OrderLMAdjBiomeCoefDF$Order[start:end] <- order
  OrderLMAdjBiomeCoefDF$Coefficient[start:end] <- coefvec
  OrderLMAdjBiomeCoefDF$LowLim[start:end] <- lowlim_vec
  OrderLMAdjBiomeCoefDF$UpLim[start:end] <- uplim_vec
}

saveRDS(OrderLMAdjBiomeCoefDF, "Data/OrdAdjBiomeLMCoefDF.rds")

# 6.0 DATA VIZ
# plot that shows coefficients and confidence intervals for each order for any parameter

# 6.1 Make a dataframe for data viz
# column for order
# column for parameter
# column for coefficient
# column for lower limit of confidence interval (LowLim)
# column for upper limit of confidence intercal (UpperLim)

test <- order_lm("Hypnales")
parameters <- c(names(test$coefficients))

nrow <- length(MossOrdRich10to100NoHook)*length(parameters)
OrderCoefPlotDF <- data.frame(
  "Parameter" = rep(parameters, length(MossOrdRich10to100NoHook)),
  "Order" = rep(NA,nrow),
  "Coefficient" = rep(NA, nrow), 
  "LowLim" = rep(NA, nrow), 
  "UpperLim" = rep(NA, nrow))

end <- 0
for(i in 1:length(MossOrdRich10to100NoHook)){
  start <- end + 1
  end <-  start + length(parameters) - 1
  
  order <- MossOrdRich10to100NoHook[i]
  lower_lim_name <- paste(order, "confint_lower", sep = "_")
  upper_lim_name <- paste(order, "confint_upper", sep = "_")
  
  coef_vec <- OrderLMCoefStatsDF[[order]]
  lower_lim_vec <- OrderLMCoefStatsDF[[lower_lim_name]]
  upper_lim_vec <- OrderLMCoefStatsDF[[upper_lim_name]]
  
  OrderCoefPlotDF$Order[start:end] <- order
  OrderCoefPlotDF$Coefficient[start:end] <- coef_vec
  OrderCoefPlotDF$LowLim[start:end] <- lower_lim_vec
  OrderCoefPlotDF$UpperLim[start:end] <- upper_lim_vec
}

saveRDS(OrderCoefPlotDF, "Data/OrderCoefPlotDF.rds")

# See MossOrderLMPlots.R for plotting code
