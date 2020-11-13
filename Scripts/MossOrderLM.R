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
# Find in DataProcessing.R

# 1.0 Add NAs for 0s in TotalRichness ------
# 1.1 Replace 0s in RichnessVec with NAs
RichnessVecNA <- RichnessVec
RichnessVecNA[RichnessVecNA == 0] <- NA
RichnessVecNA

RichnessDFNoNA <- data.frame(Alpha = RichnessVecNA, CellID = 1:15038)
RichnessDFNoNA <- RichnessDFNoNA %>%
  filter(!is.na(RichnessDFNoNA$Alpha))
nrow(RichnessDFNoNA)

RichnessTopoDFNoNA <- left_join(RichnessDFNoNA, AlphaMountLM, by = "CellID")
nrow(RichnessTopoDFNoNA)
range(RichnessDFNoNA$CellID)

(RichnessTopoDFNoNA$CellID)

# 1.2 Replace TotalRichness column in LMDF2 with RichnessVecNA(repeated 22 times)
LMDF2$TotalRichness <- rep(RichnessVecNA, 22)

nrow(LMDF)
nrow(LMDF2)

range(AlphaMountLM$CellID)
nrow(AlphaMountLM)

LMDF2 <- right_join(LMDF, AlphaMountLM, by="CellID")
