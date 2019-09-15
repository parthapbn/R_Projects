### Product performance differance in different experimental (PCM) stages-------
### Partha P Banerjee ----
### 20 May 2019

### Setup working directory ----
getwd()
setwd("C:/Users/PBANE/OneDrive - Monsanto/Migrated from My PC/Desktop/Working/YLD_vs_ProdPerformance")

### Required packages ------------
library(dplyr)
library(ggplot2)
library(Hmisc)
library(pastecs)
library(car)

## Import data ------------------
yldData <- read.csv("yldDataIndiaAllPCM2014_18.csv", header = TRUE)

# Changing level order
levels(yldData$STAGE)
yldData$STAGE <- factor(yldData$STAGE, levels = c("P1", "P2", "P3", "P4", "MD"))

## Filtering data ---------------
yldDataFiltered <- yldData %>% filter(TARGET_NAME == "ME4"
                                      , HARVEST_YEAR == 2018
                                      # , PRODUCT_NAME == "DKC9120" | PRODUCT_NAME == "P3522"
                                      )

## Data exploration

# Density plot
yldDataP4 <- yldDataFiltered %>% filter(STAGE == "P4")

ggplot(yldDataP4, aes(ENTRY_MEAN))+
  geom_density()

yldDataMD <- yldData %>% filter(TARGET_NAME == "ME1") %>% filter(STAGE == "MD") %>% filter(HARVEST_YEAR == 2018)

ggplot(yldDataMD, aes(ENTRY_MEAN))+
  geom_density()

# Product stage vs product mean
ggplot(yldDataFiltered, aes(STAGE, ENTRY_MEAN, fill = PRODUCT_NAME)) +
  geom_boxplot(width = .5, outlier.colour = "red", outlier.shape = 8)+
  # geom_jitter(width = .1, alpha = .1, size = .5)+
  # stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = .1)+
  # stat_summary(fun.y = mean, geom = "point", colour = 6, size = 3) +
  # stat_summary(fun.y = mean, aes(group = 1),  geom = "line", linetype = "dashed", colour = "blue")+
  scale_y_continuous(breaks = seq(0,200,10))



