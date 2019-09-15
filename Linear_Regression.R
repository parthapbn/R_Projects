### Linear Regression Analysis
# Partha Banerjee, 06 June, 2019

# Clear plots -----------------------------
if(!is.null(dev.list())) dev.off()
# Clear console
cat("\014") 
# Clean workspace
rm(list=ls())
# Set working directory
setwd("C:/Users/PBANE/OneDrive - Monsanto/Migrated from My PC/Documents/DataScienceAnalytics/ML_Kaggle/CaliforniaHousingProject")

### REquired Packages ---------------------
library(car) # reg disgonistics
library(boot) # bootstrap sampling
library(QuantPsyc) # reg coef

### Data upload
housingData <- read.csv("housingRegtrain.csv", row.names = "Id")

### Data Exploration ---------------------
summary(housingData)
str(housingData)
