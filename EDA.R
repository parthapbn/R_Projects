### Set up working Dir --------------------------------------------
getwd()
setwd("C:\\Users\\PBANE\\OneDrive - Monsanto\\Migrated from My PC\\Desktop\\Working")

### Required libraries
library(readxl)
library(dplyr)
library(ggplot2)


### Data load -----------------------------------------
data2018 <- read_excel("IND_PM_DATA_2018.xlsx", 
                       sheet = "2018")
data2017 <- read_excel("IND_PM_DATA_2018.xlsx", 
                       sheet = "2017")
data2016 <- read_excel("IND_PM_DATA_2018.xlsx", 
                       sheet = "2016")
data2015 <- read_excel("IND_PM_DATA_2018.xlsx", 
                       sheet = "2015")
data2014 <- read_excel("IND_PM_DATA_2018.xlsx", 
                       sheet = "2014")

myData <- bind_rows(data2014, data2015, data2016, data2017, data2018)

### Data Exploration ----------------------------------
# EDA

myData %>% filter(SEASON == "2018:06") %>% filter(EXPERIMENT_STAGE_REF_ID  == "P4") %>% summarise(entries = count(PEDIGREE_NAME))
unique(myData$EXPERIMENT_STAGE_REF_ID)
