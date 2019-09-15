# Compairing several means - Single Factor ANOVA -------
# Partha P Banerjee ----
# 17 May 2019

### Setup working directory ------------
getwd()
setwd("C:/Users/PBANE/OneDrive - Monsanto/Migrated from My PC/Desktop/Working")

### Required packages
library(ggplot2)
library(pastecs) # for descriptive statistics
library(car) # for Levene's test for homoginity of variance

### Import data -------------------------
viagraData <- read.delim("~/DataScienceAnalytics/R/Discovering_R_Using_Stastics/Chapter10_OnewayANOVA/Viagra.dat")

### EDA ---------------------------------
summary(viagraData)
str(viagraData)

# Variable correction 
viagraData$dose <- ifelse(viagraData$dose == 1, "Placibo", 
                          ifelse(viagraData$dose == 2, "2Pints",
                                 "4Pints"))
viagraData$dose <- factor(viagraData$dose, levels = c("Placibo", "2Pints", "4Pints"))

# Descriptive stats
by(viagraData$libido, viagraData$dose, stat.desc)

# Visualizing data, homogenity/heteroginity of variance, trend etc.
ggplot(viagraData, aes(dose, libido))+
  stat_summary(fun.y = mean, geom = "bar")+
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = .2)

### Testing assumptions-------------------
# Lavenee's test for homoginity of variance
leveneTest(viagraData$libido, viagraData$dose, center = median)

### ANOVA --------------------------------
# if levenee's test is non-significant
viagraModel <- aov(data = viagraData, libido ~ dose)
summary(viagraModel)
plot(viagraModel)
# if variance among groups are not equal (Levene's test significant) 
# Welch's F
oneway.test(data = viagraData, libido ~ dose)

### Contrasts ---------------------
summary.lm(viagraModel)
# Planned contrasts 
# contrasts(viagraData$dose) <- contr.helmert(3)
# contrasts(viagraData$dose) <- contr.treatment(3, base = 1)
# Manually assigning contrasts
contrast1 <- c(-2, 1, 1)
contrast2 <- c(0, -1, 1)
contrasts(viagraData$dose) <- cbind(contrast1, contrast2)
contrasts(viagraData$dose) <- cbind(c(-2, 1, 1), c(0, -1, 1))

viagraPlanned <- aov(data = viagraData, libido ~ dose)
summary.lm(viagraModelPlanned)

### Trend analysis ------------
contrasts(viagraData$dose) <- contr.poly(3)
viagraTrend <- aov(libido ~ dose, data = viagraData)
summary.lm(viagraTrend)
