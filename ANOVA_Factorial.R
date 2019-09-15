### Factorial ANOVA
# Partha Banerjee
# 23 May 2019

# Required packages
library(car)
library(ggplot2)
library(pastecs) # for descriptive stats

# Data upload in R
goggles <- read.csv("~/DataScienceAnalytics/R/Discovering_R_Using_Stastics/Chapter12_FactorialANOVA/goggles.csv", header = TRUE)

# Data Exploration
summary(goggles)
str(goggles)

# releveling factors
goggles$alcohol <- factor(goggles$alcohol, levels = c("None", "2 Pints", "4 Pints"))

# Descriptive stats
by(goggles$attractiveness, list(goggles$gender, goggles$alcohol), stat.desc)
 
# EDA through visualization
ggplot(goggles, aes(alcohol, attractiveness, fill = gender))+
  stat_summary(fun.y = mean, geom = "bar", position = "dodge")+
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", position = position_dodge(width = .9), width = .2)

# Testing assumptions
# Visualization of homogenity of variance
ggplot(goggles, aes(alcohol, attractiveness, color = gender))+
  geom_boxplot()

# Levene's test for homoginity of variance
leveneTest(goggles$attractiveness, interaction(goggles$alcohol, goggles$gender), center = median)

# ANOVA
gogglesModel <- aov(attractiveness ~ gender + alcohol + gender:alcohol, data = goggles)

Anova(gogglesModel, type = "III")

# Visualization
ggplot(goggles, aes(alcohol, attractiveness, color = gender))+
  stat_summary(fun.y = mean, geom = "point", size = 3)+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = .2)+
  stat_summary(fun.y = mean, geom = "line", aes(group = gender), linetype = "dashed", size = .5)
