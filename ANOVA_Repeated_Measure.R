# ANOVA - repeated measure design
# Partha P Banerjee
# May 26, 2019

# Clear plots -----------------------------
if(!is.null(dev.list())) dev.off()
# Clear console
cat("\014") 
# Clean workspace
rm(list=ls())
# Set working directory
# setwd("----------------")

### Required packages ---
library(reshape)
library(pastecs) # descriptive stat
library(ggplot2)
library(ez) # ANOVA
library(nlme) # multilevel ANOVA
library(multicomp) # for post hoc test

### Load data ------------------
bushtuckerData <- read.delim("~/DataScienceAnalytics/R/Discovering_R_Using_Stastics/Data files/Bushtucker.dat")

### EDA
summary(bushtuckerData)
str(bushtuckerData)

# Data frame restructuring - data should be in long forma
bushtuckerDataLong <- melt(bushtuckerData, id = "participant", measured = c("stick_insect", "kangaroo_testicles", "fish_eye", "witchetty_grub"))
# Renaming columns
colnames(bushtuckerDataLong) <- c("participant", "animal", "retchTime")
bushtuckerDataLong <- bushtuckerDataLong[order(bushtuckerDataLong$participant), ]

# Data exploration
# descriptive stat
by(bushtuckerDataLong$retchTime, bushtuckerDataLong$animal, stat.desc)

# Visual data exploration
ggplot(bushtuckerDataLong, aes(animal, retchTime))+
  stat_summary(fun.y = mean, geom = "bar")+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = .1, colour = "red")

ggplot(bushtuckerDataLong, aes(animal, retchTime))+
  geom_boxplot()

### Building contrasts --------------
insects_animal <- c(1, -1, -1, 1)
testicle_eye <- c(0, -1, 1, 0)
insect_grub <- c(1, 0, 0, -1)
contrasts(bushtuckerDataLong$animal) <- cbind(insects_animal, testicle_eye, insect_grub)
bushtuckerDataLong$animal

### ANOVA
model1 <- ezANOVA(data = bushtuckerDataLong, dv = .(retchTime), wid = .(participant), within = .(animal), detailed = TRUE, type = 3)
model1

# post hoc test -----
pairwise.t.test(bushtuckerDataLong$retchTime, bushtuckerDataLong$animal, paired = TRUE, p.adjust.method = "bonferroni")

### Multilevel Approach
modelMultiLvl <- lme(retchTime ~ animal, random = ~1|participant/animal, data = bushtuckerDataLong, method = "ML")

baseline <- lme(retchTime ~ 1, random = ~1|participant/animal, data = bushtuckerDataLong, method = "ML")

anova(baseline, modelMultiLvl)
summary(modelMultiLvl)

### post hoc test --------------
postHocs <- glth(modelMultiLvl, linfct = mcp(animal = "Tukey"))
summary(postHocs)
confint(postHocs)
