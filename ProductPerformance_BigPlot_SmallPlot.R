### Required package
library(readxl)
library(tidyverse)
library(dplyr)
library(magrittr) # for %>% 
library(reshape)
library(pastecs) # descriptive stat
library(ggplot2)
library(ez) # ANOVA
library(nlme) # multilevel ANOVA
library(multcomp) # for post hoc test

# Product shortlisting for analysis. Identifying same products accross pipeline in a given year
### Load data
yldData <- read_excel(path = "C:/Users/PBANE/OneDrive - Monsanto/Migrated from My PC/Desktop/Working/YLD_vs_ProdPerformance/YLD_Data_India_2013-2019.xlsx", sheet = 1)

# Select required columns
yldData <- yldData %>% dplyr::select(ME, GROWING_PROGRAM_REF_ID, TEST_SET_SEASON, HARVEST_YEAR1, EXPERIMENT_STAGE_NAME, EXPERIMENT_STAGE_REF_ID, PRODUCT_NAME, REP_NUMBER, YLD_BE, SUB_COUNTRY_NAME, SUB_SUB_COUNTRY_NAME)
View(head(yldData))

str(yldData)

# Correcting variable type
yldData[, c(1:8, 10, 11)] <- lapply(yldData[, c(1:8, 10, 11)], factor)

yldData$EXPERIMENT_STAGE_REF_ID <- factor(yldData$EXPERIMENT_STAGE_REF_ID, levels = c("P1", "P2", "P3", "P4", "MD"))

# Identifying products which are common in P1 - MD stages 
prodPipeline <- yldData %>% 
  group_by(ME, HARVEST_YEAR1, EXPERIMENT_STAGE_REF_ID, PRODUCT_NAME) %>% summarise(LOCTested = n())

prodPipeline <- prodPipeline %>% 
  filter(ME != "NA")

prodPipeline <- spread(prodPipeline, key = EXPERIMENT_STAGE_REF_ID, LOCTested)

prodPipeline <- prodPipeline %>% 
  filter(!is.na(MD) & !is.na(P4) & !is.na(P3) & !is.na(P2) & !is.na(P1)) %>% 
  arrange(ME, HARVEST_YEAR1)

prodPipeline

### Exploratory Data Analysis ------------------------------------
data2018ME1 <- yldData %>% filter(ME == "ME1", HARVEST_YEAR1 == 2018, PRODUCT_NAME %in% c("DKC9133", "DKC8161", "DKC9141", "NK6240", "P3401", "P3501", "S 6668"))

summary(data2018ME1)
str(data2018ME1)

# Data exploration
# descriptive stat
options(scipen = 999)

by(data2018ME1$YLD_BE, data2018ME1$EXPERIMENT_STAGE_REF_ID, stat.desc)

### Visual data exploration ------------------
# Density plot
ggplot(data2018ME1 %>% filter(EXPERIMENT_STAGE_REF_ID == "P4"), aes(YLD_BE))+
  geom_density()+
  scale_x_continuous(breaks = seq(0, 150, 10))

# Mean YLD_BE accross pipeline
ggplot(data2018ME1, aes(EXPERIMENT_STAGE_REF_ID, YLD_BE))+
  stat_summary(fun.y = mean, geom = "bar", fill = c("737373", "#FF9200", "737373", "#FF9200", "737373"))+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = .1, col = "#FF0000")
# Mean YLD_BE and trend accross pipeline
ggplot(data2018ME1, aes(EXPERIMENT_STAGE_REF_ID, YLD_BE))+
  geom_jitter(width = .1, alpha = .1, size = 1)+
  stat_summary(fun.y = mean, geom = "point", size = 5, color = "red")+
  stat_summary(fun.y = median, group = 1, geom = "line", linetype = "dashed", size = 1,  colour = "blue")

# Product wise YLD_BE in different experiment stages
ggplot(data2018ME1 %>% filter(YLD_BE >= 60 & YLD_BE <= 130), aes(PRODUCT_NAME, YLD_BE, fill = EXPERIMENT_STAGE_REF_ID))+
  stat_summary(fun.y = mean, geom = "bar", position = "dodge")+
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = .9, aes(group = EXPERIMENT_STAGE_REF_ID), position = "dodge" )
# Product performance trend accross stages
ggplot(data2018ME1 %>% filter(YLD_BE >= 40 & YLD_BE <= 150), aes(EXPERIMENT_STAGE_REF_ID, YLD_BE, colour = PRODUCT_NAME))+
  stat_summary(fun.y = mean, geom = "point", size = 3)+
  # stat_summary(fun.data = mean_cl_normal, geom = "errorbar", aes(group = PRODUCT_NAME), position = "dodge", width = .9)+
  stat_summary(fun.y = mean, geom = "line", size = 1, aes(group = PRODUCT_NAME))

### Repeated Measure One Way ANOVA -------------------------
contrasts(data2018ME1$EXPERIMENT_STAGE_REF_ID) <- contr.SAS(5)
contrasts(data2018ME1$EXPERIMENT_STAGE_REF_ID) <- contr.helmert(5)
# Choosing contrasts
P1.2.3.4_MD <- c(1, 1, 1, 1, -4)
P1.2.3_P4   <- c(1, 1, 1, -3, 0)
P1.2_P3   <- c(1, 1, -2, 0, 0)
P1_P2       <- c(1, -1, 0, 0, 0)

contrasts(data2018ME1$EXPERIMENT_STAGE_REF_ID) <- cbind(P1.2.3.4_MD, P1.2.3_P4, P1.2_P3, P1_P2)

data2018ME1$EXPERIMENT_STAGE_REF_ID


### ezANOVA - easy ANOVA
ezyModel <- ezANOVA(data = data2018ME1
                    , dv = .(YLD_BE)
                    , wid = .(PRODUCT_NAME)
                    , within = .(EXPERIMENT_STAGE_REF_ID)
                    , detailed = TRUE
                    , type = 3)
ezyModel

# post hoc test
# Calculating means for accross experimental stage and accross products for paired t test
meanData2018ME1  <-  data2018ME1 %>% group_by(EXPERIMENT_STAGE_REF_ID, PRODUCT_NAME) %>% summarise(meanYLD = mean(YLD_BE))

# post hoc test - Bonferroni
pairwise.t.test(meanData2018ME1$meanYLD, meanData2018ME1$EXPERIMENT_STAGE_REF_ID, paired = TRUE, p.adjust.method = "bonferroni")


### Multilevel Approach -----------------------------------
mlModel <- lme(YLD_BE ~ EXPERIMENT_STAGE_REF_ID, random = ~1|PRODUCT_NAME/EXPERIMENT_STAGE_REF_ID, data = data2018ME1, method = "ML")

summary(mlModel)

baseline <- nlme::lme(YLD_BE ~ 1, random = ~1|PRODUCT_NAME/EXPERIMENT_STAGE_REF_ID, data = data2018ME1, method = "ML")

anova(baseline, mlModel)

### post hoc test 
postHocs <- glht(mlModel, linfct = mcp(EXPERIMENT_STAGE_REF_ID = "Tukey"))
summary(postHocs)
confint(postHocs)

### Effect size measurement -------------------------------
rcontrast <- function(t, df){
  r <- sqrt(t^2/(t^2 + df))
  print(paste("r = ", r))
}

rcontrast(13.77792,24) # P1234_MD
rcontrast(-4.92278, 24) # P123_P4
rcontrast(7.69203, 24) # P12_P3
rcontrast(.55321, 24)
