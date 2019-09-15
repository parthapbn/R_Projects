### Data Quality Visualization using CV and LQI ----------
### Partha Banerjee --------

### Required Libraries ----
library(tidyverse)
library(ggridges)
### Setting Work Dir -----
getwd()
setwd("C:/Users/PBANE/OneDrive - Monsanto/Migrated from My PC/Desktop/Working")

### Data Upload -------
apacData <- read.csv("export.csv")

### Data preperation ----
apacData <-separate(apacData, TEST_SET_SEASON, c("PLANTING_YEAR", "MONTH"))
apacData$PLANTING_YEAR <- as.factor(apacData$PLANTING_YEAR)
sapply(apacData, class)
# Filterin P0 to P4
apacData <- apacData %>% filter(EXPERIMENT_STAGE_REF_ID %in% c("P0", "P1", "P2", "P3", "P4", "S1", "S2", "S3"), PLANTING_YEAR %in% c(2015:2018))

### Visualizing CV_LQI distribution --------
ggplot(apacData, aes(VALUE, EXPERIMENT_STAGE_REF_ID, fill=EXPERIMENT_STAGE_REF_ID)) +
  geom_density_ridges(color = "black", size = .1) +
  scale_x_log10(breaks = c(5, 10, 15, 20, 25, 30, 40))+
  geom_vline(xintercept = 15, linetype = "dashed", size = 1.5, color = "red")+
  theme(legend.position = "none", axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))+
  labs(title = "ASIA CV/LQI OF 27576 FIELDS - PIPELINE STAGES",
       subtitle = "Planting Year 2013-2016",
       x = "CV/LQI (log10 scale)",
       color = "NULL")


library(tidyverse)

ind_data <- apacData %>% 
  filter(COUNTRY_ABBREVIATION == "IND")
pansea_data <- apacData %>% 
  filter(Region == "PANSEA")



plot_india <- ggplot(apacData, aes(COUNTRY_NAME, VALUE, fill=PLANTING_YEAR))+
  geom_boxplot(outlier.colour = "red", outlier.size = .7)+
  # geom_jitter(size=.3, alpha=.05, height = .1)+
  geom_hline(yintercept = 15, color="blue", size=.5, linetype="dashed")+
  scale_y_log10(breaks=c(5, 10, 15, 20, 25, 30, 40, 50, 60, 70, 80, 90, 100))+
  theme_minimal()+
  labs(title = "ASIA CV/LQI Distribution",
       x= "Country",
       y= "CV/LQI")
  
plot_india


plot_pansea <- ggplot(pansea_data, aes(EXPERIMENT_STAGE_REF_ID, CV_LQI, fill=PLANTING_YEAR))+
  geom_boxplot(outlier.colour = "red", outlier.size = 1.5)+
  geom_jitter(size=.3, alpha=.05, height = .1)+
  geom_hline(yintercept = 15, color="red", size=1, linetype="dashed")+
  scale_y_log10(breaks=c(5, 10, 15, 20, 25, 30, 40, 50, 60, 70, 80, 90, 100))+
  theme_minimal()+
  labs(title = "PANSEA Pipeline Stage And Planting Year Wise CV/LQI Distribution",
       x= NULL,
       y= "CV/LQI (log10 scale)")

plot_pansea
library(gridExtra)
grid.arrange(plot_india, plot_pansea)
  


# library




##  CV_LQI distribution as function of mechanization in SSA
afqdata_SSA <- afqdata %>% 
  filter(PIPELINE == "SSA")

ggplot(afqdata_SSA, aes(PLANTING_YEAR, CV_LQI, fill= MECHANIZATION))+
  geom_jitter(aes(fill = MECHANIZATION), size=1, alpha=.1, height = .1)+
  geom_boxplot(outlier.colour = "red")+
  geom_hline(yintercept = 15, color="red", size=1, linetype="dashed")+
  scale_y_log10(breaks=c(5, 10, 15, 20, 25, 30, 40, 50, 60, 70, 80, 90, 100))


library(readr)

write_excel_csv(apacData, path = "apacData.csv")

ggplot(ind_data, aes(PLANTING_YEAR, CV_LQI, fill= MECHANIZATION))+
  geom_jitter(aes(fill = MECHANIZATION), size=1, alpha=.1, height = .1)+
  geom_boxplot(outlier.colour = "red")+
  geom_hline(yintercept = 15, color="red", size=1, linetype="dashed")+
  scale_y_log10(breaks=c(5, 10, 15, 20, 25, 30, 40, 50, 60, 70, 80, 90, 100))



## Repeatibility RSA
##### Repeatibility YLD_BE##########################################
ggplot(rdata_r, aes(ME, YLD_BE, fill=ADV_YEAR))+
  geom_boxplot(outlier.color = "red")+
  geom_hline(yintercept = .2, linetype="dashed", color="red", size=1)+
  geom_jitter(alpha=.1)+
  ggtitle("YLD_BE REPEATIBILITY (all sets)")


########Repeatibility MST_BE#####################################

##### Repeatibility YLD_BE##########################################
ggplot(rdata_r, aes(ME, YLD_BE, fill=ADV_YEAR))+
  geom_boxplot(outlier.color = "red")+
  geom_hline(yintercept = .2, linetype="dashed", color="red", size=1)+
  geom_jitter(alpha=.1)+
  ggtitle("YLD_BE REPEATIBILITY (all sets)")


########Repeatibility MST_BE#####################################

sapply(rsa_rpt, class)

yld_rpt <- ggplot(rsa_rpt, aes(factor(COUNTRY), YLD_BE, fill=factor(YEAR)))+
  geom_boxplot(outlier.colour = "red")+
  geom_hline(yintercept = .3, linetype="dashed", size=1)+
  geom_jitter(alpha=.1)+
  ggtitle("YLD_BE REPEATIBILITY (2015-2016 all sets")

mst_rpt <- ggplot(rsa_rpt, aes(factor(COUNTRY), MST_BE, fill=factor(YEAR)))+
  geom_boxplot(outlier.colour = "red")+
  geom_hline(yintercept = .15, linetype="dashed", size=1)+
  geom_jitter(alpha=.1)+
  ggtitle("MST_BE REPEATIBILITY (2015-2016 all sets")

library(gridExtra)
grid.arrange(yld_rpt, mst_rpt, ncol=1)




