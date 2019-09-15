setwd("G:\\Gunnvant Singh\\Data Exploration and Preparation")
cr<-read.csv("Credit.csv",na.strings=c("",NA))
library(dplyr)
options(scipen=999)
##Data Exploration##
#This is a credit card dataset

#Sanity check
#Identify outliers, replace them
#Approaches to impute missing values
#Bin the data-> Quantile function, ntile() for binning
#Partitioning data: test and training samples

names(cr)
#Duplicate columns present, remove them
cr<-cr[,-c(1,12)]

#Sanity check
summary(cr)

#Missing values
index<-which(is.na(cr$Good_Bad))

cr<-cr[-index,]

summary(cr)

#Look at individual summaries

summary(cr$RevolvingUtilizationOfUnsecuredLines) #Ratio variable

cr%>%filter(RevolvingUtilizationOfUnsecuredLines==0)%>%nrow()

cr%>%filter(RevolvingUtilizationOfUnsecuredLines>=0.99)%>%nrow()

#Percentile breakup

quantile(cr$RevolvingUtilizationOfUnsecuredLines,p=c(1:100)/100)



#Discuss with client, 2 is the limit on the number, replace

cr%>%filter(RevolvingUtilizationOfUnsecuredLines<=2)%>%nrow()

cr%>%filter(RevolvingUtilizationOfUnsecuredLines<=2)->cr


summary(cr$age)

cr%>%filter(age==0)%>%nrow()

quantile(cr$age,p=(1:100)/100)

cr%>%filter(age!=0)->cr



summary(cr$Gender)

summary(cr$Region)






summary(cr$MonthlyIncome)

cr%>%filter(MonthlyIncome==0)%>%nrow()

quantile(cr$MonthlyIncome,p=c(1:100)/100,na.rm=TRUE)

cr%>%filter(MonthlyIncome>25000)%>%nrow()



quantile(cr$MonthlyIncome,p=c(990:1000)/1000,na.rm=TRUE)


#We find after discussions that '0' here means a missing value

cr$MonthlyIncome<-ifelse(cr$MonthlyIncome==0,NA,cr$MonthlyIncome)


summary(cr$Rented_OwnHouse)

summary(cr$Occupation)

summary(cr$Education)

summary(cr$NumberOfTime30.59DaysPastDueNotWorse)

quantile(cr$NumberOfTime30.59DaysPastDueNotWorse,p=c(1:100)/100)

quantile(cr$NumberOfTime30.59DaysPastDueNotWorse,p=c(990:1000)/1000,na.rm=TRUE)

# We find on discussions with stakeholders that large numbers are missing values

cr$NumberOfTime30.59DaysPastDueNotWorse<-ifelse(cr$NumberOfTime30.59DaysPastDueNotWorse==98,NA,cr$NumberOfTime30.59DaysPastDueNotWorse)

cr$NumberOfTime30.59DaysPastDueNotWorse<-ifelse(cr$NumberOfTime30.59DaysPastDueNotWorse==96,NA,cr$NumberOfTime30.59DaysPastDueNotWorse)


summary(cr$DebtRatio)

cr%>%filter(DebtRatio==0)%>%nrow()

quantile(cr$DebtRatio,p=c(1:100)/100)

##Cap at 2## (After discussions with stakeholders)

cr$DebtRatio<-ifelse(cr$DebtRatio>2,2,cr$DebtRatio)



summary(cr$NumberOfOpenCreditLinesAndLoans)

quantile(cr$NumberOfOpenCreditLinesAndLoans,p=c(1:100)/100,na.rm=TRUE)

#Higher magnitude numbers represent missing value

cr$NumberOfOpenCreditLinesAndLoans<-ifelse(cr$NumberOfOpenCreditLinesAndLoans==58,NA,cr$NumberOfOpenCreditLinesAndLoans)

cr$NumberOfOpenCreditLinesAndLoans<-ifelse(cr$NumberOfOpenCreditLinesAndLoans==57,NA,cr$NumberOfOpenCreditLinesAndLoans)

cr$NumberOfOpenCreditLinesAndLoans<-ifelse(cr$NumberOfOpenCreditLinesAndLoans==56,NA,cr$NumberOfOpenCreditLinesAndLoans)

cr$NumberOfOpenCreditLinesAndLoans<-ifelse(cr$NumberOfOpenCreditLinesAndLoans==54,NA,cr$NumberOfOpenCreditLinesAndLoans)

cr$NumberOfOpenCreditLinesAndLoans<-ifelse(cr$NumberOfOpenCreditLinesAndLoans>24,NA,cr$NumberOfOpenCreditLinesAndLoans)

summary(cr$NumberOfTimes90DaysLate)

quantile(cr$NumberOfTimes90DaysLate,p=c(1:100)/100)

quantile(cr$NumberOfTimes90DaysLate,p=c(990:1000)/1000,na.rm=TRUE)

#Higher numbers represent missing value

cr$NumberOfTimes90DaysLate<-ifelse(cr$NumberOfTimes90DaysLate==98,NA,cr$NumberOfTimes90DaysLate)

cr$NumberOfTimes90DaysLate<-ifelse(cr$NumberOfTimes90DaysLate==96,NA,cr$NumberOfTimes90DaysLate)

summary(cr$NumberRealEstateLoansOrLines)

quantile(cr$NumberRealEstateLoansOrLines,p=c(1:100)/100)

quantile(cr$NumberRealEstateLoansOrLines,p=c(990:1000)/1000,na.rm=TRUE)

cr%>%filter(NumberRealEstateLoansOrLines==54)%>%nrow()

cr$NumberRealEstateLoansOrLines<-ifelse(cr$NumberRealEstateLoansOrLines==54,NA,cr$NumberRealEstateLoansOrLines)

cr$NumberRealEstateLoansOrLines<-ifelse(cr$NumberRealEstateLoansOrLines==32,NA,cr$NumberRealEstateLoansOrLines)

cr$NumberRealEstateLoansOrLines<-ifelse(cr$NumberRealEstateLoansOrLines>9,NA,cr$NumberRealEstateLoansOrLines)

summary(cr$NumberOfTime60.89DaysPastDueNotWorse)

quantile(cr$NumberOfTime60.89DaysPastDueNotWorse,p=c(1:100)/100,na.rm=TRUE)

cr$NumberOfTime60.89DaysPastDueNotWorse<-ifelse(cr$NumberOfTime60.89DaysPastDueNotWorse==98,NA,cr$NumberOfTime60.89DaysPastDueNotWorse)

cr$NumberOfTime60.89DaysPastDueNotWorse<-ifelse(cr$NumberOfTime60.89DaysPastDueNotWorse==96,NA,cr$NumberOfTime60.89DaysPastDueNotWorse)


summary(cr$NumberOfDependents)

unique(cr$NumberOfDependents)

cr$NumberOfDependents<-as.character(cr$NumberOfDependents)

##Approach towards missing value treatment and data binning

summary(cr)

## See the behaviour of dv wrt the idv which has missing values to arrive  at a missing value imputation

#NumberOfTime30.59DaysPastDueNotWorse
table1<-table(cr$NumberOfTime30.59DaysPastDueNotWorse,cr$Good_Bad)

bad_rate<-table1[,1]/rowSums(table1)

ind2<-which(is.na(cr$NumberOfTime30.59DaysPastDueNotWorse))

table(cr$Good_Bad[ind2])/length(ind2)

## One estimate of NA will be 6
cr$NumberOfTime30.59DaysPastDueNotWorse[ind2]<-6
summary(cr$NumberOfTime30.59DaysPastDueNotWorse)

----------------------------------------------------------------------------------------
##Divide into deciles and see the event rate------------------------------------------
----------------------------------------------------------------------------------------
library(dplyr)

cr%>%mutate(quantile=ntile(MonthlyIncome,10))%>%group_by(Good_Bad,quantile)%>%summarize(N=n())%>%filter(Good_Bad=="Bad")->dat

cr%>%mutate(quantile=ntile(MonthlyIncome,10))%>%group_by(quantile)%>%summarize(N=n())->dat1

dat$Percentage<-dat$N/dat1$N

#Replace with values in 8th quantile
#What is 8th quantile?
quantile(cr$MonthlyIncome,p=(0:10)/10,na.rm=T)
cr$MonthlyIncome[is.na(cr$MonthlyIncome)]<-9200
summary(cr$MonthlyIncome)

##Data Prepration requires binnig data and then collapsing the number of bins
#Age variable

cr%>%mutate(Quantile=ntile(age,10))%>%group_by(Good_Bad,Quantile)%>%summarize(N=n())%>%filter(Good_Bad=="Bad")->dat2

cr%>%mutate(Quantile=ntile(age,10))%>%group_by(Quantile)%>%summarize(N=n())->dat3

dat2$Percent<-dat2$N/dat3$N

quantile(cr$age,p=(0:10)/100,na.rm=T)
============================================================================================

##Partitioning data##
set.seed(100)
indexP<-sample(1:nrow(cr),0.70*nrow(cr),replace = F)
train_cr<-cr[indexP,]
test_cr<-cr[-indexP,]

##Be cautious of balanced and unbalanced samples
#Use caret() 

library(caret)
indexPC<-createDataPartition(y=cr$Good_Bad,times = 1,p=0.70,list=F)
train_crC<-cr[indexPC,]
test_crC<-cr[-indexPC,]

table(train_crC$Good_Bad)/nrow(train_crC)
table(test_crC$Good_Bad)/nrow(test_crC)
