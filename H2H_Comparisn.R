setwd("~/BLR_HACKATHON")
library(dplyr)
library(plyr)
require (tidyr)
require (reshape2)
library(data.table)
library(tibble)


options("scipen"=100, "digits"=15)
Raw_Data<-read.csv("Inno_Data1.csv",header = TRUE)
Raw_Data<-Raw_Data %>% distinct

################################################################################
# Creata combination for H2H
combW <- with(Raw_Data, paste(TEST_SET_ID,FIELD_NAME,FIELD_LATITUDE, FIELD_LONGITUDE))
# Create a column to identify H2H
Raw_Data<-within(Raw_Data, id <- match(combW, unique(combW)))
################################################################################
# Missing value replacement
replace_missings <- function(x, replacement) {
  is_miss <- is.na(x)
  x[is_miss] <- replacement
  
  message(sum(is_miss), " missings replaced by the value ", replacement)
  x
}

Raw_Data<-replace_missings(Raw_Data,"")


################################################################################
Modified_Data<-with(Raw_Data, aggregate(ENTRY_MEAN, 
                                        by=list(CROP_NAME=CROP_NAME,
                                                TARGET_NAME=TARGET_NAME,
                                                TYPE=TYPE,
                                                STAGE=STAGE,
                                                EXPER_DESIGN_NAME=EXPER_DESIGN_NAME,
                                                PROGRAM_REF_ID=PROGRAM_REF_ID,
                                                SET_RM=SET_RM,
                                                TEST_SET_ID=TEST_SET_ID,
                                                TEST_SET_NAME=TEST_SET_NAME,
                                                HARVEST_TYPE=HARVEST_TYPE,
                                                PRODUCT_NAME=PRODUCT_NAME,
                                                TEST_STAGE_YEAR=TEST_STAGE_YEAR,
                                                TEST_SET_SEASON=TEST_SET_SEASON,
                                                COUNTRY_NAME=COUNTRY_NAME,
                                                SUB_COUNTRY_NAME=SUB_COUNTRY_NAME,
                                                SUB_SUB_COUNTRY_NAME=SUB_SUB_COUNTRY_NAME,
                                                LOCATION_NAME=LOCATION_NAME,
                                                FIELD_NAME=FIELD_NAME,
                                                FIELD_LATITUDE=FIELD_LATITUDE,
                                                FIELD_LONGITUDE=FIELD_LONGITUDE,
                                                IS_IRRIGATED=IS_IRRIGATED,
                                                PLANTING_DATE=PLANTING_DATE,
                                                HARVEST_DATE=HARVEST_DATE,
                                                HARVEST_YEAR=HARVEST_YEAR,
                                                TRAIT_ABBREV_NAME=TRAIT_ABBREV_NAME,
                                                id=id),FUN='mean'))


data.table::setnames(Modified_Data,"x","ENTRY_MEAN")

x<-count(Modified_Data$id)

Modified_Data<-left_join(x = Modified_Data,y = x, by = c("id" = "x"))

Modified_Data<-Modified_Data[Modified_Data$freq >= 2,]


################################################################################

split_Data<-split(Modified_Data,list(Modified_Data$id)) 

################################################################################
################################################################################
Analyzed_result<-data.frame()

for (i in 1:length(split_Data)){
  mydata<-data.frame()
  mynames<-data.frame()
  mydata<-ldply(split_Data[i],.id = NULL)
  mynames<-distinct(subset(mydata,select = c("CROP_NAME","TARGET_NAME","TYPE","STAGE","EXPER_DESIGN_NAME",
                                             "PROGRAM_REF_ID","SET_RM", "TEST_SET_ID", "TEST_SET_NAME","HARVEST_TYPE",
                                             "TEST_STAGE_YEAR","TEST_SET_SEASON","COUNTRY_NAME",
                                             "SUB_COUNTRY_NAME","SUB_SUB_COUNTRY_NAME","LOCATION_NAME",
                                             "FIELD_NAME","FIELD_LATITUDE","FIELD_LONGITUDE","IS_IRRIGATED",
                                             "PLANTING_DATE","HARVEST_DATE","HARVEST_YEAR")))
  mydata1<-subset(mydata,select = c("PRODUCT_NAME","ENTRY_MEAN"))
  row.names(mydata1)<-mydata1$PRODUCT_NAME
  res = sapply(mydata1$ENTRY_MEAN, function(x) round(mydata1$ENTRY_MEAN-x,3))
  colnames(res) = row.names(mydata1)
  rownames(res) = row.names(mydata1)
  res_flat<-reshape2::melt(res)
  res_flat<-res_flat[res_flat$value != 0,]
  res_flat<-left_join(x = res_flat,y = mydata1, by = c("Var1" = "PRODUCT_NAME"))
  data.table::setnames(res_flat,"ENTRY_MEAN","Head_ENTRY_MEAN")
  res_flat<-left_join(x = res_flat,y = mydata1, by = c("Var2" = "PRODUCT_NAME"))
  data.table::setnames(res_flat,"ENTRY_MEAN","Comparison_ENTRY_MEAN")
  data.table::setnames(res_flat,"Var1","Head")
  data.table::setnames(res_flat,"Var2","Comparison")
  data.table::setnames(res_flat,"value","Difference")
  temp_result<-merge(mynames,res_flat)
  Analyzed_result<-bind_rows(Analyzed_result,temp_result)
  print(i)
}

rm(combW,i,mydata,mydata1,mynames,res,res_flat,temp_result,x)
