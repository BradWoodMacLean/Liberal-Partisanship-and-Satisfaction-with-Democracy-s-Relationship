#POL314 Final Paper V.2
setwd("/Users/bradleywood-maclean/Desktop")
library(tidyverse)
library(dplyr)
load("2021 Canadian Election Study v1.0.RData")

table2 <- table

#Prepare variables
##create new var that is cps21_demsat but without 5 as response category and re-order correctly
table2$cps21_demsat2 <- replace(table2$cps21_demsat, table2$cps21_demsat==5, NA)

table2 <- mutate(table2,
                 cps21_demsat2 = as.numeric(cps21_demsat2))

table2 <- mutate(table2,
                 cps21_demsat2 = recode_factor(cps21_demsat2,
                                                 "4" = "1",
                                                 "3" = "2",
                                                 "2" = "3",
                                                 "1" = "4") )

table2 <- mutate(table2,
                 cps21_demsat2 = as.numeric(cps21_demsat2))

##create binary measures for partisanship to use in linear regression
table2$Libs <- NA #empty variable
table2$Libs[table2$cps21_fed_id == 2 | 3 | 4 | 5 | 6 | 7 | 8] <- 0 #0 means not lib
table2$Libs[table2$cps21_fed_id == 1] <- 1 #1 mean lib
table(table2$Libs)

#Average Democratic Satisfaction
##Average satisifaction with democracy for Liberals
mean(table2$cps21_demsat2[table$cps21_fed_id==1], na.rm = TRUE) #3.166877

#Average satisfaction with democracy for non-liberals
(mean(table2$cps21_demsat2[table$cps21_fed_id==2], na.rm = TRUE) +
    mean(table2$cps21_demsat2[table$cps21_fed_id==3], na.rm = TRUE) +
    mean(table2$cps21_demsat2[table$cps21_fed_id==4], na.rm = TRUE) +
    mean(table2$cps21_demsat2[table$cps21_fed_id==5], na.rm = TRUE) +
    mean(table2$cps21_demsat2[table$cps21_fed_id==6], na.rm = TRUE) +
    mean(table2$cps21_demsat2[table$cps21_fed_id==7], na.rm = TRUE) +
    mean(table2$cps21_demsat2[table$cps21_fed_id==8], na.rm = TRUE)) / 7 #2.595023

#Linear Regression Model
LibsRegression <- lm(formula = cps21_demsat2 ~ Libs, data = table2)
library(jtools)
summary(LibsRegression)
summ(LibsRegression, confint = TRUE, ci.width = 0.95)
#Coefficients:
#  (Intercept)         Libs  
#2.6568       0.5101 

##create new var that is cps21_fed_gov_sat but without 5 as response category and re-order correctly
table2$cps21_fed_gov_sat2 <- replace(table2$cps21_fed_gov_sat, table2$cps21_fed_gov_sat==5, NA)

table2 <- mutate(table2,
                 cps21_fed_gov_sat2 = as.numeric(cps21_fed_gov_sat2))

table2 <- mutate(table2,
                 cps21_fed_gov_sat2 = recode_factor(cps21_fed_gov_sat2,
                                               "4" = "1",
                                               "3" = "2",
                                               "2" = "3",
                                               "1" = "4") )

table2 <- mutate(table2,
                 cps21_fed_gov_sat2 = as.numeric(cps21_fed_gov_sat2))

#Multiregression (satisfaction with democracy (1-4) = Libs(0-1) + satisfaction with fed govt(1-4) + ideology(0-10))

OLSregression <- lm(formula = cps21_demsat2 ~ Libs + cps21_fed_gov_sat2 + cps21_lr_scale_bef_1, data = table2)
summary(OLSregression)
summ(OLSregression, confint = TRUE, ci.width = 0.95, digits = 4)

#Coefficients:
#  (Intercept)                  Libs    cps21_fed_gov_sat2  cps21_lr_scale_bef_1  
#1.982918              0.129022              0.356371              0.000845 

#Export tables
library(huxtable)
export_summs(LibsRegression)

library(officer)
library(flextable)
export_summs(LibsRegression, 
             error_format = "[{conf.low}, {conf.high}]",
             to.file = "docx", file.name = "LibsRegression.docx")


export_summs(OLSregression)
export_summs(OLSregression, 
             error_format = "[{conf.low}, {conf.high}]",
             digits = 4,
             to.file = "docx", file.name = "OLSregression.docx")










