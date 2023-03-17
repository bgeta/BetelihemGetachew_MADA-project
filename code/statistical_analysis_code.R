###############################
# analysis script
#ordinal logistic regression for each predictor 
#and saves the results to the results folder

install.packages("ordinal")
install.packages("rcompanion")
install.packages("MASS")
install.packages("brant")

library(ordinal)  #ordinal regression package
library(rcompanion) #pseudo R square 
library(MASS) #plyr method (for getting data that allows the test of proportional odds)
library(brant)# test of proportional odds
#load needed packages. make sure they are installed.
library(ggplot2) #for plotting
library(broom) #for cleaning up output from lm()
library(here) #for data loading/saving
library(tidymodels) #for building models


#Path to data. Note the use of the here() package and not absolute paths
data_location <- here::here("data","processed_data","processeddata.rds")
processeddata<-readRDS(data_location)

#path to data
#note the use of the here() package and not absolute paths
data_location <- here::here("data","processed_data","processeddata.rds")


######################################
#Data fitting/statistical analysis
######################################

############################
#### Null model fit
# fit ordinal logistic model using HSI as outcome, Gender as predictor
#Null model 
modelnull<- clm(as.factor(HSI)~1,
                data=processeddata,
                link="logit")
fitnull<- broom::tidy(modelnull)
print(fitnull)
# save fit results table
save_summary_location <- here::here("results","modelfitnull.rds")
saveRDS(fitnull, file = save_summary_location) 
###############################################
##Individual models with single predictors
#Gender as predictor
modelGender<- clm(as.factor(HSI)~as.factor(A01),
                  data=processeddata,
                  link="logit")

model1tbl<- broom::tidy(modelGender)
print(model1tbl)
# save fit results table  
save_summary_location <- here::here("results","modelfit1.rds")
saveRDS(model1tbl, file = save_summary_location) 
#######################################
#Current age as predictor
modelAge<- clm(as.factor(HSI)~AGE,
                  data=processeddata,
                  link="logit")
model1tb2<- broom::tidy(modelAge)
print(model1tb2)
save_summary_location <- here::here("results","modelfit2.rds")
saveRDS(model1tb2, file = save_summary_location) 
########################################

#Age of smoking initiation as predictor

modelInitAge<- clm(as.factor(HSI)~B04,
               data=processeddata,
               link="logit")
model1tb3<- broom::tidy(modelInitAge)
print(model1tb3)
save_summary_location <- here::here("results","modelfit3.rds")
saveRDS(model1tb3, file = save_summary_location) 
#########################################
#Education as predictor

modelEdn<- clm(as.factor(HSI)~A04,
                  data=processeddata,
                  link="logit")
model1tb4<- broom::tidy(modelEdn)
print(model1tb4)
save_summary_location <- here::here("results","modelfit4.rds")
saveRDS(model1tb4, file = save_summary_location) 
###############################################

#Wealth index as predictor

modelWlth<- clm(as.factor(HSI)~Wealth,
                  data=processeddata,
                  link="logit")

model1tb5<- broom::tidy(modelWlth)
print(model1tb5)
save_summary_location <- here::here("results","modelfit5.rds")
saveRDS(model1tb5, file = save_summary_location) 
###############################################

#Residence as a predictor

modelRES<- clm(as.factor(HSI)~RESIDENCE,
                  data=processeddata,
                  link="logit")
model1tb6<- broom::tidy(modelRES)
print(model1tb6)
save_summary_location <- here::here("results","modelfit6.rds")
saveRDS(model1tb6, file = save_summary_location) 
###############################################

#quit attempt as predictor 
modelquitattempt<- clm(as.factor(HSI)~D08,
                    data=processeddata,
                   link="logit")
model1tb7<- broom::tidy(modelquitattempt)
print(model1tb7)
save_summary_location <- here::here("results","modelfit7.rds")
saveRDS(model1tb7, file = save_summary_location) 
###############################################
