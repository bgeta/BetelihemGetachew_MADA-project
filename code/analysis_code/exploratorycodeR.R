###############################
# processing script
#
#this script loads the raw data, processes and cleans it 
#and saves it as Rds file in the processed_data folder
#
# Note the ## ---- name ---- notation
# This is done so one can pull in the chunks of code into the Quarto document
# see here: https://bookdown.org/yihui/rmarkdown-cookbook/read-chunk.html


## ---- packages --------
#load needed packages. make sure they are installed.

library(plyr)  # for data processing/cleaning
library(dplyr) #for data processing/cleaning
library(tidyr) #for data processing/cleaning
library(skimr) #for nice visualization of data 
library(here)  #to set paths 
library(gmodels)#to look at the tables 
library(ggplot2) #to plot histograms and charts
library(janitor) # to create crosstabs
library(vtree)  # to create crosstabs


## ---- loaddata --------
#path to data

#Path to data. Note the use of the here() package and not absolute paths
data_location <- here::here("data","processed_data","processeddata.rds")
processeddata<-readRDS(data_location)

## ---- exploredata --------

## ---- Data Exploration Through Tables --------

## Understanding the Gender and Age(A01) breakdown within each group of HSI 
table1<-tabyl(processeddata, A01,AGE,HSI)%>%
  adorn_percentages("col")%>%
  adorn_pct_formatting(digits=1)
## Understanding the Education and Employement breakdown within ech group of HSI
tabyl(processeddata, A04,A05, HSI)%>%
  adorn_percentages("col")%>%
  adorn_pct_formatting(digits=1)
## Understanding the Wealth and Education breakdown within ech group of HSI
tabyl(processeddata, Wealth,A11,HSI)%>%
  adorn_percentages("col")%>%
  adorn_pct_formatting(digits=1)
## Understanding the RESIDENCE within ech group of HSI
tabyl(processeddata, RESIDENCE,HSI)%>%
  adorn_percentages("col")%>%
  adorn_pct_formatting(digits=1)
## Understanding smoking status and behavior/frequency breakdown within each group of HSI. 
tabyl(processeddata, B01,B07,HSI)%>%
  adorn_percentages("col")%>%
  adorn_pct_formatting(digits=1)
## Understanding the quit intentions breakdown within each group of HSI
tabyl(processeddata,D08,HSI)%>%
  adorn_percentages("col")%>%
  adorn_pct_formatting(digits=1)

## ---- Save Tables --------

# save the above collection of tables to file
save_summary_location <- here::here("results","table1.rds")
saveRDS(table1, file = save_summary_location) 


## ---- Data Exploration Through Figures --------

## Understanding the distribution of age when participant started smoking daily
ggplot(processeddata)+
  aes(x=B04)+ geom_histogram(bins=30L,fill="#0c4c8a")+
  theme_minimal()
# take 2
figure1 <- ggplot(processeddata,aes(HSI,B04))+geom_boxplot()

## ---- Save Figures as png files --------

figure1 <- ggplot(processeddata,aes(HSI,B04))+geom_boxplot()
plot(figure1)+title("HSI and AGE of when participant began daily smoking")
figure_file <- here::here("results","figure1.png")
ggsave(filename = figure_file, plot=figure1)

figure2 <- ggplot(processeddata)+
  aes(x=B04)+ geom_histogram(bins=30L,fill="#0c4c8a")+
  theme_minimal()+ title("Histogram_HSI and AGE of daily smoking")
plot(figure2)
figure_file <- here::here("results","figure2.png")
ggsave(filename = figure_file, plot=figure2)


