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
library(tidymodels) #for building model
library(table1)
library("kableExtra") # For producing a nice table 1


## ---- loaddata --------
#path to data

#Path to data. Note the use of the here() package and not absolute paths
data_location <- here::here("data","processed_data","processeddata.rds")
processeddata<-readRDS(data_location)

## ---- exploredata --------

## ---- Data Exploration Through Tables --------

processeddata %>%
  count(HSI)%>%
  mutate(prop = n/sum(n))

library(table1)
Table1_Demographics <- (table1(~ factor(A01) + AGE +B04+ factor(A04) + factor(A05) + factor(Wealth)+factor(A11)+factor(RESIDENCE)+factor(D01)+factor(D08) | HSI, data=processeddata))

Table1_Demographics <- t1kable(Table1_Demographics)
Table1_Demographics


#save demographics table under results 

save_summary_location <- here::here("results","Table1_Demographics.rds")
saveRDS(Table1_Demographics, file = save_summary_location) 

## Understanding the Gender and Age(A01) breakdown within each group of HSI. Also if anyone knows how to order the response options in order rom 
#low to high it would be great for example Education ...no formal, primary, secondary, higher than secondary 
table1 <-tabyl(processeddata, A01,HSI)%>%
  adorn_percentages("col")%>%
  adorn_pct_formatting(digits=1)
## Understanding the Education breakdown within ech group of HSI
table2 <-tabyl(processeddata, A04, HSI)%>%
  adorn_percentages("col")%>%
  adorn_pct_formatting(digits=1)
## Understanding the Wealth and Education breakdown within ech group of HSI
table3 <- tabyl(processeddata, Wealth, HSI)%>%
  adorn_percentages("col")%>%
  adorn_pct_formatting(digits=1)
## Understanding the RESIDENCE within ech group of HSI
table4 <- tabyl(processeddata, RESIDENCE,HSI)%>%
  adorn_percentages("col")%>%
  adorn_pct_formatting(digits=1)
## Understanding smoking quit intention within each group of HSI. 
table5 <- tabyl(processeddata,D08,HSI)%>%
  adorn_percentages("col")%>%
  adorn_pct_formatting(digits=1)


#save the above created 5 tables in the results folder 
## ---- Save Tables --------

# save the above collection of tables to file
save_summary_location <- here::here("results","table1.rds")
saveRDS(table1, file = save_summary_location) 

save_summary_location <- here::here("results","table2.rds")
saveRDS(table2, file = save_summary_location) 

save_summary_location <- here::here("results","table3.rds")
saveRDS(table3, file = save_summary_location) 

save_summary_location <- here::here("results","table4.rds")
saveRDS(table4, file = save_summary_location) 

save_summary_location <- here::here("results","table5.rds")
saveRDS(table5, file = save_summary_location) 


## ---- Data Exploration Through Figures --------

## Understanding the distribution of age when participant started smoking daily

ggplot(processeddata)+
  aes(x=AGE)+ geom_histogram(bins=30L,fill="#0c4c8a")+
  theme_minimal()

processeddata %>%
  ggplot( aes(x=HSI, y=B04, fill=HSI)) +
  geom_boxplot() 
 

## ---- Save Figures as png files  --------

figure1 <- ggplot(processeddata,aes(HSI,AGE))+geom_boxplot()
plot(figure1)+title("HSI and Current Age")
figure_file <- here::here("results","figure1.png")
ggsave(filename = figure_file, plot=figure1)


figure2 <- ggplot(processeddata,aes(HSI,B04))+geom_boxplot()
plot(figure2)+title("HSI and Age of smoking Initiation")
figure_file <- here::here("results","figure2.png")
ggsave(filename = figure_file, plot=figure2)

figure3 <- ggplot(processeddata, aes(AGE)) +
  geom_histogram(aes(fill=HSI), color = "black", binwidth = 2)
plot(figure3)+title("Distribution of AGe by HSI")
figure_file <- here::here("results","figure3.png")
ggsave(filename = figure_file, plot=figure3)

figure4 <- ggplot(processeddata,aes(B04)) +
  geom_histogram(aes(fill=HSI), color = "black", binwidth = 2)
plot(figure4)+title("Distribution of Age of smoking Initiation by HSI")
figure_file <- here::here("results","figure4.png")
ggsave(filename = figure_file, plot=figure4)

#the below three figures saved using the plots tab on the menu. I attempted to write a code but it didnt work

figure3<-mosaicplot(HSI~A01,data=processeddata,col=c("Blue","Pink"))

figure4<-mosaicplot(HSI~D08,data=processeddata,col=c("Green","Gray"))

figure5<-mosaicplot(HSI~A04,data=processeddata,col=c("Blue","Red","Pink","Purple"))


