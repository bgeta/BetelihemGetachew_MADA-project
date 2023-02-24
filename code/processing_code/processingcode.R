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

## ---- loaddata --------
#path to data

data_location <- here::here("data","raw_data","Botswana.sav")
rawdata<- read_sav(data_location)


## ---- exploredata --------
#take a look at the data by conducting the below glimpse, summary and head functions
glimpse(rawdata)

#another way to look at the data
summary(rawdata)

#yet another way to get an idea of the data
head(rawdata)



# This dataset has  4643 number of observations and 608 variables, as part of the data cleaning process 
# The first thing will be to select only variables of interest to make the dataset more managable.
#The variables included in this study are 
#sociodemographics -  A01 (Gender), AGE, A04 (Educational level), A05 (Employment status), Wealth (Wealth index)
#A11 (Marital Status), Residence. 
#Tobacco Smoking - B01 (Do you currenlty smoke tobacco on a daily basis(1), less than daily (2), not at all(3), 
#B04 (Age when you first started smoking daily), B07 (smoking how soon after you wake up),B06A (number of cigarettes smoked), B07 (how soon after you wake you do you usually smoke ( 1-within 5 minutes, 2 - 6 to 30 minutes, 3 -  31 to 60 minutes, 4 -  more than 60 minutes))
# Cessation  - D08 ( thinking about quitting, 1 -  quit within the next month, 2 -  thiking within teh next 12 months, 3 -  quit someday but not next 12 months, 4 -  not interested in quitting)
#Media - G201A1 (noticed information about the dangers of tobacco products 1 - Yes, 2 - No 7- dont know), G201B2 (noticed on televeision)
# G201C1 on Radion , G201D1 on billboards, G201E1 (somewhere else), 
#G202 (notice warnings on cig packages)

## ---- cleandata1 --------

#reducing the variables from 608 to 17 variables by sub-setting for variables of interest AS indicated above and assinging the appropriate class  

rawdata1<- rawdata[,c("A01", "A11","RESIDENCE", "AGE","A04", "A05", "Wealth", "B01","B04","B07","B06A", "D08","G201C1", "G201D1", "G201B2", "G201E1", "G202")]

## ---- cleandata2 --------
#creating a subset for smokers only based on B01 Do you currently smoke response?
#only interested in responses 1,2 only (for daily and less than daily smokers)

rawdata2=subset(rawdata1, B01 !=3)
#check if the subset worked properly 
View(rawdata2)
dim(rawdata2)

#The dataset now has 631 observations and 17 variables 

## ---- cleandata3 --------
#now add the computed variable of interest HSI Heavy smoking Index by adding B01 and B07,
rawdata2$HSI<-rawdata2$B01 + rawdata2$B07

#check if appropriatly added 
View(rawdata2)
dim(rawdata2)
#we now have 631 observations and 18 variables 

## ---- cleandata3 --------
#checking for outliers and other odd observations 
hist(rawdata2$HSI)
# there is an outlier value of 10 that needs to be removed 
df_rawdata3<-subset(rawdata2, HSI!=10)
#check if outlier removed 
View(df_rawdata3)
hist(df_rawdata3$HSI)

#Check AGE distribution 
ggplot(df_rawdata3)+
  aes(x=AGE)+ geom_histogram(bins=30L,fill="#0c4c8a")+
  theme_minimal()
#There are a few respondents aged greater than 90 which is a possibility, so not removing these values
#Look at each variable individually by using the summary function to detect any odd values
summary(df_rawdata3$A04)
#it looks like there is a value of 99 for refused  that need to be removed 
df_rawdata4<-subset(df_rawdata3, A04!=99)
#check if removed
summary(df_rawdata4$A04)

summary(df_rawdata4$A05)
#it looks like there is a value of 99 and 77 for refused and dont know that need to be removed
df_rawdata5<-subset(df_rawdata4, A05!=99)
#check if removed
summary(df_rawdata5$A05)
df_rawdata6<-subset(df_rawdata5, A05!=77)
#check if removed
summary(df_rawdata6$A05)

summary(df_rawdata6$Wealth)
summary(df_rawdata6$A11)

#Challange I wasnt able to resolve when changing from rawdata2 to df_rawdata3

#check age when first started smoking daily
ggplot(df_rawdata6)+
  aes(x=B04)+ geom_histogram(bins=30L,fill="#0c4c8a")+
  theme_minimal()
#need to remove the 99 for i dont know 
df_rawdata7<-subset(df_rawdata6, B04!=99)
#check if outlier is removed 
ggplot(df_rawdata7)+
  aes(x=B04)+ geom_histogram(bins=30L,fill="#0c4c8a")+
  theme_minimal()
summary(df_rawdata7$B07)
#Summary(df_rawdata7$D08)

View(df_rawdata7)
dim(df_rawdata7)
names(df_rawdata7)
str(df_rawdata7)
#With the removal of missing values, the datset now has 416 observations and 18 variables 

## ---- cleandata4 --------
# Inspecting the data, we find some problems that need addressing:
# First, there is an entry for height which says "sixty" instead of a number. 
# Does that mean it should be a numeric 60? It somehow doesn't make
# sense since the weight is 60kg, which can't happen for a 60cm person (a baby)
# Since we don't know how to fix this, we might decide to remove the person.
# This "sixty" entry also turned all Height entries into characters instead of numeric.
# That conversion to character also means that our summary function isn't very meaningful.
# So let's fix that first.


#this is a nice way to look at data
skimr::skim(df_rawdata7)


## ---- cleandata5 --------

#accuratley designate the class for each of the variables add  
df_rawdata7$A01 <- as.factor(df_rawdata7$A01)
df_rawdata7$A11 <- as.factor(df_rawdata7$A11)
df_rawdata7$AGE <- as.numeric(df_rawdata7$AGE)
df_rawdata7$A04 <- as.numeric(df_rawdata7$A04)
df_rawdata7$RESIDENCE <- as.factor(df_rawdata7$RESIDENCE)
df_rawdata7$A05 <- as.factor(df_rawdata7$A05)
df_rawdata7$B01 <- as.factor(df_rawdata7$B01)
df_rawdata7$B04 <- as.numeric(df_rawdata7$B04)
df_rawdata7$B07 <- as.factor(df_rawdata7$B07)
df_rawdata7$B06A <- as.numeric(df_rawdata7$B06A)
df_rawdata7$B07 <- as.factor(df_rawdata7$B07)
df_rawdata7$D08 <- as.factor(df_rawdata7$D08)
df_rawdata7$Wealth <- as.factor(df_rawdata7$Wealth)
df_rawdata7$HSI <- as.numeric(df_rawdata7$HSI)

## ---- cleandata5 --------

#catagorize HSI scores by 0 for scores 0-2, 1 for scores 3 to 4,2 for scores 5-6scores in low addiction(score 0-2), medium addiction (score 3-4), high addiction (5-6).
df_rawdata7$HSI[df_rawdata7$HSI==0 | df_rawdata7$HSI==1 |df_rawdata7$HSI==2] <- 0
df_rawdata7$HSI[df_rawdata7$HSI==3 | df_rawdata7$HSI==4] <- 1
df_rawdata7$HSI[df_rawdata7$HSI==5 | df_rawdata7$HSI==6] <- 2
#check your changes here 
View(df_rawdata7)
#labeling the values 

df_rawdata7 <- df_rawdata7 %>%
  mutate(A01 = case_when(
    A01 == "1" ~ "Male",
    A01 == "2" ~ "Female")) %>%

mutate(AGE= case_when(
  AGE >= 15 & AGE < 25 ~ "15 to 24", 
  AGE >= 25 & AGE < 45 ~ "25 to 44",
  AGE >= 45 & AGE < 65 ~ "45 to 64",
  AGE >= 65 ~ "65+")) %>%
  
mutate(A04 = case_when(
  A04 == "1" ~ "No Formal Education",
  A04 == "2" ~ "Primary Education",
  A04 == "3" ~ "Primary Education",
  A04 == "4" ~ "Secondary Education",
  A04 == "5" ~ "Secondary Education",
  A04 == "6" ~ "Secondary Education",
  A04 == "7" ~ "Secondary Education",
  A04 == "8" ~ "Higher than Secondary Education",
  A04 == "9" ~ "Higher than Secondary Education")) %>%
  
  mutate(A05 = case_when(
    A05 == "1" ~ "Employed",
    A05 == "2" ~ "Employed",
    A05 == "3" ~ "Employed",
    A05 == "4" ~ "Student",
    A05 == "5" ~ "Homemaker",
    A05 == "6" ~ "Retired or Unemployed",
    A05 == "7" ~ "Retired or Unemployed",
    A05 == "8" ~ "Retired or Unemployed")) %>%
  
  mutate(Wealth = case_when(
    Wealth == "1" ~ "Lowest",
    Wealth == "2" ~ "Low",
    Wealth == "3" ~ "Middle",
    Wealth == "4" ~ "High",
    Wealth == "5" ~ "Higher")) %>%
  
  mutate(A11 = case_when(
    A11 == "1" ~ "Single",
    A11 == "2" ~ "Married",
    A11 == "3" ~ "Single",
    A11 == "4" ~ "Single",
    A11 == "5" ~ "Single")) %>%

  mutate(RESIDENCE = case_when(
    RESIDENCE == "1" ~ "Urban",
    RESIDENCE == "2" ~ "Rural")) %>%
  
  mutate(B01 = case_when(
    B01 == "1" ~ "Daily",
    B01 == "2" ~ "Less than Daily",
    B01 == "1" ~ "Not at all")) %>%
  
  mutate(B07 = case_when(
    B07 == "1" ~ "Within 5 Minutes",
    B07 == "2" ~ "6 to 30 Minutes",
    B07 == "3" ~ "31 to 60 Minutes",
    B07 == "4" ~ "More than 60 Minutes")) %>%
  
  mutate(D08 = case_when(
    D08 == "1" ~ "Quit within the next Month",
    D08 == "2" ~ "Thinking within the next 12 Months",
    D08 == "3" ~ "Quit someday, but not next 12 Months",
    D08 == "4" ~ "Not interested in quitting")) %>%
  
  mutate(HSI = case_when(
    HSI == "0" ~ "Low Addiction",
    HSI == "1" ~ "Medium Addiction",
    HSI == "2" ~ "High Addiction",
)) 

summary(df_rawdata7$HSI)
sapply(df_rawdata7, class)


sapply(df_rawdata7, class)


## ---- savedata --------
processeddata <- df_rawdata7
# location to save file
save_data_location <- here::here("data","processed_data","processeddata.rds")
saveRDS(processeddata, file = save_data_location) 


