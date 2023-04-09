###############################
# processing script
#
#this script loads the raw data, processes and cleans it 
#and saves it as Rds file in the processed_data folder
#
# Note the ## ---- name ---- notation
# This is done so one can pull in the chunks of code into the Quarto document
# see here: https://bookdown.org/yihui/rmarkdown-cookbook/read-chunk.html
#REVISED after submission on 3.7.23

## ---- packages --------
#load needed packages. make sure they are installed.

library(plyr)  # for data processing/cleaning
library(dplyr) #for data processing/cleaning
library(tidyr) #for data processing/cleaning
library(skimr) #for nice visualization of data 
library(here)  #to set paths 
library(haven)  #to read SAV file 
library(gmodels)#to look at the tables 
library(ggplot2) #to plot histograms and charts
library(car) #to recode to 0 and 1
library(foreign)
library(MASS)
library(Hmisc)
library(reshape2)
## ---- loaddata --------
#path to data
getwd()
setwd('C:/Data/GitHub/MADA23/BetelihemGetachew_MADA-project')
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
# G201C1 on Radion , G201D1 on billboards, G201E1 (somewhere else), D01 quit attempt
#G202 (notice warnings on cig packages)

## ---- cleandata1 --------

#reducing the variables from 608 to 13 variables by sub-setting for variables of interest AS indicated above and assinging the appropriate class  

rawdata1<- rawdata[,c("A01", "A11","RESIDENCE", "AGE","A04", "A05", "Wealth", "B01","B04","B07","B06A", "D08","D01")]

## ---- cleandata2 --------
#creating a subset for smokers only based on B01 Do you currently smoke response?
#only interested in responses 1,2 only (for daily and less than daily smokers)
View(rawdata1)

rawdata2=subset(rawdata1, B01 !=3)
#check if the subset worked properly 
View(rawdata2)
dim(rawdata2)

#The dataset now has 631 observations and 13 variables 

## ---- cleandata3 --------
#now add the computed variable of interest HSI Heavy smoking Index by adding B01 and B07,
rawdata2$HSI<-rawdata2$B01 + rawdata2$B07
View(rawdata2)

#check if appropriatly added 
View(rawdata2)
dim(rawdata2)
#we now have 631 observations and 14 variables 

## ---- cleandata3 --------
#checking for outliers and other odd observations 
hist(rawdata2$HSI)
# there is an outlier value of 10 that needs to be removed 
df_rawdata3 <- rawdata2 %>% 
  filter(!HSI %in% c(10))

#check if outlier removed 
View(df_rawdata3)
hist(df_rawdata3$HSI)

#Check AGE distribution 
ggplot(df_rawdata3)+
  aes(x=AGE)+ geom_histogram(bins=30L,fill="#0c4c8a")+
  theme_minimal()
#check age of smoking initiation distribution 
ggplot(df_rawdata3)+
  aes(x=B04)+ geom_histogram(bins=30L,fill="#0c4c8a")+
  theme_minimal()
#There are a few respondents aged greater than 90 which is a possibility, so not removing these values
#Look at each variable individually by using the summary function to detect any odd values
summary(df_rawdata3$AGE)
summary(df_rawdata3$B04)
#it looks like there is a value of 99 for refused  that need to be removed 
df_rawdata4<-subset(df_rawdata3, A04!=99)
df_rawdata4<-subset(df_rawdata3, B04!=99)
df_rawdata4<-subset(df_rawdata3, AGE!=99)
#check if removed
summary(df_rawdata4$AGE)
summary(df_rawdata4$A04)
summary(df_rawdata4$B04)
#it looks like there is a value of 99 and 77 for refused and dont know that need to be removed
df_rawdata5<-subset(df_rawdata4, A05!=99)
#check if removed
summary(df_rawdata5$A05)
df_rawdata6<-subset(df_rawdata5, A05!=77)
#check if removed
summary(df_rawdata6$A05)
summary(df_rawdata6$Wealth)
summary(df_rawdata6$A11)

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
Summary(df_rawdata7$D01)
Summary(df_rawdata7$D08)

View(df_rawdata7)
dim(df_rawdata7)
names(df_rawdata7)
str(df_rawdata7)
#With the removal of missing values, the datset now has 418 observations and 14 variables 

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
df_rawdata7$AGE <- as.integer(df_rawdata7$AGE)
df_rawdata7$A04 <- as.numeric(df_rawdata7$A04)
df_rawdata7$RESIDENCE <- as.factor(df_rawdata7$RESIDENCE)
df_rawdata7$A05 <- as.factor(df_rawdata7$A05)
df_rawdata7$B01 <- as.factor(df_rawdata7$B01)
df_rawdata7$B04 <- as.numeric(df_rawdata7$B04)
df_rawdata7$B07 <- as.factor(df_rawdata7$B07)
df_rawdata7$B06A <- as.numeric(df_rawdata7$B06A)
df_rawdata7$B07 <- as.factor(df_rawdata7$B07)
df_rawdata7$D01 <- as.factor(df_rawdata7$D01)
df_rawdata7$D08 <- as.numeric(df_rawdata7$D08)
df_rawdata7$Wealth <- as.factor(df_rawdata7$Wealth)
df_rawdata7$HSI <- as.numeric(df_rawdata7$HSI)


## ---- cleandata5 --------

#catagorize HSI scores by 0 for scores 0-3 indicating low dependence and 1 for scores 4 to 6 indicating high dependence
df_rawdata7$HSI[df_rawdata7$HSI==0 | df_rawdata7$HSI==1 | df_rawdata7$HSI==2 | df_rawdata7$HSI==3] <- 0
df_rawdata7$HSI[df_rawdata7$HSI==4 | df_rawdata7$HSI==5| df_rawdata7$HSI==6] <- 1


#categorize D08 Thinking about quitting 1 Yes, 0 No
df_rawdata7$D08[df_rawdata7$D08==1 | df_rawdata7$D08==2] <- 1
df_rawdata7$D08[df_rawdata7$D08==3 | df_rawdata7$D08==4] <- 0

#categorize A04 level of Education into no formal 0, primary 1, secondary 2, college 3
df_rawdata7$A04[df_rawdata7$A04==1]<-0
df_rawdata7$A04[df_rawdata7$A04==2 |df_rawdata7$A04==3] <- 1
df_rawdata7$A04[df_rawdata7$A04==4 |df_rawdata7$A04==5|df_rawdata7$A04==6  |df_rawdata7$A04==7] <- 2
df_rawdata7$A04[df_rawdata7$A04==8 |df_rawdata7$A04==9]<-3


#categorize A05 Employment 2 Employed, 1 Unemployed , still trying to resolve why this categorization didnt work
               
df_rawdata7$A05[df_rawdata7$A05==1 |df_rawdata7$A05==2|df_rawdata7$A05==3] <- 2
df_rawdata7$A05[df_rawdata7$A05==4 |df_rawdata7$A05==5|df_rawdata7$A05==6 |df_rawdata7$A05==7|df_rawdata7$A05==8] <- 1

#categorize A11 Marital Status 1 Single, 2 Married till tyring to resolve why this categorization didnt work
df_rawdata7$A11[df_rawdata7$A11==1 |df_rawdata7$A11==3|df_rawdata7$A11==4 |df_rawdata7$A11==5] <- 1
df_rawdata7$A11[df_rawdata7$A11==2 ] <- 2

#catagorize RESIDENCE into Rural 2 and Urban 1
df_rawdata7$RESIDENCE[df_rawdata7$RESIDENCE==2 ] <- 2
df_rawdata7$RESIDENCE[df_rawdata7$RESIDENCE==1 ] <- 1

#Catagorize D01 Attempt to quit 1 Yes,2 No
df_rawdata7$D01[df_rawdata7$D01==2 ] <- 2
df_rawdata7$D01[df_rawdata7$D01==1 ] <- 1

#Catagorize D08 thinking about quitting 1 Yes,0 No
df_rawdata7$D08[df_rawdata7$D08==1|df_rawdata7$D08==2 ] <- 1
df_rawdata7$D08[df_rawdata7$D08==3|df_rawdata7$D08==4 |df_rawdata7$D08==7] <- 0

View(df_rawdata7)


#it looks lsike D01 has many missing data lets check 
sum(is.na(df_rawdata7$D01))
#Also remove D01 too many missing values 
#check your changes here 

#final variables of interest ..removed B01, B07 and B06A variables used to subset desired dataset and calculate HSI
df_rawdata8<- df_rawdata7[,c("A01","RESIDENCE", "AGE","A04","A05","A11", "Wealth", "B04", "D08","D01", "HSI")]
View(df_rawdata8)
#adust age to show with 0 deciman place 
#labeling the values 


df_rawdata9 <- df_rawdata8 %>%
  
  mutate(A01 = case_when(
    A01 == "1" ~ "Male",
    A01 == "2" ~ "Female")) %>%

  
  mutate(A11 = case_when(
    A11 == "1" ~ "Single",
    A11 == "2" ~ "Married")) %>%
  
mutate(A04 = case_when(
  A04 == "0" ~ "No Formal Education",
  A04 == "1" ~ "Primary Education",
  A04 == "2" ~ "Secondary Education",
  A04 == "3" ~ "College and above")) %>%
  
  mutate(A05 = case_when(
    A05 == "2" ~ "Employed",
    A05 == "1" ~ "Unemployed")) %>%

  mutate(Wealth = case_when(
    Wealth == "1" ~ "Lowest",
    Wealth == "2" ~ "Low",
    Wealth == "3" ~ "Middle",
    Wealth == "4" ~ "High",
    Wealth == "5" ~ "Higher")) %>%

  mutate(RESIDENCE = case_when(
    RESIDENCE == "1" ~ "Urban",
    RESIDENCE == "2" ~ "Rural")) %>%

  mutate(D08 = case_when(
    D08 == "0" ~ "No",
    D08 == "1" ~ "Yes")) %>%
  
  mutate(D01 = case_when(
    D01 == "2" ~ "No",
    D01 == "1" ~ "Yes")) %>%
  
  mutate(HSI = case_when(
    HSI == "0" ~ "LowAddiction",
    HSI == "1" ~ "HighAddiction",
  )) 

#added labels for clarity of variables but it didnt work 
df_rawdata9 %>% dplyr::rename(
  Gender = A01,
  Educational_level=A04,
  Age_smoking_Initiation=B04,
  Quit_Intention=D08)

#check if labels are now changed 
head(df_rawdata9)
str(df_rawdata9)

xtabs (~HSI+A01,data=df_rawdata9)#HSI and Gender 
xtabs (~HSI+A04,data=df_rawdata9)#table for HSI and Education
xtabs (~HSI+Wealth,data=df_rawdata9)#table for HSI and Wealth
xtabs (~HSI+RESIDENCE,data=df_rawdata9) # HSI and RESIDENCE
xtabs (~HSI+D08,data=df_rawdata9)#table for HSI and quit intention
#the mean for the two continous variables current age and age of initiation 
ddply(df_rawdata9, .(HSI), summarise, mean(AGE))
ddply(df_rawdata9, .(HSI), summarise, mean(B04))

View(df_rawdata9)


## ---- cleandata5 --------


## ---- savedata --------
processeddata <- df_rawdata9

# location to save file
save_data_location <- here::here("data","processed_data","processeddata.rds")
saveRDS(processeddata, file = save_data_location) 


