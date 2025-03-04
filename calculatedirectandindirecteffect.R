

#rm(list = ls()) - delete all variables


library(shiny)
library(leaflet)
#library(rgdal)
library(terra)
library(sf)
library(here)
library(tmap)
library(DiagrammeR)
library(ggplot2)

library(readxl)



#Temperature only as Confounder - calculate Direct and Indirect effects on the Outcome variable of Interest 
#Outcome variable is death due to alcohol consumption in this example


#Training for 2010-2023 


outcome_slope1 = vector(,2);
outcome_intercept = vector(,2);
outcome_slope2 = vector(,2);


outcome_slope1= matrix(, nrow = 2, ncol = 2);
outcome_slope2 = matrix(, nrow = 2, ncol = 2);

newpredictions = vector(,2);

realcasesplot = vector(,2);
realcases = vector(,2);

#predictions2 = mat = matrix(, nrow = 34, ncol = 2);

modellfit =  matrix(, nrow = 2, ncol = 2);

gdp_agriculture_intercept = vector(,2)
gdp_agriculture_slope1  = matrix(, nrow = 2, ncol = 2);
gdp_agriculture_slope2  = matrix(, nrow = 2, ncol = 2);

DE = matrix(, nrow = 3, ncol = 8) ;
IE = matrix(, nrow = 3, ncol = 8) ;


compIE1 = matrix(, nrow = 3, ncol = 8) ;
compIE2 = matrix(, nrow = 3, ncol = 8) ;
compIE3 = matrix(, nrow = 3, ncol = 8) ;

intercept1 = matrix(, nrow = 3, ncol = 8) 
intercept2 = matrix(, nrow = 3, ncol = 8) 

sumoutcome =  matrix(, nrow = 3, ncol = 8) 
sumgdpagri =  matrix(, nrow = 3, ncol = 8) 



charac = vector(,8);
charac[1] = "G"
charac[2] = "H"
charac[1] = "I"
charac[2] = "J"
charac[3] = "K"
charac[4] = "L"
charac[5] = "M"
charac[6] = "N"
charac[7] = "O"
charac[8] = "P"


# Index j goes from 1 to 8 as the direct/indirect effects are calculated for 8 different
# periods of time for comparison purposes: 2010 to 2016; 2011 to 2017; 2012 to 2018; 2013 to 2019; 2014 to 2020; 
# 2015 to 2021; 2016 to 2022; 2017 to 2023

for (j in 1:8)
{
  # Index i goes from 1 to 3 corresponding to GDP, GDP from agriculture, GDP from manufacturing 
  for (i in 1:3)
   {
    print(j)
    b1 = as.character(i)
    b2 = as.character(i+1)

  
    train_data <- sprintf("C%s:%s%s",b1,charac[j],b2)
    #test_data <- sprintf("M%s:M%s",b1,b2)


    #Training 
    temperature_train <- read_excel("C:/backup/PredictionDisease/SoftZenodo/Data test.xlsx", sheet = "Maximum Temperature", range = train_data)#S38
    #temperature_test<- read_excel("C:/backup/PredictionDisease/SoftZenodo/Data test.xlsx", sheet = "Maximum Temperature", range = test_data)#S38

    #outcome event of interest 
    outcome_train <- read_excel("C:/backup/PredictionDisease/SoftZenodo/Data test.xlsx", sheet = "Outcome", range = train_data)#G38
    #outcome_test <- read_excel("C:/backup/PredictionDisease/SoftZenodo/Data test.xlsx", sheet = "Outcome", range = test_data)#G38

  
    gdpagriculture_train <- read_excel("C:/backup/PredictionDisease/SoftZenodo/Data test.xlsx", sheet = "GDP", range = train_data)#S38
    #gdpagriculture_test <- read_excel("C:/backup/PredictionDisease/SoftZenodo/Data test.xlsx", sheet = "GDP", range = test_data)#S38


    #train data
    temperature_train_unlist <- unlist(temperature_train)
    outcome_train_unlist <- unlist(outcome_train)
    gdpagriculture_train_unlist <- unlist(gdpagriculture_train)

    #test data
    #gdpagriculture_test_unlist <- unlist(gdpagriculture_test)
    #temperature_test_unlist <- unlist(temperature_test)
    #outcome_test_unlist <- unlist(outcome_test)


    #outcome event of interest 
    outcome <- lm(formula = outcome_train_unlist ~ temperature_train_unlist + gdpagriculture_train_unlist,family = "gaussian")

    #GDP from agriculture
    gdp_agriculture <- lm(formula = gdpagriculture_train_unlist ~ temperature_train_unlist,family = "gaussian")

    #sumoutcome <- summary(outcome)
    #sumgdpagri <- summary(gdp_agriculture)
    
    #print(sumoutcome)
    #print(sumgdpagri)
    
    outcome_intercept[1] = outcome$coefficients[1]
    outcome_slope1[1] = outcome$coefficients[2] #temperature
    outcome_slope2[1] = outcome$coefficients[3] #gdp_agriculture


    gdp_agriculture_intercept[1] = gdp_agriculture$coefficients[1]
    gdp_agriculture_slope1[1] = gdp_agriculture$coefficients[2]  #temperature
    gdp_agriculture_slope2[1] = gdp_agriculture$coefficients[3]  #


    Directeffect  = outcome_slope1[1]   #temperature
    
    Indirecteffect = outcome_slope2[1]*gdp_agriculture_slope1[1]     #gdp*temperature
    
    
    #print(Directeffect)
    #print(Indirecteffect)

   
    DE[i,j] = Directeffect 
    IE[i,j] = Indirecteffect   
    
    
    compIE1[i,j]  = outcome_slope2[1]  #gdp_agriculture
    compIE2[i,j]  = gdp_agriculture_slope1[1]   #temperature
    compIE3[i,j]  = outcome_slope1[1] #temperature

    intercept1[i,j] = outcome_intercept[1]
    intercept2[i,j] = gdp_agriculture_intercept[1]
    
  }
 
  
}


#print all direct effects
print(DE)

#print all indirect effects
print(IE)

