# population growthrate per year
#load packages
library(ggplot2)
library(testthat)
library(lme4)
library(bbmle)
library(plyr); library(dplyr)
library(tidyr)
library(lme4)
options(stringsAsFactors = F)

# read data
data<-read.csv('data/isotria_long.csv')

# exclude NA from the survival data
survivers <- data[complete.cases( data$surv_t1),]

# creating a dataframe that holds the sum of surviving and there by living plants for every year (n_now)
alive<-survivers  %>% group_by(year_t1)%>% summarise(n_now=sum(surv_t1,na.rm=TRUE))

# creating a second dataframe that holds the living plants of the last year as n_past in the next collum
alive2<-survivers %>% group_by (year_t1) %>% summarise(n_past=sum(surv_t1,na.rm=TRUE))

alive2$year_t1<-alive$year_t1+1  

#joining the two dataframes
df<-full_join(alive,alive2)

# creating a new colum that holds the growthrate (n_now/n_past) 
df$gr<-df$n_now/df$n_past  

# exclude NA
dfc<- df[complete.cases( df$gr),]

# getting the mean growthrate 
 mean_gr<-dfc$gr %>% log %>% mean %>% exp



    