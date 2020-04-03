library(ggplot2)
library(testthat)
library(lme4)
library(bbmle)
library(plyr); library(dplyr)
library(tidyr)
library(lme4)
library(reshape)
library(tidyr)
library(dplyr)
library(readr)
install.packages("tidyverse")
library(tidyverse)
install.packages("climwin")
devtools::install_github("LiamDBailey/climwin")
library(climwin)

# reading the cimatdata

clima1<-read.csv('data/prism_isotria.csv')
head(clima1)

clima2<-read.csv('data/station_monthly.csv')
head(clima2)

#narrowing it down to Bridgeton and Portland
w_bridgton <-clima2 %>% filter(site=='BRIDGTON')


w_portland <-clima2 %>% filter(site=='PORTLAND')
 

# reading the plant data and ultering it to be useable in the modell
plants<-read.csv('data/isotria_long.csv')%>%
subset( size_t0 != 0 ) %>%
  mutate( size_t0 = log(size_t0),
          size_t1 = log(size_t1),
          Site = as.factor(Site)) %>% 
  mutate( size_t1 = replace( size_t1,
                             size_t1 == -Inf,
                             NA) )

# creating a colum for the Date that climwin can use
plants$Date<- paste(01,06,plants$year_t1,sep='/')

plants<-plants%>%filter(!is.na(plants$size_t0)) 
plants<-plants%>%filter(!is.na(plants$size_t1)) 


w_bridgton$Date<- paste(15,w_bridgton$month,w_bridgton$year,sep='/')


# changing the variables in to colums
weather_b<-w_bridgton %>% pivot_wider(names_from = variable, values_from = value)
weather_b<-weather_b%>% filter(!is.na(weather_b$tmean))
View(weather_b)

#creating an object that holds the results of slidingwin which is a part of climwin
results<-slidingwin(
  baseline=lmer(size_t1 ~ size_t0 + (size_t0 | year_t1), data = plants),
  xvar= list(Rain=weather_b$ppt,Temp=weather_b$tmean),
  type = "absolute", 
  range = c(10, 0),
  stat = c("mean","slope"),
  func = c("lin","quad"),
  refday = c(30, 6),
  cinterval = "month",
  cdate = weather_b$Date, bdate = plants$Date)

# getting an overvew over results
output<-results
output$combos  
head(output[[1]]$Dataset)


summary(output[[1]]$BestModel)
head(output[[1]]$BestModelData)
plot(output[[1]]$BestModel)


#plotting the results

plotall(
                 dataset = output[[5]]$Dataset, 
                 bestmodel = output[[5]]$BestModel,
                 bestmodeldata = output[[5]]$BestModelData,
                 title=output$combos[5,])




