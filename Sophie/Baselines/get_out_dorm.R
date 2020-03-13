library(plyr); library(dplyr)
library(tidyr)
library(testthat)
library(lme4)
library(bbmle)
library(ggplot2)

## Read and format data
d <- read.csv('data/isotria_long.csv')
dorm <- d[which( d$surv_t1 == 1),] 


# quick check if there are individuals that remain in dormancy more than one year
a <- ddply(dorm[which(!is.na(dorm$remain_dorm_t1)),], .(New_Tag), transform, duration_dormancy = cumsum(remain_dorm_t1))

max(a$duration_dorm)


# get out of dormancy probability

p_out <- mean(dorm$remain_dorm_t1, na.rm = TRUE)

## exploratory plots
source("../../52 Scrap code/plot_binned_prop_df.R")


ggplot(df_bin, aes(x = size_t0, y = dormancy_t1)) + geom_point() + facet_wrap(vars(year_t1))

