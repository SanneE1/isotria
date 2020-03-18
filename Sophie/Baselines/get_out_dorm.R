library(plyr); library(dplyr)
library(tidyr)
library(testthat)
library(lme4)
library(bbmle)
library(ggplot2)

## Read and format data
d <- read.csv('data/isotria_long.csv')

head(d)
dorm <- d[which( d$surv_t1 == 1),] 
dorm$size_t0
head(isotria_long)
# quick check if there are individuals that remain in dormancy more than one year
a <- ddply(dorm[which(!is.na(dorm$remain_dorm_t1)),], .(New_Tag), transform, duration_dormancy = cumsum(remain_dorm_t1))
dorm$remain_dorm_t1
max(a$duration_dorm)


# get out of dormancy probability

p_stay <- mean(dorm$remain_dorm_t1, na.rm = TRUE) 
p_stay
p_out <- 1 - p_stay
p_out

## exploratory plots
#source("../../52 Scrap code/plot_binned_prop_df.R")
source("/Users/sophi/Documents/idivproject/isotria/plot_binned_prop_df.R")
df_bin <- logitbin_df(dorm, resp = dormancy_t1, xvar = size_t0, year_t1, Site)
df_bin <- df_bin %>%
 mutate(site = as.factor(Site))

ggplot(df_bin, aes(x = size_t0, y = dormancy_t1)) + geom_point() + facet_wrap(vars(year_t1))


