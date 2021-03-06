library(plyr); library(dplyr)
library(tidyr)
library(testthat)
library(lme4)
library(bbmle)
library(ggplot2)

## Read and format data
d <- read.csv('data/isotria_long.csv')
dorm <- d[complete.cases(d$size_t0, d$Habitat_Man, d$dormancy_t1),] %>%
  subset(size_t0 != 0)  %>%
  mutate(size_t0 = log(size_t0),
         Site = as.factor(Site))

#quick check if there are individuals that go into dormancy more than once
a <- ddply(dorm[which(!is.na(dorm$dormancy_t1)),], .(New_Tag), transform, N_dormancy = cumsum(dormancy_t1))
max(a$N_dormancy)

## model options

candidate_mods <- list(
  "null" = dormancy_t1 ~ size_t0 + (1 | year_t1),
  "site1" = dormancy_t1 ~ size_t0 * Site + (1 | year_t1),
  "site2" = dormancy_t1 ~ size_t0 + Site + (1 | year_t1),
  "yearsize" = dormancy_t1 ~ size_t0 + (size_t0 | year_t1),
  "yearsize2" = dormancy_t1 ~ size_t0 + (0 + size_t0 | year_t1)
)

dorm_m    <- lapply( candidate_mods, 
                     function(x) glmer(x, data=dorm, family='binomial') )

compare <- as.data.frame(AICtab(dorm_m, weights = TRUE, base = TRUE))

## Overview of best model
summary(dorm_m[[row.names(compare)[2]]])
plot(dorm_m[[row.names(compare)[2]]])


## exploratory plots
#source("../..52 Scrap code/plot_binned_prop_df.R")
source("/Users/sophi/Documents/idivproject/isotria/plot_binned_prop_df.R")

df_bin <- logitbin_df(dorm, resp = dormancy_t1, xvar = size_t0, year_t1, n = 50)

ggplot(df_bin, aes(x = size_t0, y = dormancy_t1)) + geom_point() + facet_wrap(vars(year_t1))


