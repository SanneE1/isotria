library(dplyr)
library(tidyr)
library(testthat)
library(lme4)
library(bbmle)

## Read and format data
d <- read.csv('data/isotria_long.csv')
flower_p <- d %>%
  mutate(size_t0 = log(size_t0),
         Site = as.factor(Site))

flower_p$size_t0[which(flower_p$size_t0 < -999)] <- NA
flower_p$size_t1[which(flower_p$size_t1 < -999)] <- NA

## model options

candidate_mods <- list(
  "null" = flower_t1 ~ size_t0 + (1 | year_t1),
  "site1" = flower_t1 ~ size_t0 * Site + (1 | year_t1),
  "site2" = flower_t1 ~ size_t0 + Site + (1 | year_t1),
  "yearsize" = flower_t1 ~ size_t0 + (size_t0 | year_t1),
  "yearsize2" = flower_t1 ~ size_t0 + (0 + size_t0 | year_t1)
)

flwrp_m    <- lapply( candidate_mods, 
                     function(x) glmer(x, data=flower_p, family='binomial') )

compare <- as.data.frame(AICtab(flwrp_m, weights = TRUE, base = TRUE))

summary(flwrp_m[[row.names(compare)[1]]])

plot(flwrp_m[[row.names(compare)[1]]])
