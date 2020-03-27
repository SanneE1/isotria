library(dplyr)
library(tidyr)
library(testthat)
library(lme4)
library(bbmle)

## Read and format data
d <- read.csv('data/isotria_long.csv')
growth <- d[complete.cases(d$size_t0, d$size_t1, d$Habitat_Man),] %>%
  subset(stage_t0 == "plant" & stage_t1 == "plant" & size_t0 != 0 & size_t1 != 0)  %>%
  mutate(size_t0 = log(size_t0),
         size_t1 = log(size_t1),
         Site = as.factor(Site))

## model options

candidate_mods <- list(
  "null" = size_t1 ~ size_t0 + (1 | year_t1),
  "site1" = size_t1 ~ size_t0 * Site + (1 | year_t1),
  "site2" = size_t1 ~ size_t0 + Site + (1 | year_t1),
  "yearsize" = size_t1 ~ size_t0 + (size_t0 | year_t1),
  "yearsize2" = size_t1 ~ size_t0 + (0 + size_t0 | year_t1)
)

growth_m    <- lapply( candidate_mods, 
                     function(x) lmer(x, data=growth) )

compare <- as.data.frame(AICtab(growth_m, weights = TRUE, base = TRUE))
summary(growth_m[[row.names(compare)[1]]])
plot(growth_m[[row.names(compare)[1]]])

