library(dplyr)
library(tidyr)
library(testthat)
library(lme4)
library(bbmle)

## Read and format data
d <- read.csv('data/isotria_long.csv')
flower_n <- d[complete.cases(d$size_t0, d$surv_t1, d$Habitat_Man),] %>%
  subset(size_t0 != 0)  %>%
  mutate(size_t0 = log(size_t0))

# finding out the maximum number of flowers
flower_n_max<- flower_n %>% filter(!is.na(n_flower_t1))
max(flower_n_max$n_flower_t1)

## model options
candidate_mods <- list(
  "null" = n_flower_t1 ~ size_t0 + (1 | year_t1),
  "site1" = n_flower_t1 ~ size_t0 * Site + (1 | year_t1),
  "site2" = n_flower_t1 ~ size_t0 + Site + (1 | year_t1),
  "yearsize" = n_flower_t1 ~ size_t0 + (size_t0 | year_t1),
  "yearsize2" = n_flower_t1 ~ size_t0 + (0 + size_t0 | year_t1)
)

### The AIC score for both sites are almoste equal. I therefor suggest using site2 as
# this one probably overfits less

flwrn_m    <- lapply( candidate_mods, 
                      function(x) glmer(x, data=flower_n, family='poisson') )

compare <- as.data.frame(AICtab(flwrn_m, weights = TRUE, base = TRUE))

### The AIC score for both sites are almoste equal. I therefor suggest using site2 as
# this one probably overfits less


summary(flwrn_m[[row.names(compare)[2]]])

plot(flwrn_m[[row.names(compare)[2]]])
